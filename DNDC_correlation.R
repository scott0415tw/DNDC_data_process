rm(list = ls())
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(tidyr)

# -----------------------------
# 函數：讀取 DNDC dnd 檔案並回傳寬表
# folder: 資料夾路徑
# encoding: 讀檔編碼 (預設 "BIG5")
# -----------------------------
read_dnd_to_wide <- function(folder, encoding = "BIG5") {
  # 找出所有 .dnd 檔案
  files <- list.files(folder, pattern = "\\.dnd$", full.names = TRUE)
  
  all_params <- list()
  
  for (file in files) {
    # 轉碼 Big5 -> UTF-8
    lines <- readLines(file, encoding = encoding)
    lines <- iconv(lines, from = encoding, to = "UTF-8")
    
    # 擷取參數
    param_records <- lapply(lines, function(line) {
      line <- trimws(line)
      if (grepl("^_+", line)) {
        parts <- strsplit(line, "\\s+", perl = TRUE)[[1]]
        if (length(parts) >= 2) {
          return(c(parts[1], paste(parts[-1], collapse = " ")))
        } else {
          return(c(parts[1], "")) # 沒有數值
        }
      } else {
        return(NULL)
      }
    })
    
    param_records <- param_records[!sapply(param_records, is.null)]
    
    if (length(param_records) > 0) {
      df <- as.data.frame(do.call(rbind, param_records), stringsAsFactors = FALSE)
      colnames(df) <- c("Parameter", "Value")
      
      # 處理重複參數名稱
      dup_id <- ave(seq_len(nrow(df)), df$Parameter, FUN = seq_along)
      df$Parameter <- paste0(df$Parameter, "_", dup_id)
      
      # 轉成 named vector
      vec <- setNames(df$Value, df$Parameter)
      all_params[[basename(file)]] <- vec
    }
  }
  
  # 合併成寬表
  df_wide <- bind_rows(
    lapply(all_params, function(x) as.data.frame(t(x), stringsAsFactors = FALSE)),
    .id = "Version"
  )
  
  return(df_wide)
}

# 資料夾路徑
folder <- "C:/Users/USER/Desktop/力瑜實驗室/DNDC/correlation_test/dndc_porosity_test"
df_wide <- read_dnd_to_wide(folder)
cat("寬格式資料共有", nrow(df_wide), "列\n")


# -----------------------------
# 4. 將數值轉成 numeric，排除 Version 欄
# -----------------------------
df_num <- df_wide
param_cols <- setdiff(names(df_num), "Version")
df_num[param_cols] <- lapply(df_num[param_cols], function(x) as.numeric(as.character(x)))

# -----------------------------
# 5. 計算每個參數的範圍（最大值 - 最小值），只保留變化量 != 0 的參數
# -----------------------------
param_range <- sapply(df_num[param_cols], function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
changed_params <- names(param_range[param_range != 0])
df_num_changed <- df_num[, c("Version", changed_params)]

# -----------------------------
# 6. 轉成長表 long format
# -----------------------------
df_long <- melt(df_num_changed, id.vars = "Version", variable.name = "Parameter", value.name = "Value")
df_long$Value <- as.numeric(df_long$Value)

# -----------------------------
# 7. 排序 Version（依檔名數字後綴排序）並轉 factor
# -----------------------------
df_long$Version_num <- as.numeric(str_extract(df_long$Version, "(?<=-)\\d+"))
df_long <- df_long %>%
  arrange(Version_num) %>%
  mutate(Version = factor(Version, levels = unique(Version)))

# -----------------------------
# 8. 畫折線圖
# -----------------------------
ggplot(df_long, aes(x = Version, y = Value, color = Parameter, group = Parameter)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "升高porosity (10%/次)", x = "版本", y = "參數數值")


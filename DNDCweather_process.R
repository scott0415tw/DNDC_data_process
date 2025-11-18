rm(list = ls())

input_path ="C:\\Users\\USER\\Desktop\\力瑜實驗室\\DNDC\\raw_weather\\2024_all"
output_path = "C:\\Users\\USER\\Desktop\\力瑜實驗室\\DNDC"
file_list <- list.files(input_path, full.names = TRUE)

need_column = c("ymd", "T Min", "T Max", "Precp", "RH", "GloblRad", "WS")
#day, maxT, minT, prec, WindSpeed


# ObsTime, StnPres, SeaPres, Temperature, Td, dew, point, RH, WS, WD, 
# WSGust, WDGust, Precp, PrecpHour, SunShine, GloblRad, Visb, UVI, Cloud Amount, TxSoil0cm, TxSoil5cm, TxSoil10cm, TxSoil20cm, TxSoil30cm, TxSoil50cm, TxSoil100cm



all_data = lapply(file_list, function(file){
  library(dplyr)
  library(lubridate)
  
  data = read.csv(file, header = FALSE, stringsAsFactors = FALSE)
  colnames(data) = data[2,]
  data = data[-c(1,2), ]
  
  file_parts <- basename(file) %>%  # 466881-2024-08.csv
    sub(".csv", "", .) %>%          # 466881-2024-08
    strsplit("-") %>%               # [466881, 2024, 08]
    unlist()
  
  data$ymd = paste0(file_parts[2], "/", file_parts[3], "/", data$ObsTime) %>%
    ymd()
  
  data = data[, (colnames(data)
                 %in% need_column)] #根據need_column選擇需要的欄位
  
}
)


out_data = bind_rows(all_data)

out_data[] <- lapply(out_data, function(col) {
  if (is.character(col)) {
    col[col == "--"] = NA
  }
  return(col)
})

# T 表示微量(小於 0.5mm)，x 表故障，& 代表降水量資料累積於後，V 表風向不定，/表不
# 明，...表無觀測
out_data$Precp[out_data$Precp %in% c( "x", "&", "/", "...")] = NA
out_data$Precp[out_data$Precp == "T"] = 0

out_data = out_data %>%
  select(ymd, `T Max`, `T Min`, Precp, WS, GloblRad, RH)

# out_data = out_data[out_data$Jday >= as.Date("2024-03-18") & out_data$Jday <= as.Date("2024-06-20"), ]
out_data$ymd = yday(out_data$ymd) #轉為累計日數



# n <- sum(complete.cases(out_data))
# 
# if (n < 365) {
#   # 重複 out_data，直到超過 365 天
#   times <- ceiling(365 / n)
#   out_data_filled <- out_data[rep(1:n, times), ]
#   
#   # 截斷到 365 天
#   out_data <- out_data_filled[1:365, ]
#   
#   # 修正 Jday 變成 1:365
#   out_data$Jday <- 1:365
# }

file_name = paste0(output_path, "//2024weather.csv")
write.csv(out_data, file_name, fileEncoding = "utf8", row.names = F)

# ====== 製作DNDC格式氣候檔案（Format 2） ======
# out_data$Jday <- as.numeric(out_data$Jday)
# out_data$t_max <- as.numeric(out_data$t_max)
# out_data$t_min <- as.numeric(out_data$t_min)
# out_data$precp <- as.numeric(out_data$precp)

out_data[] <- lapply(out_data, as.numeric)


output_txt_file <- paste0(output_path, "//2024weather.txt")
writeLines("ANKENG", output_txt_file)  # 第一行地點名
write.table(out_data,
            file = output_txt_file,
            append = TRUE,
            row.names = FALSE,
            col.names = FALSE,
            sep = "\t")


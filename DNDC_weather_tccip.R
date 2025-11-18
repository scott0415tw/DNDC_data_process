rm(list = ls())

main_folder = "C:\\Users\\USER\\Desktop\\力瑜實驗室\\DNDC\\20_years_weathr"
sub_folders = list.dirs(main_folder, recursive = FALSE, full.names = TRUE)
sub_names = basename(sub_folders)
assembly_folder = "C:\\Users\\USER\\Desktop\\力瑜實驗室\\DNDC\\assembly"

# 調取MAin folder
# 讀出個資料夾的CSV LIST
# 對每一CSV進行
# 座標篩選
# 日期轉換
# 寬轉長格式
# 各自存到新list, new_T_min......

# 各自從新list挑出特定年分進行合併left_join(by = date)


# test_folder = "C:\\Users\\USER\\Desktop\\力瑜實驗室\\DNDC\\20_years_weathr\\MinT"
# test_data =  "C:\\Users\\USER\\Desktop\\力瑜實驗室\\DNDC\\20_years_weathr\\MinT\\TReAD_日資料_新北市_最低溫_1980.csv"


#POINT (121.5216 24.96396)
weather_process = function(csv_folder, lon_val = 121.52158 , lan_val =24.96396 ){
  library(dplyr)
  library(lubridate)
  library(stringr)
  csv_list = list.files(csv_folder, pattern = "\\.csv$", full.names = T)
  
  data_list = list()
  
  # 讀取folder
  message("正在讀取", basename(csv_folder))
  for (file in csv_list) {
    message("正在讀取", basename(file))
    
    data <- read.csv(file, fileEncoding = "utf8")
    
    trans_data = data %>%
      filter(LON == lon_val, LAT == lan_val ) %>%
      {as.data.frame(t(.))} %>%
      slice(-c(1,2))%>%
      rename_with(~ basename(csv_folder), "V1")
    
    trans_data$Jday = sub("^X", "", rownames(trans_data)) %>%
      as.Date( format = "%Y%m%d") %>%
      yday()
    
    rownames(trans_data) = NULL
    data_name = str_extract(basename(file), "\\d{4}(?=\\.csv$)")# "TReAD_日資料_新北市_最低溫_1980.csv" to "1980"
    
    
     data_list[[data_name]]  = trans_data 
    
  }
  
  message( basename(csv_folder), "處理完成")
  return(data_list)
}
  
# test = weather_process(test_folder)


all_data_list <- list()
for (folder in sub_folders) {
  all_data_list[[basename(folder)]] <- weather_process(folder)
}


year = c(2003:2023)
final_list = list()

for (y in year) {
  y <- as.character(y)
  year_list <- lapply(sub_names, function(f) all_data_list[[f]][[y]])
  
  # 左合併
  library(dplyr)
  final_list[[y]] <- Reduce(function(x, y) left_join(x, y, by = "Jday"), year_list)
  
  # 指定欄位順序
  desired_order <- c(
    "Jday", "MaxT", "MinT", "Prec", "WindSpeed", "Radi", "Humi"
  )
  
  # 根據實際欄位名稱匹配順序（防止部分資料不存在）
  col_order <- intersect(desired_order, names(final_list[[y]]))
  final_list[[y]] <- final_list[[y]][, col_order, drop = FALSE] %>%
    slice(c(1:365))
  final_list[[y]][] = lapply(final_list[[y]], as.numeric)
  
  #check NA
  if (any(is.na(final_list[[y]]))) {
    print(is.na(final_list[[y]]))
  }
  #輸出成文字檔
  out_filename <- paste0("C:\\Users\\USER\\Desktop\\力瑜實驗室\\DNDC\\weather\\20y\\", y, "weather.txt")
  writeLines("ANKENG", out_filename)  # 第一行地點名
  write.table(
    final_list[[y]],
    file = out_filename,
    sep = "\t",
    append = TRUE,
    row.names = FALSE,
    col.names = F,
    quote = FALSE,
  )
message("已輸出", y, "weather.txt")
}
list2env(final_list, envir = globalenv())





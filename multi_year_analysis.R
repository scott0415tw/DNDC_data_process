rm(list = ls())
library(dplyr)
library(ggplot2)

setwd("C:\\Users\\USER\\Desktop\\力瑜實驗室\\DNDC")

test_name = "sup_15f_5c_a_1030"
batch_num = ""
year_period = c(16:20) # last 5 years

res_path <- file.path("dndc_output", test_name)

#### water ####
water_list = list.files(res_path, pattern = "Day_SoilWater_", full.names = TRUE)

water_list <- lapply(water_list, function(file) {
  df_name = basename(file)
  df <- read.csv(file, header = FALSE)
  df <- df %>%
    slice(-1) %>%                                # 移除第一列
    { setNames(.[-1, ], as.character(.[1, ])) } %>%  # 設定欄名
    mutate(Day = as.numeric(Day))                # 將 Day 轉成數值
})




water = read.csv(paste0("dndc_output\\", test_name, "\\Day_SoilWater_", year, ".csv"), header = F)
water = water %>%
  slice(-1) %>%
  { setNames(.[-1, ], as.character(.[1, ])) } %>%
  mutate(Day = as.numeric(Day))







# 累計的天數偏移量
offset <- 0

for (y in sort(years)) {
  file_path <- file.path(res_path, paste0("Day_SoilWater_", y, ".csv"))
  
  # 確保檔案存在
  if (!file.exists(file_path)) next
  
  # 讀取資料
  df <- read.csv(file_path)
  
  
  # 假設日期欄位叫 Day
  if (!"Day" %in% names(df)) stop("檔案中缺少 Day 欄位！")
  
  # 調整日期為連續日數
  df$Day_global <- df$Day + offset
  
  # 更新 offset (假設每年365天，可依實際調整為 366)
  offset <- max(df$Day_global)
  
  # 新增 year 欄位方便之後分辨
  df$Year <- y
  
  # 儲存
  data_list[[as.character(y)]] <- df
}

# 合併所有年份資料
merged_df <- do.call(rbind, data_list)



#### soil ####
library(data.table)

soilclimate = read.csv(paste0("dndc_output\\", test_name, "\\Day_SoilClimate_", year, ".csv"), header = F)

new_names <- ifelse(is.na(soilclimate[2, ]), soilclimate[3, ], 
                    paste0(soilclimate[2, ], "_", soilclimate[3, ]))
new_names[1] = as.character("Day")
new_names[6:11] <- paste0("Soil_temperature_", new_names[6:11])
new_names[13:18] <- paste0("Soil_moisture(wfps)_", new_names[13:18])
new_names[20:24] <- paste0("Soil_oxygen(mol/L)_", new_names[20:24])
new_names[26:30] <- paste0("Soil_Eh(mv)_", new_names[26:30])
new_names[33:37] <- paste0("Ice(wfps)_", new_names[33:37])
new_names[43:47] <- paste0("Soil_pH_", new_names[43:47])
soilclimate = soilclimate[-c(1:3),]
names(soilclimate) = new_names

#### soilC ####


soil_c <- read.csv(paste0("dndc_output\\", test_name, "\\Day_SoilC_", year, ".csv"), header = FALSE)

# 以第2列作為欄名
colnames(soil_c) <- as.character(unlist(soil_c[2, ]))

# 刪除第1、2列
soil_c <- soil_c[-c(1, 2), ]

# 刪除第15到19欄
soil_c <- soil_c[, -c(15:19)]


#### ghg_simulation ####
ghg_sim = read.csv(paste0("dndc_output\\", test_name, "\\Day_SoilC_", year, ".csv"), fileEncoding = "utf8", header = F)
library(dplyr)
ghg_sim = ghg_sim %>%
  slice(-1) %>%
  { setNames(.[-1, ], as.character(.[1, ])) }

# ghg_sim$date <- as.Date(paste0(2024, "-01-01")) + (as.numeric(ghg_sim$Day) - 1)
ghg_sim$`CH4-flux` = as.numeric(ghg_sim$`CH4-flux`)
ghg_sim$Day = as.numeric(ghg_sim$Day)

ghg_sim = ghg_sim[,-c(15:19)] %>%
  select(Day,`CH4-flux`, `SOC0-10cm`)

ghg_sim = ghg_sim%>%
  rename(sim_ch4 = `CH4-flux`,
         top_soc = `SOC0-10cm`)


#### observation ####
real = read.csv("氣體排放資料\\flux_data_raw_20231121-20241229.csv")

real$date = as.Date(real$ymd_h)

real = real %>%
  select(ymd_h, date, chamber, flux_plant_CH4, flux_plant_N2O, flux_plant_CO2)

# ## chamber 各自加總
# sep_chamber_sum = function(df, c_no, start, end){
#   library(dplyr)
#   library(lubridate)
# 
#   df = df %>%
#     filter(chamber %in% c_no) %>%
#     mutate(date = as.Date(ymd_h)) %>%
#     filter(date >= as.Date(start),
#            date <= as.Date(end)) %>%
#     dplyr::group_by(chamber) %>%
#     summarise(
#       ch4_real = sum(flux_plant_CH4, na.rm = TRUE),
#       n2o_real = sum(flux_plant_N2O, na.rm = TRUE),
#       co2_real = sum(flux_plant_CO2, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
# 
#     return(df)
# }
# 
# spring_chamber_sum = sep_chamber_sum(real,c(1, 3, 9, 13, 16, 17),
#                                     start = "2024-03-18",
#                                     end = "2024-07-15")
# fall_chamber_sum = sep_chamber_sum(real,c(1, 3, 9, 13, 16, 17),
#                                      start = "2024-08-10",
#                                      end = "2024-12-06")


chamber_sum = function(df, c_no){
  library(dplyr)
  library(lubridate)
  
  df = df %>%
    filter(chamber %in% c_no) %>%
    mutate(date = as.Date(ymd_h)) %>%
    dplyr::group_by(chamber, date) %>%
    summarise(
      ch4_real = sum(flux_plant_CH4, na.rm = TRUE),
      n2o_real = sum(flux_plant_N2O, na.rm = TRUE),
      co2_real = sum(flux_plant_CO2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(year(date) == 2024)
  
  return(df)
}

#sep -> seperate -> 分chamber
scatter_awd = chamber_sum(real,c(13, 16, 17) ) 
scatter_con = chamber_sum(real, c(1, 3, 9))

scatter_merge = rbind(scatter_awd, scatter_con)

scatter_merge$manage <- ifelse(
  scatter_merge$chamber %in% c(13, 16, 17),
  "awd",
  "con"
)


avgsum_fun = function(df, c_no= c(13, 16, 17)){
  library(dplyr)
  library(lubridate)
  
  df = df %>%
    filter(chamber %in% c_no) %>%
    group_by(ymd_h) %>%
    summarise(
      CH4_avg = mean(flux_plant_CH4, na.rm = TRUE),
      N2O_avg = mean(flux_plant_N2O, na.rm = TRUE),
      CO2_avg = mean(flux_plant_CO2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(date = as.Date(ymd_h)) %>%
    dplyr::group_by(date) %>%
    summarise(
      ch4_real = sum(CH4_avg, na.rm = TRUE),
      n2o_real = sum(N2O_avg, na.rm = TRUE),
      co2_real = sum(CO2_avg, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Day = yday(date)) %>%
    filter(year(date) == 2024)
  return(df)
}

real_awd = avgsum_fun(real, c_no = c(13, 16, 17))

real_con = avgsum_fun(real, c_no = c(1, 3, 9))

real_merge <- real_con %>%
  rename(
    ch4_real_con = ch4_real,
    n2o_real_con = n2o_real,
    co2_real_con = co2_real
  ) %>%
  left_join(
    real_awd %>%
      rename(
        ch4_real_awd = ch4_real,
        n2o_real_awd = n2o_real,
        co2_real_awd = co2_real
      ),
    by = c("date", "Day")
  )

# import weather data in dndc form (Jday)
weather = read.csv("raw_weather\\dndctest.csv", fileEncoding = "utf8")

#合併所有data frame
library(purrr)
# 先統一 Day 欄位型別
real_merge$Day <- as.numeric(real_con$Day)
ghg_sim$Day  <- as.numeric(ghg_sim$Day)
water$Day    <- as.numeric(water$Day)
soilclimate$Day <- as.numeric(soilclimate$Day)
soil_c$Day = as.numeric(soil_c$Day)

# 再合併
library(purrr)
all_df <- list(real_merge, ghg_sim, water, soilclimate, soil_c) %>%
  reduce(full_join, by = "Day")

#### total ####

# 一期作
library(dplyr)
library(tibble)

# 計算四個區段的總和與平均
periods <- list(
  spring = c("2024-03-18", "2024-07-10"),
  fall = c("2024-08-10", "2024-12-06")
)
types <- c("real" = "ch4_real_con", "sim" = "sim_ch4")

# 使用 map_dfr 直接合併
total_df <- map_dfr(names(periods), function(p){
  duration = periods[[p]]
  map_dfr(names(types), function(t) {
    df <- all_df %>%
      filter(date >= as.Date(duration[1]) & date <= as.Date(duration[2])) %>%
      summarise(total = sum(.data[[types[t]]], na.rm = TRUE),
                mean  = mean(.data[[types[t]]], na.rm = TRUE))
    
    tibble(period = p, 
           type = t,
           total = df$total, 
           mean = df$mean)
  }
  )
}
)


#### plot ####
#total flux
real_total_1 = subset(total_df, period == "spring"& type == "real")$total
real_total_2 = subset(total_df, period == "fall"& type == "real")$total

sim_total_1 = subset(total_df, period == "spring"& type == "sim")$total
sim_total_2 = subset(total_df, period == "fall"& type == "sim")$total

# error bar
df_summary <- scatter_merge[scatter_merge$manage == "con",] %>%
  group_by(date) %>%
  summarise(
    mean_ch4 = mean(ch4_real, na.rm = TRUE),
    sd_ch4   = sd(ch4_real, na.rm = TRUE)
  )

library(ggplot2)
library(dplyr)



date_spring = c("2024-03-18", "2024-05-02", "2024-05-13", "2024-06-07", "2024-07-10")
date_fall = c("2024-08-10", "2024-09-23", "2024-10-19", "2024-11-01", "2024-11-05", "2024-11-27", "2024-12-06")
date_year = c(date_spring, date_fall)

uni_ymin = -Inf
uni_ymax = 0.05
crop_high = 1
mana_high = 14.5 # management
uni_size = 3

test_batch_name <- ifelse(!is.null(batch_num) && nzchar(batch_num),
                          paste0(test_name, "_", batch_num),
                          test_name)


# 基礎圖層
p_base <- ggplot(data = all_df) +
  labs(title = "Conventional CH4 Flux (20years)",
       y = "Flux", x = "Date") +
  
  scale_x_date(breaks = as.Date(date_year), date_labels = "%m/%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  labs(subtitle = paste0("test name = ", test_batch_name, ", year = ", year, "\n","1st real mean sum: ", round(real_total_1,3),
                         " kg C/ha | 1st simulation: ", round(sim_total_1,3), " kg C/ha\n",
                         "2nd real mean sum: ", round(real_total_2,3),
                         " kg C/ha | 2nd simulation: ", round(sim_total_2,3), " kg C/ha"),
       y = "Flux",
       x = "Date"
  )

# 背景 rect
rect_layers <- list(
  # 曬田
  geom_rect(aes(xmin = as.Date("2024-05-02"), xmax = as.Date("2024-05-13"),
                ymin = uni_ymin, ymax = uni_ymax), fill = "#FFAF60", alpha = 0.01),
  geom_rect(aes(xmin = as.Date("2024-06-20"), xmax = as.Date("2024-07-10"),
                ymin = uni_ymin, ymax = uni_ymax), fill = "#FFAF60", alpha = 0.01),
  geom_rect(aes(xmin = as.Date("2024-09-23"), xmax = as.Date("2024-10-19"),
                ymin = uni_ymin, ymax = uni_ymax), fill = "#FFAF60", alpha = 0.01),
  geom_rect(aes(xmin = as.Date("2024-11-01"), xmax = as.Date("2024-11-05"),
                ymin = uni_ymin, ymax = uni_ymax), fill = "#FFAF60", alpha = 0.01),
  geom_rect(aes(xmin = as.Date("2024-11-27"), xmax = as.Date("2024-12-06"),
                ymin = uni_ymin, ymax = uni_ymax), fill = "#FFAF60", alpha = 0.01),
  
  # Flooding
  geom_rect(aes(xmin = as.Date("2024-03-01"), xmax = as.Date("2024-05-02"),
                ymin = uni_ymin, ymax = uni_ymax), fill = "#66B3FF", alpha = 0.01),
  geom_rect(aes(xmin = as.Date("2024-05-13"), xmax = as.Date("2024-06-20"),
                ymin = uni_ymin, ymax = uni_ymax), fill = "#66B3FF", alpha = 0.01),
  geom_rect(aes(xmin = as.Date("2024-07-22"), xmax = as.Date("2024-09-23"),
                ymin = uni_ymin, ymax = uni_ymax), fill = "#66B3FF", alpha = 0.01),
  geom_rect(aes(xmin = as.Date("2024-10-19"), xmax = as.Date("2024-11-01"),
                ymin = uni_ymin, ymax = uni_ymax), fill = "#66B3FF", alpha = 0.01),
  geom_rect(aes(xmin = as.Date("2024-11-05"), xmax = as.Date("2024-11-27"),
                ymin = uni_ymin, ymax = uni_ymax), fill = "#66B3FF", alpha = 0.01)
)

# CH4 資料圖層
data_layers <- list(
  
  geom_errorbar(data = df_summary,
                aes(x = date, ymin = mean_ch4 - sd_ch4, ymax = mean_ch4 + sd_ch4),
                color = "lightgray", width = 3, size = 0.05),
  
  geom_point(data = df_summary,
             aes(x = date, y = mean_ch4, color = "Observed mean"),
             shape = 18, size = 1),
  
  geom_line(data = all_df,
            aes(x = date, y = sim_ch4, color = "Simulated"),
            size = 0.6),
  # # 氧化電位
  # geom_line(data = all_df,
  #           aes(x = date, y = as.numeric(`Soil_Eh(mv)_1`)/100, color = "Eh(mv)/100"),
  #           size = 0.6),
  
  # soil ,moisture
  # geom_line(data = all_df,
  #           aes(x = date, y = as.numeric(`Soil_moisture(wfps)_1`), color = "soil moist"),
  #           size = 0.6),
  # 
  # # # soil oxygen
  # # geom_line(data = all_df,
  # #           aes(x = date, y = as.numeric(`Soil_oxygen(mol/L)_1`)*10000, color = "o2*10000"),
  # #           size = 0.6),
  # 
  # # top soc
  # geom_line(data = all_df,
  #           aes(x = date, y = as.numeric(`top_soc`)/10000, color = "top_soc/10000"),
  #           size = 0.6),
  # #soc20~30
  # geom_line(data = all_df,
  #           aes(x = date, y = as.numeric(`SOC20-30cm`)/10000, color = "soc 20-30"),
  #           size = 0.6),
  # #soc30~40
  # geom_line(data = all_df,
  #           aes(x = date, y = as.numeric(`SOC30-40cm`)/10000, color = "soc 30-40"),
  #           size = 0.6),
  
  
  scale_color_manual(name = "Legend",
                     values = c("Observed mean" = "black",
                                "Simulated" = "red",
                                "Eh(mv)/100" = "green",
                                "soil moist" = "purple",
                                "o2*10000" = "blue",
                                "top_soc/10000"= "brown",
                                "soc 20-30" = "orange",
                                "soc 30-40" = "pink"))
)


# 最終圖
p_final <- p_base + rect_layers + data_layers
p_final


uni_ymax = min(as.numeric(all_df$top_soc))*0.98
ggplot(data = all_df) +
  rect_layers +  # <-- 加在這裡！
  geom_line(data = all_df, aes(x = date, y = as.numeric(top_soc)), color = "red") +
  labs(
    title = paste0("top_soc (0–10 cm), ", test_name, "\n"),
    x = "Date", 
    y = "Top SOC (KgC/Ha)"
  ) +
  scale_x_date(breaks = as.Date(date_year), date_labels = "%m/%d") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )


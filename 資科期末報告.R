# 下載並描述TSIS資料 -----

library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

# 下載數據
url <- "https://tsis.dbas.gov.taipei/statis/webMain.aspx?sys=220&ymf=10501&kind=21&type=0&funid=a04004701&cycle=1&outmode=12&compmode=0&outkind=1&deflst=2&nzo"
download.file(url, destfile = "data.csv")

# 讀取數據
data <- read_csv("data.csv")

# 整體描述
total_cols <- ncol(data)
total_rows <- nrow(data)

# 創建描述列表
description <- list(
  total_columns = total_cols,
  total_rows = total_rows
)

# 逐欄描述函數
describe_column <- function(column) {
  col_data <- data[[column]]
  na_count <- sum(is.na(col_data))
  na_percentage <- (na_count / total_rows) * 100
  
  if (is.numeric(col_data)) {
    summary_stats <- list(
      min = min(col_data, na.rm = TRUE),
      max = max(col_data, na.rm = TRUE),
      mean = mean(col_data, na.rm = TRUE),
      median = median(col_data, na.rm = TRUE),
      quantiles = quantile(col_data, na.rm = TRUE)
    )
    result <- list(
      type = "numeric",
      na_count = na_count,
      na_percentage = na_percentage,
      summary_stats = summary_stats
    )
  } else {
    unique_vals <- unique(col_data)
    if (length(unique_vals) < 10) {
      value_counts <- table(col_data, useNA = "ifany")
      value_percentages <- prop.table(value_counts) * 100
      result <- list(
        type = "categorical",
        na_count = na_count,
        na_percentage = na_percentage,
        value_counts = value_counts,
        value_percentages = value_percentages
      )
    } else {
      result <- list(
        type = "categorical",
        na_count = na_count,
        na_percentage = na_percentage,
        unique_count = length(unique_vals)
      )
    }
  }
  return(result)
}

# 描述每個欄位
for (col in names(data)) {
  description[[col]] <- describe_column(col)
}

# 檢視數據結構
glimpse(data)

# 返回描述結果
description

# 計算並排列每年的長期機構數量 -----

# 使用 stringr 套件提取年份
data <- data %>%
  mutate(year = str_extract(ymf, "\\d{4}"))

# 計算每年的長期機構數量
institution_count <- data %>%
  group_by(year) %>%
  summarise(institution_count = n_distinct(長期機構名稱))

# 按照多到少排列
institution_count <- institution_count %>%
  arrange(desc(institution_count))

# 顯示結果
institution_count

# 繪製年分與長期照護機構數量關係的長條圖 -----

# 載入必要的套件
library(ggplot2)

# 年份和機構數量的資料
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
institutions <- c(45, 48, 48, 48, 36, 24, 24, 14, 5)

# 創建資料框
data <- data.frame(years, institutions)

# 繪製長條圖
ggplot(data, aes(x = institutions, y = years)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "機構數量", y = "年份", title = "長期照護機構數量") +
  theme_minimal()


# 繪製年分與長期照護機構/實際進住人數關係的長條圖 -----

library(ggplot2)

# 年份和進住人數的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
population <- c(1833, 1974, 2302, 2192, 1593, 1427, 849, 458, 148)

# 創建數據框
data <- data.frame(years, population)

# 繪製長條圖
ggplot(data, aes(x = population, y = years)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "進住人數", y = "年份", title = "長期照護機構進住人數") +
  theme_minimal()

# 計算每年的養護機構/機構數量並排列 -----

# 載入必要的套件
library(ggplot2)

# 年份和機構數量的資料
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
institutions <- c(1241, 1189, 1173, 1156, 1135, 1114, 1096, 1084, 540)

# 創建資料框
data <- data.frame(years, institutions)

# 繪製長條圖
ggplot(data, aes(x = years, y = institutions)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "年份", y = "機構數量", title = "每年養護機構數量加總") +
  theme_minimal()

# 載入必要的套件
library(ggplot2)

# 年份和機構/實際進住人數的資料
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
institutions_population <- c(48999, 48568, 48235, 49148, 48957, 48033, 45199, 45263, 45281)

# 創建資料框
data <- data.frame(years, institutions_population)

# 繪製長條圖
ggplot(data, aes(x = years, y = institutions_population)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "年份", y = "養護機構/實際進住人數加總", title = "每年養護機構/實際進住人數加總") +
  theme_minimal()

# 載入必要的套件
library(ggplot2)

# 年份和安養機構/機構數的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
elderly_care_facilities <- c(3, 3, 3, 3, 3, 2, 2, 2, 2)

# 計算每年的安養機構/機構數量加總
total_facilities <- sum(elderly_care_facilities)
total_facilities

# 創建資料框
data <- data.frame(years, elderly_care_facilities)

# 繪製長條圖
ggplot(data, aes(x = years, y = elderly_care_facilities)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "年份", y = "安養機構/機構數量加總", title = "每年安養機構/機構數量加總") +
  theme_minimal()

# 年份和安養機構/機構數的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
total_facilities <- c(36, 36, 24, 24, 24, 24, 36, 36, 36)

# 繪製長條圖
barplot(total_facilities, names.arg = years, xlab = "年份", ylab = "安養機構/機構數量加總", main = "每年安養機構/機構數量加總", col = "skyblue")

# 年份和安養機構/實際進住人數加總的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
total_residents <- c(7784, 7793, 7969, 7958, 7733, 7619, 10435, 9909, 9032)

# 繪製長條圖
barplot(total_residents, names.arg = years, xlab = "年份", ylab = "安養機構/實際進住人數加總", main = "每年安養機構/實際進住人數加總", col = "lightgreen")

# 年份和失智照顧型機構數量加總的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
total_institutions <- c(12, 12, 12, 12, 12, 12, 12, 12, 3)

# 繪製長條圖
barplot(total_institutions, names.arg = years, xlab = "年份", ylab = "失智照顧型機構數量加總", main = "每年失智照顧型機構數量加總", col = "lightblue")

# 安裝必要的繪圖套件
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# 年份和實際進住人數加總的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
total_residents <- c(717, 728, 730, 708, 682, 690, 602, 490, 182)

# 創建數據框
data <- data.frame(Year = years, Residents = total_residents)

# 繪製長條圖
ggplot(data, aes(x = Year, y = Residents, fill = Year)) +
  geom_bar(stat = "identity") +
  labs(title = "每年失智照顧型機構實際進住人數加總", x = "年份", y = "實際進住人數加總") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 安裝必要的繪圖套件
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# 年份和老人公寓所數加總的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
total_apartments <- c(24, 24, 24, 24, 24, 24, 24, 24, 8)

# 創建數據框
data <- data.frame(Year = years, Apartments = total_apartments)

# 繪製長條圖
ggplot(data, aes(x = Year, y = Apartments, fill = Year)) +
  geom_bar(stat = "identity") +
  labs(title = "每年老人公寓所數加總", x = "年份", y = "所數加總") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 安裝必要的繪圖套件
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# 年份和老人公寓實際進住人數加總的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
total_residents <- c(1260, 1160, 1318, 1550, 1580, 1606, 1553, 1772, 592)

# 創建數據框
data <- data.frame(Year = years, Residents = total_residents)

# 繪製長條圖
ggplot(data, aes(x = Year, y = Residents, fill = Year)) +
  geom_bar(stat = "identity") +
  labs(title = "每年老人公寓實際進住人數加總", x = "年份", y = "實際進住人數加總") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 載入必要的套件
library(ggplot2)
library(dplyr)

# 年份、機構數量和進住人數的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
institutions <- c(45, 48, 48, 48, 36, 24, 24, 14, 5)
population <- c(1833, 1974, 2302, 2192, 1593, 1427, 849, 458, 148)

# 創建數據框
data <- data.frame(years, institutions, population)

# 創建雙軸圖
ggplot(data, aes(x = years)) +
  geom_bar(aes(y = institutions * 100, fill = "機構數量"), stat = "identity", position = "dodge") + 
  geom_line(aes(y = population, group = 1, color = "進住人數"), size = 1.5) +
  geom_point(aes(y = population, color = "進住人數"), size = 3) +
  scale_y_continuous(
    name = "進住人數",
    sec.axis = sec_axis(~./100, name = "機構數量")
  ) +
  scale_fill_manual(values = c("機構數量" = "skyblue")) +
  scale_color_manual(values = c("進住人數" = "red")) +
  labs(x = "年份", title = "長期照護機構數量與實際進住人數關係") +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "skyblue"),
    axis.title.y.left = element_text(color = "red"),
    legend.position = "top",
    legend.title = element_blank()
  )

# 載入必要的套件
library(ggplot2)

# 年份、機構數量和進住人數的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
institutions <- c(1241, 1189, 1173, 1156, 1135, 1114, 1096, 1084, 540)
institutions_population <- c(48999, 48568, 48235, 49148, 48957, 48033, 45199, 45263, 45281)

# 創建數據框
data <- data.frame(years, institutions, institutions_population)

# 創建雙軸圖
ggplot(data, aes(x = years)) +
  geom_bar(aes(y = institutions / 10, fill = "機構數量"), stat = "identity", position = "dodge", alpha = 0.7) + 
  geom_line(aes(y = institutions_population, group = 1, color = "進住人數"), size = 1.5) +
  geom_point(aes(y = institutions_population, color = "進住人數"), size = 3) +
  scale_y_continuous(
    name = "進住人數",
    sec.axis = sec_axis(~.*10, name = "機構數量")
  ) +
  scale_fill_manual(values = c("機構數量" = "skyblue")) +
  scale_color_manual(values = c("進住人數" = "red")) +
  labs(x = "年份", title = "養護機構數量與實際進住人數關係") +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "skyblue"),
    axis.title.y.left = element_text(color = "red"),
    legend.position = "top",
    legend.title = element_blank()
  )

# 載入必要的套件
library(ggplot2)

# 年份、機構數量和實際進住人數的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
elderly_care_facilities <- c(3, 3, 3, 3, 3, 2, 2, 2, 2)
total_residents <- c(7784, 7793, 7969, 7958, 7733, 7619, 10435, 9909, 9032)

# 創建數據框
data <- data.frame(years, elderly_care_facilities, total_residents)

# 創建雙軸圖
ggplot(data, aes(x = years)) +
  geom_bar(aes(y = elderly_care_facilities * 3000, fill = "機構數量"), stat = "identity", position = "dodge", alpha = 0.7) + 
  geom_line(aes(y = total_residents, group = 1, color = "進住人數"), size = 1.5) +
  geom_point(aes(y = total_residents, color = "進住人數"), size = 3) +
  scale_y_continuous(
    name = "進住人數",
    sec.axis = sec_axis(~./3000, name = "機構數量")
  ) +
  scale_fill_manual(values = c("機構數量" = "skyblue")) +
  scale_color_manual(values = c("進住人數" = "red")) +
  labs(x = "年份", title = "安養機構數量與實際進住人數關係") +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "skyblue"),
    axis.title.y.left = element_text(color = "red"),
    legend.position = "top",
    legend.title = element_blank()
  )

# 載入必要的套件
library(ggplot2)
library(reshape2)

# 年份、機構數量和實際進住人數的數據
years <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
total_institutions <- c(12, 12, 12, 12, 12, 12, 12, 12, 3)
total_residents <- c(717, 728, 730, 708, 682, 690, 602, 490, 182)

# 創建數據框
data <- data.frame(years, total_institutions, total_residents)

# 繪製雙軸圖
ggplot(data, aes(x = years)) +
  geom_bar(aes(y = total_institutions * 100, fill = "機構數量"), stat = "identity", position = "dodge", alpha = 0.7) +
  geom_line(aes(y = total_residents, group = 1, color = "實際進住人數"), size = 1.5) +
  geom_point(aes(y = total_residents, color = "實際進住人數"), size = 3) +
  scale_y_continuous(
    name = "實際進住人數",
    sec.axis = sec_axis(~./100, name = "機構數量")
  ) +
  scale_fill_manual(values = c("機構數量" = "lightblue")) +
  scale_color_manual(values = c("實際進住人數" = "red")) +
  labs(x = "年份", title = "失智照顧型機構數量與實際進住人數關係") +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "lightblue"),
    axis.title.y.left = element_text(color = "red"),
    legend.position = "top",
    legend.title = element_blank()
  )

# 安裝必要的繪圖套件
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# 年份和老人公寓/所數加總的數據
years_apartments <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
total_apartments <- c(24, 24, 24, 24, 24, 24, 24, 24, 8)

# 創建老人公寓/所數加總的數據框
data_apartments <- data.frame(Year = years_apartments, Apartments = total_apartments)

# 年份和老人公寓/實際進住人數加總的數據
years_residents <- c("105年", "106年", "107年", "108年", "109年", "110年", "111年", "112年", "113年")
total_residents <- c(1260, 1160, 1318, 1550, 1580, 1606, 1553, 1772, 592)

# 創建老人公寓/實際進住人數加總的數據框
data_residents <- data.frame(Year = years_residents, Residents = total_residents)

# 合併兩個數據框
merged_data <- merge(data_apartments, data_residents, by = "Year")

# 繪製折線圖
ggplot(merged_data, aes(x = Year)) +
  geom_line(aes(y = Apartments, color = "Apartments")) +
  geom_line(aes(y = Residents, color = "Residents")) +
  geom_point(aes(y = Apartments, color = "Apartments")) +
  geom_point(aes(y = Residents, color = "Residents")) +
  labs(title = "老人公寓/所數與老人公寓/實際進住人數趨勢", x = "年份", y = "數量") +
  scale_color_manual(name = "資料類型", values = c(Apartments = "blue", Residents = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





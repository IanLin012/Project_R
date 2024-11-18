#' 請同學讀取 `cl_info_other.csv` 並存入 cl_info 變數
#' 然後從cl_info的欄位data_dt的資料中萃取出資料的「年和月」，並存到欄位 year_month 中。
#' 並且最後只留下 year_month 與 mortgage_bal兩個欄位。
#' 這裡的data_dt 是收集資料的時間點，
#' mortgage_bal 則是房貸餘額。
library(dplyr)
cl_info2 <- local({
  cl_info <- read.csv("cl_info_other.csv")
  cl_info %>%
    # 將data_dt轉換為日期格式存入新增的year_month
    mutate(year_month = format(as.Date(data_dt, format="%Y-%m-%d"), "%Y-%m")) %>%
    # 留下year_month與mortgage_bal
    select(year_month, mortgage_bal)
})
cl_info2

stopifnot(class(cl_info2$year_month)[1] == "character")
stopifnot(ncol(cl_info2) == 2)
stopifnot(!is.null(cl_info2$mortgage_bal))

#' 請算出每個月份的 mortgage_bal 總和，
#' 並且把結果放在 mortgage_total_bal 欄位中。
#' 結果請依照月份由小到大做排序。
cl_info3 <- local({
  cl_info2 %>%
    group_by(year_month) %>%
    # 計算每個year_month的mortgage_bal總和，忽略缺失值(NA)
    summarise(mortgage_total_bal = sum(mortgage_bal, na.rm = TRUE)) %>%
    # 按year_month升序排列
    arrange(year_month)
})
cl_info3

stopifnot(nrow(cl_info3) == 98)
stopifnot(ncol(cl_info3) == 2)
stopifnot(!is.unsorted(cl_info3$year_month))

#' 請用學到的方法讀取`GDP.txt`的資料、整理資料，並把最後的結果存到變數`gdp`。
#' 提示：`GDP.txt`中的第一欄數據是年/季、第二欄數據是該季的GDP(百萬)。
#' 結果應該要有兩欄的數據，第一欄是年份，第二欄是我國每年的GDP，
#' 具體細節請參考最後的`stopifnot`的檢查事項。
#' 提示：拿掉數據中間的逗號，請用：`gsub(pattern = ",", replacement = "", x = <你的字串向量>)`
gdp <- local({
  # 從沒有標題列的檔案提取前132行和前2列(年份-季度、GDP)，跳過前4行
  gdp_data <- read.csv("GDP.txt", header = FALSE, skip = 4)[1:132, 1:2]
  # 設定gdp_data的列名
  names(gdp_data) <- c("year_quarter", "gdp")
  # 去除GDP的逗號
  gdp_data$gdp <- gsub(pattern = ",", replacement = "", gdp_data$gdp) %>%
    as.numeric %>%
    '*'(1000000)
  # 提取year_quarter的前4個字元到新增的year欄位
  gdp_data$year <- substring(gdp_data$year_quarter, 1, 4)
  gdp_data %>%
    group_by(year) %>%
    summarise(gdp = sum(gdp))
})
gdp

stopifnot(is.data.frame(gdp))
stopifnot(colnames(gdp) == c("year", "gdp"))
stopifnot(class(gdp$year) == "character")
stopifnot(class(gdp$gdp) == "numeric")
stopifnot(nrow(gdp) == 33)
stopifnot(range(gdp$year) == c("1981", "2013"))
stopifnot(range(gdp$gdp) == c(1810829,14564242) * 1000000)

#' cl_info的資料包含各家銀行的房貸餘額（mortgage_bal）資訊與資料建立的時間（data_dt）。
#' 請用學到的方法整理cl_info的資料，並把最後的結果整理至`cl_info_year`。
#' 結果應該要有兩欄的數據，第一欄是年份，第二欄是每年房貸餘額的值(請以每年的一月份資料為準)。
#' 具體細節請參考最後的`stopifnot`檢查事項。
cl_info_year <- local({
  cl_info <- read.csv("cl_info_other.csv")
  cl_info %>%
    mutate(data_dt = as.Date(data_dt), year = format(data_dt, "%Y"), month = format(data_dt, "%m")) %>%
    filter(month == "01") %>%
    group_by(year) %>%
    summarise(mortgage_total_bal = sum(mortgage_bal, na.rm = TRUE))
})
cl_info_year

stopifnot(is.data.frame(cl_info_year))
stopifnot(colnames(cl_info_year) == c("year", "mortgage_total_bal"))
stopifnot(class(cl_info_year$year) == "character")
stopifnot(class(cl_info_year$mortgage_total_bal) == "numeric")
stopifnot(nrow(cl_info_year) == 9)
stopifnot(range(cl_info_year$year) == c("2006", "2014"))
stopifnot(range(cl_info_year$mortgage_total_bal) == c(3.79632e+12, 5.726784e+12))

#' 最後，請同學用這門課程所學的技術整合`gdp`與`cl_info`的資料，
#' 並計算出房貸餘額與gdp的比率（mortgage_total_bal / gdp）。
#' 請計算2006-2014年一月房貸餘額與 GDP 的比率。
#' 計算房貸餘額與 GDP 的比率時，使用當年一月的房貸余額除以去年的 GDP。
#' 請將結果輸出到一個data.frame，第一欄是年份，第二欄則是房貸餘額的GDP佔有比率。
#' 細節請參考`stopifnot`的檢查。
answerHW <- local({
  gdp_year <- gdp %>%
    # 提前GDP到下一年使用
    mutate(year = as.character(as.numeric(year) + 1))
  cl_info_year %>%
    # cl_info_year和gdp_year根據year內連接(僅保留兩者year一致的行)
    inner_join(gdp_year, by = "year") %>%
    mutate(index = mortgage_total_bal / gdp) %>%
    select(year, index)
})
answerHW

stopifnot(is.data.frame(answerHW))
stopifnot(nrow(answerHW) == 9)
stopifnot(colnames(answerHW) == c("year", "index"))
stopifnot(class(answerHW$year) == "character")
stopifnot(class(answerHW$index) == "numeric")
stopifnot(min(answerHW$index) > 0.3)
stopifnot(max(answerHW$index) < 0.5)

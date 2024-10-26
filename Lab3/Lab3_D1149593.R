##練習一
#題目一
# 這是從 <http://data.gov.tw/node/7769> 下載的海盜通報資料，
# 由於這份文件並沒有遵循任何已知的常見格式，
# 所以我們必須要利用這個章節中所學的技巧，
# 才能從中萃取出資訊。
# 首先，先將該檔案載入到R 之中。
pirate_info <- readLines(file("data/pirate-info-2015-09.txt", encoding = "BIG5"))
pirate_info

#題目二
# 接著我們要將經緯度從這份資料中萃取出來，
# 這份資料的格式，可以用`：`分割出資料的欄位與內容，
# 請同學利用`strsplit`將`pirate_info`進行切割，
# 並將結果儲存到`pirate_info_key_value`之中。
pirate_info_key_value <- strsplit(pirate_info, "：")
pirate_info_key_value

#題目三
# 我們需要的欄位名稱是「經緯度」，
# 請同學先把`pirate_info_key_value`中每個元素（這些元素均為字串向量）的第一個值取出，
# 你的答案應該要為字串向量。
pirate_info_key <- sapply(pirate_info_key_value, "[", 1)
pirate_info_key
# class(pirate_info_key)

#題目四
# 我們將`pirate_info_key`和`"經緯度"`做比較後，把結果存到變數`pirate_is_coordinate`中，
# 結果應該為一個布林向量，同時總共有11件海盜通報事件
pirate_is_coordinate <- pirate_info_key == "經緯度"
pirate_is_coordinate
# class(pirate_is_coordinate)

#題目五
# 接著我們可以利用`pirate_is_coordinate`和`pirate_info_key_value`，
# 找出所有的經緯度資料。
# 請把這個資料存到變數`pirate_coordinate_raw`中，這將會是個長度為11的字串向量。
pirate_coordinate_raw <- sapply(which(pirate_is_coordinate), function(i) pirate_info_key_value[[i]][2])
pirate_coordinate_raw
# class(pirate_coordinate_raw)

#題目六
# 接著可以使用`substring`抓出經緯度的數字，
# 請先抓出緯度並忽略「分」的部份，並存入 pirate_coordinate_latitude
pirate_coordinate_latitude <- substring(pirate_coordinate_raw, gregexpr("北緯", pirate_coordinate_raw)[[1]] + 2, gregexpr("北緯", pirate_coordinate_raw)[[1]] + 3)
pirate_coordinate_latitude <- as.numeric(pirate_coordinate_latitude)
pirate_coordinate_latitude

#題目七
# 請用同樣的要領取出經度，忽略「分」的部份，並存入 pirate_coordinate_longitude
pirate_coordinate_longitude <- substring(pirate_coordinate_raw, gregexpr("東經", pirate_coordinate_raw)[[1]] + 2, gregexpr("東經", pirate_coordinate_raw)[[1]] + 4)
pirate_coordinate_longitude <- as.numeric(pirate_coordinate_longitude)
pirate_coordinate_longitude

# 為了方便後續的分析，我們經常把非結構化的資料整理為結構化資料。
# 在R 中，結構化的資料結構就是data.frame。
# 請同學利用上述的數據，建立一個有11筆資料的data.frame，
# 其中有兩個欄位，一個是latitude, 另一個則是longitude。
# 這兩個欄位紀錄著海盜事件的位置。
pirate_df <- data.frame(latitude = pirate_coordinate_latitude, longitude = pirate_coordinate_longitude)
pirate_df

# 下列兩項 check 都應該要是 True 答案才是對的
sum(pirate_df$latitude) == 43
sum(pirate_df$longitude) == 1151

##練習二
# 上述指令展示了如何利用 R 操作資料庫的 Transaction 功能
# 透過 DBI 介面，也可以從 R 之中以 SQL 程式碼對資料庫進行新增、修改、刪除
# 請查閱 dbExecute、dbSendStatement 的文件

#題目八
# 請嘗試取出範例資料庫中 TWII 資料表的資料，進而了解其日期範圍
# ans1 輸出起始日期, 結束日期
library(RSQLite)
drv <- dbDriver("SQLite")
db <- dbConnect(drv, "data/example.db")
TWII <- dbGetQuery(db, "SELECT * FROM TWII")
dates <- as.Date(TWII$date, format="%Y-%m-%d")
ans1 <- c(min(dates), max(dates))
ans1

#題目九
# 請將 iris 資料表中，setosa 物種的資料寫入範例資料庫，並且取名為 setosa
# ans2 列出資料庫中的表名列表
# ans3 輸出setosa的前五行資料
data(iris)
dbWriteTable(db, "setosa", iris[iris$Species == "setosa", ], append = TRUE)
ans2 <- dbListTables(db)
ans2
ans3 <- head(dbReadTable(db, "setosa"), n = 5)
ans3

#題目十
# 請嘗試以 SQL 指令進行上述動作
# ans4 列出資料庫中的表名列表
# ans5 輸出setosa的前五行資料
ans4 <- dbGetQuery(db, "SELECT name FROM sqlite_master WHERE type='table'")
ans4
ans5 <- dbGetQuery(db, "SELECT * FROM setosa LIMIT 5")
ans5
dbDisconnect(db)

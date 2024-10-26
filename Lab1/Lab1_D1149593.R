# Lab1 練習題

# 我們先將 iris 資料表中部分的資料取出
sepal.len = iris$Sepal.Length  # 花萼的長度
petal.len = iris$Petal.Length  # 花瓣的長度
species = as.character(iris$Species)  # 花的種類

# 試回答下列問題
# 1. 資料集中，有幾筆樣本其品種為 setosa？
answer1 <- sum(species == "setosa")
print(answer1)
# 50筆

# 2. 分別計算各品種之其花瓣與花萼長度的平均數值，你發現了什麼？
sepal.answer2 <- tapply(sepal.len, species, mean)
print(sepal.answer2)
petal.answer3 <- tapply(petal.len, species, mean)
print(petal.answer3)
# 花萼與花瓣長度平均皆為 virginica > versicolor > setosa

# 3. 各品種中，花萼長度超過 5 cm 的樣本各有幾株？
answer4 <- table(iris[sepal.len > 5, ]$Species)
print(answer4)
# setosa: 22株, versicolor: 47株, virginica: 49株

# 4. versicolor 品種的樣本中，其花萼長度大於 virginica 品種之平均花萼長度的樣本有幾株？
answer5 <- nrow(iris[species == "versicolor" & sepal.len > mean(iris[species == "virginica", "Sepal.Length"]), ])
print(answer5)
# 8株

# 5. 資料集中，花瓣與花萼之總長最長的一株樣本，是什麼品種的？
#(hint: 請查閱 which.max() 函式的 help，並且嘗試使用之)
answer6 <- species[which.max(petal.len + sepal.len)]
print(answer6)
# virginica

# 6. 資料集中，各品種花瓣與花萼之長度總和的平均各為多少？
answer7 = tapply(sepal.len+petal.len, species, mean)
print(answer7)
# setosa: 6.468, versicolor: 10.196, virginica: 12.140

# 順便教兩個經常用於整理類別型變數的函式，請嘗試
# unique(species)
# table(species)
# 請詳閱上述兩函式的 help 文件
unique(species)
table(species)
# help("unique")
# help("table")
  

# Lab2 練習題

# 社會服務業自民國87至民國91年的年度用電量（度）
year1 <- 87:91
power1 <- c(6097059332, 6425887925, 6982579022, 7323992602.53436, 7954239517) 
# 製造業自民國87至民國91年的年度用電量（度）
power2 <- c(59090445718, 61981666330, 67378329131, 66127460204.6482, 69696372914.6949) 

# 請選出年度(`year1`)中，社會服務業用電量超過`7e9` 的年份。
# （`7e9`是R 的科學符號，代表`7 * 10^9`）
answer8 <- year1[power1 > 7e9]
print(answer8)

# 接著請計算「社會服務業從民國87年到91年的平均用電量」。
answer9 <- mean(power1)
print(answer9)
  
# 請計算「社會服務業從民國87年到91年用電量的標準差」。
answer10 <- sd(power1)
print(answer10)
  
# 在統計中，我們會計算一筆數據的「標準分數」，算法為數據減去平均數後除以標準差。
# 請計算出「社會服務業從民國87年到91年用電量的標準分數」。
answer11 <- (power1 - mean(power1)) / sd(power1)
print(answer11)
  
# 同樣的道理，請同學算出「製造業自民國87年至民國91年用電量的平均數、標準差和標準分數」。
mean.answer12 <- mean(power2)
print(mean.answer12)
sd.answer13 <- sd(power2)
print(sd.answer13)
z.answer14 <- (power2 - mean(power2)) / sd(power2)
print(z.answer14)
  
# 最後請根據年度，比較同年度中社會服務業用電量以及製造業用電量的十分之一，並列出前者高於後者的年份。
answer15 <- year1[power1 > power2/10]
print(answer15)
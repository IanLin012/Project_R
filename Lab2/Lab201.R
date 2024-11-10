##練習一 
#請練習以 data.frame 的存取方式 回答下列問題
# Q1: iris 資料表共有幾筆資料？
answer1 <- nrow(iris)
print(answer1)

# Q2: iris 資料表中個樣本記載了幾種不同的屬性？
answer2 <- ncol(iris)
print(answer2)

# Q3: iris 資料表中，有幾筆樣本其品種為 setosa？
answer3 <- sum(iris$Species == "setosa")
print(answer3)

# Q4: 分別計算各品種之其花瓣與花萼長度的平均數值，你發現了什麼？
#每個品種成一列(照setosa、versicolor、virginica順序)，各花瓣與花萼的平均數值作為行排列
answer4 <- data.frame(Petal.Length_Average = tapply(iris$Petal.Length, iris$Species, mean), Sepal.Length_Average = tapply(iris$Sepal.Length, iris$Species, mean))
print(answer4)
# 花瓣與花萼長度平均皆為 virginica > versicolor > setosa

# Q5: 各品種中，花萼長度超過 5 cm 的樣本各有幾株？
answer5 <- data.frame(Count = tapply(iris$Sepal.Length > 5, iris$Species, sum))
print(answer5)

# Q6: versicolor 品種的樣本中，其花萼長度大於 virginica 品種之平均花萼長度的樣本有幾株？
answer6 <- sum(iris$Sepal.Length[iris$Species == "versicolor"] > mean(iris$Sepal.Length[iris$Species == "virginica"]))
print(answer6)

# Q7: 在 iris 資料表中新增一個欄位記載各樣本花瓣與花萼的總長
iris$Total.Length <- iris$Sepal.Length + iris$Petal.Length
answer7 <- data.frame(iris)
print(answer7)

# Q8: iris 資料表中，花辦與花萼之總長最長的一株樣本，是什麼品種的？
iris$Total.Length <- iris$Sepal.Length + iris$Petal.Length
answer8 <- data.frame(Species = iris$Species[which.max(iris$Total.Length)], Total.Length = iris$Total.Length[which.max(iris$Total.Length)])
print(answer8)

# Q9: 資料集中，各品種花瓣與花萼之長度總和的平均各為多少？
#每個品種成一列(照setosa、versicolor、virginica順序)
answer9 <- data.frame(Total.Length = tapply(iris$Sepal.Length + iris$Petal.Length, iris$Species, mean))
print(answer9)

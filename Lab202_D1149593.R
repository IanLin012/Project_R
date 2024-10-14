##練習二 手刻以最小方差法進行線性回歸

#' 給定一個矩陣X，
X <- cbind(x1 = 1, x2 = 1:10, x3 = sin(1:10))
#' 以及一個長度為3 的向量 beta。
beta <- c(0.5, -1, 4.3)

#' 我們稱`X[,1] 為x1, X[,2] 為x2, X[,3] 為 x3
#' 向量y 的值是 x1 * beta[1] + x2 * beta[2] + x3 * beta[3]，
#' 請用矩陣乘法`%*%`算出向量y。
#'     dim(y) 應該是 c(10, 1)

y <- X %*% beta
y

#' epsilon 是一個隨機產生的雜訊向量，
epsilon <- c(-1.24462014500259, 0.146172987456978, 1.56426869006839, -0.856920339050681,
             -1.15277300953772, 0.717919832604741, -0.270623615316431, -1.66281578024014,
             -1.15557078461633, -0.730253254897595)

#' 我們讓y 參雜了雜訊。
y <- y + epsilon

#' 假設我們只知道X和y ，而看不到epsilon和beta。根據X 和y ，要如何找出beta?
#' 這是一個標準的迴歸分析問題:
#' 在握有X 和Y 等資料之後，尋找資料之間的線性關係（即beta）。
#' 例如，我們先前曾經尋找過車速與煞車滑行距離之間的關係。
#'
#' 請參考<https://en.wikipedia.org/wiki/Ordinary_least_squares#Estimation>裡的公式：
#' $(X^T X)^{-1} X^T y$（方程式的圖片版： <http://i.imgur.com/Aykv7W3.png>），利用這章學到的矩陣乘法，與線性代數函式，算出beta的估計值。
#'
#' 你可以寫很多行的程式碼，但是請把結果存到beta.hat這個變數之中
#' ps. class(beta.hat) 應該要是 matrix
#'     dim(beta.hat) 應該是 c(3, 1)
#'     rownames(beta.hat) 應該是 c("x1", "x2", "x3")

beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta.hat

#' 請各位比較 beta.hat 和  beta 的數值

#' 接下來我們引入 CO2 資料表的資料，來實踐上述的方法
#' 首先利用 model.matrix() 函式 來建立資料矩陣如下：
  
X <- model.matrix(~ Type + Treatment + conc, CO2)

#' 如此就建立了一個基於 Type、Treatment 和 conc 的矩陣

#' 我們以 uptake 的值放入 y 之中，作為迴歸的目標
y <- CO2$uptake

#' 同樣請各位利用 <https://en.wikipedia.org/wiki/Ordinary_least_squares#Estimation> 的公式
#' 利用迴歸的演算法，
#' 找出 beta.hat，也就是 X 與 y 之間的線性迴歸係數，描述 X 與 y 的關係
#' 理論上讓 X %*% beta.hat 的結果 (存成 y.hat) 會很靠近 y
#' ps. class(beta.hat) 應該為matrix
#'     dim(beta.hat) 應該為 c(4, 1)
#'     rownames(beta.hat) 應該為 c("(Intercept)","TypeMississippi","Treatmentchilled","conc")
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta.hat

y.hat <- X %*% beta.hat
y.hat

#' 這邊我們可以計算X %*% beta.hat 和 y 的correlation（提示：用函數`cor`）
my.corr <- cor(X %*% beta.hat, y)

#' my.corr 的平方，就是迴歸分析時常提到的：R-squared。
#' 很多分析師會用這個數據來判斷這個模型好不好。
#' 在R 裡面跑迴歸分析，可以簡單用`lm`這個函數來進行線性迴歸：
g <- lm(uptake ~ Type + Treatment + conc, CO2)

#' g 這個物件就會包含我們剛剛算過得答案
#' g$coef就會是beta.hat
#' g$fitted.value就會是X %*% beta.hat
#' summary(g)則會顯示各個參數的t 檢定，以及整個模型的R-squared
g.s <- summary(g)

#' mode(g.s)顯示它是一個list。
#' 請取出上述線性迴歸的 R-squared 數值，並且與 my.corr 的平方做比較
r_squared_lm <- g.s$r.squared
r_squared_my.corr <- my.corr^2
c(r_squared_lm, r_squared_my.corr)

##### 請以 cars 資料表為例，再做一次練習
# 以速度 (speed) 預測剎車距離 (dist)
car_X <- model.matrix(~ speed, cars)
car_X

car_y <- cars$dist
car_y
  
# 找出 car_beta.hat，也就是 X 與 y 之間的線性迴歸係數，描述 X 與 y 的關係
car_beta.hat <- solve(t(car_X) %*% car_X) %*% t(car_X) %*% car_y
car_beta.hat

car_y.hat <- car_X %*% car_beta.hat
car_y.hat
  
#' 計算X %*% beta.hat 和 y 的correlation（提示：用函數`cor`）
car_my.corr <- cor(car_X %*% car_beta.hat, car_y)
car_my.corr
  
#' 在R 裡面跑迴歸分析，可以簡單用`lm`這個函數來進行線性迴歸
#' summary(g)則會顯示各個參數的t 檢定，以及整個模型的R-squared
car_g <- lm(dist ~ speed, cars)
car_g

car_g.s <- summary(car_g)
car_g.s
  
#' 請取出上述線性迴歸的 R-squared 數值，並且與 car_my.corr 的平方做比較
car_r_squared_lm <- car_g.s$r.squared
car_r_squared_my.corr <- car_my.corr^2
c(car_r_squared_lm, car_r_squared_my.corr)
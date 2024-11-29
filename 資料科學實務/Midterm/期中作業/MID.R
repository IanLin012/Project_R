#' 上傳檔案須能以 source 執行

#' 第一題
#' 讀取 hsb2.csv 並存入變數 hsb

hsb <- local({
  read.csv(text = paste(readLines(file("hsb2.csv", encoding = "UTF-16"))[3:203], collapse = "\n"), 
           header = TRUE, 
           sep = ",")
})
hsb

#' 第二題
#' 呼叫函式計算科學科缺考人數

missed.num <- local({
  sum(is.na(hsb$科學))
})
missed.num

#' 第三題
#' 取出科學科缺考同學之姓名，並存入變數 missed

missed.name <- local({
  hsb$姓名[is.na(hsb$科學)]
})
missed.name

#' 第四題
#' 將 hsb 表分為兩部分儲存，hsb1 儲存科學科未缺考同學的成績，
#' hsb2 儲存科學科缺考同學的成績

hsb1 <- local({
  hsb[!is.na(hsb$科學), ]
})
hsb1

hsb2 <- local({
  hsb[is.na(hsb$科學), ]
})
hsb2

#' 第五題
#' 以 hsb1 為基礎建立一線性模型，以 古卷閱讀,盧恩書寫,數學
#' ,魔法 等四科成績 預測 科學之成績，並嘗試預測 hsb2 中科學
#' 缺考之同學的科學科成績
#' hint:預測成績部分請參照 predict.lm 的 help

missed.value <- local({
  model <- lm(科學 ~ 古卷閱讀 + 盧恩書寫 + 數學 + 魔法, data = hsb1)
  hsb2$科學 <- round(predict(model, newdata = hsb2))
  hsb2
})
missed.value

#' 第六題
#' 讀取 hsb_info.csv 並且將其中資訊捕到 hsb1 表中，存入 hsb3

hsb3 <- local({
  hsb_info <- read.csv(text = paste(readLines(file("hsb_info.csv", encoding = "BIG5")), collapse = "\n"), 
                       header = TRUE, 
                       sep = ",")
  merged_data <- merge(hsb1, hsb_info, by = "編號")
  merged_data
})
hsb3

#' 第七題
#' 以種族與性別進行分類，分別計算各類群各科目之平均
#' 分數，並存入 cs.avg 變數
#' cs.avg 應為一 data.frame
#' colnames()

cs.avg <- local({
  library(dplyr)
  hsb3 %>%
    group_by(性別, 種族) %>%
    summarise(
      古卷閱讀平均 = mean(古卷閱讀, na.rm = TRUE),
      盧恩書寫平均 = mean(盧恩書寫, na.rm = TRUE),
      數學平均 = mean(數學, na.rm = TRUE),
      科學平均 = mean(科學, na.rm = TRUE),
      魔法平均 = mean(魔法, na.rm = TRUE)
    ) %>%
    as.data.frame()
})
cs.avg

# 以下為提示勿修改
stopifnot(class(cs.avg) == "data.frame")
stopifnot(ncol(cs.avg) == 7)
stopifnot(all(colnames(cs.avg) == 
                c("性別","種族","古卷閱讀平均","盧恩書寫平均",
                  "數學平均","科學平均","魔法平均")))

#' 第八題
#' 撰寫程式碼將男性與女性的科學成績分布，以 ecdf 型式畫在
#' 同一張圖上，女性的科學成績分布以紅色線條、男性的科學成
#' 績分布以藍色線條表示

library(ggplot2)
ggplot(hsb3, aes(x = 科學, color = 性別)) +
  stat_ecdf(geom = "step") +
  scale_color_manual(values = c("female" = "red", "male" = "blue")) +
  labs(title = "男性與女性科學成績分布", x = "科學成績", y = "累積機率") +
  theme_minimal()

#' 第九題
#' 試針對男性與女性的科學成績分布，以 Kolmogorov-Smirnov
#' 檢定說明本次抽測中男性的科學成績顯著高於女性的科學成績
#' hint: 參照 ks.test 的 help，若 y 明顯高於 x 則 
#' ks.test(x, y, alternative = "greater") 的 p-value 應小於 0.05

female_science_scores <- subset(hsb3, 性別 == "female")$科學
male_science_scores <- subset(hsb3, 性別 == "male")$科學
ks_result <- ks.test(female_science_scores, male_science_scores, alternative = "greater")
ks_result

#' 第十題
#' 以程式回答 hsb2.csv 的編碼為何

hsb2.encoding <- local({
  encoding_info <- stringi::stri_enc_detect(paste(hsb2, collapse = "\n"))
  encoding_info[[1]]$Encoding[1]
})
hsb2.encoding

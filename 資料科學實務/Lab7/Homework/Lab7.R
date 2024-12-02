library(arules)
library(dplyr)

data(AdultUCI)

dim(AdultUCI)
AdultUCI[1:2,]

AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL
AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15,25,45,65,100)),
                             labels = c("Young", "Middle-aged", "Senior", "Old"))
AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]],
                                            c(0,25,40,60,168)),
                                        labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain"]],
                                          c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[["capital-gain"]]>0]),
                                            Inf)), labels = c("None", "Low", "High"))
AdultUCI[["capital-loss"]] <- ordered(cut(AdultUCI[["capital-loss"]],
                                          c(-Inf,0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]]>0]),
                                            Inf)), labels = c("None", "Low", "High"))
Adult <- as(AdultUCI, "transactions")
summary(Adult)

rules <- apriori(Adult, parameter = list(support = 0.5, confidence = 0.9))
summary(rules)

rules.none = subset(rules, subset = rhs %in% "capital-gain=None")

# 練習一：請用上述方法觀察 rules 資料表，並從中找出你認為最有用的規則，並解釋原因
inspect(rules)
rules_with_score <- apriori(Adult, parameter = list(support = 0.5, confidence = 0.9))
quality(rules_with_score)$Score <- quality(rules_with_score)$support * 0.3 + quality(rules_with_score)$lift * 0.7
rules_with_score_sorted <- sort(rules_with_score, by = "Score")
inspect(rules_with_score_sorted)

# 練習二：嘗試調整最低 support 與 confidence 產生新規則，並從中找尋有用的規則
rules_new <- apriori(Adult, parameter = list(support = 0.45, confidence = 0.85))
inspect(rules_new)
quality(rules_new)$Score <- quality(rules_new)$support * 0.3 + quality(rules_new)$lift * 0.7
rules_new_sorted <- sort(rules_new, by = "Score")
inspect(rules_new_sorted)

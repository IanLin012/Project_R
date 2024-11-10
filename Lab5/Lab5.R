# 這邊我們做一個練習，請問美國航空公司 (carrier == "AA")，其班機尾標是否都有 AA 字眼
# 不可直接打 TRUE 或 FALSE
library(dplyr)
library(nycflights13)
data(flights)
ans_1 <- flights %>%
  filter(carrier == "AA") %>%
  pull(tailnum) %>%
  grepl("AA", .) %>%
  all()
ans_1

# 請嘗試計算各航空公司，其不同時段 (0000-0559, 0600-1159, 1200-1759, 1800-2359) 之航班的平均起飛遲延、以及平均降落遲延
# hint: 會用到 cut 做分類
library(dplyr)
library(nycflights13)
data(flights)
flights <- flights %>%
  mutate(
    time_slot =
      cut(
        dep_time,
        breaks = c(0, 600, 1200, 1800, 2400),
        labels = c("0000-0559", "0600-1159", "1200-1759", "1800-2359")
      )
  )
ans_2 <- flights %>%
  group_by(carrier, time_slot) %>%
  summarise(
    dep_delay_avg = mean(dep_delay, na.rm = TRUE),
    arr_delay_avg = mean(arr_delay, na.rm = TRUE)
  ) %>%
  na.omit()
ans_2

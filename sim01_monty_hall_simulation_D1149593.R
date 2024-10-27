# 有三個門 1,2,3
# 大獎在其中的一個門
# 這邊用模擬法來估計換門或不換門的中獎機率

num = 1000
doors = c(1,2,3)

sims <- data.frame(
  door.prize = sample(doors, num, replace = T),
  first.choice = 1
)
# 每次從向量(doors)隨機(sample)抽取一個數字, 抽取1000次(num), 允許重複抽取(replace = T)
# sims

sims$door.opened = sapply(1:num, function(i) {
  c = setdiff(doors, c(sims$door.prize[i], 1))
  if(length(c) == 1) return(c)
  else sample(c, 1)
})
# 計算原向量(doors)與大獎門和初始選擇門以外門號向量(c(sims$door.prize[i], 1))的差集(setdiff)
# sims$door.opened

sims$no.change <- 1
# sims$no.change

sims$change <- sapply(1:num, function(i) {
  setdiff(doors, c(sims$door.opened[i], 1))
})
# 計算原向量(doors)與打開門和初始選擇門以外門號向量(c(sims$door.opened[i], 1))的差集(setdiff)
# sims$change

# 試計算換門與不換門的勝利機率
change_rate <- mean(sims$door.prize == sims$change)
no_change_rate <- mean(sims$door.prize == sims$no.change)
data.frame(換門勝利機率 = change_rate, 不換門勝利機率 = no_change_rate)

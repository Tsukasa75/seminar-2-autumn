#### 事例事例3 プロ野球の勝率に関する回帰分析 ####
# ID チーム番号 name チーム名 year 年次
# win_rate 勝率 n_games 年間試合数 era 防御率
# strike_out 奪三振数 error 失策数
# batting 打率 homerun ホームラン数
# ribby 打点 (Runs Batted In) stolen_base 盗塁

library(tidyverse)
dataf <- readr::read_csv("baseball-win-rate.csv")

result <- lm(win_rate~era+batting+homerun,data=dataf)
summary(result)







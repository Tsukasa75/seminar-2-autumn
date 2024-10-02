#### 事例事例4 客室乗務員の年収する回帰分析 ####
library(tidyverse)
dataf <- readr::read_csv("wage-census-ca.csv")

result <- lm(income~age+age*factor(year)+factor(sme),data=dataf)
summary(result)


##### 以下はWEBサポートページで解説しています ############
#https://note.com/toshi_matsuura/n/n37d8204ff873

# 新しいデータフレームを作成
new_data <- expand.grid(
  year = c(2004, 2009, 2014, 2019),
  age = c(25, 30, 35, 40, 45, 50, 55),
  sme = 0
)

# 予測値を計算し、new_dataに追加
new_data$predictions <- predict(result, newdata = new_data)

# パッケージを読み込む
library(ggplot2)

# グラフの作成
ggplot(new_data, aes(x = age, y = predictions, group = factor(year), color = factor(year), linetype = factor(year))) +
  geom_line(size = 1) +  # 線を太くする
  labs(x = "Age", y = "Income") +
  scale_x_continuous(breaks = seq(25, 55, 5)) +  # x軸の刻みを調整
  theme_minimal() +
  theme_classic()

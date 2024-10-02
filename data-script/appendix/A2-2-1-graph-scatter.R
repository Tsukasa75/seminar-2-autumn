#######################
#散布図の作成・ヒストグラム
#######################
library(tidyverse)

# データの読み込み
dataf <-readr::read_csv("rent-shonandai96-04.csv")

g_point1 <-ggplot(data=dataf, aes(x=floor, y=rent))+
  geom_point()
g_point1

# 背景を白にする
g_point2 <-ggplot(data=dataf, aes(x=floor, y=rent))+
  geom_point() + theme_classic()
g_point2

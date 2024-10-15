library(tidyverse)
setwd("/Users/ebuchitsukasa/Desktop/university/seminar/seminar2autumn")

x <- 1
y <- 2
z <- 7
x * y * z

dataf <- read.csv("data-script/chapter1/rent-shonandai96-04.csv")

head(data, 10) # 最初の6行だけ出力

glimpse(dataf) # package of tidyverse

dataf$rent # print column of rent

## 1.7 データ操作の基本

# 1.7.1 変数を限定する : dplyr:select()
dplyr::select(dataf, rent, auto_lock)
dataf2 <- dataf %>% dplyr::select(rent, service, floor, age)

dataf[, c("rent", "floor")] # この記法でもいける

# 1.7.2 条件を満たすデータだけを取り出す : filter()

dataf %>% dplyr::filter(bus>0)
dataf %>% dplyr::filter(bus>0&age>10)

# 1.7.3 データを並び替える dplyr::arrange()
dataf %>% dplyr::arrange(walk) # 昇順で並び替え
dataf %>% dplyr::arrange(-walk) # 降順で並び替え
dataf %>% dplyr::arrange(walk, -floor, rent) 

# 1.7.4 新しい変数を作成する mutate()
dataf <- read.csv("data-script/chapter1/rent-shonandai96-04.csv")
dataf %>% dplyr::mutate(
  rent_total=rent+service,
  dist=walk+bus
)
dataf$rent_total2 <- dataf$rent+dataf$service

dataf$rent_total=rent+service

# exercise given by the professor
# histograms of 100 to 1000 samples with a mean of 0 and a standard deviation of 1.
z1 <- c()
z2 <- c()
n <- 100
m <- 1000
for (i in 1:100){
  x <- rnorm(n, mean = 0, sd = 1)
  z1[i] <- mean(x)
  y <- rnorm(m, mean = 0, sd = 1)
  z2[i] <- mean(y)
}
z1
z2

par(mfrow = c(1, 2))
hist(z1)
hist(z2)
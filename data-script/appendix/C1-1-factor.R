# このスクリプトで必要なパッケージ
# tidyverse
# modelsummary

library(tidyverse)
library(modelsummary)

# 賃金データ読み込み
dataf <- read_csv("wage-census2022.csv")

levels(factor(dataf$education))

dataf$education2 <-factor(dataf$education,levels=c("2","1","3","4"))

levels(factor(dataf$education2))

result <-lm(wage~age+male+education2, data=dataf)
summary(result)

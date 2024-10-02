# このスクリプトで必要なパッケージ
# tidyverse
# modelsummary

library(tidyverse)

# 3.3.5 Gravity model 両対数モデル
dataf <- 
  readr::read_csv("gravity-g20asean.csv")

dataf %>% dplyr::select(Importer, Exporter, Trade, GDPex, GDPim, distance, FTA)

model_linear <-
  lm(log(Trade)~log(GDPex)+log(GDPim)+log(distance)+FTA,data=dataf)
summary(model_linear)

# パイプを使うと以下のように書くこともできます
lm(log(Trade)~log(GDPex)+log(GDPim)+log(distance)+FTA,data=dataf)　%>%
  summary()

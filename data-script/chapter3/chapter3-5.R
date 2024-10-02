# 事前にsandwichパッケージをインストールが必要
# ただしlibrary(modelsummary)を使えばlibrary(sandwich)は不要
library(tidyverse)
library(modelsummary)
library(estimatr)

# 3.5 不均一分散
dataf <-
  readr::read_csv("educ-income.csv")
# 最小二乗法
model_linear <-
  lm(educ~income,data=dataf)
summary(model_linear)

# 加重最小二乗法
model_linear <-
  lm(educ~income,data=dataf,weights=1/pop)
summary(model_linear)
# 頑健な標準誤差
model_linear <-
  lm_robust(educ~income,data=dataf)
summary(model_linear)

# msummaryで頑健な標準誤差を出力
dataf <- 
  readr::read_csv("rent-shonandai96-04.csv")
# mutate: 新しい変数の作成
dataf <- dataf %>%
  dplyr::mutate(rent_total=rent+service) 
dataf <- dataf %>%
  dplyr::mutate(dist=bus+walk) 
regs <-
  list(
    "model1" =lm(rent_total~floor,data=dataf),
    "model2" =lm(rent_total~floor+age,data=dataf),
    "model3" =lm(rent_total~floor+age+dist,data=dataf)
  )
# 表3.7
modelsummary::msummary(regs, stars=TRUE , gof_omit='RMSE|AIC|BIC|Log.Lik.|F',vcov="HC2")

lm_robust(rent_total~floor+age+dist,data=dataf) %>% summary()

regs <-
  list(
    "model1" =lm_robust(rent_total~floor,data=dataf),
    "model2" =lm_robust(rent_total~floor+age,data=dataf),
    "model3" =lm_robust(rent_total~floor+age+dist,data=dataf)
  )
modelsummary::msummary(regs, stars=TRUE , gof_omit='RMSE|AIC|BIC|Log.Lik.|F')


##################################
# 発展 標準誤差のクラスタリング
##################################
dataf <-
  readr::read_csv("acemoglu2016.csv")

# sic3でクラスタリングした場合
model_linear <-
  lm_robust(dL~dIMP,data=dataf,clusters=sic3)
summary(model_linear)


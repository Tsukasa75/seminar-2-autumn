# 5.1節
# 4章までに使用したパッケージで動作します。
library(tidyverse)
library(modelsummary)
library(estimatr)

# データ読み込み
dataf <- readr::read_csv("rent-odakyu-enoshima96-04.csv")

dataf <- dataf %>% dplyr::mutate(rent_total=rent+service)
dataf <- dataf %>% dplyr::mutate(dist=walk+bus)

# treatダミー作成
dataf <- dataf %>% dplyr::mutate(treat=if_else(station=="Shonandai",1,0)) 


# treat*2004年ダミー作成
dataf <- dataf %>% dplyr::mutate(treat2004=if_else((station=="Shonandai"&year==2004),1,0))

# 2004年ダミー作成
dataf <- dataf %>% dplyr::mutate(year2004=if_else(year==2004,1,0))

#前後比較分析（湘南台限定）
result1 <-estimatr::lm_robust(rent_total~floor+age+dist+treat2004,data=dataf, station=="Shonandai")
summary(result1)

#差の差の分析
result2 <-estimatr::lm_robust(rent_total~floor+age+dist+treat+year2004+treat2004,data=dataf)
summary(result2)

# model summary
results <-
  list(
    "model1" =estimatr::lm_robust(rent_total~floor+age+dist+treat2004,data=dataf, station=="Shonandai"),
    "model2" =estimatr::lm_robust(rent_total~floor+age+dist+treat+year2004+treat2004,data=dataf)
  )
modelsummary::msummary(results, stars=TRUE, gof_omit='RMSE|AIC|BIC|Log.Lik.|F')



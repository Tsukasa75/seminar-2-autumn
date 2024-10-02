# plmパッケージのインストールが必要
library(tidyverse)
library(plm)
library(estimatr)
library(fixest)

# データの読み込み
dataf <-readr::read_csv("nlswork.csv")
# パネルデータのフォーマットを宣言
pdataf <- plm::pdata.frame(dataf,index=c("idcode","time"))

# 賃金変化を計算：対数値の差分なので変化率の近似値となる
# mutate関数とplm::lagは併用できないので注意
pdataf$wgrowth=pdataf$ln_wage-plm::lag(pdataf$ln_wage)
pdataf$wgrowth2=pdataf$ln_wage-plm::lag(plm::lag(pdataf$ln_wage))

# t-1期のunionが0、t期のunionが1なら1．
# t-1期のunionが0、t期のunionが0なら0．
pdataf$dunion = case_when(
  (pdataf$union == 1 & plm::lag(pdataf$union) == 0) ~ 1,
  (pdataf$union == 0 & plm::lag(pdataf$union) == 0) ~ 0)  

pdataf$lag_union=plm::lag(pdataf$union)

# 変数が作成されているか確認
pdataf %>% dplyr::select(idcode,year,ln_wage,wgrowth,wgrowth2,union,dunion) 

result <-estimatr::lm_robust(wgrowth~dunion+plm::lag(age)+plm::lag(race)+plm::lag(msp)+plm::lag(grade)+plm::lag(not_smsa)+plm::lag(south)+factor(year),data=pdataf)
summary(result)

# fixest::feolsでもplm::lagは使用できる
result <-fixest::feols(wgrowth~dunion+plm::lag(age)+plm::lag(race)+plm::lag(msp)+plm::lag(grade)+plm::lag(not_smsa)+plm::lag(south)|year,data=pdataf,se="hetero")
summary(result)


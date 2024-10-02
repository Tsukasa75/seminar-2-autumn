# 5.2節
# 都道府県データによる保育所定員率と母親就業率の分析
# fixestパッケージのインストールが必要
library(tidyverse)
library(estimatr)
library(fixest)

# データ読み込み
dataf <- readr::read_csv("asai00-10.csv")

# プーリング回帰
result1 <- estimatr::lm_robust(emprate~caprate,data=dataf)
summary(result1)

# 年次ダミーを作成
dataf <- dataf %>% dplyr::mutate(y2005=if_else(year==2005,1,0))
dataf <- dataf %>% dplyr::mutate(y2010=if_else(year==2010,1,0))

# 年次固定効果（年次ダミー）を導入した推計
result2 <- estimatr::lm_robust(emprate~caprate+y2005+y2010,data=dataf)
summary(result2)

# factor関数で年次ダミーを導入
result2 <- estimatr::lm_robust(emprate~caprate+factor(year),data=dataf)
summary(result2)

# 年次固定効果、個体固定効果の導入
result3 <- estimatr::lm_robust(emprate~caprate+factor(year)+factor(pref),data=dataf) 
summary(result3)

# もう一つの固定効果の入れ方
result4 <- estimatr::lm_robust(emprate~caprate,fixed_effects=~pref+year,data=dataf) 
summary(result4)

# fixestパッケージによる推計, etableで出力
result_feols1 <- fixest::feols(emprate~caprate,data=dataf)
result_feols2 <- fixest::feols(emprate~caprate|year,data=dataf)
result_feols3 <- fixest::feols(emprate~caprate|pref+year,data=dataf)

fixest::etable(result_feols1,result_feols2,result_feols3,
               signif.code=c("***"=0.01,"**"=0.05,"*"=0.1), se.below = TRUE)

# 時間を通じて変化しない変数、dist_f_tokyo(distance from Tokyo)を導入
result_feols1 <- fixest::feols(emprate~caprate+log(dist_f_tokyo+1),data=dataf)
result_feols2 <- fixest::feols(emprate~caprate+log(dist_f_tokyo+1)|year,data=dataf)
result_feols3 <- fixest::feols(emprate~caprate+log(dist_f_tokyo+1)|pref+year,data=dataf)

fixest::etable(result_feols1,result_feols2,result_feols3, 
               signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),se.below = TRUE)

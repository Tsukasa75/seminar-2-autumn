library(tidyverse)
library(fixest)

# 6.3 中国からの輸入が米国雇用にあたえ影響
dataf <-read_csv("acemoglu2016.csv")

result_ols <- fixest::feols(dL~dIMP,data=dataf)
summary(result_ols)

result_iv <- fixest::feols(dL~1|dIMP~dIMPoth,data=dataf)
summary(result_iv,stage=1:2)

fixest::etable(result_ols,result_iv,stage=1:2,fitstat=~ivf+ivf.p,se="HC1",se.below=TRUE)

# 6.4. 奴隷貿易と経済成長：操作変数が複数ある場合
dataf <-readr::read_csv("slave_trade.csv")

result_ols <- fixest::feols(lnpcgdp2000~ln_export_area,data=dataf)
summary(result_ols)

result_iv <- fixest::feols(lnpcgdp2000~1|ln_export_area~atlantic_dist+indian_dist+saharan_dist+red_sea_dist,data=dataf)
summary(result_iv,stage=1:2,se="HC1")

fixest::etable(result_ols,result_iv,stage=1:2,se="HC1", fitstat=~ivf+ivf.p,se.below=TRUE)




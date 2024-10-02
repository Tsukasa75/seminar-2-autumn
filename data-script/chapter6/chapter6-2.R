library(tidyverse)
library(fixest)
library(openxlsx)
#固定効果操作変数法

#data("CigarettesSW",package='AER')
CigarettesSW <-readr::read_csv("CigarettesSW.csv")
CigarettesSW <- CigarettesSW %>%
                    mutate(rprice=price/cpi,
                           rincome=income/population/cpi,
                           rtax=tax/cpi)
# 1995年の横断面データで推計
dataf <- filter(CigarettesSW,year=="1995")
result_ols <-feols(log(packs)~log(rprice)+log(rincome),data=dataf)
result_iv <-feols(log(packs)~log(rincome)|log(rprice)~rtax,data=dataf)
fixest::etable(result_ols,result_iv,stage=1:2,se="HC1", fitstat=~ivf1+ivf.p+sargan+sargan.p,se.below=TRUE)

# Fixed Effect Instrument Variable Estimation
result_ols <-feols(log(packs)~log(rprice)+log(rincome)|state+year,data=CigarettesSW)
result_iv <-feols(log(packs)~log(rincome)|state+year|log(rprice)~log(rtax),data=CigarettesSW)
fixest::etable(result_ols,result_iv,stage=1:2,se="HC1", fitstat=~ivf+ivf.p,se.below=TRUE)

### 結果をEXCELファイルに出力する場合のスクリプト例
res_table <-fixest::etable(result_ols,result_iv,stage=1:2,se="HC1", fitstat=~ivf+ivf.p,se.below=TRUE)
openxlsx::write.xlsx(res_table,"result_fix_iv.xlsx")

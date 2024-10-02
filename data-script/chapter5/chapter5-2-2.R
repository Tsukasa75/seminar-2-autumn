library(tidyverse)
library(fixest)
library(openxlsx)


#####################################################
# パネルデータによる自由貿易協定の効果測定
#####################################################

# データ読み込み
dataf <- readr::read_csv("panel-gravity-data.csv")

dataf %>% dplyr::mutate(ltrade=log(tvalue/GDPdef_o)) -> dataf

dataf1960 <- dplyr::filter(dataf, year==1960)
dataf1970 <- dplyr::filter(dataf, year==1970)
dataf1980 <- dplyr::filter(dataf, year==1980)
dataf1990 <- dplyr::filter(dataf, year==1990)
dataf2000 <- dplyr::filter(dataf, year==2000)

m1960 <-fixest::feols(ltrade~log(GDPR_o)+log(GDPR_d)
                      +log(distw)+contig+comlang_off+fta,data=dataf1960)
m1970 <-fixest::feols(ltrade~log(GDPR_o)+log(GDPR_d)
                      +log(distw)+contig+comlang_off+fta,data=dataf1970)
m1980 <-fixest::feols(ltrade~log(GDPR_o)+log(GDPR_d)
                      +log(distw)+contig+comlang_off+fta,data=dataf1980)
m1990 <-fixest::feols(ltrade~log(GDPR_o)+log(GDPR_d)
                      +log(distw)+contig+comlang_off+fta,data=dataf1990)
m2000 <-fixest::feols(ltrade~log(GDPR_o)+log(GDPR_d)
                      +log(distw)+contig+comlang_off+fta,data=dataf2000)
fixest::etable(m1960,m1970,m1980,m1990,m2000,
               signif.code=c("***"=0.01,"**"=0.05,"*"=0.1), se.below = TRUE)

result_feols1 <- fixest::feols(ltrade~log(GDPR_o)+log(GDPR_d)
                       +log(distw)+contig+comlang_off+fta,data=dataf)
result_feols2 <- fixest::feols(ltrade~log(GDPR_o)+log(GDPR_d)
                       +log(distw)+contig+comlang_off+fta|year,data=dataf)
result_feols3 <- fixest::feols(ltrade~log(GDPR_o)+log(GDPR_d)
                       +log(distw)+contig+comlang_off+fta|id,data=dataf)
result_feols4 <- fixest::feols(ltrade~log(GDPR_o)+log(GDPR_d)
                       +log(distw)+contig+comlang_off+fta|id+year,data=dataf)

fixest::etable(result_feols1,result_feols2,result_feols3,result_feols4,
       signif.code=c("***"=0.01,"**"=0.05,"*"=0.1), se.below = TRUE)
# etableは、固定効果モデルの場合、デフォルトで"id"でクラスタリングした標準誤差を返す

# EXCEL出力
tabcsv <- fixest::etable(result_feols1,result_feols2,result_feols3,result_feols4,
                         signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),se.below=TRUE)
openxlsx::write.xlsx(tabcsv,"result.xlsx")


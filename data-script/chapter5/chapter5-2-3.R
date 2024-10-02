library(tidyverse)
library(fixest)


#####################################################
# EUの途上国向け輸出促進政策プログラムの効果測定
#####################################################
# Cambodia GSP
# データ読み込み
dataf <- readr::read_csv("Cambodia-GSP.csv")

result1 <- fixest::feols(log(exp61)~eu_post+log(gdp)+log(gdppc)+tariff+fta|ctyid+year,data=dataf)
result2 <- fixest::feols(log(exp62)~eu_post+log(gdp)+log(gdppc)+tariff+fta|ctyid+year,data=dataf)
result3 <- fixest::feols(log(exp64)~eu_post+log(gdp)+log(gdppc)+tariff+fta|ctyid+year,data=dataf)
result4 <- fixest::feols(log(exp10)~eu_post+log(gdp)+log(gdppc)+tariff+fta|ctyid+year,data=dataf)

fixest::etable(result1,result2,result3,result4, 
       signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),se.below = TRUE)

#############
# 図5.11作成
#############

### データの準備

# 欠損値を除去しておく
dataf <- dataf %>% drop_na(exp61)
# EU加盟国、それ以外×年で集計する(sum(exp61))
dataf <- dataf %>% group_by(eucty, year) %>% summarize(exp61=sum(exp61))

# 2010年の値で基準化する
dataf <- dataf %>% mutate(m2010=if_else(year==2010,exp61,0))
dataf <- dataf %>% group_by(eucty) %>% mutate(m2010=max(m2010))
dataf <- dataf %>% mutate(exp61=exp61/m2010*100)

###########
# グループ別に折れ線グラフを描く
###########
#　凡例用のカテゴリー変数を用意
dataf <- dataf %>% mutate(Destination=if_else(eucty==1,"Export to EU","Export to non-EU"))

g_line <-ggplot(data=dataf,aes(x=year,y=exp61,group=Destination,linetype=Destination))+
  geom_line()+geom_point()+theme_classic()
g_line

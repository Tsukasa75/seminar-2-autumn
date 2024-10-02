# このスクリプトで必要なパッケージ
# tidyverse, summarytools, psych, tableone

library(tidyverse)
library(psych)
library(tableone)

# データ読み込み
dataf <- 
  readr::read_csv("rent-shonandai96-04.csv")

## 下準備
# mutate: 新しい変数の作成
dataf <- 
  dataf %>% dplyr::mutate(rent_total=rent+service)
dataf <-
  dataf %>% dplyr::mutate(dist=bus+walk)

# クロス表
table(dataf$auto_lock)
table(dataf$auto_lock, dataf$year)
addmargins(table(dataf$auto_lock, dataf$year))
prop.table(table(dataf$auto_lock, dataf$year),margin=2)

# 件数のカウント
sum(dataf$bus==0)

##参考
# 行方向の比率
prop.table(table(dataf$auto_lock, dataf$year),margin=1)
# 全体の比率
prop.table(table(dataf$auto_lock, dataf$year))

# 結果表をCSVファイルに出力
prop.table(table(dataf$auto_lock, dataf$year),margin=1) %>%
  print() %>%
  write.csv("result.csv")

#######################
# 連続変数のカテゴリー化
#######################
dataf <-
  dataf %>% 
  mutate(r_category=case_when(
    rent_total>=3&rent_total<6 ~"03-06",
    rent_total>=6&rent_total<9 ~"06-09",
    rent_total>=9&rent_total<12 ~"09-12",
    rent_total>=12&rent_total<15~"12-15",
    rent_total>=15&rent_total<18~"15-18",
    rent_total>=18~"18-"))

table(dataf$r_category)

#ヒストグラムの作成
barplot(table(dataf$r_category))

# 1996と2004年のデータに限定してヒストグラムを作成
dataf96 <-
  dataf %>% dplyr::filter(year==1996)
barplot(table(dataf96$r_category))

dataf04 <-
  dataf %>% dplyr::filter(year==2004)
barplot(table(dataf04$r_category))

# 記述統計
summary(dataf$rent_total)
summary(dataf)
# describeはpsychパッケージのインストール&呼び出しが必要
psych::describe(dataf)
psych::describe(dataf,skew=FALSE)

# CSVファイルに出力
psych::describe(dataf,skew=FALSE) %>% 
  write.csv("result.csv")

# グループ別の平均値
dataf %>% 
  dplyr::group_by(year) %>%
  summarise(mean(rent_total),sd(rent_total))

# 年別オートロックの有無別の平均値
dataf %>% 
  dplyr::group_by(year, auto_lock) %>% 
  summarise(mean(rent_total))

# tidyr::pivot_widerを使うと見栄えがよくなる
dataf %>% 
  dplyr::group_by(year, auto_lock) %>% 
  summarise(m_rent=mean(rent_total)) %>%
  tidyr::pivot_wider(names_from=auto_lock, values_from=m_rent)

#平均値の差の検定
tableone::CreateTableOne(vars=c("rent_total","floor","age","auto_lock"),strata="year",factorVars="auto_lock",data=dataf)

tableone::CreateTableOne(vars="rent_total",strata="auto_lock",data=dataf)

# ggplotによる散布図
# 横軸:age
ggplot(data=dataf, aes(x=age, y=rent_total))+
  geom_point()
# 横軸:floor
ggplot(data=dataf, aes(x=floor, y=rent_total))+
  geom_point()

# 図をpngファイルに出力
png(filename = "scatter.png",width=400,height=300)
ggplot(data=dataf, aes(x=floor, y=rent_total))+
  geom_point()
dev.off()

# 相関係数
cor(dataf$rent_total,dataf$floor,use="pairwise.complete.obs")
# 変数を限定して相関係数行列を作成
dataf_cor <-
  dataf %>% dplyr::select(rent_total, dist, age, floor)
cor(dataf_cor,use="pairwise.complete.obs")

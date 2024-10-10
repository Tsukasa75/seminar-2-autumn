library("dplyr")

setwd("/Users/ebuchitsukasa/Desktop/university/seminar/seminar2autumn")

## 2.1 集計表の作成

# 下準備

dataf <- read.csv("data-script/chapter1/rent-shonandai96-04.csv")
dataf <- dataf %>% dplyr::mutate(
  rent_total=rent+service,
  dist=walk+bus
)

dataf %>% dplyr::mutate(
  rent_total=rent+service,
  dist=walk+bus
)

# 実行
table(dataf$auto_lock)
table(dataf$auto_lock, dataf$year)

addmargins(table(dataf$auto_lock, dataf$year)) # 行と列両方に合計を追加
prop.table(table(dataf$auto_lock, dataf$year), margin=1) # 行方向の比率を計算
prop.table(table(dataf$auto_lock, dataf$year), margin=2) # 列方向の比率を計算

sum(dataf$bus==0) # 件数をカウント

dataf <- dataf %>% dplyr::mutate(r_category=
    case_when(
      rent_total>=3&rent_total<6 ~ "03-06",
      rent_total>=6&rent_total<9 ~ "06-09",
      rent_total>=9&rent_total<12 ~ "09-12",
      rent_total>=12&rent_total<15 ~ "12-15",
      rent_total>=15&rent_total<18 ~ "15-18",
      rent_total>=18 ~ "18-",
    )
)

table(dataf$r_category)

barplot(table(dataf$r_category))

par(mfrow = c(1, 2))

dataf96 <- dataf %>% dplyr::filter(year==1996)
barplot(table(dataf96$r_category), xlab="rent_total", ylab="count")


dataf96 <- dataf %>% dplyr::filter(year==2004)
barplot(table(dataf96$r_category), xlab="rent_total", ylab="count")

## 2.2.2 Rによる計算方法

summary(dataf$rent_total)
summary(dataf)
psych::describe(dataf, skew=FALSE) # skewはさらに細かいオプションを指定

# ファイルへの書き込み
psych::describe(dataf, skew=FALSE) %>% write.csv("result.csv")

dataf %>% summarize(mean(rent_total))

dataf %>% dplyr::group_by(year) %>% summarize(mean(rent_total), sd(rent_total))

dataf %>% dplyr::group_by(year, auto_lock) %>% summarise(mean(rent_total))

dataf %>% group_by(year, auto_lock) %>%
  summarise(m_rent=mean(rent_total)) %>%
  tidyr::pivot_wider(names_from=auto_lock, values_from=m_rent)
  
tableone::CreateTableOne(
  vars=c("rent_total", "floor", "age", "auto_lock"),
  strata="year",
  factorVars="auto_lock",
  data=dataf
)


tableone::CreateTableOne(
  vars="rent_total",
  strata="auto_lock",
  data=dataf
)


## 2.3 変数間の関係性の把握

# 2.3.1 散布図・相関係数
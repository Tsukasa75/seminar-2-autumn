## 1
# 下準備
setwd("/Users/ebuchitsukasa/Desktop/university/seminar/seminar2autumn")
library("dplyr")


dataf <- read.csv("/Users/ebuchitsukasa/Desktop/university/seminar/seminar2autumn/data-script/chapter2/rent-jonan-kawasaki.csv")

# 1-1

prop.table(table(dataf$autolock, dataf$line), margin=2)
prop.table(table(dataf$line, dataf$autolock), margin=1)


# 1-2

# p54 group_byを使用
dataf %>% 
  dplyr::group_by(line) %>% 
    summarize(
      avg_rent = mean(rent),
      avg_age = mean(age),
      avg_floor = mean(floor),
    )

# xtabsを使う方法もあるらしい

# 1-3

# 度数分布表とヒストグラム

dataf <- dataf %>% dplyr::mutate(
  r_category=
    case_when(
      rent>=0&rent<3 ~ "-03",
      rent>=3&rent<6 ~ "03-06",
      rent>=6&rent<9 ~ "06-09",
      rent>=9&rent<12 ~ "09-12",
      rent>=12&rent<15 ~ "12-15",
      rent>=15&rent<18 ~ "15-18",
    )
)

dataf <- dataf %>% dplyr::mutate(
  r_category3=
    case_when(
      for (i in seq(0, 21, by=3)) {
        rent>=i&rent<i+3 ~ springf("%d-%d+3", i)
      }
    )
)

library(glue)
dataf <- dataf %>% dplyr::mutate(
  r_category=
      for (i in seq(0, max(dataf$rent), by=3)) {
        dataf$r_category2[dataf$rent>=i&rent<i+3] <- glue("{i}-{i+3}")
      }
)

for (i in seq(0, max(dataf$rent), by=3)) {
  dataf$r_category2[dataf$rent>=i&dataf$rent<i+3] <- glue("{i}-{i+3}")
}

table(dataf$r_category)

barplot(table(dataf$r_category))

hist(dataf$rent, breaks=seq(0, 21, 3))

dataf_line <- dataf %>% dplyr::mutate(
  keikyu_line=dplyr::filter(dataf$line=="keikyu"),
  tokyu_line=dplyr::filter(dataf$line=="tokyu"),
  jr_line=dplyr::filter(dataf$line=="jr"),
)

barplot(table(dataf[dataf$line=="keikyu", "r_category"]))
barplot(table(dataf[dataf$line=="tokyu", "r_category"]))
barplot(table(dataf[dataf$line=="jr", "r_category"]))


# 1-4  相関係数行列

dataf <- dataf %>% dplyr::mutate(
  rent_total=rent+service,
  rent_per_square_meter=rent/floor,
  distance=walk+bus,
)

dataf_cor <- dataf %>% dplyr::select(rent_total, rent_per_square_meter, distance)

cor(dataf_cor, use="pairwise.complete.obs")


## 2

# 下準備

dataf2 <- read.csv("/Users/ebuchitsukasa/Desktop/university/seminar/seminar2autumn/data-script/chapter2/u001/u001.csv")

# 2.1 変数の作成、比率の計算

dataf2 <- dataf2 %>% dplyr::mutate(
  univ=
    case_when(
      (ZQ23A==5|ZQ23A==6)&ZQ24==1 ~ "univ",
      .default = "others",
    ),
  LDP=
    case_when(
      ZQ42==1 ~ "LDP",
      .default = "others",
    )
)

dataf2 <- dataf2 %>% dplyr::mutate(
  univ=
    ifelse((ZQ23A==5|ZQ23A==6)&ZQ24==1, "univ", "others")
)

prop.table(table(dataf2$univ, dataf2$LDP), margin=1)

# 2.2

# 限定したデータフレームの作成
conditional_dataf2 <- dataf2 %>% dplyr::filter(
  ZQ03==1&ZQ50==2&sex==1
)

conditional_dataf2 <- conditional_dataf2 %>% dplyr::mutate(
  household_category=case_when(
    ZQ54A==1&ZQ54B==1&ZQ54C==1&ZQ54D==1 ~ "every day",
    .default = "others",
  )
)

# 比率の計算
prop.table(
  table(conditional_dataf2$univ, conditional_dataf2$household_category),
  margin=1
)


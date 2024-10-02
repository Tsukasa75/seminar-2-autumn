library(tidyverse)

# データ読み込み
dataf <- 
  readr::read_csv("rent-shonandai96-04.csv")

# 平均値
dataf <- dataf %>% dplyr::mutate(rent_mean=mean(rent))
dataf %>% dplyr::select(rent, rent_mean)

# 年別の平均値
dataf <- dataf %>%
  dplyr::group_by(year)%>%
  dplyr::mutate(rent_mean_year=mean(rent))

dataf %>% dplyr::select(rent, rent_mean, rent_mean_year)

########################################
# 分位数: デフォルトは四分位数を出力
quantile(dataf$rent)
# 第1四分位を出力
quantile(dataf$rent)[2]
quantile(dataf$rent, prob=0.25)
# 例：第3四分位(75%)より大きければ１をとるダミー
dataf <- dataf %>%
  dplyr::mutate(rent_Q1=if_else(rent>quantile(rent)[4],1,0))

# probオプションで任意の分位数を取得
# 例：第9十分位
quantile(dataf$rent,prob=0.9)

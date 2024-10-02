library(tidyverse)

# データ読み込み
dataf <- 
  readr::read_csv("rent-shonandai96-04.csv")

# 複数の変数についてグループごとの平均値を計算
dataf %>% dplyr::group_by(year) %>% 
  summarise(m_rent=mean(rent),
            m_service=mean(service),
            m_floor=mean(floor),
            m_age=mean(age))

# across関数を使用
dataf %>% dplyr::group_by(year) %>% 
  summarise(dplyr::across(c("rent", "service", "floor", "age"),
                mean))

# グループ分けを解除しておく
dataf <- ungroup(dataf)

# mutate関数とacross関数を使用して複数の変数の平均値を計算
dataf <- dataf %>% 
  mutate(dplyr::across(c("rent", "service", "floor", "age"),
                mean, .names = "{col}_mean"))

dataf %>% dplyr::select(rent, floor, rent_mean,floor_mean)

# 対数値を計算
dataf <- dataf %>% 
  mutate(dplyr::across(c("rent", "service", "floor", "age"),
                log, .names = "{col}_log"))

# for文の使い方
for(x in 1:5){
  y <-x^2
  print(y)
}

# for文を使って偏差（平均値からの乖離）を計算する
for (col in c("rent", "service", "floor", "age")) {
  new_col <- paste0(col, "_demean")
  dataf[, new_col] <-dataf[[col]] -mean(dataf[[col]]) 
}
# 結果を表示させる
dataf %>% dplyr::select(rent,rent_mean,rent_demean)

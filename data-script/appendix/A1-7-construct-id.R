#
# グループに番号を付与する
#
library(tidyverse)

# データ読み込み
dataf <- 
  readr::read_csv("construct_id.csv")

# 通し番号を振る
dataf <- dataf %>%
  dplyr::mutate(seqno1=row_number())
dataf %>% dplyr::select(prefecture, city, seqno1)

# グループのIDを作成：グループごとにユニークな番号を振る
# 方法１
dataf <- dataf %>%
  dplyr::mutate(city_code1=prefecture*1000+city)

# 方法２
dataf <- dataf %>%
  dplyr::group_by(prefecture,city) %>%
  mutate(city_code2=cur_group_id())

dataf %>% dplyr::select(city_code1, city_code2)


# グループのIDごとに通し番号を振る
dataf <- dataf %>% group_by(city_code2) %>% mutate(seqno2=row_number(city_code2))
dataf %>% dplyr::select(city_code1, city_code2, seqno2)



library(tidyverse)
#データ読み込み
# rentに文字列が含まれているデータを読み込む
dataf <- readr::read_csv("rent-shonandai96-04-w-error.csv")

# mutateでrentを使って計算しようとするとエラーが発生
# 先に進むには8行目に#を付ける
dataf <- dataf %>% dplyr::mutate(rent_total=rent+service)

# データの中身を確認
dataf

table(substr(dataf$rent,1,1))

# rentが"Y"で始まるデータを表示する
dataf %>% dplyr::filter(substr(dataf$rent,1,1)=="Y")

# rentが"Y"で始まるデータを除外する方法
dataf_rev <- dataf %>% dplyr::filter(substr(dataf$rent,1,1)!="Y")
dataf_rev
dataf_rev <- dataf_rev %>% dplyr::mutate(rent=as.numeric(rent))
dataf_rev

# rentが"Y"で始まるデータを除外する方法
dataf <- dataf %>% dplyr::mutate(rent=if_else(rent=="Yes18","18",rent))
dataf <- dataf %>% dplyr::mutate(rent=as.numeric(rent))
dataf

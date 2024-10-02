##差し当たり1-3章で必要なパッケージをインストールする
# 4-5行目のインストールする際は行頭の"#"を消す、
# 2回目以降は再度のインストールは不要なので"#"を入れる
#install.packages(c("tydiverse","estimatr","modelsummary",
#"psych","tableone","openxlsx","tableone","sandwich"))

library(tidyverse)
# データ読み込み
dataf <- readr::read_csv("rent-shonandai96-04.csv")

# datafの中身を確認
dataf
glimpse(dataf)
glimpse(dataf$rent)

dataf$rent %>% mean() 

mean(dataf$rent)


#### 変数を限定する
dplyr::select(dataf, rent, auto_lock)

# パイプ(%>%)を使った書き方
dataf %>% dplyr::select(rent, auto_lock)

# 変数を限定して新しいオブジェクトを作成
dataf2 <- dataf %>%
  dplyr::select(rent, service, floor, age)

#### 条件にあうデータを抽出する
# 駅まで交通機関でバスを利用する
# 物件（バス利用所要時間bus>0）を抽出する
dataf %>% dplyr::filter(bus>0)

# 条件式は2つ以上並べる
# 例：バスを利用しかつ築年数ageが10年以上のもの
dataf %>% dplyr::filter(bus>0&age>10)

#### データを並び替える
# 駅からの徒歩分数walkの小さいものから並べる
dataf %>% dplyr::arrange(walk)

# 大きいほうから並べる場合
dataf %>% dplyr::arrange(-walk)

# 2変数での並び替え：変数1で並び替え、変数1が
# 同じ値をとるものについては変数2で並び替える
dataf %>% dplyr::arrange(walk,floor)

#### # 新しい変数を作成：dplyr::mutate
# 賃貸料rentと管理費serviceの合計値をrent_totalにする
dataf <- dataf %>% dplyr::mutate(rent_total=rent+service)
# 徒歩所要時間walkとバス所要時間busの合計をdistにする
dataf <- dataf %>% dplyr::mutate(dist=walk+bus)

dataf$rent_total2 <-dataf$rent+dataf$service

dataf %>% dplyr::select(rent, service, rent_total, rent_total2, walk, bus, dist)




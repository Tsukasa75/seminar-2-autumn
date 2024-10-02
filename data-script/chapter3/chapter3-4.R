library(tidyverse)
library(modelsummary)
library(psych)

dataf <-
  readr::read_csv("rent-kunitachi.csv")

# データを確認
glimpse(dataf)
dataf %>%
  psych::describe(skew=FALSE)

cor(dataf$floor,dataf$bus, use="pairwise.complete.obs")


# 欠損値をゼロにする、その後busを確認
# is.na(x)は「xが欠損値」を示す
dataf <- dataf %>% dplyr::mutate(bus=if_else(is.na(bus),0,bus))
glimpse(dataf$bus)
psych::describe(dataf$bus, skew=FALSE)
cor(dataf$floor,dataf$bus, use="pairwise.complete.obs")

dataf %>% dplyr::select(rent,service,floor,walk,bus,n_room) %>% cor(use="pairwise.complete.obs")
# cor()で変数に欠損値がある中、"pairwise.complete.obs"オプションを外すとどうなるか
dataf %>% dplyr::select(rent,service,floor,walk,bus,n_room) %>% cor()

# mutate: 新しい変数の作成
dataf <- dataf %>%
  dplyr::mutate(dist=bus+walk)

# rentは1万円単位なので、単位をそろえる
dataf <- dataf %>%
  dplyr::mutate(rent_total=rent+service/10000)

### 外れ値の対応
# arrange(x)で、xを小さいほうから並べる
# arrange(-x)で、xを大きいほうから並び替える
dataf %>%
  dplyr::arrange(-rent)

# 文字列の扱い
dataf <-
  dataf %>%
  dplyr::mutate(kunitachi_city=if_else(city=="kunitachi_city",1,0))


# 外れ値を除外したデータフレームdataf2を作成
dataf2 <- dataf %>%
  dplyr::filter(dataf$rent<30000)


# 記述統計をチェック
psych::describe(dataf2,skew=FALSE)

regs <-
  list(
  "model1" =lm(rent_total~floor+age+dist+kunitachi_city ,data=dataf),
  "model2" =lm(rent_total~floor+age+dist+kunitachi_city ,data=dataf2)
  )
modelsummary::msummary(regs, stars=TRUE , gof_omit='RMSE|AIC|BIC|Log.Lik.|F')

# 欠損値のあるデータを排除したオブジェクトの作成
dataf3 <- dataf %>% dplyr::filter(!is.na(floor))
# すべの変数のサンプル数がfloorのサンプル数131と同一になっていることを確認
dataf3 %>% psych::describe(skew=FALSE)

############################################
# rent=30000のAlternativeな処理法
# rent=30000の物件のrent_totalを欠損値にする
dataf$rent_total[dataf$rent==30000] <-NA
dataf %>% dplyr::arrange(-rent) %>% dplyr::select(rent_total, rent)
result <- lm(rent_total~floor+age+dist+kunitachi_city ,data=dataf)
summary(result)

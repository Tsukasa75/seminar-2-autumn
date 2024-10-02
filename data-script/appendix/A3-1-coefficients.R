# 回帰分析の結果から係数を取り出す
# このスクリプトで必要なパッケージ
# tidyverse, psych
library(tidyverse)
library(psych)
# データ読み込み

# データ読み込み
dataf <- 
  readr::read_csv("rent-shonandai96-04.csv")

dataf <- dataf %>%
  dplyr::mutate(rent_total=rent+service)

# 回帰分析
model_linear <-
  lm(rent_total~floor,data=dataf)
summary(model_linear)

# 係数の取り出し
coef <- model_linear$coefficients
coef

# 取り出し係数で予測値を計算する
dataf <- dataf %>% dplyr::mutate(pred1=coef[1]+coef[2]*floor)

# fitted.valueで予測値を取り出す
dataf <- dataf %>% mutate(pred2=model_linear$fitted.value)
dataf %>% dplyr::select(rent, pred1, pred2)

# 推計に使った変数の基本統計量を出力
model_linear$model %>% psych::describe(skew=FALSE)

# 決定係数を取り出す
model_linear2 <- summary(model_linear)
r2 <- model_linear2$r.squared
r2
# 自由度調整済み
adj_r2 <- model_linear2$adj.r.squared
adj_r2

#####################################
# logitmfx, probitmfxの係数の取り出し
#####################################
library(tidyverse)
library(margins)
library(mfx)
library(openxlsx)
# 事前にパッケージwooldridgeのインストールが必要
#data(mroz, package='wooldridge')
# データ読み込み
mroz <- read_csv("mroz.csv")

# 限界効果の出力:mfxパッケージのインストールと呼び出しが必要
res1_mfx <-logitmfx(inlf~age+educ+kidslt6+kidsge6,
                  data=mroz)
res1_mfx
res1_mfx[["mfxest"]]
result <- res1_mfx[["mfxest"]]
write.csv(result,'result.csv')
openxlsx::write.xlsx(result,'result.xlsx')


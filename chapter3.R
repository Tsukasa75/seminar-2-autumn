library(dplyr)
library(psych)
library(modelsummary)
dataf <- read.csv("data-script/chapter3/rent-shonandai96-04.csv")

dataf <- dataf %>% dplyr::mutate(
  rent_total=rent+service,
  dist=bus+walk
)

# 回帰分析

# 表し方
# model_linear <- lm(fomula=非説明変数~説明変数, data=データフレーム名)
# summary(model_linear)

# 単回帰分析

lm(formula=rent_total ~ floor, data=dataf) %>% summary()

# 重回帰分析

lm(formula=rent_total ~ floor+age, data=dataf) %>% summary()

# 重回帰モデルによる予測値

model_linear <- lm(rent_total~floor+age, data=dataf)
summary(model_linear)
dataf <- dataf %>% dplyr::mutate(
  y_hat=predict(model_linear), # 予測値
  res=residuals(model_linear) # 残差
)

dataf %>% dplyr::arrange(res) %>% dplyr::select(rent_total, y_hat, res, floor, age)

# 3.2.3 複数の推計結果の取りまとめ
regs <- list(
  "model1"=lm(rent_total~floor, data=dataf),
  "model2"=lm(rent_total~floor+age, data=dataf),
  "model3"=lm(rent_total~floor+age+dist, data=dataf)
)
regs

modelsummary::msummary(regs, stars=TRUE, gof_omit="RMSE|AIC|BIC|Log.Lik.|F")

# wordに出力
modelsummary::msummary(regs, stars=TRUE, gof_omit="RMSE|AIC|BIC|Log.Lik.|F", 'results.docx')
# latexに出力
modelsummary::msummary(regs, stars=TRUE, gof_omit="RMSE|AIC|BIC|Log.Lik.|F", "results.tex")
# excelに出力
results_tab <- modelsummary::msummary(regs, got_omit="RMSE|AIC|BIC|Log.Lik.|F", "data.frame")
openxlsx::write.xlsx(results_tab, 'results.xlsx')

# if_elseを用いてダミー変数を作成
dataf <- dataf %>% dplyr::mutate(d_autolock=if_else(auto_lock=="Yes", 1, 0))
# case_whenを用いてダミー変数を作成
dataf <- dataf %>% dplyr::mutate(
  d_autolock1=
    case_when(auto_lock=='Yes'~1, auto_lock=='No'~0)
)
# 0で変数を作成し、条件を満たすものだけ1にする方法でダミー変数を作成
dataf$d_autolock2 <- 0
dataf$d_autolock2[dataf$auto_lock=="Yes"] <- 1

# 回帰式を推定
lm(formula=rent_total~floor+age+dist+d_autolock, data=dataf) %>% summary()

# 事例紹介5 大学教授の賃金関数
dataf2 <- read.csv("data-script/chapter3/wage-census2019-professor.csv")
lm(formula=income~age+I(age^2)+factor(pos)+factor(size), data=dataf2) %>% summary()
lm(formula=income~age+factor(pos)+factor(size), data=dataf2) %>% summary()

# 3.3.4 多重共線性
dataf3 <- read.csv("data-script/chapter3/wage-census2022.csv")
lm(formula=wage~age+tenure+male, data=dataf3) %>% summary()

# 相関係数の高い変数を除去
lm(formula=wage~tenure+male, data=dataf3) %>% summary()

# 3.3.5 対数変換した回帰式の係数の意味
dataf <- readr::read_csv("data-script/chapter3/gravity-g20asean.csv")
dataf %>% dplyr::select(Importer, Exporter, Trade, GDPex, GDPim, distance, FTA)

lm(log(Trade)~log(GDPex)+log(GDPim)+log(distance)+FTA, data=dataf) %>% summary()

# 3.4 実データによる回帰分析のための準備
dataf <- readr::read_csv("data-script/chapter3/rent-kunitachi.csv")

# データを確認
glimpse(dataf)
dataf %>% psych::describe(skew=FALSE) # skew=歪度
cor(dataf$floor, dataf$bus, use="pairwise.complete.obs")

# busの欠損値を調整
dataf <- dataf %>% dplyr::mutate(bus=if_else(is.na(bus), 0, bus)) # あったらbusの値のまま
glimpse(dataf$bus)
dataf %>% psych::describe(skew=FALSE) # skew=歪度
cor(dataf$floor, dataf$bus, use="pairwise.complete.obs")

# floor の欠損値2件は0だと不自然なので放置

# rentとserviceの単位を揃える(万円, 円)
dataf <- dataf %>% dplyr::mutate(rent_total=rent+service/10000)

# rentの異常値を修正
dataf %>% arrange(-rent)
dataf2 <- dataf %>% dplyr::filter(dataf$rent<30000)
max(dataf2$rent)
nrow(dataf)
nrow(dataf2)
# 回帰分析を行い、外れ値の有無での結果を比較
dataf <- dataf %>% dplyr::mutate(
  kunitachi_city=if_else(city=="kunitachi_city", 1, 0),
  dist=walk+bus
)
dataf2 <- dataf2 %>% dplyr::mutate(
  kunitachi_city=if_else(city=="kunitachi_city", 1, 0),
  dist=walk+bus
)
nrow(dataf)
nrow(dataf2)
regs <-
  list(
    "model1" = lm(rent_total~floor+age+dist+kunitachi_city, data=dataf),
    "model2" = lm(rent_total~floor+age+dist+kunitachi_city, data=dataf2)
  )
msummary(regs, stars=TRUE, got_omit='RMSE|AIC|BIC|Log.Lik.|F') # 比較結果が変わらない？

# 外れ値をNAとして処理するバージョン
dataf$rent_total[dataf$rent==30000] <- NA
dataf %>% dplyr::arrange(-rent) %>% dplyr::select(rent_total, rent)

# 3.5.3 頑健な標準誤差
install.packages("estimatr")
library("estimatr")
# estimatr::lm_robust(y~x1+x2+x3, data=dataf) で推計が可能

# 不均一分散
library("readr")
dataf <- readr::read_csv("data-script/chapter3/educ-income.csv")
# 最小二乗法
model_linear <- lm(educ~income, data=dataf)
summary(model_linear)
# 頑健な標準誤差
model_linear <- estimatr::lm_robust(educ~income, data=dataf)
summary(model_linear)
# 加重最小二乗法
model_linear <- lm(educ~income, data=dataf, weights = 1/pop)
summary(model_linear)

# lm関数で頑健な標準誤差を表示
# 使うデータはrent-shonandaだった
dataf <- readr::read_csv("data-script/chapter3/rent-kunitachi.csv")
dataf <- dataf %>% dplyr::mutate(bus=if_else(is.na(bus), 0, bus)) # あったらbusの値のまま
dataf <- dataf %>% dplyr::mutate(
  rent_total=rent+service/10000,
  dist=bus+walk
)
dataf$rent_total[dataf$rent==30000] <- NA
regs <- list(
  "model1" <- lm(rent_total~floor, data=dataf),
  "model2" <- lm(rent_total~floor+age, data=dataf),
  "model3" <- lm(rent_total~floor+age+dist, data=dataf)
 )
modelsummary::modelsummary(regs, stars = TRUE,
gof_omit = 'RMSE|AIC|BIC|Log.Lik.|F', vcov="HC2")

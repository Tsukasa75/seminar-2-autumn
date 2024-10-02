# このスクリプトで必要なパッケージ
# tidyverse
# modelsummary
# openxlsx

library(tidyverse)
library(modelsummary)
library(openxlsx)
# データ読み込み

# データ読み込み
dataf <- 
  readr::read_csv("rent-shonandai96-04.csv")

# mutate: 新しい変数の作成
dataf <- dataf %>%
  dplyr::mutate(rent_total=rent+service) 

dataf <- dataf %>%
  dplyr::mutate(dist=bus+walk) 

# 3.2 回帰分析
model_linear <-
  lm(rent_total~floor,data=dataf)
summary(model_linear)

# 3.2.2 重回帰モデル
model_linear <-
  lm(rent_total~floor+age, data=dataf)
summary(model_linear)

# 注意　係数の大きさと説明変数の説明力
dataf <- dataf %>%
  dplyr::mutate(age_rev=age/12)
model_linear <-
  lm(rent_total~floor+age_rev, data=dataf)
summary(model_linear)


# 自由度調整済み決定係数
model_linear <-
  lm(rent_total~floor+age+dist,data=dataf)
summary(model_linear)

# 予測値
model_linear <-
  lm(rent_total~floor+age, data=dataf)
dataf <- dataf %>%
  dplyr::mutate(y_hat=predict(model_linear))
dataf <- dataf %>%
  dplyr::mutate(res=residuals(model_linear))
# 残差が小さいほうから並べて、実績値と予測値を表示させる
dataf %>%
  dplyr::arrange(res) %>%
  dplyr::select(rent_total, y_hat, res, floor, age)

# 結果の出力 modelsummaryのインストールとlibraryでの呼び出しが必要
regs <-
  list(
    "model1" =lm(rent_total~floor,data=dataf),
    "model2" =lm(rent_total~floor+age,data=dataf),
    "model3" =lm(rent_total~floor+age+dist,data=dataf)
  )
modelsummary::msummary(regs, stars=TRUE , gof_omit='RMSE|AIC|BIC|Log.Lik.|F')

#################################
# 以下はmsummaryのオプションを変更すると結果がどう変わるかを示しています
# オプションなし
modelsummary::msummary(regs, stars=TRUE)
# *の付け方を変更
modelsummary::msummary(regs, stars = c("*" = .1, "**" = .05, "***" = .01))
# RMSE|AIC|BIC|Log.Lik.の表示を省略
modelsummary::msummary(regs, stars= c("*" = .1, "**" = .05, "***" = .01) , gof_omit='RMSE|AIC|BIC|Log.Lik.')
# 小数点下4桁まで表示
modelsummary::msummary(regs, stars=TRUE ,fmt='%.4f', gof_omit='RMSE|AIC|BIC|Log.Lik.|F')
# 頑健な標準誤差を出力
modelsummary::msummary(regs, stars=TRUE , gof_omit='RMSE|AIC|BIC|Log.Lik.|F',vcov="robust")
#################################


# msummaryの結果をEXCELに出力
results_tab <- msummary(regs, gof_omit='RMSE|AIC|BIC|Log.Lik.|F', 'data.frame')
openxlsx::write.xlsx(results_tab, 'results.xlsx')

#3.3 ダミー変数

#3.3.1 オートロックダミー
#方法1 dplyr::mutateとif_elseを使う方法
dataf <-
  dataf %>%
  dplyr::mutate(d_autolock=if_else(auto_lock=="Yes",1,0))

#方法2 dplyr::mutateとcase_whenを使う方法
dataf <-
  dataf %>%
  dplyr::mutate(d_autolock1=case_when(auto_lock=="Yes"~1,
                                     auto_lock=="No"~0))

#方法3 []で条件式を指定する方法
dataf$d_autolock2 <- 0
dataf$d_autolock2[dataf$auto_lock=="Yes"] <-1

# 作成された変数の確認
dataf %>% dplyr::select(rent_total,floor,auto_lock,d_autolock,d_autolock1,d_autolock2)
table(dataf$d_autolock)
table(dataf$d_autolock2)

# オートロックダミーを導入した回帰式
model_linear <-
  lm(rent_total~floor+age+dist+d_autolock,data=dataf)
summary(model_linear)

# auto_lockをそのまま説明変数に追加
model_linear <-
  lm(rent_total~floor+age+dist+auto_lock,data=dataf)
summary(model_linear)


# 3.3.2 他の要因をコントロールする
# model summary
regs <-
  list(
    "model1" =lm(rent_total~auto_lock,data=dataf),
    "model2" =lm(rent_total~auto_lock+floor,data=dataf),
    "model3" =lm(rent_total~auto_lock+floor+age,data=dataf)
  )
modelsummary::msummary(regs, stars=TRUE , gof_omit='RMSE|AIC|BIC|Log.Lik.|F')

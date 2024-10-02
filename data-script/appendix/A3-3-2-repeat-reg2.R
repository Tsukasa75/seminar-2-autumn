library(tidyverse)
library(modelsummary)
library(fixest)
library(openxlsx)

# データ読み込み
dataf <- 
  readr::read_csv("wage-census2022-by-scale.csv")

#########################################
# lm()関数とmodelsummary()を使う場合
#########################################

# 1) リストを初期化しておく
model_list <- list()

# 2) dep(被説明変数)に"wage_month", "wage_annual", "wage_hour"を入れていく
for (dep in c("wage_month", "wage_annual", "wage_hour")) {
  # 3) 推計式の名前dep_nameを作成、ここではmodel+被説明変数とする
  dep_name <- paste0("model_",dep)
  # 4) 被説明変数+説明変数をオブジェクトfmに格納する
  fm <- paste0(dep, "~ age+I(age^2)+factor(education)+male+factor(size)")
  # 5) 推計結果を推計式の名前dep_nameとともにオブジェクトmodel_listに格納
  model_list[[dep_name]] <- lm(fm, data = dataf)
}
results_tab <- msummary(model_list, stars = TRUE, gof_omit='RMSE|AIC|BIC|Log.Lik.|F', 'data.frame')
openxlsx::write.xlsx(results_tab, 'results.xlsx')

#########################################
# feols()関数とetabley()を使う場合
#########################################
# リストを初期化
model_list <- list()

for (dep in c("wage_month", "wage_annual", "wage_hour")) {
  dep_name <- paste0("model_", dep)
  fm <- paste0(dep, "~ age+I(age^2)+factor(education)+male+factor(size)")
  # feolsの場合は、"被説明変数＋説明変数"のオブジェクトはfomula(fm)で指定する 
  # また推計結果をmodelとする
  model <- fixest::feols(formula(fm), data = dataf)
  # 推計結果を一度summary()に入れ、これをmodel_listに格納する
  model_summary <- summary(model)
  model_list[[dep_name]] <- model_summary
}
etable(model_list,se.below = TRUE)

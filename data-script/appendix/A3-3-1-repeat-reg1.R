library(tidyverse)
library(modelsummary)

# データ読み込み
dataf <- readr::read_csv("panel-gravity-data.csv")

dataf <- dataf %>% dplyr::mutate(ltrade=log(tvalue/GDPdef_o))

model <- "ltrade~log(GDPR_o)+log(GDPR_d)+log(distw)+contig+comlang_off+fta"

# 1) リストを初期化しておく
model_list <-list()

# 2) yに1980, 1990, 2000を入れていく
for(y in c(1980, 1990, 2000)) {
  # 3) 推計結果の名前res_nameを作成、ここではresult+年(y)とする
  res_name <-paste0("result",y)
  # 4) 条件を満たすサブ・データを作成
  data_sub <- dataf %>% dplyr::filter(year == y)
  # 5) 推計結果を推計式の名前res_nameとともにオブジェクトmodel_listに格納
    model_list[[res_name]] <-  lm(model, data = data_sub)
}  
msummary(model_list, stars = TRUE, gof_omit='RMSE|AIC|BIC|Log.Lik.|F')

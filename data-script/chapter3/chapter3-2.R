# このスクリプトで必要なパッケージ
# tidyverse
# modelsummary

library(tidyverse)
library(modelsummary)

# 賃金データ読み込み
dataf <- read_csv("wage-census2022.csv")
glimpse(dataf)

#教育水準ごとに平均賃金を計算
dataf %>%
  dplyr::group_by(education) %>%
  summarize(mean(wage))

# 学歴ダミー
dataf <-
  dataf %>%
  mutate(edu1=if_else(education==1,1,0))
dataf <-
  dataf %>%
  mutate(edu2=if_else(education==2,1,0))
dataf <-
  dataf %>%
  mutate(edu3=if_else(education==3,1,0))
dataf <-
  dataf %>%
  mutate(edu4=if_else(education==4,1,0))

dataf %>%
  dplyr::select(age, male, education,edu1,edu2,edu3,edu4)

# 学歴ダミーを一つ除く場合
model_linear <-
  lm(wage~age+male+edu2+edu3+edu4,data=dataf)
summary(model_linear)
# 学歴ダミーを全部（４つ）入れた場合
model_linear <-
  lm(wage~age+male+edu1+edu2+edu3+edu4,data=dataf)
summary(model_linear)

# パイプ%>%を使えば以下のように書くこともできます
lm(wage~age+male+edu2+edu3+edu4,data=dataf) %>%
  summary()

# factor()関数による学歴ダミーの導入
model_linear <-
  lm(wage~age+male+factor(education),data=dataf)
summary(model_linear)

#########################################
# 以下は# 補論C.1の3で紹介するfactor()関数
# の基準の変更のためのスクリプトです
# educationをfactor()でカテゴリー化した際の基準の確認
levels(factor(dataf$education))
# educationを2.高卒を基準にしてカテゴリー化し、
# education2に導入
dataf$education2 <-factor(dataf$education, levels=c("2","1","3","4"))
# levels()関数でeducation2の基準が"2"に変更されていることを確認
#education2はすでにカテゴリー変数化されているので改めてfactor()関数は必要ない
levels(dataf$education2)
# factor(education)の代わりにeducation2を説明変数とする回帰式を推定
model_linear <-
  lm(wage~age+male+education2,data=dataf)
summary(model_linear)
# 補論C.1 基準変更ここまで
#########################################

# 交差項
dataf <-
  dataf %>%
  mutate(age_male=age*male)
glimpse(dataf)

model_linear <-
  lm(wage~age+male+age_male,data=dataf)
summary(model_linear)

# 同じことを、":"で。
# ":"で相互作用のみ	
model_linear <-
  lm(wage~age+male+age:male,data=dataf)
summary(model_linear)

# 同じことを、"*"で。	
# "*"を使用すると、単体・相互作用をすべて自動的に作成・分析	
model_linear <-lm(wage~age+male+age_male,data=dataf)
model_linear <-
  lm(wage ~ age*male, data=dataf)
summary(model_linear)

# 年齢の２乗項
model_linear <-
  lm(wage~age+male,data=dataf)
summary(model_linear)


dataf <-
  dataf %>%
  mutate(age2=age^2)

model_linear <-
  lm(wage~age+male+age2,data=dataf)
summary(model_linear)

model_linear <-
  lm(wage~age+male+I(age^2),data=dataf)
summary(model_linear)

model_linear <-
  lm(wage~age+male,data=dataf)
summary(model_linear)

# 多重共線性
model_linear <-
  lm(wage~age+tenure+male,data=dataf)
summary(model_linear)

cor(dataf$age, dataf$tenure, use="pairwise.complete.obs")

model_linear <-
  lm(wage~tenure+male,data=dataf)
summary(model_linear)


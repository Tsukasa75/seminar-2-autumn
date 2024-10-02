library(tidyverse)
library(readxl)

# データの読み込み
caprate <-readxl::read_excel("asai-data-construct.xlsx", sheet="caprate")
caprate

# 1) WIDE形式をLONG形式に変換
caprate <- tidyr::pivot_longer(caprate,starts_with("caprate"), names_to="year",values_to="caprate")
caprate
caprate$year <- gsub("caprate","",caprate$year)
caprate
# yearが文字列<chr>になっているので変換しておく
caprate$year <-as.numeric(caprate$year)
caprate

# 2) データの縦方向の接続:bind_rows
caprate_hokkaido <-readxl::read_excel("asai-data-construct.xlsx", sheet="caprate-hokkaido")
caprate_hokkaido
caprate <-bind_rows(caprate_hokkaido, caprate)
caprate

# 北海道のみpref_idとprefが欠損しているので埋める
caprate$pref_id[is.na(caprate$pref_id)] <- 1 
caprate$pref[is.na(caprate$pref)] <- "北海道"
caprate

# 3) データの横方向の接続(識別番号を２つ用いるケース)
emprate <-readxl::read_excel("asai-data-construct.xlsx", sheet="emprate")
dataf <-merge(caprate,emprate,by=c("year","pref_id"))
dataf
table(dataf$year)


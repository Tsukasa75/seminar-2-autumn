#
# 集計量から構成されるデータセットを作成する
#
# 注意：事前に"doBy"のインストールが必要

library(tidyverse)
library(doBy)
# データ読み込み
dataf <- 
  readr::read_csv("rent-odakyu-enoshima96-04.csv")

dataf_agg <- summaryBy(rent+age~station,data=dataf)
dataf_agg

dataf_agg <- summaryBy(rent+age~year+station,data=dataf,FUN=list(mean,sum))
dataf_agg


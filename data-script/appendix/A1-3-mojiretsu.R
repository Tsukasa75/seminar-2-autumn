#データの読み込み
library(tidyverse)
library(stringr)

dataf <- readr::read_csv("mojiretsu.csv")

dataf

#################################################
# codeから文字を抽出する
#################################################

# codeの3文字目を抽出（start=3, stop=3で3文字目から3文字目）
dataf <- dataf %>% mutate(corp=substr(code,start=3, stop=3))
dataf$corp

# codeの1文字目から2文字目を抽出して数値にする
dataf <- dataf %>% mutate(industry=substr(code,start=1, stop=2))
dataf
dataf <- dataf %>% mutate(industry=as.numeric(industry))
dataf

# 上記の2行を1行で記述した場合
dataf <- dataf %>% mutate(industry=as.numeric(substr(code,start=1, stop=2)))


#################################################
# idの1文字目から2文字目を抽出して数値にする
#################################################
# idを文字列にする
dataf <- dataf %>% mutate(id=as.character(id))
# idの最初の2文字を抽出する
dataf <- dataf %>% mutate(prefecture=substr(id,start=1, stop=2))
dataf
# 数値に変換
dataf <- dataf %>% mutate(prefecture=as.numeric(prefecture))

# 上記の3行を1行で記述した場合
dataf <- dataf %>% mutate(prefecture=
                            as.numeric(
                              substr(as.character(dataf$id),start=1, stop=2)))
dataf

#################################################
# addressから市区町村名を抽出する
#################################################
# 東京都なら１をとるダミー変数
dataf <- dataf %>% 
  mutate(d_tokyo=if_else(str_count(address,"東京都")>0,1,0))
table(dataf$d_tokyo)

         
# 市区を抽出する
dataf$city_name <- substr(dataf$address, 
                          str_locate(dataf$address, "[都 県]") + 1, 
                          str_locate(dataf$address, "[区市]"))
dataf %>% dplyr::select(address,city_name)


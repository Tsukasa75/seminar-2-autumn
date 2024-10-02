#データの読み込み
library(tidyverse)

# EXCELファイルの読み込み
library(readxl)
dataf <- readxl::read_excel("wage-census2022.xlsx", sheet="wage-census2022")

# Stata dtaファイルの読み込み
library(haven)
dataf <- haven::read_dta("wage-census2022.dta")

# Rds形式でデータフレームを保存
readr::write_rds(dataf,"wage-census2022.Rds")

# 一度、enviromentペインのオブジェクトを消す
rm(list=ls())

# Rds形式のファイルを呼び出す
dataf2 <- readr::read_rds("wage-census2022.Rds")

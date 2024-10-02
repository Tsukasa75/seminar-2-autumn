library(tidyverse)

# 文字化けする場合
dataf <- readr::read_csv("rent-odakyu-enoshima96-04-shift-jis.csv")
dataf %>% dplyr::select(rent, floor, station,station_j)


dataf <- readr::read_csv("rent-odakyu-enoshima96-04-shift-jis.csv", locale = locale(encoding = "shift-jis"))
dataf %>% dplyr::select(rent, floor, station,station_j)

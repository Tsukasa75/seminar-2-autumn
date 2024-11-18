library(readr)
library(dplyr)
library(psych)
library(modelsummary)

# (1) y=rent_total, x=floor, age, dist, 
dataf <- readr::read_csv("data-script/chapter2/rent-jonan-kawasaki.csv")
dataf <- dataf %>% dplyr::mutate(
  rent_total=rent+service,
  dist=walk+bus
)
model_linear <- lm(
  
)
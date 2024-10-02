library(tidyverse)
# データ読み込み

# データ読み込み
dataf <- readr::read_csv("panel-gravity-data.csv")

## 1980, 1990, 2000年の3時点のデータで

dataf <- dataf %>% dplyr::mutate(ltrade=log(tvalue/GDPdef_o))

dataf1980 <- dplyr::filter(dataf, year==1980)
dataf1990 <- dplyr::filter(dataf, year==1990)
dataf2000 <- dplyr::filter(dataf, year==2000)

result1980 <- lm(ltrade~log(GDPR_o)+log(GDPR_d)+log(distw)+
     contig+comlang_off+fta,data=dataf1980)
summary(result1980)
result1990 <- lm(ltrade~log(GDPR_o)+log(GDPR_d)+log(distw)+
     contig+comlang_off+fta,data=dataf1990)
summary(result1990)
result2000 <- lm(ltrade~log(GDPR_o)+log(GDPR_d)+log(distw)+
     contig+comlang_off+fta,data=dataf2000)
summary(result2000)

model <- "ltrade~log(GDPR_o)+log(GDPR_d)+log(distw)+contig+comlang_off+fta"
result1980 <- lm(model,data=dataf1980)
summary(result1980)
result1990 <- lm(model,data=dataf1990)
summary(result1990)
result2000 <- lm(model,data=dataf2000)
summary(result2000)


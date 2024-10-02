library(tidyverse)
library(ggrepel)

########################
# 折れ線グラフ
########################
dataf <- readr::read_csv("wage-census2022.csv")
dataf <- dataf %>% dplyr::filter(male==1&education==4)

g_line <-ggplot(data=dataf,aes(x=age,y=wage))+
  geom_line()+geom_point()+theme_classic()
g_line

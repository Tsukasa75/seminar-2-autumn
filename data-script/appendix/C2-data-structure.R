# C.2 データの構造
library(tidyverse)

# 1 ベクトル
V1=c(1,2,3,4)
V1
V2=c(5,6,7,8)
V3=c("taro","jiro","saburo","shiro")
V3

# 2 行列
mat1 <- matrix(V1)
mat1

mat2 <- matrix(V1, nrow=2)
mat2

# 3 data frame
id <- c(1:3)
name <-c("taro","jiro","saburo")
dataf <-tibble(id,name)
dataf

# 4 list
list_test <-list(V1, mat1,name)
list_test

list_test[3]

list_test[[3]][1]

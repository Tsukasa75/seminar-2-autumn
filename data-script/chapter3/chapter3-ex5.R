#### 事例事例5 大学教員の賃金関数 ####
# position
# Assistant: Assistant Professor, 助教
# Associate: Associate Professor, 准教授
# Full: Full professor, 教授

library(tidyverse)

dataf <-
  readr::read_csv("wage-census2019-professor.csv")

#dataf <-dataf %>% dplyr::mutate(income=income/100)

result <- lm(income~age+factor(pos)+factor(size),data=dataf)
summary(result)

result <- lm(income~age+I(age^2)+factor(pos)+factor(size),data=dataf)
summary(result)

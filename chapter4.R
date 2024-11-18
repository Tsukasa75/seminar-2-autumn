library(wooldridge)
data(mroz, package = 'wooldridge')
install.packages("DescTools")
library("DescTools")

# プロビット, ロジットの推計
dataf <- mroz
model_logit <- glm(y~x1+x2+x3, family=binomial(link="logit"), data=dataf)
model_probit <- glm(y~x1+x2+x3, family=binomial(link="logit"), data=dataf)
                                      
# 擬似決定係数を計算
DescTools::PseudoR2(model_probit)

table(mroz$inlf)

result0 <- lm(inlf~age+educ+kidslt6+kidsge6, data=mroz)
summary(result0)
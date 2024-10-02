########################################
# 7.3 傾向スコア回帰
########################################
library(tidyverse)
library(tableone)
library(estimatr)
library(DescTools)
library(openxlsx)
library(modelsummary)

nswcps <- readr::read_csv("nswcps_psmatch.csv")

table(nswcps$treated)

nswcps %>% tableone::CreateTableOne(vars=(c("re78","age","educ","married","black","hispanic")),strata="treated")

# 傾向スコア回帰
result <- lm(re78 ~ treated+age+age2+educ+nodegree+black+hispanic+re74+re75, 
             data     = nswcps)
summary(result)

# Logit Model
PS_model <- glm(treated ~ age+age2+educ+educ2+married+nodegree+black+hispanic+re74+re75, family = binomial(link="logit"),data=nswcps)
summary(PS_model)
DescTools::PseudoR2(PS_model)

nswcps <- nswcps %>% mutate(Propensity_Score=predict(PS_model,type="response"))
### 予測値はPS_modelのfitted.valuesに格納されているので以下も同義です
#nswcps <- nswcps %>% mutate(Propensity_Score=PS_model$fitted.values)

# ATEのためのウエイト
nswcps <- nswcps %>% mutate(weight_ATE=ifelse(treated==1, 1/Propensity_Score, 1/(1-Propensity_Score)))
# ATTのためのウエイト
nswcps <- nswcps %>% mutate(weight_ATT=ifelse(treated==1, 1, Propensity_Score/(1-Propensity_Score)))

# 傾向スコア回帰
result <- lm(re78 ~ treated+age+age2+educ+educ2+married+nodegree+black+hispanic+re74+re75, 
             data     = nswcps,weights=weight_ATT)
summary(result)

# OLS（ウエイト無し）と傾向スコア回帰の比較
# 結果をEXCEL出力する
regs <- list(
  "OLS(unweighted)" = lm(re78 ~ treated+age+age2+educ+educ2+married+nodegree+black+hispanic+re74+re75, 
        data     = nswcps),
  "PS-reg(weighted)" = lm(re78 ~ treated+age+age2+educ+educ2+married+nodegree+black+hispanic+re74+re75, 
                data     = nswcps,weights=weight_ATT)
  )

modelsummary::msummary(regs,stars=TRUE, gof_omit='RMSE|AIC|BIC|Log.Lik.|F')
res_table <-modelsummary::msummary(regs,stars=TRUE, gof_omit='RMSE|AIC|BIC|Log.Lik.|F','data.frame')
openxlsx::write.xlsx(res_table,"ps_reg.xlsx")

library(tidyverse)
library(lmtest)
library(modelsummary)
library(DescTools)
library(margins)
library(openxlsx)
library(censReg)
library(mfx)

# 事前にパッケージwooldridgeのインストールが必要
data(mroz, package='wooldridge')

# データ読み込み
mroz <- read_csv("mroz.csv")

table(mroz$inlf)

#-----------------------------
# 最小二乗法による推計
#-----------------------------
result0 <- lm(inlf~age+educ+kidslt6+kidsge6,data=mroz)
summary(result0)
# 予測値の計算
mroz <- mroz %>% mutate(y_hat=predict(result0)) 
summary(mroz$y_hat)


#-----------------------------
# ロジット・モデルの推定  
#-----------------------------
result1 <- glm(inlf~age+educ+kidslt6+kidsge6,family=binomial(link=logit),data=mroz)
summary(result1)

# 予測値の計算
mroz <- mroz %>%
  dplyr::mutate(prob_res1=predict(result1,type="response"))
summary(mroz$prob_res1)

# McFadden's Pseudo R2 （疑似決定係数）
DescTools::PseudoR2(result1)

pseudoR2 <- 
  1 - (result1$deviance) / (result1$null.deviance)
pseudoR2

# 限界効果の出力:mfxパッケージのインストールと呼び出しが必要
res1_mfx <- 
  logitmfx(inlf~age+educ+kidslt6+kidsge6,
           data=mroz)
res1_mfx

# なお、以下で頑健な標準誤差を出力できます
lmtest::coeftest(result1, vcovHC,type="HC1")


#-----------------------------
# プロビット・モデルの推定  
#-----------------------------
result2 <- glm(inlf~age+educ+kidslt6+kidsge6,family=binomial(probit),data=mroz)
summary(result2)

# McFadden's Pseudo R2 （疑似決定係数）
DescTools::PseudoR2(result2)

# mfxパッケージのインストールと呼び出しが必要
res2_mfx <- 
  probitmfx(inlf~age+educ+kidslt6+kidsge6,
           data=mroz)
res2_mfx

#-----------------------------
# msummaryによる結果の出力
# 頑健な標準誤差による結果を出力
#-----------------------------
regs <- 
  list(
    "model1" <- glm(inlf~age+educ+kidslt6+kidsge6,family=binomial(link=logit),data=mroz),
    "model2" <- glm(inlf~age+educ+kidslt6+kidsge6,family=binomial(probit),data=mroz)
    )
msummary(regs,  stars=TRUE,gof_omit = 'AIC|BIC|Log.Lik.|F',vcov="HC1")
res_table <- modelsummary(regs, stars=TRUE,gof_omit = 'AIC|BIC|Log.Lik.|F',vcov="HC1",output="data.frame")
openxlsx::write.xlsx(res_table,"result.xlsx")

regs <- 
  list(
    "model1" <- glm(inlf~age+educ+kidslt6+kidsge6,family=binomial(link=logit),data=mroz),
    "model2" <- glm(inlf~age+educ+kidslt6+kidsge6,family=binomial(probit),data=mroz)
  )
msummary(regs, gof_omit = 'AIC|BIC|Log.Lik.|F',vcov="HC1")


#-----------------------------
# Tobit model
#-----------------------------
#被説明変数をhoursにする
#平均・最大・最小値
summary(mroz$hours)
# 就業女性に限定して労働時間hoursの平均値を計算
mroz %>% filter(inlf==1) %>% summarize(m_horus=mean(hours))

# censRegパッケージのインストールと呼び出しが必要
res_tobit1 <-censReg::censReg(hours~age+educ+kidslt6+kidsge6, left=0, data=mroz)
summary(res_tobit1)
summary(margEff(res_tobit1))

# 最小二乗法（OLS）との比較
result_ols <-lm(hours~age+educ+kidslt6+kidsge6,data=mroz)
summary(result_ols)


tobits <-	
  list(	
    "res_tobit1" <-	
      censReg(hours~age+educ+kidslt6+kidsge6,	
              left=0,	
              data=mroz),	
    "result_ols" <-	
      lm(hours~age+educ+kidslt6+kidsge6,	
         data=mroz)	
  )	
msummary(tobits, stars=TRUE, gof_omit = 'AIC|BIC|Log.Lik.|F')	
res_table <-	
  modelsummary(tobits, gof_omit = 'AIC|BIC|Log.Lik.|F',output="data.frame")	
write.xlsx(res_table,"tobit.xlsx")

# 傾向スコア法
# MatchiItのインストールが必要
library(tidyverse)
library(MatchIt)
library(tableone)
library(DescTools)
library(openxlsx)
library(modelsummary)

nswcps <- readr::read_csv("nswcps_psmatch.csv")
table(nswcps$treated)

nswcps %>% tableone::CreateTableOne(vars=(c("re78","age","educ","married","black","hispanic")),strata="treated")

# 結果をファイル出力する場合
#result_tableone <-nswcps %>% tableone::CreateTableOne(vars=(c("re78","age","educ","married","black","hispanic")),strata="treated")
#print(result_tableone) %>% write.csv("simple-comparison.csv")

# Logit Model
result_logit1<-glm(treated ~ age+age2+educ+educ2+married+nodegree
              +black+hispanic+re74+re75,family = binomial(link = "logit"),data=nswcps)
summary(result_logit1)
DescTools::PseudoR2(result_logit1)

# 結果をファイルに出力する場合は"#"を消す
#result_logit2<- summary(result_logit1)
#write.csv(result_logit2$coefficients,"ch7-logit.csv")

# matchitでマッチング
m_result1 <- MatchIt::matchit(treated ~ age+age2+educ+educ2+married+nodegree+black+hispanic+re74+re75,
                 data     = nswcps,
                 method   = "nearest",
                 distance = "glm",discard="both",
                 replace  = TRUE)

summary(m_result1)

# 結果のファイル出力
# マッチングの状況（サンプル数）
#write.csv(results_m_res1$nn,"ch7-matching.csv")
# 全体での処置群・比較群の差
#write.csv(results_m_res1$sum.all,"ch7-matching-all.csv")
# 処置群とマッチされた比較群の差
#write.csv(results_m_res1$sum.matched,"ch7-matching-matched.csv")


summary(m_result1) %>% plot(xlim=c(0,1.5))

# match.data関数で処置群とマッチさせた比較群だけのデータセットに変換
matched_data <- m_result1 %>% MatchIt::match.data()

# 処置効果の推計
m_result2 <- lm(re78 ~ treated,
     data =matched_data, weights = weights)
summary(m_result2)


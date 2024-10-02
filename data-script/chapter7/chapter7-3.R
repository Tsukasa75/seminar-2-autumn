########################################
# 7.4 パネルデータによる傾向スコアマッチング
library(tidyverse)
library(MatchIt)
library(plm)
library(tidyr)
library(modelsummary)
library(openxlsx)

# データの読み込み
dataf <-readr::read_csv("nlswork.csv")

# パネルデータのフォーマットを宣言
pdataf <- pdata.frame(dataf,index=c("idcode","time"))
pdataf <- pdataf %>%
  arrange(idcode, year)
  
### 差分を計算する ###
# t時点からt+1時点、あるいはt+2時点の変化を計算する
# 賃金変化を計算：対数値の差分なので変化率の近似値となる
# mutate関数とlagは併用できないので注意
pdataf$wgrowth=plm::lead(pdataf$ln_wage)-pdataf$ln_wage
pdataf$wgrowth2=plm::lead(plm::lead(pdataf$ln_wage))-pdataf$ln_wage

# t期のunionが0、t+1期のunionが1なら1．
# t期のunionが0、t+1期のunionが0なら0．
pdataf$dunion = case_when(
  (pdataf$union == 0 & plm::lead(pdataf$union) == 1) ~ 1,
  (pdataf$union == 0 & plm::lead(pdataf$union) == 0) ~ 0)  


# dunionが欠損値でない人に限定（is,na(x)は「xが欠損値」の意味）
pdataf <- pdataf %>% dplyr::filter(!is.na(dunion))

# ロジット・モデルの推定
ps_result0 <- glm(dunion~age+I(age^2)+race+nev_mar+ttl_exp+msp+grade+not_smsa+south+factor(year), family = binomial(link="logit"),data=pdataf)
summary(ps_result0)
DescTools::PseudoR2(ps_result0)

pdataf <- pdataf %>% tidyr::drop_na(age,nev_mar,not_smsa, grade)


ps_result1 <-MatchIt::matchit(dunion~age+I(age^2)+race+nev_mar+ttl_exp+msp+grade+not_smsa+south+factor(year),
                     data     = pdataf,
                     method   = "nearest",
                     distance = "glm",discard="both",
                     replace  = TRUE)
summary(ps_result1)
# ラブ・プロット
summary(ps_result1) %>% plot(xlim=c(0,1.5))

# match.data関数で処置群とマッチさせた比較群だけのデータセットに変換
matched_data <- ps_result1 %>% MatchIt::match.data()

# 処置効果の推計
regs <- list (
  "ols-wg1" =  lm(wgrowth ~ dunion,
              data =pdataf),
  "ols-wg2" =  lm(wgrowth2 ~ dunion,
                  data =pdataf),
  "psm-wg1" =  lm(wgrowth ~ dunion,
                 data =matched_data, weights = weights),
  "psm-wg2" =  lm(wgrowth2 ~ dunion,
     data =matched_data, weights = weights)
  )
modelsummary::msummary(regs,stars=TRUE)
res_table <-modelsummary::msummary(regs,stars=TRUE,'data.frame')
openxlsx::write.xlsx(res_table,"psmatch_res7-3.xlsx")

## 1
# 下準備
setwd("/Users/ebuchitsukasa/Desktop/university/seminar/seminar2autumn")
dataf <- read.csv("/Users/ebuchitsukasa/Desktop/university/seminar/seminar2autumn/data-script/chapter2/rent-jonan-kawasaki.csv")

# 1-1

prop.table(table(dataf$autolock, dataf$line), margin=2)
prop.table(table(dataf$line, dataf$autolock), margin=1)


# 1-2

prop.table(table(dataf$line, c(dataf$rent, dataf$age, dataf$floor)))

# xtabsを使用して集計表を作成
aggregated_table <- xtabs(some_value_column ~ line + rent + age + area, data = dataf)

# 1-3

# 1-4
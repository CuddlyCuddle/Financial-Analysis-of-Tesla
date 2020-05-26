# Analysis of Tesla Daily 
# 
# Jan1-2017 to May1-2020
#
# TSLAD Data
TSLAD <- read.csv("C:/Users/jeanp/OneDrive/Econ 500/TSLAD.csv")

TSLAD$Returns <- Returns_p(TSLAD$Adj.Close)

y <- TSLAD$Returns
x <- 5000
TSLAD_Invest <- KapitalGrowth(x, y)
sum(TSLAD$Returns, na.rm = TRUE)
summary(TSLAD$Returns)
sd(TSLAD$Returns, na.rm = TRUE)
var(TSLAD$Returns, na.rm = TRUE)
NDVaR(TSLAD$Returns)
NON_NDVaR(TSLAD$Returns)
summary(TSLAD_Invest)
tail(TSLAD_Invest, n =1)

y <- TSLAD$Returns[1:754]
x <- 5000
TSLAD_Invest <- KapitalGrowth(x, y)
sum(y, na.rm = TRUE)
summary(y)
sd(y, na.rm = TRUE)
var(y, na.rm = TRUE)
NDVaR(y)
NON_NDVaR(y)
summary(TSLAD_Invest)
tail(TSLAD_Invest, n =1)

y <- TSLAD$Returns[-(1:754)]
x <- 5000
TSLAD_Invest <- KapitalGrowth(x, y)
sum(y, na.rm = TRUE)
summary(y)
sd(y, na.rm = TRUE)
var(y, na.rm = TRUE)
NDVaR(y)
NON_NDVaR(y)
summary(TSLAD_Invest)
tail(TSLAD_Invest, n =1)


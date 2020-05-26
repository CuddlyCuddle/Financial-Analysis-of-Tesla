# Preliminary Functions
# 
# Function for Determining Capital Growth over time
KapitalGrowth <- function(x, y){
  m <- length(y)
  y <- y/100
  k <- vector()
  g <- vector()
  k[1] <- x
  for(i in 2:m){
    g[i]<- k[i-1]*y[i]
    k[i]<- k[i-1] + g[i]
  }
  return(k)
}

# Function for Determining Returns of a Given Stock or Index
Returns_p <- function(x) {
  Returns <- vector()
  for(i in 2:length(x)){
    Returns[i]  <- ((x[i] - x[i-1])/x[i-1])*100
  }
  return(Returns)
}

LagReturns1 <- function(x) {
  LagReturns1 <- vector()
  for(i in 2:length(x)){
    LagReturns1[i] <- (x[i-1])
  }
  return(LagReturns1)
}

# Function for Determing the Value at Risk Assuming Normal Distribution
NDVaR <- function(x){
  MU <- mean(x, na.rm = T)
  SD <- sd(x, na.rm = T)
  NDV <- MU + qnorm(.05)*SD
  return(NDV)
}


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


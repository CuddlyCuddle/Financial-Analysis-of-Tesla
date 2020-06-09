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

# Function for Determing the Value at Risk Assuming Non-Normal Distribution
NON_NDVaR <- function(x){
  temp0 <- na.omit(x)
  o.x <- sort(temp0,decreasing = F)
  i <- .05*length(o.x)
  i.r <- round(i)
  d <- abs(i.r-1- i)
  NNVaR <- o.x[i.r-1] + d*(abs(o.x[i.r-1]-o.x[i.r]))
  return(NNVaR)
}

# Analysis of Tesla Daily 
# 
# Jan1-2017 to May1-2020
#
# TSLAD Data
TSLAD <- read.csv(".../TSLAD.csv")

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


# Analysis of Tesla Daily 
# 
# Jan1-2017 to December 31, 2019
#
# TSLAD Data

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



# Analysis of Tesla Daily 
# 
# Jan1-2020 to May 1, 2019
#
# TSLAD Data
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


# Simulating Tesla Price Action Using Random Walk Theory
# Mu and sigma of the Stock or Asset
mu <- mean(TSLAD$Returns, na.rm = TRUE)
sig <- sd(TSLAD$Returns, na.rm = TRUE)
t <- 837
Starting_price <- 216.99

# Start simulating prices
price <- rep(NA,t)
price[1] <- Starting_price
for(i in 2:t){
  price[i] <- price[i-1] + price[i-1]*(rnorm(1,mu,sig)/100)
}
Exp_Growth <- rep(NA, t)
Exp_Growth <- Starting_price
for(i in 2:t){
  Exp_Growth[i] <- Exp_Growth[i-1]*(mu/100)+Exp_Growth[i-1]
}
P.data <- data.frame(TSLAD$Adj.Close, price)
plot(price, type = "o")
points(TSLAD$Adj.Close, col = "Red")
points(Exp_Growth, col = "Purple")
abline(a = Starting_price, b = mu)


# Part 2 Simulating multiple stock movements
# Simulating Tesla Price Action Using Random Walk Theory
# Mu and sigma of the Stock or Asset 
mu <- mean(TSLAD$Returns, na.rm = TRUE)
sig <- sd(TSLAD$Returns, na.rm = TRUE)
t <- 837
n <- 10


# Start simulating prices
price <- matrix(data = NA, nrow = t, ncol = n)
price[1,] <- 216.99
for(j in 1:n){
  for(i in 2:t){
  price[i, j] <- price[i-1,j] + price[i-1,j]*(rnorm(1,mu,sig)/100)
  }
}

matplot(price, type = "l", pch = NULL)
points(TSLAD$Adj.Close, col = "Red")
abline(a = Starting_price, b = mu)


# Testing Code for formatting output
library(qwraps2)
options(qwraps2_markup = "markdown")
summary_statistics <-
  list(
    "Statistics" =
      list(
        "Mean (sd)" = ~qwraps2::mean_sd(Returns, na_rm = TRUE),
        "median (Q1, Q3)" = ~qwraps2::median_iqr(Returns, na_rm = TRUE),
        "min" = ~min(Returns, na.rm = TRUE),
        "max" = ~max(Returns, na.rm = TRUE),
        "Cumulative Return"= ~sum(Returns, na.rm = TRUE)
      ),
    "Value At Risk" =
      list(
        "Normal Distribution VaR" = ~NDVaR(Returns),
        "Non-Normal Distributed VaR" = ~NON_NDVaR(Returns)
      ),
    "Investment" =
      list(
        "Purchase Price" = ~head(Adj.Close, n = 1),
        "Sell Price" = ~tail(Adj.Close, n = 1),
        "min" = ~min(Invest, na.rm = TRUE),
        "max" = ~max(Invest, na.rm = TRUE),
        "Ending Investment" = ~tail(Invest, n = 1)
      )
  )

summary_table(TSLAD, summary_statistics)


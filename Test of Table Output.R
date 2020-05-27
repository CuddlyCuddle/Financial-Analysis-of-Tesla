TSLAD <- read.csv("C:/Users/jeanp/OneDrive/Econ 500/TSLAD.csv")

TSLAD$Returns <- Returns_p(TSLAD$Adj.Close)

y <- TSLAD$Returns
x <- 5000
TSLAD$Invest <- KapitalGrowth(x, y)
sum(TSLAD$Returns, na.rm = TRUE)
summary(TSLAD$Returns)
sd(TSLAD$Returns, na.rm = TRUE)
var(TSLAD$Returns, na.rm = TRUE)
NDVaR(TSLAD$Returns)
NON_NDVaR(TSLAD$Returns)
summary(TSLAD_Invest)
tail(TSLAD_Invest, n =1)


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

names(TSLAD)
summary_table(TSLAD, summary_statistics)

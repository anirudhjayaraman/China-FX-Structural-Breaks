## load package and data
library("fxregime")
data("FXRatesCHF", package = "fxregime")
## compute returns for CNY (and explanatory currencies)
## for one year after abolishing fixed USD regime
cnyex <- fxreturns("CNY", frequency = "daily",
                 start = as.Date("2005-07-25"), end = as.Date("2006-07-24"),
                 other = c("USD", "JPY", "EUR", "GBP"))
## compute all segmented regression with minimal segment size of
## h = 20 and maximal number of breaks = 5.
regex <- fxregimes(CNY ~ USD + JPY + EUR + GBP,
                 data = cnyex, h = 20, breaks = 5, ic = "BIC")
summary(regex)
## minimum BIC is attained for 2-segment (1-break) model
plot(regex)
## two regimes
## 1: tight USD peg
## 2: slightly more relaxed USD peg
round(coef(regex), digits = 3)
sqrt(coef(regex)[, "(Variance)"])
## inspect associated confidence intervals
ci <- confint(regex, level = 0.9)
ci
breakdates(ci)
## plot LM statistics along with confidence interval
fm <- fxlm(CNY ~ USD + JPY + EUR + GBP, data = cnyex)
scus <- gefp(fm, fit = NULL)
plot(scus, functional = supLM(0.1))
lines(ci)

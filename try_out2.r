## load package and data
library("fxregime")
data("FXRatesCHF", package = "fxregime")
## compute returns for CNY (and explanatory currencies)
## since China abolished fixed USD regime
cny <- fxreturns("CNY", frequency = "daily",
                 start = as.Date("2005-07-25"), end = as.Date("2010-02-12"),
                 other = c("USD", "JPY", "EUR", "GBP"))
## compute all segmented regression with minimal segment size of
## h = 100 and maximal number of breaks = 10

regx <- fxregimes(CNY ~ USD + JPY + EUR + GBP,
                 data = cny, h = 100, breaks = 10, ic = "BIC")
summary(regx)
## minimum BIC is attained for 2-segment (1-break) model
plot(regx)
round(coef(regx), digits = 3)
sqrt(coef(regx)[, "(Variance)"])
## inspect associated confidence intervals
conf_int <- confint(regx, level = 0.9)
breakdates(conf_int)
## plot LM statistics along with confidence interval
flm <- fxlm(CNY ~ USD + JPY + EUR + GBP, data = cny)
scus <- gefp(flm, fit = NULL)
plot(scus, functional = supLM(0.1))
lines(conf_int)

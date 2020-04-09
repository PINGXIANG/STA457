library(readxl)
# library(stochvol)
library(forecast)
library(rugarch)
# library(fGarch)
library(tseries)
library(TSA)
library(aTSA)
install.packages("aTSA")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
######################################################
### Part I: Set up
DJI <- read_excel("^DJI.xlsx")
# View(DJI)
attach(DJI)

n = length(DJI$Date)
mean(Excess_return)
sd(Excess_return)

X = ts(Excess_return, start=c(2000, 1), end=c(2020, 2), frequency=12)
######################################################

#######################
### Part I: iid test
plot(X, type="l") # The last point need to be cancelled out
abline(h = 0, lty = 2, col="red")

acf(X)
pacf(X)

Box.test(X, type = "Ljung-Box")
#######################

#######################
### Part II: force to use ARIMA to fit the model
fit <- auto.arima(X,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = T,ic = 'aicc')
plot(forecast(fit,h=2))
str(fit)
summary(fit)
Excess_return[n]
#######################

######################################################
### Part III: Find the optimal p and q for GARCH(p,q) by AICC

sink("file")
AICCs = c()
for (p in 1:6) {
  for (q in 1:5) {
    model = garch(X,c(q,p))
    # invisible(capture.output(model = garch(X,c(0,p)))) change the output for some reason
    # summary(model)
    AICC = -2 * n/(n-p) * logLik(model) + 2 * n * (p+q+2)/(n-p-q-3)
    AICCs = c(AICCs, AICC)
  }
}
sink()
AICCs = matrix(AICCs, ncol = p, byrow = F)
order_AICC = as.numeric(which(AICCs == min(AICCs), arr.ind = TRUE))
q = order_AICC[1]
p = order_AICC[2]
sink("file")
model = garch(X, c(q,p))
sink()
summary(model)
######################################################

######################################################
### Part: IV: Model Check 
nom_e_hat = na.omit(model$residuals)
## Check for iid
acf(nom_e_hat)
pacf(nom_e_hat)
## check for normality 
qqnorm(nom_e_hat, pch = 1, frame = FALSE)
qqline(nom_e_hat, col = "steelblue", lwd = 2) # Faiiled 
######################################################

######################################################
### Part V: Try student-T distribution assumption and check
spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                  distribution.model = "std")
std_fit = ugarchfit(spec, data = X)
coef(std_fit)

## model checking
std_e_hat = residuals(std_fit)
## Check for iid
acf(std_e_hat)
pacf(std_e_hat)
######################################################

######################################################
### Part: VI: forecast
forc = ugarchforecast(std_fit, n.ahead = 2)
plot(forc, which = "half")
plot(std_fit)

detach(DJI)
x_mar = -0.143543446379

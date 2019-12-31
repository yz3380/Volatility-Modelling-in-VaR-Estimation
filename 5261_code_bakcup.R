# VaR and ES Calculation sample using t
data(SP500, package="Ecdat")
n = 2783
SPreturn = SP500$r500[(n - 999):n]
year = 1981 + (1:n) * (1991.25 - 1981) / n
year = year[(n - 999):n]
alpha = 0.05
library(MASS)
fitt = fitdistr(SPreturn, "t")
param = as.numeric(fitt$estimate)
mean = param[1]
lambda = param[2]
df = param[3]
sd = lambda * sqrt((df) / (df - 2))
qalpha = qt(alpha, df = df)
VaR_par = -20000 * (mean + lambda * qalpha)
es1 = dt(qalpha, df = df) / (alpha)
es2 = (df + qalpha^2) / (df - 1)
es3 = -mean + lambda * es1 * es2
ES_par = 20000*es3
VaR_par
ES_par

# VaR and ES in Garch fit
garch.t = ugarchspec(mean.model=list(armaOrder=c(0,0)), variance.model=list(garchOrder=c(1,1)), distribution.model="std")
sp.garch.t = ugarchfit(data=SPreturn, spec=garch.t)

pred = ugarchforecast(sp.garch.t, data=SPreturn, n.ahead=1)
alpha = 0.05
nu = as.numeric(coef(sp.garch.t)[6]) # shape parameter as df
q = qstd(alpha, mean=fitted(pred), sd=sigma(pred), nu=nu)
VaR = -20000*q # VaR

lambda = sigma(pred)/sqrt( (nu)/(nu-2) )
qalpha = qt(alpha, df=nu)
es1 = dt(qalpha, df=nu)/(alpha)
es2 = (nu + qalpha^2) / (nu - 1)
es3 = -mean + lambda*es1*es2
ES_par = 20000*es3 # ES / cVaR
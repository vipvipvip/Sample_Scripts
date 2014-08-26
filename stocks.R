library(stockPortfolio)
library(quadprog)

run <- function() {
	stocks <- c("SPY", "EFA", "IWM", "VWO", "LQD", "HYG")
	returns <- getReturns(stocks, freq="week")
	returns
}

optPort <- function() {
#===> obtain data <===#
data(stock99)
data(stock94Info)
mgm <- stockModel(stock99, drop=25, model='MGM', industry=stock94Info$industry)
#===> build optimal portfolios <===#
opMgm1 <- optimalPort(mgm)
opMgm2 <- optimalPort(mgm, Rf=0.004)
print(opMgm1)
summary(opMgm1)
#===> plot the optimal porfolios <===#
par(mfrow=c(1,2))
# these plots provide a "head coloring" of
# the allocation by optimalPort
plot(opMgm1)
plot(opMgm2)
#===> additional plotting 1 <===#
par(mfrow=c(1,1))
plot(opMgm1, addNames=TRUE)
#===> additional plotting 2 <===#
plot(opMgm1, optPortOnly=TRUE, colOP=2, pchOP=2)
points(opMgm2, colOP=2, pchOP=4)
#=====> Watch out -- choosing Rf too large causes errors <=====#
data(stock99)
data(stock94Info)
non <- stockModel(stock99, drop=25, model='none',
industry=stock94Info$industry)
portPossCurve(non)
opTemp <- optimalPort(non, Rf=-10^5)
points(opTemp)
## Error if Rf >= vertex (y value)
# optimalPort(non, 0.02)
# optimalPort(non, opTemp$R)
# optimalPort(non, opTemp$R+0.01)
# optimalPort(non, opTemp$R-0.01)
## May give error if Rf too close to vertex
# optimalPort(non, opTemp$R-0.0001)
}

all <- function() {
#===> build four models <===#
data(stock99)
data(stock94Info)
non <- stockModel(stock99, drop=25, model='none', industry=stock94Info$industry)
sim <- stockModel(stock99, model='SIM', industry=stock94Info$industry, index=25)
ccm <- stockModel(stock99, drop=25, model='CCM', industry=stock94Info$industry)
mgm <- stockModel(stock99, drop=25, model='MGM', industry=stock94Info$industry)
#===> build optimal portfolios <===#
opNon <- optimalPort(non)
opSim <- optimalPort(sim)
opCcm <- optimalPort(ccm)
opMgm <- optimalPort(mgm)
#===> test portfolios on 2004-9 <===#
data(stock04)
tpNon <- testPort(stock04, opNon)
tpSim <- testPort(stock04, opSim)
tpCcm <- testPort(stock04, opCcm)
tpMgm <- testPort(stock04, opMgm)
#===> compare performances <===#4 adjustBeta
plot(tpNon)
lines(tpSim, col=2, lty=2)
lines(tpCcm, col=3, lty=3)
lines(tpMgm, col=4, lty=4)
legend('topleft', col=1:4, lty=1:4, legend=c('none', 'SIM', 'CCM', 'MGM'))
}


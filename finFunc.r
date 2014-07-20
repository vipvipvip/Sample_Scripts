#######################################################################
get.stock.data <- function( symbol, start.date=c(1,1,2008),
                            stop.date=c(12,31,2008), print.info=TRUE ) {
  # get stock data from yahoo.com for specified symbol in the
  # specified time period. The result is a data.frame with columns for:
  # Date, Open, High, Low, Close,Volume, Adj.Close
  url <- paste("http://ichart.finance.yahoo.com/table.csv?a=",
               start.date[1]-1,"&b=",start.date[2],"&c=",start.date[3],
               "&d=",stop.date[1]-1,"&e=",stop.date[2],"&f=",stop.date[3],"&s=",
               symbol,sep="")
  x <- read.csv(url)
  # data has most recent days first, going back to start date
  n <- length(x$Date); date <- as.character(x$Date[c(1,n)])
  if (print.info) cat(symbol,"has", n,"values from",date[2],"to",date[1],"\n")
  # data is in reverse order from the read.csv command
  x$Date <- rev(x$Date)
  x$Open <- rev(x$Open)
  x$High <- rev(x$High)
  x$Low <- rev(x$Low)
  x$Close <- rev(x$Close)
  x$Volume <- rev(x$Volume)
  x$Adj.Close <- rev(x$Adj.Close)
  return(x) }
#######################################################################

get.stock.price <- function( symbol, start.date=c(1,1,2014),
                             stop.date=c(12,31,2014), print.info=TRUE ) {
  # gets adjusted closing price data from yahoo.com for specified symbol
  x <- get.stock.data(symbol,start.date,stop.date,print.info)
  return(x$Adj.Close) }
#######################################################################
get.portfolio.returns = function( symbols, start.date=c(1,1,2008),
                                  stop.date = c(12,31,2008) ){
  # get a table of returns for the specified stocks in the stated time period
  n = length(symbols)
  for (i in 1:n) {
    t1 = get.stock.data( symbols[i], start.date=start.date, stop.date=stop.date)
    # need to merge columns, possibly with mismatching dates
    a = data.frame(t1$Date,t1$Adj.Close)
    names(a) = c("Date",symbols[i])
    if (i == 1) {b=a}
    else {b = merge(b,a,sort=FALSE)}
  }
  # leave off the date column
  nn = dim(b)[1]
  cat(" ",nn,"dates with values for all stocks,",nn-1,"returns calculated\n")
  b = b[,2:ncol(b)]
  bb = data.frame(apply(b,2,"log"))
  names(bb) = symbols
  return(bb) }
#######################################################################
compute.VaR <- function( S, alpha, V, T ){
  # compute a VaR for the price data S at level alpha, value V
  # and time horizon T (which may be a vector)
  ret <- diff(log(S)) # return = log(S[i]/S[i-1])
  mu <- mean(ret)
  sigma <- sd(ret)
  cat("mu=",mu," sigma=",sigma," V=",V,"\n")
  for (n in T) {
    VaR <- -V * ( exp(qnorm( alpha, mean=n*mu, sd=sqrt(n)*sigma)) - 1 )
    cat("T=",n, " VaR=",VaR, "\n")}
}
#######################################################################
# call.optionBS( 100, 105, .02, 90, .1 )
# a <- call.optionBS( 100,105,.02, 0:90,.1)
# ts.plot(rev(a$value),main="Black-Scholes call option",xlab="day",ylab="value")
call.optionBS <- function(CurrentPrice,Strike,interest,DaysToMaturity,sigma) {
  d1 <- (log(CurrentPrice/Strike)+(interest+sigma^2/2)*DaysToMaturity )/ (sigma*sqrt(DaysToMaturity))
  d2 <- (log(CurrentPrice/Strike)+(interest-sigma^2/2)*DaysToMaturity )/ (sigma*sqrt(DaysToMaturity))
  return(list(value=CurrentPrice*pnorm(d1)-Strike*pnorm(d2),delta=pnorm(d1)))}
#######################################################################
# x <- get.portfolio.returns( c("GOOGL","MSFT","GE","IBM") )
# portfolio.VaR( x, c(.25,.25,.25,.25), 100000, c(1,5,30) )

portfolio.VaR <- function( x, w, V, T=1, alpha=0.05) {
  # compute portfolio VaR by fitting multivariate normal to returns
  # x is a matrix of returns for the portfolio, w are the allocation weights
  # V = total value of investment, T = time horizon (possibly a vector)
  # alpha = confidence level
  # fit multivariate normal distribution
  mu <- mean(x)
  covar <- cov(x)
  # compute mean and variance for 1-day weighted returns
  mu1 <- sum(w*mu)
  var1 <- t(w) %*% covar %*% w
  cat("mu1=",mu1," var1=",var1," alpha=",alpha," V=",V, "\nweights:")
  for (i in 1:length(symbols)) {cat(" ",symbols[i],":",w[i]) }
  cat("\n")
  # compute VaR for different time horizons
  for (t in T) {
    VaR <- -V * ( exp(qnorm(alpha,mean=t*mu1,sd=sqrt(t*var1))) - 1.0)
    cat("T=",t," VaR=",VaR,"\n") }
}
#######################################################################
#######################################################################
#######################################################################
#######################################################################

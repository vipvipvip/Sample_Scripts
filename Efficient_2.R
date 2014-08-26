setwd("C:/R/Sample_Scripts")
# Economist at Large
# Modern Portfolio Theory
# Use solve.QP to solve for efficient frontier
# Last Edited 5/3/13

# This file uses the solve.QP function in the quadprog package to solve for the
# efficient frontier.
# Since the efficient frontier is a parabolic function, we can find the solution
# that minimizes portfolio variance and then vary the risk premium to find
# points along the efficient frontier. Then simply find the portfolio with the
# largest Sharpe ratio (expected return / sd) to identify the most
# efficient portfolio

library(stockPortfolio) # Base package for retrieving returns
library(ggplot2) # Used to graph efficient frontier
library(reshape2) # Used to melt the data
library(quadprog) #Needed for solve.QP

#run <- function(plot=FALSE) {
  # Create the portfolio using ETFs, incl. hypothetical non-efficient allocation
  stocks <- c(
    "VTSMX"
    ,"SPY"
#    ,"VEU"
#    ,"VNQ"
    ,"TLT"
#    ,"HYG"
)

  # Retrieve returns, from earliest start date possible (where all stocks have
  # data) through most recent date
  dt <- getReturns('SPY', freq="month", start='2006-10-15')
  alloc <- data.frame()
  for (d in rownames(dt$R)) {
    sdt = as.Date(d) - 5*365
    if (substr(d, 1,4)== '2006') {break}
  
    returns <- getReturns(stocks[-1], freq="month", start=sdt, end = d) #Currently, drop index
    port_model <- stockModel(returns, model="none", Rf=.01/12)  
    op <- optimalPort(port_model)

    #Show allocations
    s <- rbind(c(d,op$X))
    colnames(s) = c('dt', stocks[-1])
    
    alloc <- rbind(alloc, s)

    #print(s)
  }
  #sort oldest date first
  alloc <- alloc[with(alloc, order(as.Date(dt))),]

  write.csv(alloc, "alloc.csv")

    # graph efficient frontier
    # Start with color scheme
    #ealred <- "#7D110C"
    #ealtan <- "#CDC4B6"
    #eallighttan <- "#F7F6F0"
    #ealdark <- "#423C30"
    
    #ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
    #  geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
    #             color=ealred, size=5) +
    #  annotate(geom="text", x=eff.optimal.point$Std.Dev,
    #           y=eff.optimal.point$Exp.Return,
    #           label=paste("Risk: ",
    #                       round(eff.optimal.point$Std.Dev*100, digits=3),"\nReturn: ",
    #                       round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
    #                       round(eff.optimal.point$sharpe*100, digits=2), "%\nRow: ",  eff.optimal.row, sep=""),
    #          hjust=0, vjust=-1.2) +
    #  ggtitle("Efficient Frontier\nand Optimal Portfolio") +
    #  labs(x="Risk (standard deviation of portfolio)", y="Return") +
    #  theme(panel.background=element_rect(fill=eallighttan),
    #        text=element_text(color=ealdark),
    #        plot.title=element_text(size=24, color=ealred))
    #ggsave("Efficient Frontier.png")  
  
#}


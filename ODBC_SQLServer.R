library(RODBC)
Connection <- odbcDriverConnect(connection='driver={SQL Server};server=localhost;database=IDToDSN_DKC;Trusted Connection=yes')
Query <- "select tid, dt, price from tbl_Return_Rank where tid = 827"
myData <- sqlQuery(Connection, Query, errors=TRUE)
odbcCloseAll()
summary(myData)
str(myData)
plot(myData$price)

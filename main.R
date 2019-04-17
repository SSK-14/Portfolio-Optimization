library(GA)
library(ggplot2)


sharpe_ratio = function(x) { 
  return (mean(portfolio_returns(x))/sqrt(var(portfolio_returns(x))))
  
}
obj  = function(x) {
  return (-sharpe_ratio(x)+100*penalty(x))
}

penalty = function(x) {
  penalties = (sum(x)-1)*(sum(x)-1)   
  
  for (i in 1:length(x)) {
    penalties = penalties +  max(c(0,x[i]-1))*max(c(0,x[i]-1)) +  max(c(0,-x[i]))*max(c(0,-x[i]))     
  }
  
  return (penalties)
}


portfolio_returns = function(x) {
  port.returns = 0
  
  for (i in 1:length(x)) {
    port.returns = port.returns + profit[,i] * x[i]
  }
  
  return (port.returns)
}


setwd("D:\\PortfolioOptimizationUsingGA")
csv_files = c("AAPL.csv","TSLA.csv","AMZN.csv","GOOG.csv","NFLX.csv","FB.csv")
merged_file = NULL

n = length(csv_files)

for (i in 1:n) {
  csv = read.csv(csv_files[i])
  csv = csv[,c("Date","Close")]
  names(csv) = c("Date",csv_files[i])
  if (i == 1) merged_file = csv
  else merged_file = merge(merged_file,csv)
}

n = ncol(merged_file)
for (i in 2:n) {
  stock_prices = merged_file[,i] 
  
  stock_prices_prev = c(NA,stock_prices[1:(length(stock_prices)-1)]) 
  
  returns = (stock_prices-stock_prices_prev)/stock_prices_prev 
  
  merged_file[,i] = returns 
}

profit = merged_file[2:nrow(merged_file),2:ncol(merged_file)]



ga_res = ga(
  type="real-valued", 
  function(x){-obj(x)}, 
  lower = rep(0,ncol(profit)), 
  upper = rep(1,ncol(profit)), 
  maxiter = 10000,
  run=100, 
  monitor=TRUE,
  seed=1
)

sol = as.vector(summary(ga_res)$solution)

cbind(names(profit),sol)

results = portfolio_returns(sol)

plot(cumsum(results),type="l",lwd=5)

lines(cumsum(profit[,1]),col="red")
lines(cumsum(profit[,2]),col="green")
lines(cumsum(profit[,3]),col="blue")
lines(cumprod(profit[,4]),col="yellow")
lines(cumsum(profit[,5]),col="orange")
lines(cumsum(profit[,6]),col="purple")


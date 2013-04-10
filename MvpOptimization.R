# Load Systematic Investor Toolbox (SIT)
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod,quadprog,lpSolve,kernlab')

tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

data.weekly <- new.env()
for(i in tickers) data.weekly[[i]] = to.weekly(data[[i]], indexAt='endof')

bt.prep(data, align='remove.na', dates='2000::2012')
bt.prep(data.weekly, align='remove.na', dates='2000::2012')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices   
n = ncol(prices)
r = nrow(prices)

# find week ends
week.ends = endpoints(prices, 'weeks')
week.ends = week.ends[week.ends > 0]    


# Equal Weight 1/N Benchmark
data$weight[] = NA
data$weight[week.ends,] = ntop(prices[week.ends,], n)		
#data$weight = ntop(prices, n)

capital = 36000
feesPerShare = 0.05230769
data$weight[] = (capital / prices) * data$weight
equal.weight = bt.run(data, type='share', capital=capital, commission=feesPerShare)


#*****************************************************************
# Create Constraints
#*****************************************************************
#constraints = new.constraints(n, lb = -Inf, ub = +Inf)
constraints = new.constraints(n, lb = 0, ub = +Inf)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		

#*****************************************************************
# Code Strategies: Daily
#******************************************************************
plots = list()
plotCount = 0
windowMin = 10
windowMax = 180
step = 10

ret = prices / mlag(prices) - 1
weight = coredata(prices)
weight[] = NA

for(window in seq(windowMin, windowMax, by=step)){
  for( i in (window+1):r) {
    hist = ret[ (i- window +1):i, ]
    
    # create historical input assumptions
    ia = create.historical.ia(hist, 252)
    s0 = apply(coredata(hist),2,sd)		
    ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
    
    weight[i,] = min.risk.portfolio(ia, constraints)
  }
  
  # Minimum Variance
  data$weight[] = weight
  data$weight[] = (capital / prices) * data$weight
  min.var.daily = bt.run(data, type='share', capital=capital, commission=feesPerShare)
  
  plotCount = plotCount + 1
  plots[[plotCount]] = min.var.daily
}
plotbt(plots)

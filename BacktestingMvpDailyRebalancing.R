# Load Systematic Investor Toolbox (SIT)
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod,quadprog,lpSolve,kernlab')

# Investment Universe:
#******************************************************************

# USO: Crude Futures
# UNG: Natural Gas Futures
# DBA: Agricultural Futures
# DBB: Base Metals Futures
# GLD: Physical Gold
# SLV: Physical Silver

# SPY: Large Cap Blend
# QQQ: Large Cap Growth
# DIA: Large Cap Value
# MDY: Mid Cap Blend
# IWP: Mid Cap Growth
# DVY: Mid Cap Value
# IWM: Small Cap Blend
# IWO: Small Growth
# IWN: Small Value

# IYZ: Telecom Stocks
# XLY: Retail Discretionary Stocks
# XLP: Retail Staples Stocks
# XLE: Energy Stocks
# XLF: Financial Stocks
# XLV: Healthcare Stocks
# XLI: Industrial Stocks
# XLB: Materials Stocks
# IYR: Real Estate Stocks
# XLK: Technology Stocks
# XLU: Utilities Stocks

# UUP: Dollar Index Futures
# VXX: VIX Futures

# EWZ: Brazil Stocks
# EWW: Mexico Stocks
# EWJ: Japan Stocks
# RSX: Russia Stocks
# EWG: Germany Stocks
# EWQ: France Stocks
# FXI: China FTSE Stocks
# EWT: Taiwan Stocks
# EWA: Australia Stocks
# EWS: Singapore Stocks
# EWY: South Korea Stocks
# EEM: Emerging Markets Stocks

#******************************************************************

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
constraints = new.constraints(n, lb = -Inf, ub = +Inf)
#constraints = new.constraints(n, lb = 0, ub = +Inf)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		

#*****************************************************************
# Code Strategies: Daily
#******************************************************************
window = 80
ret = prices / mlag(prices) - 1
weight = coredata(prices)
weight[] = NA

#for( i in week.ends[week.ends >= (63 + 1)] ) {
for( i in (window+1):r) {
  # one quarter is 63 days
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
# Next let’s create Minimum Variance portfolios using weekly data:

#*****************************************************************
# Code Strategies: Weekly
#****************************************************************** 	
retw = data.weekly$prices / mlag(data.weekly$prices) - 1

weightw = coredata(prices)
weightw[] = NA

#for( i in week.ends[week.ends >= (63 + 1)] ) {
for( i in (63+1):r) {
  # map: kinda ugly but...
  currentdate = as.Date(index(ret[i,]))
  pastretw = retw[paste("/",currentdate,sep="")]
  pastretwlatestdate = as.Date(index(pastretw[nrow(pastretw),]))
  j = which(as.Date(index(retw)) == pastretwlatestdate)
  
  #j = which(index(ret[i,]) == index(retw))
  # one quarter = 13 weeks
  hist = retw[ (j- 13 +1):j, ]
  
  # create historical input assumptions
  ia = create.historical.ia(hist, 52)
  s0 = apply(coredata(hist),2,sd)		
  ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
  
  weightw[i,] = min.risk.portfolio(ia, constraints)
}	

data$weight[] = weightw		
data$weight[] = (capital / prices) * data$weight
min.var.weekly = bt.run(data, type='share', capital=capital, commission=feesPerShare)

#*****************************************************************
# Create Report
#****************************************************************** 

plotbt.custom.report(min.var.weekly, min.var.daily, equal.weight)
#plotbt.custom.report.part1(min.var.weekly, min.var.daily, equal.weight)
#plotbt.custom.report.part2(min.var.weekly, min.var.daily, equal.weight)

layout(1:3)
plotbt(min.var.weekly, min.var.daily, equal.weight)      	
plotbt.transition.map(min.var.daily$weight)
plotbt.transition.map(min.var.weekly$weight)
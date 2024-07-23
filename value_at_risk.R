require(ggplot2)
require(zoo)
require(reshape2)

# Compute historical simulation VaR (qtl: 99%, holding: 1month, lookback: 10y) for a portfolio 
# with 10y Zero Coupon (ZC) bond and 5y ZC bond (each with a face: USD 1m). 
# Also, compute the contribution to total VaR of each bond

# load checkpoint
load("dataframes/reg.Rda")

# compute bond prices
face.val <- 1e6 # face value
df$P5 <- face.val / (1+df$DGS5/100)^5
df$P10 <- face.val / (1+df$DGS10/100)^10

# compute portfolio weights
df$Weight.P5 <- df$P5 / (df$P5 + df$P10)
df$Weight.P10 <- df$P10 / (df$P5 + df$P10)

tail(df)

# plot bond prices
df.melt <- melt(df, id="DATE", measure.vars=c("P5", "P10"), variable.name="BOND", value.name="PRICE")
ggplot(df.melt, aes(x=DATE, y=PRICE, color=BOND)) + geom_line()

# compute individual realtive returns, assign first return to first date
df$R5.rel <- c(diff(df$P5)/df$P5[-length(df$P5)], NA)
df$R10.rel <- c(diff(df$P10)/df$P10[-length(df$P10)], NA)

# compute $ returns
df$R5 <- c(diff(df$P5), NA)
df$R10 <- c(diff(df$P10), NA)

# compute weighted portfolio returns
df$R.Portfolio.rel <- df$R5*df$Weight.P5 + df$R10*df$Weight.P10

# compute $ portfolio returns
df$R.Portfolio <- df$R5 + df$R10

# plot(df$DATE, cumsum(df$R.Portfolio))

# plot returns
df.melt <- melt(df, id="DATE", measure.vars=c("R5", "R10", "R.Portfolio"), variable.name="INSTRUMENT", value.name="RETURN")
ggplot(df.melt, aes(x=DATE, y=RETURN, color=INSTRUMENT)) + geom_line(alpha=0.5)

# cumulate monthly return

# define lookback period
lookback <- 10
start_date <- as.Date(format(df$DATE[length(df$DATE)]- 365*lookback, "%Y-%m-01")) # "2014-07-01"
end_date <- as.Date(format(df$DATE[length(df$DATE)], "%Y-%m-01"))-1 # "2024-06-30"
tail(df)

# assumption: calendar month is ok, could change to 30 day periods to have equal length!
# assumption: take non-overlapping time windows to get return observations that are less correlated

df.lookback <- df[(df$DATE>=start_date) & (df$DATE<=end_date),]

# Aggregate $ returns by year_month
df.lookback$year_month <- format(df.lookback$DATE, "%Y-%m")
R.Portfolio.monthly <- aggregate(R.Portfolio ~ year_month, df.lookback, sum)

# Aggregate % returns by year_month
# df$R.Portfolio1 <- df$R.Portfolio + 1
# R.Portfolio.monthly <- aggregate(R.Portfolio1 ~ year_month, df.lookback, prod)
# R.Portfolio.monthly$R.Portfolio <- R.Portfolio.monthly$R.Portfolio1 - 1

# compute VaR
var.portfolio <- quantile(R.Portfolio.monthly$R.Portfolio, 0.01)
var.portfolio

# plot distribution with VaR
hist(R.Portfolio.monthly$R.Portfolio, 
     main="Histogram of Monthly Portfolio Returns",
     xlab="Monthly Portfolio Return in $",
     probability=F
)
abline(v=var.portfolio, col="red")
text(var.portfolio*0.7,25, paste0("VaR at 99%", "\n", round(var.portfolio), " USD"), col="red")

# in 99% of calendar months, the portfolio value would change no more than VaR

# Also, compute the contribution to total VaR of each bond
R5.monthly <- aggregate(R5 ~ year_month, df.lookback, sum)
var5 <- quantile(R5.monthly$R5, 0.01)

R10.monthly <- aggregate(R10 ~ year_month, df.lookback, sum)
var10 <- quantile(R10.monthly$R10, 0.01)

var5+var10

# VaR
var.vector <- c(var5[[1]], var10[[1]])
sqrt(t(var.vector) %*% cor(cbind(R5.monthly$R5, R10.monthly$R10)) %*% (var.vector))


# Contribution of individual bonds to portfolio VaR via Component Value at Risk
# before and after approach from https://bazekon.uek.krakow.pl/zeszyty/171217959
# CoVaR(a) = VaR(p) – VaR(p–a) where VaR(p-a) is VaR of portfolio without asset a
# here, VaR(p-a) = VaR(b) as there are only two bonds a and b

covar5 <- var.portfolio - var10
covar5

covar10 <- var.portfolio - var5
covar10
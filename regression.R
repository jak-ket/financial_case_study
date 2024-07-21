require(ggplot2)
require(zoo)
require(reshape2)
require(fitdistrplus)
require(lmtest)

# read data, set types, encode missings
data_path <- "data/"
inflat <- read.csv(
  file=paste0(data_path, "T10YIE.csv"), 
  colClasses = c("DATE"="Date", "T10YIE"="numeric"),
  na.strings = c(".", "", "NA")
)
yield5 <- read.csv(
  file=paste0(data_path, "DGS5.csv"),
  colClasses = c("DATE"="Date", "DGS5"="numeric"),
  na.strings = c(".", "", "NA")
)
yield10 <- read.csv(
  file=paste0(data_path, "DGS10.csv"),
  colClasses = c("DATE"="Date", "DGS10"="numeric"),
  na.strings = c(".", "", "NA")
)

# plot time series
ggplot(inflat, aes(DATE, T10YIE)) + geom_line()
ggplot(yield5, aes(DATE, DGS5)) + geom_line()
ggplot(yield10, aes(DATE, DGS10)) + geom_line()

# explore basic location and dispersion statistics
summary(inflat)
summary(yield5)
summary(yield10)

# fill missings by carrying last observation forward
inflat <- na.locf(inflat)
yield5 <- na.locf(yield5)
yield10 <- na.locf(yield10)

stopifnot(sum(is.na(inflat))==0)
stopifnot(sum(is.na(yield5))==0)
stopifnot(sum(is.na(yield10))==0)

# check whether all have same dates
df.list <- list(inflat=inflat, yield5=yield5, yield10=yield10)
df.pairs <- combn(c("inflat", "yield5", "yield10"), 2)
for (i in 1:dim(df.pairs)[2]){
  name1 <- df.pairs[1,i]
  name2 <- df.pairs[2,i]
  
  df1 <- df.list[[name1]]
  df2 <- df.list[[name2]]
  
  date_setdiff <- setdiff(unique(df1$DATE), unique(df2$DATE))
  if(length(date_setdiff)>0){
    print(paste("dates set diff between", name1, "-", name2, ":", as.Date(date_setdiff)))
  }
  
  date_setdiff_back <- setdiff(unique(df2$DATE), unique(df1$DATE))
  if(length(date_setdiff_back)>0){
    print(paste("dates set diff between", name2, "-", name1, ":", as.Date(date_setdiff_back)))
  }
}  
# inflat has one day more, 2024-07-19 (we can omit this date to align date axis)

# put all in one dataframe to align dates
df <- merge(merge(inflat, yield5), yield10)
stopifnot(length(unique(df$DATE))==length(unique(yield5$DATE)))
stopifnot(sum(is.na(df))==0) 

# plot all time series at once
df.melt <- melt(df, id="DATE", measure.vars=c("T10YIE", "DGS5", "DGS10"), variable.name="TICKER", value.name="RATE")
ggplot(df.melt, aes(x=DATE, y=RATE, color=TICKER)) + geom_line()

# evaluate assumptions of linear regression
# linearity: from the plot it is doubtful whether the relationship between x=T10YIE and E[DGS10] is linear
ggplot(df, aes(x=T10YIE, y=DGS10)) + geom_point()
# homoscedasticity: 
plot(df$DATE, df$DGS10)
# independence: observations of dependent and independent variable are highly autocorrelated
acf(df$T10YIE)
acf(df$DGS10)
# normality: 
shapiro.test(df$DGS10) # p-value < 2.2e-16, normal assumption violated
fit.norm <- fitdist(df$DGS10, distr="norm")
plot(fit.norm) # significant deviations from normal distribution, especially in tails

# regress 10y yield (y) against inflation (x)
fit <- lm(DGS10 ~ 1 + T10YIE, data=df)
summary(fit)

ggplot(df, aes(x=T10YIE, y=DGS10)) + 
  geom_point() +
  geom_line(aes(x=T10YIE, y=fit$fitted.values), color="red")

# diagnose residuals
plot(fit)
hist(fit$residuals) # hist shows heavy tailed residuals
plot(fit, which=1) # Tuckey-Anscombe plot: assumptions of zero mean and homoscedasticity are violated!
plot(fit, which=2) # Q-Q plot shows deviation from normal in tails

# power of test is very high due to high number of observations => use TA-plot
bptest(fit) # p-value < 2.2e-16


require(ggplot2)
require(zoo)

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

# explore statistics
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

# put all in one dataframe to align dates
# check whether all have same dates
# plot all time series at once
# regress yield (y) against inflation (x)
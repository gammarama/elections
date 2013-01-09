### Andee Kaplan + Eric Hare
### STAT 579
### Final Project

library(lubridate)
library(XML)
suppressWarnings(library(ggplot2))
library(plyr)
library(reshape2)
library(RColorBrewer)
suppressWarnings(library(zoo))
suppressWarnings(library(forecast))

# NOTE: Commenting out the old code to parse the website since the WEBSITE IS DOWN
# 
# getDate <- function(x) {
#     return(strsplit(as.character(x), split = "-")[[1]][2])
# }
# 
# url <- "http://webcache.googleusercontent.com/search?q=cache:EkORMQMGsskJ:www.nationalpolls.com/2012/obama-vs-romney.html+&cd=1&hl=en&ct=clnk&gl=us"
# polls.data <- readHTMLTable(url, colClasses = c("character", "factor", "character", "numeric", "numeric"))[6][[1]]
# 
# names(polls.data)[c(2, 4:5)] <- c("State", "Obama", "Romney")
# 
# polls.data$Date <- sapply(polls.data$Date, getDate)
# polls.data$Date <- as.Date(polls.data$Date, "%m/%d/%y")
# polls.data$isNational <- (polls.data$State == "National")
# 
# polls.data$State <- gsub("-", " ", as.character(polls.data$State))
# polls.data$Obama.Romney <- polls.data$Obama - polls.data$Romney
# 
# polls.data <- subset(polls.data, Date > as.Date("2012/04/25"))

polls.data <- read.csv("../../Data/polls-2012.csv")
polls.data$Date <- as.Date(polls.data$Date)

polls.week <- ddply(subset(polls.data, !isNational), .(week = factor(week(Date)), isNational), summarise, Obama.Romney.Avg = mean(Obama.Romney), Obama = mean(Obama), Romney = mean(Romney))
polls.week$week <- as.numeric(as.character(polls.week$week))
names(polls.week)[4] <- "Obama.Poll"
names(polls.week)[5] <- "Romney.Poll"

##
# Added exponential smoother function to polling data
##
source("../../Code/UDF_ExponentialSmoother.R")

polls.Nat.smooth<-exp_smoother(time=subset(polls.data, isNational)[,"Date"], 
                               data=subset(polls.data, isNational)[,"Obama.Romney"],
                               unit="days",
                               alpha=.1)
polls.Nat.smooth$isNational<-TRUE

polls.Swing.smooth<-exp_smoother(time=subset(polls.data, !isNational)[,"Date"], 
                                 data=subset(polls.data, !isNational)[,"Obama.Romney"],
                                 unit="days",
                                 alpha=.03)
polls.Swing.smooth$isNational<-FALSE

polls.smooth<-rbind(polls.Nat.smooth, polls.Swing.smooth)
names(polls.smooth)<-c("Date","Obama.Romney","isNational")




##Outdated code
# polls.avg<-ddply(polls.data, .(isNational, Date), summarise, Obama.Romney=mean(Obama.Romney))
# 
# polls.zoo.Nat<-zoo(subset(polls.avg, isNational)[,3],subset(polls.avg, isNational)[,2])
# polls.zoo.Swing<-zoo(subset(polls.avg, !isNational)[,3],subset(polls.avg, !isNational)[,2])
# 
# x.Nat <- merge(polls.zoo.Nat, zoo(order.by=seq(start(polls.zoo.Nat), end(polls.zoo.Nat), by="days")))
# x.Swing <- merge(polls.zoo.Swing, zoo(order.by=seq(start(polls.zoo.Swing), end(polls.zoo.Swing), by="days")))
# 
# x.Nat <- na.spline(x.Nat)
# x.Nat.df <- data.frame(dates=index(x.Nat), weight=coredata(x.Nat))
# 
# x.Swing <- na.spline(x.Swing)
# x.Swing.df <- data.frame(dates=index(x.Swing), weight=coredata(x.Swing))
# 
# polls.avg.Nat.Smooth<-HoltWinters(x.Nat, alpha=.1, beta=FALSE, gamma=FALSE, l.start=x.Nat[1])
# polls.avg.Swing.Smooth<-HoltWinters(x.Swing, alpha=.03, beta=FALSE, gamma=FALSE, l.start=x.Swing[1])
# 
# polls.avg.smooth<-ts.union(polls.avg.Nat.Smooth$fitted, polls.avg.Swing.Smooth$fitted)
# 
# polls.smooth<-data.frame(polls.avg.smooth)[,c(1,3)]
# polls.smooth$Date<-as.Date(as.numeric(time(polls.avg.smooth)))
# 
# polls.smooth<-data.frame(rbind(cbind(polls.smooth$Date,rep("TRUE", nrow(polls.smooth)),polls.smooth[,1]),
#                                cbind(polls.smooth$Date,rep("FALSE", nrow(polls.smooth)),polls.smooth[,2])))
# colnames(polls.smooth)<-c("Date","isNational", "Obama.Romney")
# polls.smooth$Date<-as.Date(as.numeric(as.character(polls.smooth$Date)))
# polls.smooth$isNational<-as.factor(polls.smooth$isNational)
# polls.smooth$Obama.Romney<-as.numeric(as.character(polls.smooth$Obama.Romney))

source("NationalPollingParse-Di.R")
source("SPEDataCleanup.R")

polls.iowa <- subset(polls.state, Region == "iowa")[,-6]

library(reshape2)
polls.iowam <- melt(polls.iowa, id = c("Date", "Pollster", "Region"))
qplot(Date, value, data = polls.iowam, colour = variable) + geom_smooth()
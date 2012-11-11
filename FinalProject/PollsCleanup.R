### Andee Kaplan + Eric Hare
### STAT 579
### Final Project

library(lubridate)
library(XML)
library(ggplot2)
library(plyr)
library(reshape2)
library(RColorBrewer)

getDate <- function(x) {
    return(strsplit(as.character(x), split = "-")[[1]][2])
}

url <- "http://www.nationalpolls.com/2012/obama-vs-romney.html"
polls.data <- readHTMLTable(url, colClasses = c("character", "factor", "character", "numeric", "numeric"))[6][[1]]

names(polls.data)[c(2, 4:5)] <- c("State", "Obama", "Romney")

polls.data$Date <- sapply(polls.data$Date, getDate)
polls.data$Date <- as.Date(polls.data$Date, "%m/%d/%y")
polls.data$isNational <- (polls.data$State == "National")

polls.data$State <- gsub("-", " ", as.character(polls.data$State))
polls.data$Obama.Romney <- polls.data$Obama - polls.data$Romney

polls.data <- subset(polls.data, Date > as.Date("2012/04/25"))
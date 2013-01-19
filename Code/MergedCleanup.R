### Andee Kaplan + Eric Hare
### STAT 579
### Final Project

source("../../Code/SpendCleanup.R")
source("../../Code/PollsCleanup.R")

polls.week <- ddply(subset(polls.data, !isNational), .(week = factor(week(Date)), isNational), summarise, Obama.Romney.Avg = mean(Obama.Romney), Obama = mean(Obama), Romney = mean(Romney))
polls.week$week <- as.numeric(as.character(polls.week$week))
names(polls.week)[4] <- "Obama.Poll"
names(polls.week)[5] <- "Romney.Poll"


unmelted.sum <- dcast(num.weeks.sum, week+Date ~ beneful_can, value.var = "sum")
unmelted.sum$obama.romney <- unmelted.sum$obama - unmelted.sum$romney
names(unmelted.sum)[1] <- "weeknum"

unmelted.sum$weeknum <- as.numeric(as.character(unmelted.sum$weeknum))
unmelted.sum$weeknum <- unmelted.sum$weeknum + 1
unmelted.sum$weeknum <- factor(unmelted.sum$weeknum)
unmelted.sum <- unmelted.sum[-c(28, 27), ]
final.df3 <- cbind(unmelted.sum, polls.week[-c(1,2),])

final.df3$postHeavySpend <- final.df3$week > week(as.Date("08/7/2012","%d/%m/%Y"))


ObamaPollPrevWeek <- c(final.df3$Obama.Poll[1], final.df3$Obama.Poll[1:length(final.df3$Obama.Poll) - 1])
final.df3$ObamaPollChange <- final.df3$Obama.Poll - ObamaPollPrevWeek

RomneyPollPrevWeek <- c(final.df3$Romney.Poll[1], final.df3$Romney.Poll[1:length(final.df3$Romney.Poll) - 1])
final.df3$RomneyPollChange <- final.df3$Romney.Poll - RomneyPollPrevWeek

Obama.Romney.AvgPollPrevWeek <- c(final.df3$Obama.Romney.Avg[1], final.df3$Obama.Romney.Avg[1:length(final.df3$Obama.Romney.Avg) - 1])
final.df3$ObamaRomneyPollChange <- final.df3$Obama.Romney.Avg - Obama.Romney.AvgPollPrevWeek


ObamaSpendPrevWeek <- c(final.df3$obama[1], final.df3$obama[1:length(final.df3$obama) - 1])
final.df3$ObamaSpendChange <- final.df3$obama - ObamaSpendPrevWeek

RomneySpendPrevWeek <- c(final.df3$romney[1], final.df3$romney[1:length(final.df3$romney) - 1])
final.df3$RomneySpendChange <- final.df3$romney - RomneySpendPrevWeek

Obama.Romney.AvgSpendPrevWeek <- c(final.df3$obama.romney[1], final.df3$obama.romney[1:length(final.df3$obama.romney) - 1])
final.df3$ObamaRomneySpendChange <- final.df3$obama.romney - Obama.Romney.AvgSpendPrevWeek

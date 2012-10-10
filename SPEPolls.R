source("NationalPollingParse-Di.R")
source("SPEDataCleanup.R")

analyzeRyan <- function(state) {
    polls.chosen <- subset(polls.state, Region %in% state)[,-6]
    print(head(polls.chosen))
    polls.chosenm <- melt(polls.chosen, id = c("Date", "Pollster", "Region"))
    qplot(Date, value, data = polls.chosenm, colour = variable) + geom_smooth() +
        annotate(geom = "rect", xmin = as.Date("2012-8-11"), xmax = as.Date("2012-8-12"), ymin = 40, ymax = 55, alpha = .4, colour = "darkred") +
        scale_colour_manual(name = "Candidate", values = c("#3D64FF", "#CC0033"), labels=c("Obama","Romney" )) +
        facet_wrap(facets = ~Region) +
        opts(title = "Paul Ryan Effect on State Polling") +
        ylab("Percentage Support")
}

swing.states <- c("Colorado", "Florida", "Iowa", "Michigan", "Nevada", "New Hampshire", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "Wisconsin")
analyzeRyan("wisconsin")
analyzeRyan("florida")
analyzeRyan("iowa")
analyzeRyan(tolower(swing.states))

### Tracking polls vs state trends?
#qplot(long, lat, data = polls.state.map, group = group, geom = "polygon", fill = Obama.Romney)

# Colorado, Florida, Iowa, Michigan, Nevada, New Hampshire, North Carolina, Ohio, Pennsylvania, Virginia, Wisconsin
polls.sub2 <- subset(polls.sub[,c(1:2, 5:6)], Date > as.Date("2012-04-25"))
polls.subswing <- subset(polls.sub2, Region %in% c("National", "Colorado", "Florida", "Iowa", "Michigan", "Nevada", "New Hampshire", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "Wisconsin"))

polls.subswing$isNational <- (polls.subswing$Region == "National")

# How does the candidate's performance compare in 11 swing states relative to the overall national performance?
qplot(Date, Obama.Romney, data = polls.subswing, colour = isNational) + geom_smooth()
qplot(Date, Obama.Romney, data = subset(polls.subswing, Region %in% c("Pennsylvania", "Ohio", "Florida")), colour = Region) + geom_smooth()
qplot(Date, Obama.Romney, data = subset(polls.subswing, Date > as.Date("2012-09-15")), colour = isNational) + geom_smooth() +
    annotate(geom = "rect", xmin = as.Date("2012-10-04"), xmax = as.Date("2012-10-04"), ymin = -5, ymax = 10, alpha = .4, colour = "darkred") +
    geom_line(y = 0, colour = "darkgrey")


# Here we notice an extremely abrupt increases in spending.
# Romney: beginning the week of 2012-07-18
num.weeks.sum <- ddply(num.weeks, .(week, beneful_can), summarise, sum = sum(WeeklySum, na.rm = TRUE))
num.weeks.sum$Date <- as.Date("2012-04-25") + (7 * (as.numeric(num.weeks.sum$week) - week(as.Date("2012-04-25"))))
qplot(Date, sum, data = subset(num.weeks.sum, Date < as.Date("2012-10-10")), colour = beneful_can) + scale_y_log10() + geom_line()
qplot(Date, Obama.Romney, data = polls.subswing, colour = isNational) + geom_smooth() + 
    annotate(geom = "rect", xmin = as.Date("2012-07-18"), xmax = as.Date("2012-07-19"), ymin = -5, ymax = 10, alpha = .4, colour = "darkgrey")

# So what happened around this time?
wtf.sub <- subset(pres.data, exp_dat >= as.Date("2012-07-11") & exp_dat <= as.Date("2012-07-25"))
wtf.sub <- wtf.sub[with(wtf.sub, order(-exp_amo)), ]
head(wtf.sub, n = 10)

wtf_sub2<-wtf.sub[,c("exp_amo", "beneful_can", "bucket2")]
wtf_sum <- dcast(melt(wtf.sub2,id=c("beneful_can", "bucket2")), wtf.sub2$bucket2 ~ wtf.sub2$beneful_can, sum)
wtf_sum$both<-wtf_sum$obama + wtf_sum$romney

## For the plot
wtf_wtf <- ddply(wtf_sub2, .(bucket2, beneful_can), summarise, Sum = sum(exp_amo))
ggplot(wtf_wtf, aes(bucket2, Sum, fill = beneful_can)) + geom_bar(position = "dodge") + coord_flip() + scale_fill_manual(name = "Candidate", values = c("#3D64FF", "#CC0033")) + xlab("") + ylab("Amount Spent (Log 10)")
twoweeksum <- subset(wtf_wtf, bucket2 == "ad" & beneful_can == "romney")$Sum
overallsum <- subset(sum_exp2_p, bucket2 == "ad" & beneful_can == "romney")$Sum

twoweeksum / overallsum
# vs...
2 / (as.numeric(max(num.weeks$week)) - as.numeric(min(num.weeks$week)))

pres.datam <- pres.data[,c("beneful_can", "exp_amo", "exp_dat")]
pres.datamp <- ddply(pres.datam, .(exp_dat, beneful_can), summarise, day_amo = sum(exp_amo))
qplot(exp_dat, day_amo, data = subset(pres.datamp, exp_dat > as.Date("2012-09-15")), colour = beneful_can) + scale_y_log10() + geom_smooth()
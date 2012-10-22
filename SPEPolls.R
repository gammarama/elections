source("NationalPollingParse-Di.R")
source("SPEDataCleanup.R")

library(RColorBrewer)

##--------------------------------------------------
##2. Display State trends
analyzeStateTrends <- function(state) {
  polls.chosen <- subset(polls.state, Region %in% state)[,-6]
  polls.chosenm <- melt(polls.chosen, id = c("Date", "Pollster", "Region"))
  qplot(Date, value, data = polls.chosenm, colour = variable) + geom_smooth() +
    scale_colour_manual(name = "Candidate", values = c("#3D64FF", "#CC0033"), labels=c("Obama","Romney" )) +
    facet_wrap(facets = ~Region) +
    scale_x_date(limits=c(as.Date("25/4/2012","%d/%m/%Y"), as.Date("2/11/2012","%d/%m/%Y"))) +
    ylab("Percentage Support") +
    annotate("text", x=as.Date("11/8/2012","%d/%m/%Y"), y=30, label="1", color="#FF0000", hjust=-0.5, alpha=0.2, size=3) + 
    annotate("rect", xmin=as.Date("10/8/2012","%d/%m/%Y"), ymin=30, xmax=as.Date("11/8/2012","%d/%m/%Y"), ymax=45, fill="#FF0000", alpha=0.2) + 
    annotate("text", x=as.Date("28/8/2012","%d/%m/%Y"), y=30, label="2", color="#FF0000", hjust=-0.7, alpha=0.2, size=3) + 
    annotate("rect", xmin=as.Date("27/8/2012","%d/%m/%Y"), ymin=30, xmax=as.Date("30/8/2012","%d/%m/%Y"), ymax=45, fill="#FF0000", alpha=0.2) +
    annotate("text", x=as.Date("5/9/2012","%d/%m/%Y"), y=60, label="3", color="#330099", hjust=-0.5, alpha=0.2, size=3) + 
    annotate("rect", xmin=as.Date("4/9/2012","%d/%m/%Y"), ymin=45, xmax=as.Date("6/9/2012","%d/%m/%Y"), ymax=60, fill="#330099", alpha=0.2) +
    annotate("text", x=as.Date("17/9/2012","%d/%m/%Y"), y=60, label="4", color="grey50", hjust=-0.5, alpha=0.4, size=3) + 
    annotate("text", x=as.Date("17/9/2012","%d/%m/%Y"), y=30, label="4", color="grey50", hjust=-0.5, alpha=0.4, size=3) +
    annotate("rect", xmin=as.Date("17/9/2012","%d/%m/%Y"), ymin=30, xmax=as.Date("18/9/2012","%d/%m/%Y"), ymax=60, fill="grey50", alpha=0.4) +
    annotate("text", x=as.Date("3/10/2012","%d/%m/%Y"), y=60, label="5", color="grey50", hjust=-0.5, alpha=0.4, size=3) + 
    annotate("text", x=as.Date("3/10/2012","%d/%m/%Y"), y=30, label="5", color="grey50", hjust=-0.5, alpha=0.4, size=3) +
    annotate("rect", xmin=as.Date("3/10/2012","%d/%m/%Y"), ymin=30, xmax=as.Date("4/10/2012","%d/%m/%Y"), ymax=60, fill="grey50", alpha=0.4) +
    opts(legend.position="bottom")
}

#list taken from http://www.realclearpolitics.com
swing.states <- c("Colorado", "Florida", "Iowa", "Michigan", "Nevada", "New Hampshire", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "Wisconsin")
analyzeStateTrends("wisconsin")
analyzeStateTrends("florida")
analyzeStateTrends("iowa")
analyzeStateTrends(tolower(swing.states))

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
##--------------------------------------------------
##2. Build the plots
num.weeks.sum <- ddply(num.weeks, .(week, beneful_can), summarise, sum = sum(WeeklySum, na.rm = TRUE))
num.weeks.sum$Date <- as.Date("2012-04-25") + (7 * (as.numeric(num.weeks.sum$week) - week(as.Date("2012-04-25"))))

polls.week <- ddply(polls.subswing, .(week = factor(week(Date)), isNational), summarise, Obama.Romney.Avg = mean(Obama.Romney))
unmelted.polls <- dcast(polls.week, week ~ isNational)
names(unmelted.polls) <- c("week", "Swing", "National")

unmelted.sum <- dcast(num.weeks.sum, week+Date ~ beneful_can, value.var = "sum")
unmelted.sum$obama.romney <- unmelted.sum$obama - unmelted.sum$romney
final.df <- cbind(unmelted.sum[,-c(3,4)], unmelted.polls[,-1])

qplot(obama.romney, Swing, data = final.df) + geom_smooth(method = "lm")
qplot(Date, Swing, data = final.df, size = obama.romney) + geom_smooth()


qplot(Date, sum, data = subset(num.weeks.sum, Date < as.Date("2012-10-10")), colour = beneful_can) + scale_y_log10() + geom_line() +
  annotate("text", x=as.Date("18/7/2012","%d/%m/%Y"), y=5e+07, label="1", color="#FF0000", hjust=-0.5, alpha=0.4, size=10) +
  annotate("rect", xmin=as.Date("18/7/2012","%d/%m/%Y"), ymin=1e+04, xmax=as.Date("19/7/2012","%d/%m/%Y"), ymax=5e+07, fill="#FF0000", alpha=0.4) +
  annotate("text", x=as.Date("5/9/2012","%d/%m/%Y"), y=5e+07, label="2", color="#330099", hjust=-0.5, alpha=0.4, size=10) +
  annotate("rect", xmin=as.Date("5/9/2012","%d/%m/%Y"), ymin=1e+04, xmax=as.Date("6/9/2012","%d/%m/%Y"), ymax=5e+07, fill="#330099", alpha=0.4) +
  scale_colour_manual(name = "Candidate", values = c("#3D64FF", "#CC0033"), labels=c("Obama","Romney" )) +
  ylab("Total Spending (Log 10)") + 
  scale_x_date(limits=c(as.Date("25/4/2012","%d/%m/%Y"), as.Date("2/11/2012","%d/%m/%Y"))) +
  theme(legend.position="bottom")

qplot(Date, Obama.Romney, data = polls.subswing, colour = isNational) + geom_smooth() + 
  annotate("text", x=as.Date("18/7/2012","%d/%m/%Y"), y=20, label="1", color="#FF0000", hjust=-0.5, alpha=0.4, size=5) +
  annotate("rect", xmin=as.Date("18/7/2012","%d/%m/%Y"), ymin=-20, xmax=as.Date("19/7/2012","%d/%m/%Y"), ymax=20, fill="#FF0000", alpha=0.4) +
  annotate("text", x=as.Date("5/9/2012","%d/%m/%Y"), y=20, label="2", color="#330099", hjust=-0.5, alpha=0.4, size=5) +
  annotate("rect", xmin=as.Date("5/9/2012","%d/%m/%Y"), ymin=-20, xmax=as.Date("6/9/2012","%d/%m/%Y"), ymax=20, fill="#330099", alpha=0.4) +
  scale_colour_manual(name = " ", values = brewer.pal(3, "Dark2")[1:2], labels=c("Swing State","National" )) + 
  scale_x_date(limits=c(as.Date("25/4/2012","%d/%m/%Y"), as.Date("2/11/2012","%d/%m/%Y"))) +
  scale_y_continuous(limits=c(-20,20)) +
  ylab("Romney -------- | -------- Obama  ") +
  annotate("rect", xmin=as.Date("2/2/2012","%d/%m/%Y"), xmax=as.Date("2/12/2012","%d/%m/%Y"), ymin=0, ymax=Inf, fill="blue", alpha=0.03) +
  annotate("rect", xmin=as.Date("2/2/2012","%d/%m/%Y"), xmax=as.Date("2/12/2012","%d/%m/%Y"), ymin=-Inf, ymax=0, fill="red", alpha=0.03) +
  geom_text(data=labels.tracking[1,], mapping=aes(x=Date, y=Obama.Romney, label=text), colour=I("#FF0000"), size=5, alpha=I(0.2), shape=1) +
  geom_text(data=labels.tracking[2,], mapping=aes(x=Date, y=Obama.Romney, label=text), colour=I("#330099"), size=5, alpha=I(0.2), shape=1) +
  theme_bw() +
  theme(legend.position="bottom")

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
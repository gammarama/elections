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
    theme(legend.position="bottom")
}

#list taken from http://www.realclearpolitics.com
swing.states <- c("Colorado", "Florida", "Iowa", "Michigan", "Nevada", "New Hampshire", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "Wisconsin")
analyzeStateTrends("wisconsin")
analyzeStateTrends("florida")
analyzeStateTrends("iowa")
analyzeStateTrends(tolower(swing.states))

### Tracking polls vs state trends?
#qplot(long, lat, data = polls.state.map, group = group, geom = "polygon", fill = Obama.Romney)

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
ggplot(wtf_wtf, aes(bucket2, Sum, fill = beneful_can)) + geom_bar(position = "dodge") + coord_flip() + scale_fill_manual(name = "Candidate", values = c("#3D64FF", "#CC0033")) + xlab("") + ylab("Amount Spent (Log 10)")
qplot(exp_dat, day_amo, data = subset(pres.datamp, exp_dat > as.Date("2012-09-15")), colour = beneful_can) + scale_y_log10() + geom_smooth()

### Events to Symbols
## Paul Ryan: +
## DNC: Triangle
## RNC: Square with X in it
## 47% Video: Star
## 1st Debate: Circle
## All other Points: Square
### 
qplot(obama, Obama.Poll, data = final.df3[-1,], geom = "path", colour = I("blue")) + 
  scale_x_log10() + 
  geom_path(aes(romney, Romney.Poll), colour = "red") + 
  geom_point(aes(shape = Event, size = WeekNumber)) + 
  geom_point(aes(romney, Romney.Poll, shape = Event, size = WeekNumber)) +
  scale_shape(solid = FALSE) +
  ylab("Percentage Support (Weekly Average)") +
  xlab("Super PAC Spending (Weekly Average)") + 
  ggtitle("Polling Average over Spending by Week") +
  coord_fixed()
  #theme(legend.position="none")


## One Week Lag
qplot(ObamaSpendChange, ObamaPollChange, data = final.df3.lag1[-1,], geom = "point", colour = week) + 
  ylab("Change in Percentage Support (Weekly Average)") +
  xlab("Change in Super PAC Spending") +
  geom_point() + 
  annotate("text", x = -1088052.97, y = 1.30219780, label = "1", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x =  1455100.32, y = 0.39285714, label = "2", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = -1883070.69, y = 0.54135338, label = "3", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = 418553.52, y = 1.02534562, label = "4", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = 3386047.23, y = 0.37073171, label = "5", color = "#000000", hjust = -0.5, size = 5) +
  geom_smooth()

qplot(RomneySpendChange, RomneyPollChange, data = final.df3.lag1[-1,], geom = "point", colour = week) + 
  ylab("Change in Percentage Support (Weekly Average)") +
  xlab("Change in Super PAC Spending") +
  geom_point() + 
  annotate("text", x = 406377.99, y = -1.13736264, label = "1", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = -1748705.58, y = 1.32142857, label = "2", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = -13342978.01, y = -2.78195489, label = "3", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = -1723434.56, y = -0.06682028, label = "4", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = 4885549.67, y = 0.54982578, label = "5", color = "#000000", hjust = -0.5, size = 5) +
  scale_colour_gradientn(colours = c("#850707", "#F78686")) +
  geom_smooth(colour = I("red"))



## Two Week Lag
qplot(ObamaSpendChange, ObamaPollChange, data = final.df3.lag2[-1,], geom = "point", colour = week) + 
  ylab("Change in Percentage Support (Weekly Average)") +
  xlab("Change in Super PAC Spending") +
  geom_point() + 
  annotate("text", x = 1619629.98, y = 1.30219780, label = "1", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x =  -619310.00, y = 0.39285714, label = "2", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = 1455100.32, y = 0.54135338, label = "3", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = 4453646.40, y = 1.02534562, label = "4", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = -2553605.82, y = 0.37073171, label = "5", color = "#000000", hjust = -0.5, size = 5) +
  geom_smooth()

qplot(RomneySpendChange, RomneyPollChange, data = final.df3.lag2[-1,], geom = "point", colour = week) + 
  ylab("Change in Percentage Support (Weekly Average)") +
  xlab("Change in Super PAC Spending") +
  geom_point() + 
  annotate("text", x = 7536978.98, y = -1.13736264, label = "1", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = 8966694.68, y = 1.32142857, label = "2", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = -1748705.58, y = -2.78195489, label = "3", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = 7307676.72, y = -0.06682028, label = "4", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = 3616271.36, y = 0.54982578, label = "5", color = "#000000", hjust = -0.5, size = 5) +
  scale_colour_gradientn(colours = c("#850707", "#F78686")) +
  geom_smooth(colour = I("red"))

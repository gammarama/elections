source("MergedCleanup.R")

ggplot(num.weeks, aes(date, WeeklySum, colour = beneful_can)) + geom_point() + geom_line() + facet_grid(facets=bucket2~sup_opp) + 
    scale_colour_manual(name = "Candidate who benefits", values = c("#3D64FF", "#CC0033"), labels=c("Obama","Romney" )) + 
    xlab("Weeks") + 
    ylab("Amount Spent (Log 10)") + 
    annotate(geom = "rect", xmin = as.Date("2012-8-11", format="%Y-%m-%d"), xmax = as.Date("2012-8-12", format="%Y-%m-%d"), ymin = 1, ymax = 1e+08, alpha = .2, fill = "#CC0033") + 
    annotate(geom = "rect", xmin = as.Date("2012-8-28", format="%Y-%m-%d"), xmax = as.Date("2012-8-30", format="%Y-%m-%d"), ymin = 1, ymax = 1e+08, alpha = .2, fill = "#CC0033") + 
    annotate(geom = "rect", xmin = as.Date("2012-9-4", format="%Y-%m-%d"), xmax = as.Date("2012-9-6", format="%Y-%m-%d"), ymin = 1, ymax = 1e+08, alpha = .2, fill = "#3D64FF") + 
    annotate(geom = "rect", xmin = as.Date("2012-9-17", format="%Y-%m-%d"), xmax = as.Date("2012-9-18", format="%Y-%m-%d"), ymin = 1, ymax = 1e+08, alpha = .2, fill = "grey50") + 
    annotate(geom = "rect", xmin = as.Date("2012-10-3", format="%Y-%m-%d"), xmax = as.Date("2012-10-4", format="%Y-%m-%d"), ymin = 1, ymax = 1e+08, alpha = .2, fill = "grey50") + 
    scale_y_log10() + 
    theme(legend.position="bottom")

qplot(romney, obama, data=sum_exp2, color = `df2$bucket2`) +
  geom_abline() + 
  geom_text(aes(label=`df2$bucket2`), data=sum_exp2, hjust=-.1, vjust=-.02) + 
  scale_x_log10() + 
  scale_y_log10()

ggplot(sum_exp2_p, aes(bucket2, Sum, fill = beneful_can)) + 
  geom_bar(position = "dodge") + 
  coord_flip() + 
  scale_y_log10() + 
  scale_fill_manual(name = "Candidate", values = c("#3D64FF", "#CC0033")) + 
  xlab("") + 
  ylab("Amount Spent (Log 10)")

ggplot(sum_exp.spep, aes(beneful_can, Sum, fill = spe_nam)) + 
  geom_bar() + 
  xlab("Benefiting Candidate") + 
  ylab("Amount Spent") + 
  scale_fill_manual(values=c(blue, red), labels = substring(sum_exp.spep$spe_nam, 1, 30), name = "Super PAC") + 
  labs(title = "Spending by Super PAC")

qplot(Date, sum, data = subset(num.weeks.sum, Date < as.Date("2012-10-10")), colour = beneful_can) + scale_y_log10() + geom_line() +
  annotate("text", x=as.Date("14/7/2012","%d/%m/%Y"), y=5e+07, label="1", color="#FF0000", hjust=-0.5, alpha=0.4, size=10) +
  annotate("rect", xmin=as.Date("14/7/2012","%d/%m/%Y"), ymin=1e+04, xmax=as.Date("15/7/2012","%d/%m/%Y"), ymax=5e+07, fill="#FF0000", alpha=0.4) +
  scale_colour_manual(name = "Candidate", values = c("#3D64FF", "#CC0033"), labels=c("Obama","Romney" )) +
  ylab("Total Spending (Log 10)") + 
  scale_x_date(limits=c(as.Date("25/4/2012","%d/%m/%Y"), as.Date("2/11/2012","%d/%m/%Y"))) +
  theme(legend.position="bottom")


### Polling Plots ###

analyzeStateTrends <- function(state) {
  polls.chosen <- subset(polls.data[-(6:7)], State %in% state)
  polls.chosenm <- melt(polls.chosen, id = c("Date", "Pollster", "State"))
  qplot(Date, value, data = polls.chosenm, colour = variable) + geom_smooth() +
    scale_colour_manual(name = "Candidate", values = c("#3D64FF", "#CC0033"), labels=c("Obama","Romney" )) +
    facet_wrap(facets = ~State) +
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
swing.states <- c("Colorado", "Florida", "Iowa", "Michigan", "Missouri", "Nevada", "New Hampshire", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "Wisconsin")
analyzeStateTrends(swing.states)


qplot(Date, Obama.Romney, data = subset(polls.data, Date > as.Date("2012-09-15") & State %in% c("National", swing.states)), colour = isNational) + geom_smooth() +
  annotate(geom = "rect", xmin = as.Date("2012-10-04"), xmax = as.Date("2012-10-04"), ymin = -5, ymax = 10, alpha = .4, colour = "darkred") +
  geom_line(y = 0, colour = "darkgrey")

## One Week Lag
qplot(ObamaSpendChange, ObamaPollChange, data = final.df3[-1,], geom = "point", colour = week) + 
  ylab("Change in Percentage Support (Weekly Average)") +
  xlab("Change in Super PAC Spending") +
  annotate("text", x = final.df3$ObamaSpendChange[15], y = final.df3$ObamaPollChange[15], label = "1", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = final.df3$ObamaSpendChange[18], y = final.df3$ObamaPollChange[18], label = "2", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = final.df3$ObamaSpendChange[19], y = final.df3$ObamaPollChange[19], label = "3", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = final.df3$ObamaSpendChange[21], y = final.df3$ObamaPollChange[21], label = "4", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = final.df3$ObamaSpendChange[24], y = final.df3$ObamaPollChange[24], label = "5", color = "#000000", hjust = -0.5, size = 5) +
  geom_smooth()

qplot(RomneySpendChange, RomneyPollChange, data = final.df3[-1,], geom = "point", colour = week) + 
  ylab("Change in Percentage Support (Weekly Average)") +
  xlab("Change in Super PAC Spending") +
  annotate("text", x = final.df3$RomneySpendChange[15], y = final.df3$RomneyPollChange[15], label = "1", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = final.df3$RomneySpendChange[18], y = final.df3$RomneyPollChange[18], label = "2", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = final.df3$RomneySpendChange[19], y = final.df3$RomneyPollChange[19], label = "3", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = final.df3$RomneySpendChange[21], y = final.df3$RomneyPollChange[21], label = "4", color = "#000000", hjust = -0.5, size = 5) +
  annotate("text", x = final.df3$RomneySpendChange[24], y = final.df3$RomneyPollChange[24], label = "5", color = "#000000", hjust = -0.5, size = 5) +
  scale_colour_gradientn(colours = c("#850707", "#F78686")) +
  geom_smooth(colour = I("red"))
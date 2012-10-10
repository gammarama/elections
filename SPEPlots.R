library(ggplot2)
library(lubridate)
library(RColorBrewer)

source("SPEDataCleanup.R", echo = TRUE)

###
# Plots
###
# qplot(exp_dat, exp_amo, data = pres.data, color=beneful_can) + facet_grid(facets=bucket2~sup_opp, scales = "free_y") + scale_colour_manual(name = "Benefiting Candidate", values = c("#3D64FF", "#CC0033"), labels=c("Obama","Romney" )) + xlab("") + ylab("Amount Spent") + annotate(geom = "rect", xmin = as.Date("2012-8-11"), xmax = as.Date("2012-8-12"), ymin = 0, ymax = 10000, alpha = .4, colour = "purple") + annotate(geom = "rect", xmin = as.Date("2012-8-28"), xmax = as.Date("2012-8-30"), ymin = 0, ymax = 10000, alpha = .4, colour = "purple")+ annotate(geom = "rect", xmin = as.Date("2012-9-4"), xmax = as.Date("2012-9-6"), ymin = 0, ymax = 10000, alpha = .4, colour = "purple") + scale_y_log10()
#qplot(week, WeeklySum, geom = "point", data = num.weeks, color=beneful_can) + 
#  facet_grid(facets=bucket2~sup_opp) + 
#  scale_colour_manual(name = "Benefiting Candidate", values = c("#3D64FF", "#CC0033"), labels=c("Obama","Romney" )) + 
#  xlab("") + 
#  ylab("Amount Spent (Log 10)") + 
#  annotate(geom = "rect", xmin = week(as.Date("2012-8-11")), xmax = week(as.Date("2012-8-12")), ymin = 0, ymax = 10000, alpha = .4, colour = "purple") + 
#  annotate(geom = "rect", xmin = week(as.Date("2012-8-28")), xmax = week(as.Date("2012-8-30")), ymin = 0, ymax = 10000, alpha = .4, colour = "purple") + 
#  annotate(geom = "rect", xmin = week(as.Date("2012-9-4")), xmax = week(as.Date("2012-9-6")), ymin = 0, ymax = 10000, alpha = .4, colour = "purple") + 
#  scale_y_log10()

ggplot(num.weeks, aes(week, WeeklySum, fill = beneful_can)) + geom_bar(position = "dodge") + facet_grid(facets=bucket2~sup_opp) + 
  scale_fill_manual(name = "Benefiting Candidate", values = c("#3D64FF", "#CC0033"), labels=c("Obama","Romney" )) + 
  xlab("Week") + 
  ylab("Amount Spent (Log 10)") + 
  annotate(geom = "rect", xmin = factor(week(as.Date("2012-8-11"))), xmax = factor(week(as.Date("2012-8-12"))), ymin = 1, ymax = 1e+08, alpha = .4, colour = "darkgrey") + 
  annotate(geom = "rect", xmin = factor(week(as.Date("2012-8-28"))), xmax = factor(week(as.Date("2012-8-30"))), ymin = 1, ymax = 1e+08, alpha = .4, colour = "darkgrey") + 
  annotate(geom = "rect", xmin = factor(week(as.Date("2012-9-4"))), xmax = factor(week(as.Date("2012-9-6"))), ymin = 1, ymax = 1e+08, alpha = .4, colour = "darkgrey") + 
  scale_y_log10() +
  opts(title = "Spending by Category over Time")

#qplot(romney, obama, data=sum_exp2) +geom_abline() + geom_text(aes(label=`df2$bucket2`), data=subset(sum_exp2, (romney > 5*10^6 | obama > 5*10^6)), hjust=-.1)
qplot(romney, obama, data=sum_exp2, color = `df2$bucket2`) +geom_abline() + geom_text(aes(label=`df2$bucket2`), data=sum_exp2, hjust=-.1, vjust=-.02) + scale_x_log10() + scale_y_log10()

ggplot(sum_exp2_p, aes(bucket2, Sum, fill = beneful_can)) + geom_bar(position = "dodge") + coord_flip() + scale_y_log10() + scale_fill_manual(name = "Candidate", values = c("#3D64FF", "#CC0033")) + xlab("") + ylab("Amount Spent (Log 10)")
#ggplot(sum_exp2_p, aes(bucket2, Sum, fill = beneful_can)) + geom_bar(position = "dodge") + coord_flip() + scale_fill_manual(name = "Candidate", values = c("#3D64FF", "#CC0033")) + xlab("") + ylab("Amount Spent")

#ggplot(sum_exp2_p, aes(bucket2, Sum, fill = beneful_can)) + geom_bar() + coord_flip() + scale_y_log10() + scale_fill_manual(name = "Candidate", values = c("#3D64FF", "#CC0033")) + xlab("") + ylab("Amount Spent (Log 10)")
#ggplot(sum_exp2_p, aes(beneful_can, Sum, fill = bucket2)) + geom_bar() + coord_flip() + xlab("") + ylab("Amount Spent")


# By SUPER PAC
red <- rev(brewer.pal(7, "Reds"))
blue <- rev(brewer.pal(5, "Blues"))
ggplot(sum_exp.spep, aes(beneful_can, Sum, fill = spe_nam)) + geom_bar() + xlab("Benefiting Candidate") + ylab("Amount Spent") + scale_fill_manual(values=c(blue, red), labels = substring(sum_exp.spep$spe_nam, 1, 30), name = "Super PAC") + opts(title = "Spending by Super PAC")


#plot(romney, obama, data=sum_exp) +geom_abline() + geom_text(aes(label=`df$bucket`), data=subset(sum_exp, (romney > 5*10^6 | obama > 5*10^6)), hjust=-.1)
#qplot(romney, obama, data=sum_exp, color = `df$bucket`) +geom_abline() + geom_text(aes(label=`df$bucket`), data=sum_exp, hjust=-.1, vjust=-.02) + scale_x_log10() + scale_y_log10()

#ggplot(sum_exp_p, aes(bucket, Sum, fill = beneful_can)) + geom_bar(position = "dodge") + coord_flip() + scale_y_log10() + scale_fill_manual(name = "Candidate", values = c("#3D64FF", "#CC0033")) + xlab("") + ylab("Amount Spent (Log 10)")
#ggplot(sum_exp_p, aes(bucket, Sum, fill = beneful_can)) + geom_bar() + coord_flip() + scale_y_log10() + scale_fill_manual(name = "Candidate", values = c("#3D64FF", "#CC0033")) + xlab("") + ylab("Amount Spent (Log 10)")
#ggplot(sum_exp_p, aes(beneful_can, Sum, fill = bucket)) + geom_bar() + coord_flip() + xlab("") + ylab("Amount Spent")
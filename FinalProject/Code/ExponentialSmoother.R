source("FinalProject/Code/PollsCleanup.R")

library(zoo)
library(forecast)

polls.week <- ddply(subset(polls.data, !isNational), .(week = factor(week(Date)), isNational), summarise, Obama.Romney.Avg = mean(Obama.Romney), Obama = mean(Obama), Romney = mean(Romney))
polls.week$week <- as.numeric(as.character(polls.week$week))
names(polls.week)[4] <- "Obama.Poll"
names(polls.week)[5] <- "Romney.Poll"

polls.avg<-ddply(polls.data, .(isNational, Date), summarise, Obama.Romney=mean(Obama.Romney))

polls.zoo.Nat<-zoo(subset(polls.avg, isNational)[,3],subset(polls.avg, isNational)[,2])
polls.zoo.Swing<-zoo(subset(polls.avg, !isNational)[,3],subset(polls.avg, !isNational)[,2])

x.Nat <- merge(polls.zoo.Nat, zoo(order.by=seq(start(polls.zoo.Nat), end(polls.zoo.Nat), by="days")))
x.Swing <- merge(polls.zoo.Swing, zoo(order.by=seq(start(polls.zoo.Swing), end(polls.zoo.Swing), by="days")))

x.Nat <- na.spline(x.Nat)
x.Nat.df <- data.frame(dates=index(x.Nat), weight=coredata(x.Nat))

x.Swing <- na.spline(x.Swing)
x.Swing.df <- data.frame(dates=index(x.Swing), weight=coredata(x.Swing))

polls.avg.Nat.Smooth<-HoltWinters(x.Nat, alpha=.1, beta=FALSE, gamma=FALSE, l.start=polls.avg.Nat[1])
polls.avg.Swing.Smooth<-HoltWinters(x.Swing, alpha=.03, beta=FALSE, gamma=FALSE, l.start=polls.avg.Swing[1])

polls.avg.smooth<-ts.union(polls.avg.Nat.Smooth$fitted, polls.avg.Swing.Smooth$fitted)

polls.smooth<-data.frame(polls.avg.smooth)[,c(1,3)]
polls.smooth$Date<-as.Date(as.numeric(time(polls.avg.smooth)))

polls.smooth<-data.frame(rbind(cbind(polls.smooth$Date,rep("TRUE", nrow(polls.smooth)),polls.smooth[,1]),
                               cbind(polls.smooth$Date,rep("FALSE", nrow(polls.smooth)),polls.smooth[,2])))
colnames(polls.smooth)<-c("Date","isNational", "Obama.Romney")
polls.smooth$Date<-as.Date(as.numeric(as.character(polls.smooth$Date)))
polls.smooth$isNational<-as.factor(polls.smooth$isNational)
polls.smooth$Obama.Romney<-as.numeric(as.character(polls.smooth$Obama.Romney))

labels.tracking <- data.frame(Obama.Romney=c(-12, 12), date=c(as.Date("1/8/2012","%d/%m/%Y"), as.Date("1/8/2012","%d/%m/%Y")), text=c("Romney", "Obama"), colour=c("red", "blue"))

qplot(Date, Obama.Romney, data = polls.data, colour = isNational) +  
  annotate("text", x=as.Date("18/7/2012","%d/%m/%Y"), y=20, label="1", color="#FF0000", hjust=-0.5, alpha=0.4, size=5) +
  annotate("rect", xmin=as.Date("18/7/2012","%d/%m/%Y"), ymin=-20, xmax=as.Date("19/7/2012","%d/%m/%Y"), ymax=20, fill="#FF0000", alpha=0.4) +
  scale_colour_manual(name = " ", values = c("#600CAC","#FF7600"), labels=c("Swing State","National" ), breaks=c(FALSE, TRUE)) + 
  scale_x_date(limits=c(as.Date("25/4/2012","%d/%m/%Y"), as.Date("2/11/2012","%d/%m/%Y"))) +
  scale_y_continuous(limits=c(-20,20)) +
  ylab("Romney -------- | -------- Obama  ") +
  annotate("rect", xmin=as.Date("2/2/2012","%d/%m/%Y"), xmax=as.Date("2/12/2012","%d/%m/%Y"), ymin=0, ymax=Inf, fill="blue", alpha=0.03) +
  annotate("rect", xmin=as.Date("2/2/2012","%d/%m/%Y"), xmax=as.Date("2/12/2012","%d/%m/%Y"), ymin=-Inf, ymax=0, fill="red", alpha=0.03) +
  geom_text(data=labels.tracking[1,], mapping=aes(x=date, y=Obama.Romney, label=text), colour=I("#FF0000"), size=5, alpha=I(0.2), shape=1) +
  geom_text(data=labels.tracking[2,], mapping=aes(x=date, y=Obama.Romney, label=text), colour=I("#330099"), size=5, alpha=I(0.2), shape=1) +
  theme_bw() +
  theme(legend.position="bottom") +
  geom_line(data=polls.smooth, aes(x=Date, y=Obama.Romney, colour=isNational), inherit.aes=FALSE)


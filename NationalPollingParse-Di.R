library(ggplot2)
library(XML)
library(lubridate)
library(plyr)
#current.polls.tracking <- read.csv("current-polls-tracking.csv")
#current.polls.tracking$date <- as.Date(current.polls.tracking$date)
webpg ="http://www.nationalpolls.com/2012/obama-vs-romney.html"
doc <- htmlParse(webpg)
r <- xmlRoot(doc)
body <- xmlChildren(r)[[2]]
tables <- getNodeSet(doc, "//table")
polls<-NULL

x <- xmlValue(tables[[6]], "td")
#dt <- as.Date(paste(strsplit(x, " ")[[1]][2], "2012"), "%b %d %Y")
xp <- getNodeSet(tables[[6]], "tr")
xps <- data.frame(do.call("rbind", sapply(xp[-1], function(x) (xpathSApply(x, "td", xmlValue)))))
names(xps) <- c("Pollster", "Region", "Date.Range", "Obama", "Romney")
xps$Date.Range <- as.character(xps$Date.Range)
xps$Obama <- as.numeric(as.character(xps$Obama))
xps$Romney <- as.numeric(as.character(xps$Romney))

xps$Date <- as.Date(sapply(xps$Date.Range, FUN= function(i) strsplit(i, "-")[[1]][2]), "%m/%d/%y")
xps$Region[which(xps$Region=="ConnecticutPPP")] <- "Connecticut"

polls.sub <- xps[which(rowSums(is.na(xps))==0), -3]
polls.sub$Obama.Romney<-polls.sub$Obama-polls.sub$Romney
polls.sub$isNational <- polls.sub$Region=="National"

write.csv(polls.sub, file="NationalPolling-current-polls.csv", row.names=F)

labels.tracking <- data.frame(Obama.Romney=c(-12, 12), date=c(as.Date("1/8/2012","%d/%m/%Y"), as.Date("1/8/2012","%d/%m/%Y")), text=c("Romney", "Obama"), colour=c("red", "blue"))

polls.sub$Pollster<-as.character(polls.sub$Pollster)
polls.sub$MajorPollster <- polls.sub$Pollster

polls.sub$MajorPollster[grepl("Gallup", polls.sub$Pollster, ignore.case=TRUE)] <- "Gallup"
polls.sub$MajorPollster[grepl("Rasmussen", polls.sub$Pollster, ignore.case=TRUE)] <- "Rasmussen" 
polls.sub$MajorPollster[grepl("Ramussen", polls.sub$Pollster, ignore.case=TRUE)] <- "Rasmussen" 
polls.sub$MajorPollster[grepl("ABC", polls.sub$Pollster, ignore.case=TRUE)] <- "ABC" 
polls.sub$MajorPollster[grepl("NBC", polls.sub$Pollster, ignore.case=TRUE)] <- "NBC" 
polls.sub$MajorPollster[grepl("CBS", polls.sub$Pollster, ignore.case=TRUE)] <- "CBS" 
polls.sub$MajorPollster[grepl("CNN", polls.sub$Pollster, ignore.case=TRUE)] <- "CNN"
polls.sub$MajorPollster[grepl("Pew", polls.sub$Pollster, ignore.case=TRUE)] <- "Pew" 
polls.sub$MajorPollster[grepl("Public Policy", polls.sub$Pollster, ignore.case=TRUE)] <- "PPP" 
polls.sub$MajorPollster[grepl("PPP", polls.sub$Pollster, ignore.case=TRUE)] <- "PPP"
polls.sub$MajorPollster[grepl("Fox", polls.sub$Pollster, ignore.case=TRUE)] <- "Fox"
polls.sub$MajorPollster[grepl("Quinnipiac", polls.sub$Pollster, ignore.case=TRUE)] <- "Quinnipiac"
polls.sub$MajorPollster[grepl("Dixon", polls.sub$Pollster, ignore.case=TRUE)] <- "MasonDixon"
polls.sub$MajorPollster[grepl("Democracy", polls.sub$Pollster, ignore.case=TRUE)] <- "DemocracyCorps"
polls.sub$MajorPollster[grepl("Survey", polls.sub$Pollster, ignore.case=TRUE)&
    grepl("USA", polls.sub$Pollster, ignore.case=TRUE)] <- "SurveyUSA"
polls.sub$MajorPollster[grepl("Reuters", polls.sub$Pollster, ignore.case=TRUE)] <- "AP.Reuters"
polls.sub$MajorPollster[grepl("AP", polls.sub$Pollster, ignore.case=TRUE)] <- "AP.Reuters"
polls.sub$MajorPollster[grepl("YouGov", polls.sub$Pollster, ignore.case=TRUE)] <- "YouGov"

tracking.polls<-subset(polls.sub, Region=="National")
tracking.polls$MajorPollster <- as.character(tracking.polls$MajorPollster)
pollsters <- unique(tracking.polls$MajorPollster)
for (i in 1:length(pollsters)) {
    indx <- c(1:nrow(tracking.polls))[tracking.polls$MajorPollster == pollsters[i]]
    if (length(indx) < 10) 
        tracking.polls$MajorPollster[indx] <- "Other"
}
tracking.polls$MajorPollster <- factor(tracking.polls$MajorPollster)
tracking.polls <- subset(tracking.polls, Date > as.Date("9/4/2012","%d/%m/%Y"))

pollsters <- unique(polls.sub$MajorPollster)
for (i in 1:length(pollsters)) {
    indx <- c(1:nrow(polls.sub))[polls.sub$MajorPollster == pollsters[i]]
    if (length(indx) < 10) 
        polls.sub$MajorPollster[indx] <- "Other"
}
polls.sub$MajorPollster <- factor(polls.sub$MajorPollster)
polls.sub$IsMajor <- polls.sub$MajorPollster!="Other"

#qplot(data=subset(polls.sub, year(polls.sub$Date)>=2011), x=Date, y=Obama, 
#      geom="point", col=I("blue"), alpha=.2+.8*IsMajor, position="jitter") + 
#  geom_point(aes(y=Romney, alpha=.2+.2*IsMajor), col=I("red"), position="jitter") +  
#  geom_smooth(aes(y=Romney), colour=I("red")) +  
#  geom_smooth(aes(y=Obama)) + 
#  scale_alpha_continuous(range=c(.2, .4)) 

labels.tracking <- data.frame(Obama.Romney=c(-14, 14), Date=c(as.Date("1/8/2012","%d/%m/%Y"), as.Date("1/8/2012","%d/%m/%Y")), text=c("Romney", "Obama"), colour=c("red", "blue"))
polls.means <- data.frame(Date=tracking.polls$Date, Obama.Romney=tracking.polls$Obama.Romney)
polls.means$Date <- as.Date(polls.means$Date)
polls.means$wks <- as.Date("1/1/2012","%d/%m/%Y") + weeks(week(polls.means$Date))
polls.m <- ddply(polls.means, "wks", summarise, median=median(Obama.Romney))
tracking.polls$weeks <- as.Date("1/1/2012","%d/%m/%Y") + weeks(week(tracking.polls$Date))

#ggplot(data=tracking.polls, ylim=c(-15,15)) +
#  geom_point(data=tracking.polls, mapping=aes(x=Date, y=Obama.Romney)) + 
#  annotate("rect", xmin=as.Date("2/2/2012","%d/%m/%Y"), xmax=as.Date("2/12/2012","%d/%m/%Y"), ymin=0, ymax=Inf, fill="blue", alpha=0.03) +
#  annotate("rect", xmin=as.Date("2/2/2012","%d/%m/%Y"), xmax=as.Date("2/12/2012","%d/%m/%Y"), ymin=-Inf, ymax=0, fill="red", alpha=0.03) +
#  geom_text(data=labels.tracking[1,], mapping=aes(x=Date, y=Obama.Romney, label=text), colour=I("#FF0000"), size=10, alpha=I(0.2), shape=1) +
#  geom_text(data=labels.tracking[2,], mapping=aes(x=Date, y=Obama.Romney, label=text), colour=I("#330099"), size=10, alpha=I(0.2), shape=1) + 
#  scale_x_date(limits=c(as.Date("9/4/2012","%d/%m/%Y"), as.Date("2/11/2012","%d/%m/%Y"))) + 
#  annotate("text", x=as.Date("11/8/2012","%d/%m/%Y"), y=-10, label="1", color="#FF0000", hjust=-0.5, alpha=0.2) + 
#  annotate("rect", xmin=as.Date("10/8/2012","%d/%m/%Y"), ymin=0, xmax=as.Date("11/8/2012","%d/%m/%Y"), ymax=-10, fill="#FF0000", alpha=0.2) + 
#  annotate("text", x=as.Date("28/8/2012","%d/%m/%Y"), y=-10, label="2", color="#FF0000", hjust=-0.7, alpha=0.2) + 
#  annotate("rect", xmin=as.Date("27/8/2012","%d/%m/%Y"), ymin=0, xmax=as.Date("30/8/2012","%d/%m/%Y"), ymax=-10, fill="#FF0000", alpha=0.2) +
#  annotate("text", x=as.Date("5/9/2012","%d/%m/%Y"), y=10, label="3", color="#330099", hjust=-0.5, alpha=0.2) + 
#  annotate("rect", xmin=as.Date("4/9/2012","%d/%m/%Y"), ymin=0, xmax=as.Date("6/9/2012","%d/%m/%Y"), ymax=10, fill="#330099", alpha=0.2) +
#  geom_smooth(data=tracking.polls, mapping=aes(x=Date, y=Obama.Romney), colour=I("black"), alpha=0.1, span=0.9) +
#  geom_point(data=tracking.polls, mapping=aes(x=Date, y=Obama.Romney, colour=MajorPollster), size=I(3)) +
#  geom_hline(yintercept=0, alpha=0.3) + xlab("Date") + ylab("Difference") + 
#  scale_y_continuous(limits=c(-15,15)) 


#ggplot(data=tracking.polls, ylim=c(-15,15)) +
#  geom_point(data=tracking.polls, mapping=aes(x=Date, y=Obama.Romney)) + 
#  annotate("rect", xmin=as.Date("2/2/2012","%d/%m/%Y"), xmax=as.Date("2/12/2012","%d/%m/%Y"), ymin=0, ymax=Inf, fill="blue", alpha=0.03) +
#  annotate("rect", xmin=as.Date("2/2/2012","%d/%m/%Y"), xmax=as.Date("2/12/2012","%d/%m/%Y"), ymin=-Inf, ymax=0, fill="red", alpha=0.03) +
#  geom_text(data=labels.tracking[1,], mapping=aes(x=Date, y=Obama.Romney, label=text), colour=I("#FF0000"), size=10, alpha=I(0.2), shape=1) +
#  geom_text(data=labels.tracking[2,], mapping=aes(x=Date, y=Obama.Romney, label=text), colour=I("#330099"), size=10, alpha=I(0.2), shape=1) + 
#  geom_hline(yintercept=0, colour="white", size=2) + scale_x_date(limits=c(as.Date("9/4/2012","%d/%m/%Y"), as.Date("2/11/2012","%d/%m/%Y"))) + 
#  annotate("text", x=as.Date("11/8/2012","%d/%m/%Y"), y=-10, label="1", color="#FF0000", hjust=-0.5, alpha=0.2) + 
#  annotate("rect", xmin=as.Date("10/8/2012","%d/%m/%Y"), ymin=0, xmax=as.Date("11/8/2012","%d/%m/%Y"), ymax=-10, fill="#FF0000", alpha=0.2) + 
#  annotate("text", x=as.Date("28/8/2012","%d/%m/%Y"), y=-10, label="2", color="#FF0000", hjust=-0.7, alpha=0.2) + 
#  annotate("rect", xmin=as.Date("27/8/2012","%d/%m/%Y"), ymin=0, xmax=as.Date("30/8/2012","%d/%m/%Y"), ymax=-10, fill="#FF0000", alpha=0.2) +
#  annotate("text", x=as.Date("5/9/2012","%d/%m/%Y"), y=10, label="3", color="#330099", hjust=-0.5, alpha=0.2) + 
#  annotate("rect", xmin=as.Date("4/9/2012","%d/%m/%Y"), ymin=0, xmax=as.Date("6/9/2012","%d/%m/%Y"), ymax=10, fill="#330099", alpha=0.2) +
#  geom_point(data=tracking.polls, mapping=aes(x=Date, y=Obama.Romney, colour=MajorPollster), size=I(3)) + 
#  facet_wrap(~MajorPollster, ncol=4, margins=T) +
#  geom_line(data=polls.m, mapping=aes(x=wks, y=median), colour=I("grey50"), size=2, alpha=0.4) +
#  xlab("Date") + ylab("Difference") + 
#  scale_y_continuous(limits=c(-15,15)) + theme_bw()

#bysum <- with(tracking.polls, reorder(MajorPollster, Obama.Romney, sum))
bymedian <- with(tracking.polls, reorder(MajorPollster, Obama.Romney, median))
tracking.polls$MajorPollster <- bymedian
ggplot(data=tracking.polls, ylim=c(-15,15)) +
    geom_point(data=tracking.polls, mapping=aes(x=Date, y=Obama.Romney), size=0.01) + 
    annotate("rect", xmin=as.Date("2/2/2012","%d/%m/%Y"), xmax=as.Date("2/12/2012","%d/%m/%Y"), ymin=0, ymax=Inf, fill="blue", alpha=0.03) +
    annotate("rect", xmin=as.Date("2/2/2012","%d/%m/%Y"), xmax=as.Date("2/12/2012","%d/%m/%Y"), ymin=-Inf, ymax=0, fill="red", alpha=0.03) +
    geom_text(data=labels.tracking[1,], mapping=aes(x=Date, y=Obama.Romney, label=text), colour=I("#FF0000"), size=10, alpha=I(0.2), shape=1) +
    geom_text(data=labels.tracking[2,], mapping=aes(x=Date, y=Obama.Romney, label=text), colour=I("#330099"), size=10, alpha=I(0.2), shape=1) + 
    geom_hline(yintercept=0, colour="white", size=2) + scale_x_date(limits=c(as.Date("9/4/2012","%d/%m/%Y"), as.Date("2/11/2012","%d/%m/%Y"))) + 
    annotate("text", x=as.Date("11/8/2012","%d/%m/%Y"), y=-10, label="1", color="#FF0000", hjust=-0.5, alpha=0.2) + 
    annotate("rect", xmin=as.Date("10/8/2012","%d/%m/%Y"), ymin=0, xmax=as.Date("11/8/2012","%d/%m/%Y"), ymax=-10, fill="#FF0000", alpha=0.2) + 
    annotate("text", x=as.Date("28/8/2012","%d/%m/%Y"), y=-10, label="2", color="#FF0000", hjust=-0.7, alpha=0.2) + 
    annotate("rect", xmin=as.Date("27/8/2012","%d/%m/%Y"), ymin=0, xmax=as.Date("30/8/2012","%d/%m/%Y"), ymax=-10, fill="#FF0000", alpha=0.2) +
    annotate("text", x=as.Date("5/9/2012","%d/%m/%Y"), y=10, label="3", color="#330099", hjust=-0.5, alpha=0.2) + 
    annotate("rect", xmin=as.Date("4/9/2012","%d/%m/%Y"), ymin=0, xmax=as.Date("6/9/2012","%d/%m/%Y"), ymax=10, fill="#330099", alpha=0.2) +
    geom_point(data=tracking.polls, mapping=aes(x=Date, y=Obama.Romney, colour=MajorPollster), size=3, alpha=0.4) + 
    geom_smooth(mapping=aes(x=Date, y=Obama.Romney, colour=MajorPollster), se=F, size=1.5, alpha=0.2) + 
    facet_wrap(~MajorPollster, ncol=4) + 
    xlab("Date") + ylab("Difference") + 
    scale_y_continuous(limits=c(-15,15)) + scale_colour_discrete("") + 
    theme_bw() + 
    theme(legend.title=element_text(""),legend.position="bottom", 
          strip.background = element_rect(fill="grey95", colour="grey95"), 
          panel.border = element_rect(colour="grey95"),
          strip.text = element_text(colour="white", size=12, face="bold"))

states <- map_data("state")
missing.states <- data.frame(long=c(-122, -122, -118, -118, -122, -113, -113, -109, -109, -113, -68, -68, -72, 
                                    -72, -68, -68, -68, -72, -72, -68), lat=c(27, 29, 29, 27, 27, 27, 29, 29, 27, 27, 33, 35, 35, 33, 33, 30, 
                                                                              32, 32, 30, 30), group=rep(64:67, rep(5,4)), order=rep(1:5, 4), region=c(rep("alaska", 5), rep("hawaii", 5), 
                                                                                                                                                       rep("rhode island", 5), rep("district of columbia", 5)), subregion=NA)
states <- rbind(states, missing.states)
states.unique <- data.frame(State=unique(states$region))
# Find most recent polls
polls.state <- subset(polls.sub, Region!="National", Pollster:Obama.Romney)
polls.state <- subset(polls.state, Date > as.Date("2012-9-4", format="%Y-%d-%m"))
polls.state$Pollster <- as.character(polls.state$Pollster)
polls.state$Date <- as.character(polls.state$Date)
polls.state$Region <- tolower(as.character(polls.state$Region))
polls.state$Region<-gsub("-", " ", polls.state$Region)
polls.state$Region<-gsub("hamsphire", "hampshire", polls.state$Region)
x<-unique(states$region)
y<-unique(polls.state$Region)
missing.polls <- x[!x%in%y]
missing.values <- NULL
if ("alabama" %in% missing.polls) 
    missing.values <- rbind(missing.values, c("guess", "alabama", 40, 55, "2012-04-09", -15))
if ("arkansas" %in% missing.polls) 
    missing.values <- rbind(missing.values, c("guess", "arkansas", 40, 55, "2012-04-09", -15))
if ("delaware" %in% missing.polls) 
    missing.values <- rbind(missing.values,c("guess", "delaware", 55, 40, "2012-04-09", 15))
if ("district of columbia" %in% missing.polls)
    missing.values <- rbind(missing.values,c("guess", "district of columbia", 55, 40, "2012-04-09", 15))
if ("idaho" %in% missing.polls)
    missing.values <- rbind(missing.values,c("guess", "idaho", 40, 55, "2012-04-09", -15))
if ("kansas" %in% missing.polls)
    missing.values <- rbind(missing.values,c("guess", "kansas", 40, 55, "2012-04-09", -15))
if ("louisiana" %in% missing.polls) 
    missing.values <- rbind(missing.values,c("guess", "louisiana", 40, 55, "2012-04-09", -15))
if ("maryland" %in% missing.polls) 
    missing.values <- rbind(missing.values,c("guess", "maryland", 55, 40, "2012-04-09", 15))
if ("mississippi" %in% missing.polls) 
    missing.values <- rbind(missing.values,c("guess", "mississippi", 40, 55, "2012-04-09", -15))
if ("rhode island" %in% missing.polls) 
    missing.values <- rbind(missing.values,c("guess", "rhode island", 55, 40, "2012-04-09", 15))
if ("south carolina" %in% missing.polls) 
    missing.values <- rbind(missing.values,c("guess", "south carolina", 40, 55, "2012-04-09", -15))
if ("south dakota" %in% missing.polls) 
missing.values <- rbind(missing.values,c("guess", "south dakota", 40, 55, "2012-04-09", -15))
if ("west virginia" %in% missing.polls) 
missing.values <- rbind(missing.values,c("guess", "west virginia", 40, 55, "2012-04-09", -15))
if ("wyoming" %in% missing.polls) 
    missing.values <- rbind(missing.values,c("guess", "wyoming", 40, 55, "2012-04-09", -15))
if ("alaska" %in% missing.polls) 
    missing.values <- rbind(missing.values,c("guess", "alaska", 40, 55, "2012-04-09", -15))
if ("hawaii" %in% missing.polls) 
    missing.values <- rbind(missing.values,c("guess", "hawaii", 55, 40, "2012-04-09", 15))
colnames(missing.values)<-c("Pollster", "Region", "Obama", "Romney", "Date", "Obama.Romney")
missing.values <- as.data.frame(missing.values)
missing.values$Pollster <- as.character(missing.values$Pollster)
missing.values$Region <- as.character(missing.values$Region)
missing.values$Obama <- as.numeric(as.character(missing.values$Obama))
missing.values$Romney <- as.numeric(as.character(missing.values$Romney))
missing.values$Date <- as.character(missing.values$Date)
missing.values$Obama.Romney <- as.numeric(as.character(missing.values$Obama.Romney))
#polls.state <- list(missing.values, polls.state) 
#polls.state <- ldply(polls.state)[,-1]
polls.state <- rbind(polls.state, missing.values)
polls.state$Date <- as.Date(polls.state$Date, format="%Y-%m-%d")
#polls.state$Region <- factor(polls.state$Region)
#polls.state.recent <- polls.state
#for (i in 1:length(indx)) { # Only keep the polls in last three weeks, if they exist
#  x<-subset(polls.state.recent, Region==indx[i])
#  if (length(x$Date[x$Date>=(today()-days(21))])>0) {
#    rmrows <- c(1:nrow(polls.state.recent))[(polls.state.recent$State==indx[i]) & (polls.state.recent$Date<(today()-days(21)))]
#    if (nrow(polls.state.recent[-rmrows,])>0)
#      polls.state.recent <- polls.state.recent[-rmrows,]
#  }
#  else { # Keep the latest poll
#    ltstdate <- max(x$Date)
#    rmrows <- c(1:nrow(polls.state.recent))[(polls.state.recent$State==indx[i]) & (polls.state.recent$Date!=ltstdate)]
#    if (nrow(polls.state.recent[-rmrows,])>0)
#      polls.state.recent <- polls.state.recent[-rmrows,]    
#  }
#}
indx<-unique(polls.state$Region)
polls.state.recent<-NULL
for (i in 1:length(indx)) {
    x<-subset(polls.state, Region==indx[i])
    if (length(x$Date[x$Date>=(today()-days(21))])>0) 
        polls.state.recent <- rbind(polls.state.recent, x[x$Date>=(today()-days(21)), ])
    else  # Keep the latest poll
        polls.state.recent <- rbind(polls.state.recent, x[order(x$Date, decreasing=T)[1], ])
}
polls.state.av <- ddply(polls.state.recent, "Region", summarise, Obama.Romney=mean(Obama.Romney, na.rm=T))

ord <- order(polls.state.av$Obama.Romney, decreasing=T)
polls.state.av$Region <- factor(polls.state.av$Region, levels=polls.state.av$Region[ord])
polls.state.recent <- merge(polls.state.recent, states.unique, all=TRUE)
polls.state.recent$Region <- factor(polls.state.recent$Region, levels=levels(polls.state.av$Region))
labels <- data.frame(Region=c("north carolina", "north carolina"), Obama.Romney=c(30, -30), text=c("Romney", "Obama"), colour=c("red", "blue"))
polls.state.recent$Obama.Romney <- -polls.state.recent$Obama.Romney
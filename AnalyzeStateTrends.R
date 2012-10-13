##--------------------------------------------------
##0. Load libraries
library(ggplot2)
library(XML)
library(lubridate)
library(plyr)
library(reshape2)

##--------------------------------------------------
##1. Load and prepare data
webpg ="http://www.nationalpolls.com/2012/obama-vs-romney.html"
doc <- htmlParse(webpg)
r <- xmlRoot(doc)
body <- xmlChildren(r)[[2]]
tables <- getNodeSet(doc, "//table")
polls<-NULL

x <- xmlValue(tables[[6]], "td")
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

labels.tracking <- data.frame(Obama.Romney=c(-14, 14), Date=c(as.Date("1/8/2012","%d/%m/%Y"), as.Date("1/8/2012","%d/%m/%Y")), text=c("Romney", "Obama"), colour=c("red", "blue"))
polls.means <- data.frame(Date=tracking.polls$Date, Obama.Romney=tracking.polls$Obama.Romney)
polls.means$Date <- as.Date(polls.means$Date)
polls.means$wks <- as.Date("1/1/2012","%d/%m/%Y") + weeks(week(polls.means$Date))
polls.m <- ddply(polls.means, "wks", summarise, median=median(Obama.Romney))
tracking.polls$weeks <- as.Date("1/1/2012","%d/%m/%Y") + weeks(week(tracking.polls$Date))

bymedian <- with(tracking.polls, reorder(MajorPollster, Obama.Romney, median))
tracking.polls$MajorPollster <- bymedian

polls.state <- subset(polls.sub, Region!="National", Pollster:Obama.Romney)
polls.state <- subset(polls.state, Date > as.Date("2012-9-4", format="%Y-%d-%m"))
polls.state$Pollster <- as.character(polls.state$Pollster)
polls.state$Date <- as.character(polls.state$Date)
polls.state$Region <- tolower(as.character(polls.state$Region))
polls.state$Region<-gsub("-", " ", polls.state$Region)
polls.state$Region<-gsub("hamsphire", "hampshire", polls.state$Region)

polls.state$Date <- as.Date(polls.state$Date, format="%Y-%m-%d")

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
polls.state.recent$Region <- factor(polls.state.recent$Region, levels=levels(polls.state.av$Region))
labels <- data.frame(Region=c("north carolina", "north carolina"), Obama.Romney=c(30, -30), text=c("Romney", "Obama"), colour=c("red", "blue"))
polls.state.recent$Obama.Romney <- -polls.state.recent$Obama.Romney

##--------------------------------------------------
##2. Display State trends
analyzeStateTrends <- function(state) {
  polls.chosen <- subset(polls.state, Region %in% state)[,-6]
  polls.chosenm <- melt(polls.chosen, id = c("Date", "Pollster", "Region"))
  qplot(Date, value, data = polls.chosenm, colour = variable) + geom_smooth() +
    scale_colour_manual(name = "Candidate", values = c("#3D64FF", "#CC0033"), labels=c("Obama","Romney" )) +
    facet_wrap(facets = ~Region) +
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
swing.states <- c("Colorado", "Florida", "Iowa", "Missouri", "Michigan", "Nevada", "New Hampshire", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "Wisconsin")
analyzeStateTrends(tolower(swing.states))



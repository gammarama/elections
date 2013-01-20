library(plyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(animation)
library(maps)

states <- map_data("state")
missing.states <- data.frame(long=c(-122, -122, -118, -118, -122, -113, -113, -109, -109, -113, -68, -68, -72, -72, -68, -68, -68, -72, -72, -68), lat=c(27, 29, 29, 27, 27, 27, 29, 29, 27, 27, 33, 35, 35, 33, 33, 30, 32, 32, 30, 30), group=rep(64:67, rep(5,4)), order=rep(1:5, 4), region=c(rep("alaska", 5), rep("hawaii", 5), rep("rhode island", 5), rep("district of columbia", 5)), subregion=NA)
states <- rbind(states, missing.states)
missing.states.txt <- data.frame(long=c(-117.5, -108.5, -67.5, -67.5), lat=c(28, 28, 34, 31), text=c("AK","HI","RI","DC"))

obama.states <- c("California", "Connecticut", "Delaware", "District of Columbia", "Hawaii", "Illinois", "Maine", "Maryland", "Massachusetts", "Minnesota", "New Jersey", "New Mexico", "New York", "Oregon", "Rhode Island", "Vermont", "Washington")
swing.states <- c("Colorado", "Florida", "Iowa", "Missouri", "Michigan", "Nevada", "New Hampshire", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "Wisconsin")

states$swing <- "romney"
states$swing[states$region %in% tolower(obama.states)] <- "obama"
states$swing[states$region %in% tolower(swing.states)] <- "swing"
states$isSwing <- states$swing == "swing"

swingStatePlot <- qplot(long, lat, geom = "polygon", data = states, group = group, fill = swing) +
    geom_path(size = .05, colour = "darkgrey") +
    scale_fill_manual(values = c("darkblue", "darkred", "#FFBE0D")) +
    #scale_alpha_manual(values = c(.25, 1)) +
    geom_text(data=missing.states.txt, mapping=aes(x=long, y=lat, label=text), size=3.5,  colour="grey50", hjust=0, inherit.aes=FALSE) +
    theme_bw() +
    theme(aspect.ratio=1/1.5, legend.position = "none") +
    theme(axis.ticks = element_blank(),
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())

# 
# pastElectionResults <- read.csv("../../Data/elections-1900-to-2012.csv")
# columnNames <- paste(c("Dem", "Rep", "Other"), as.character(rep(seq(2012, 1900, by = -4), each = 3)), sep = "")
# names(pastElectionResults) <- c("State", columnNames[-length(columnNames)])
# 
# pres.results.temp <- pastElectionResults[,-seq(4, 85, by = 3)]
# 
# pres.results <- data.frame(State = NULL, Year = NULL)
# 
# j <- seq(2012, 1900, by = -4)
# for (i in seq(2, ncol(pres.results.temp), by = 2)) {
#     
#     demcol <- pres.results.temp[,i]
#     repcol <- pres.results.temp[,i + 1]
#     
#     newcol <- demcol - repcol
#     
#     result <- cbind(as.character(pastElectionResults[,1]), rep(j[i / 2], 51), demcol, repcol, newcol)
#     pres.results <- rbind(result, pres.results)
# }
# names(pres.results) <- c("State", "Year", "Democratic", "Republican", "Margin")
# pres.results$Margin <- as.numeric(as.character(pres.results$Margin))
# #write.csv(pres.results, file = "PresidentialResults.csv")
# 
# adjMargin <- pres.results$Margin
# nationalMargin <- c(-6, -19, -9, 14, 3, -26, -25, -17, 18, 24, 10, 7, 4, -11, -15, 0, 23, -1, -23, 2, -10, -18, -8, 6, 9, 1, -2, 7, 4)
# for (i in seq(1900, 2012, by = 4)) {
#     adjMargin[which(pres.results$Year == i)] <- adjMargin[which(pres.results$Year == i)] - nationalMargin[(i - 1896)/4]
# }
# pres.results$adjMargin <- adjMargin
# 
# getChangePlot <- function(from, to, swing = FALSE) {
#     changeMargin <- pres.results$adjMargin[which(pres.results$Year == to)] - pres.results$adjMargin[which(pres.results$Year == from)]
#     df <- as.data.frame(cbind(tolower(subset(pres.results, Year == to)$State), changeMargin))
#     
#     names(df) <- c("region", "Margin")
#     df$Margin <- as.numeric(as.character(df$Margin))
#     df$region <- as.character(df$region)
#     df$region[df$region == "d.c."] <- "district of columbia"
#     
#     states.margin <- merge(states, df, by = "region")
#     
#     states.margin$ModMargin <- sapply(states.margin$Margin, min, min(25, max(states.margin$Margin)))
#     states.margin$ModMargin <- sapply(states.margin$ModMargin, max, max(-25, min(states.margin$Margin)))
#     
#     qplot(long, lat, geom = "polygon", data = states.margin, group = group, fill = ModMargin) + theme(aspect.ratio=1/1.5) +
#         scale_fill_gradient2() +
#         scale_size_discrete(range = c(.1, (swing + .1))) +
#         geom_path(aes(size = swing))
# }
# 
# getAnimPlots <- function() {
#     for (i in seq(1964, 2008, by = 4)) {
#         print(getChangePlot(i, 2012, swing = FALSE))
#     }
# }

#saveVideo(getAnimPlots(), other.opts = "-b 1000k")

#getChangePlot(1984, 2012, swing = FALSE) # Reagan's re-election to Obama's
#getChangePlot(1984, 2012, swing = TRUE) # Reagan's re-election to Obama's
#getChangePlot(1964, 2012, swing = TRUE) # LBJ to Obama

#qplot(Year, adjMargin, group = State, colour = State, data = subset(results2000s, State %in% c("Iowa", "Kentucky", "North Carolina", "Virginia", "West Virginia")), geom = "line") + geom_point()
#qplot(Year, adjMargin, group = State, colour = State, data = subset(results2000s, State %in% c("Rhode Island", "New York", "New Hampshire", "Maine", "Vermont")), geom = "line") + geom_point()
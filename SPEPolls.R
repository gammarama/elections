source("NationalPollingParse-Di.R")
source("SPEDataCleanup.R")

analyzeRyan <- function(state) {
    polls.chosen <- subset(polls.state, Region == state)[,-6]
    polls.chosenm <- melt(polls.chosen, id = c("Date", "Pollster", "Region"))
    qplot(Date, value, data = polls.chosenm, colour = variable) + geom_smooth() +
        annotate(geom = "rect", xmin = as.Date("2012-8-11"), xmax = as.Date("2012-8-12"), ymin = 40, ymax = 55, alpha = .4, colour = "darkred")
}

analyzeRyan("wisconsin")
analyzeRyan("florida")

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
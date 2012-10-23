library(plyr)
library(reshape2)

#Get csv from URL
data<-read.csv("http://www.fec.gov/data/IndependentExpenditure.do?format=csv&election_yr=2012")

#Format dates as dates
#data$rec_dat<-as.POSIXlt(format(data$rec_dat, format='%m/%d/$Y'),format='%m/%d/%Y')
data$rec_dat<-as.Date(data$rec_dat,format='%m/%d/%Y')
data$exp_dat<-as.Date(data$exp_dat,format='%m/%d/%Y')

#Format exp_amo, agg_amo as numeric
data$exp_amo<-as.numeric(as.character(gsub(",","",gsub("\\$","",data$exp_amo))))
data$agg_amo<-as.numeric(as.character(gsub(",","",gsub("\\$","",data$agg_amo))))

#Change data to lower case for string searching
dl<-data.frame(lapply(data,function(x) if("factor" %in% class(x)) tolower(x) else x))

# Subset some of the data of interest
pres.data <- dl[grep("obama|romne", dl$can_nam), ]
pres.data <- subset(pres.data, exp_dat >= "2012-04-25")

pres.data$bucket <- rep(NA, nrow(pres.data))

createBucket <- function(text, bucket = rep(NA, nrow(pres.data)), entryText = text) {
  empty <- (sum(is.na(bucket)) / nrow(pres.data) == 1)
  if (empty) {
    bucket[grep(text, as.character(pres.data$pur))] <- entryText
  } else {
    text.grep <- grep(text, as.character(pres.data$pur))
    bucket[text.grep] <- ifelse(is.na(bucket[text.grep]), entryText, bucket[text.grep])
  }
  return(bucket)
}
bucket <- createBucket("tv|television|production|video", entryText = "tv")
bucket <- createBucket("web|internet|online|digital", bucket, "web")
bucket <- createBucket("radio", bucket)
bucket <- createBucket("salar", bucket, "salary")
bucket <- createBucket("email|e\\-mail", bucket, "email")
bucket <- createBucket("call|phone|cell", bucket, "call")
bucket <- createBucket("travel|lodging|diem|travel|hotel|airport|flight", bucket, "travel")
bucket <- createBucket("flyer|flier|palm cards", bucket, "flyer")
bucket <- createBucket("mail", bucket)
bucket <- createBucket("^(sign)|banner|billboard", bucket, "sign")
bucket <- createBucket("design", bucket)
bucket <- createBucket("yard|sign", bucket, "sign")
bucket <- createBucket("vehicle|car|truck|van|bus|gas|transport|mileage", bucket, "transport")
bucket <- createBucket("canvas", bucket, "canvass")
bucket <- createBucket("media", bucket, "media")
bucket <- createBucket("printing", bucket, "printing")
bucket <- createBucket("shirts|button|hat|bumper", bucket, "swag")
bucket[bucket == "design"] <- NA
bucket[is.na(bucket)]<-"other"

pres.data$bucket <- bucket

pres.data$bucket2 <- pres.data$bucket
pres.data[pres.data$bucket2 %in% c("tv", "radio", "media", "web"), "bucket2"] <- "ad"
pres.data[pres.data$bucket2 %in% c("sign", "flyer", "printing", "swag"), "bucket2"] <- "swag"
pres.data[pres.data$bucket2 %in% c("travel", "transport"), "bucket2"] <- "transport"
pres.data[pres.data$bucket2 %in% c("email", "call", "mail", "canvass"), "bucket2"] <- "direct contact"


pres.data$can_nam<-factor(pres.data$can_nam)
obama.flag <- rep(0, nrow(pres.data))
obama.flag[grep("obama", pres.data$can_nam)] <- 1
pres.data$oflag<-factor(obama.flag)

#There is one record without sup_opp, we looked up
#the PAC, and they are conservative, we filled out
#sup_opp accordingly (oppose Obama)
pres.data[pres.data$sup_opp==" ","sup_opp"]<-"oppose"

pres.data$beneful_can <- rep(0, nrow(pres.data))
pres.data$beneful_can[pres.data$oflag==1 & pres.data$sup_opp=="support"] <- "obama"
pres.data$beneful_can[pres.data$oflag==0 & pres.data$sup_opp=="oppose"] <- "obama"
pres.data$beneful_can[pres.data$oflag==0 & pres.data$sup_opp=="support"] <- "romney"
pres.data$beneful_can[pres.data$oflag==1 & pres.data$sup_opp=="oppose"] <- "romney"
pres.data$beneful_can<-factor(pres.data$beneful_can)
summary(pres.data$beneful_can)

#get rid of empty levels
pres.data$sup_opp <- factor(pres.data$sup_opp)

num.weeks <- ddply(pres.data, .(week = factor(week(exp_dat)), beneful_can, bucket2, sup_opp),.drop=FALSE, summarise, WeeklySum = sum(exp_amo))
num.weeks[num.weeks$WeeklySum == 0,"WeeklySum"] = NA

#There is one spe with duplicate names (entry error). This is remedied:
levels(pres.data$spe_nam)[grep("united food",levels(pres.data$spe_nam))]<-"united food and commerical workers international union"
pres.data$spe_nam<-factor(pres.data$spe_nam)


#There are many pacs that are "benefitting" both candidates. This is probably
#due to people who file the paperwork not understanding the support/oppose 
#structure or just data entry errors.

test<-pres.data[,c("spe_nam","sup_opp","oflag","beneful_can")]
count_pac_can<-ddply(test,.(spe_nam,beneful_can),"nrow"); names(count_pac_can)[1]<-"spe_nam"

length(unique(count_pac_can$spe_nam))
#There are 77 unique pacs in this dataset, but 82 rows in count_pac_can => there are 5
#pacs that have mistakes. which ones?
prob_spe<-count_pac_can$spe_nam[duplicated(count_pac_can$spe_nam)]
prob_count<-count_pac_can[count_pac_can$spe_nam %in% prob_spe,]

#These pacs have names that show we are right in thinking these are mistakes. 
#Let's assume that it is a mistake and change beneful_can appropriately.

for(i in 1:(dim(prob_count)[1] - 1)) {
  if(prob_count$spe_nam[i] == prob_count$spe_nam[i+1]) {
    pres.data[pres.data$spe_nam == prob_count$spe_nam[i],"beneful_can"] <- ifelse(prob_count$nrow[i] < prob_count$nrow[i+1],as.character(prob_count$beneful_can[i+1]),as.character(prob_count$beneful_can[i]))
  }
}
pres.data$beneful_can<-factor(pres.data$beneful_can)

test<-pres.data[,c("spe_nam","sup_opp","oflag","beneful_can")]
count_pac_can<-ddply(test,.(spe_nam,beneful_can),"nrow"); names(count_pac_can)[1]<-"spe_nam"

# The most common expenses
count_exp<-table(pres.data$bucket,pres.data$beneful_can)

numBuckets <- length(unique(pres.data$bucket))

# The most spent on expenses
df<-pres.data[,c("exp_amo", "beneful_can", "bucket")]
sum_exp <- dcast(melt(df,id=c("beneful_can", "bucket")), df$bucket ~ df$beneful_can, sum)
sum_exp$both<-sum_exp$obama + sum_exp$romney

## For the plot
sum_exp_p <- ddply(df, .(bucket, beneful_can), summarise, Sum = sum(exp_amo))

#What buckets do we want to look at first?
sum_exp[with(sum_exp, order(-both)), ]

###
# High Level Buckets
###
# The most spent on expenses
df2<-pres.data[,c("exp_amo", "beneful_can", "bucket2")]
sum_exp2 <- dcast(melt(df2,id=c("beneful_can", "bucket2")), df2$bucket2 ~ df2$beneful_can, sum)
sum_exp2$both<-sum_exp2$obama + sum_exp2$romney

## For the plot
sum_exp2_p <- ddply(df2, .(bucket2, beneful_can), summarise, Sum = sum(exp_amo))

#What buckets do we want to look at first?
sum_exp2[with(sum_exp2, order(-both)), ]

###
# STATS by State
###
# Problem...


###
# STATS by PAC
###
# Grab only ones that exist
sum_exp_spe <- ddply(pres.data, .(spe_nam), summarise, Sum = sum(exp_amo))
sum_exp_spe <- sum_exp_spe[with(sum_exp_spe, order(-Sum)),]
x <- as.character(sum_exp_spe$spe_nam[1:10])
x[11] <- "Other Obama Super PACs"
x[12] <- "Other Romney Super PACs"

not.top10 <- count_pac_can[with(count_pac_can, order(-nrow)), ][11:nrow(count_pac_can), ]
obama.nottop <- subset(not.top10, beneful_can == "obama")
romney.nottop <- subset(not.top10, beneful_can == "romney")
## NOTE: The top 5 superpacs account for 9149 / 10686 entries!

pres.spe <- subset(pres.data, spe_nam %in% x)
obama.spe <- subset(pres.data, spe_nam %in% as.character(obama.nottop$spe_nam))
obama.spe$spe_nam <- "Other Obama Super PACs"
romney.spe <- subset(pres.data, spe_nam %in% as.character(romney.nottop$spe_nam))
romney.spe$spe_nam <- "Other Romney Super PACs"

pres.spe <- rbind(pres.spe, obama.spe, romney.spe)

df.spe<-pres.spe[,c("exp_amo", "beneful_can", "spe_nam")]
sum_exp.spe <- dcast(melt(df.spe,id=c("beneful_can", "spe_nam")), df.spe$spe_nam ~ df.spe$beneful_can, sum)

## For the plot
sum_exp.spep <- ddply(df.spe, .(spe_nam, beneful_can), summarise, Sum = sum(exp_amo))
sum_exp.spep <- sum_exp.spep[with(sum_exp.spep, order(beneful_can, -Sum)), ]
sum_exp.spep$spe_nam <- factor(sum_exp.spep$spe_nam, levels = sum_exp.spep$spe_nam)

# What buckets do we want to look at first?
sum_exp.spe[with(sum_exp.spe, order(-(obama + romney))), ]


### Let's play with the polling data
# Source in Di's code
source("NationalPollingParse-Di.R")

polls.sub2 <- subset(polls.sub[,c(1:2, 5:6)], Date > as.Date("2012-04-28"))
polls.subswing <- subset(polls.sub2, Region %in% c("National", "Colorado", "Florida", "Iowa", "Michigan", "Nevada", "New Hampshire", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "Wisconsin"))

polls.subswing$isNational <- (polls.subswing$Region == "National")


polls.sub3 <- subset(polls.sub[,c(1:6)], Date > as.Date("2012-04-28"))
polls.subswing3 <- subset(polls.sub3, Region %in% c("National", "Colorado", "Florida", "Iowa", "Michigan", "Nevada", "New Hampshire", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "Wisconsin"))

polls.subswing3$isNational <- (polls.subswing$Region == "National")


num.weeks.sum <- ddply(subset(num.weeks, week > 17), .(week, beneful_can), summarise, sum = sum(WeeklySum, na.rm = TRUE))
num.weeks.sum$Date <- as.Date("2012-04-28") + (7 * (as.numeric(num.weeks.sum$week) - week(as.Date("2012-04-28"))))

polls.week <- ddply(polls.subswing, .(week = factor(week(Date)), isNational), summarise, Obama.Romney.Avg = mean(Obama.Romney))
polls.week3 <- ddply(subset(polls.subswing3, !isNational), .(week = factor(week(Date)), isNational), summarise, Obama.Romney.Avg = mean(Obama.Romney), Obama = mean(Obama), Romney = mean(Romney))
polls.week3$week <- as.numeric(as.character(polls.week3$week))
names(polls.week3)[4] <- "Obama.Poll"
names(polls.week3)[5] <- "Romney.Poll"

unmelted.polls <- dcast(polls.week, week ~ isNational)
names(unmelted.polls) <- c("weeknum", "Swing", "National")

unmelted.sum <- dcast(num.weeks.sum, week+Date ~ beneful_can, value.var = "sum")
unmelted.sum$obama.romney <- unmelted.sum$obama - unmelted.sum$romney
final.df <- cbind(subset(unmelted.sum[,-c(3,4)], unmelted.sum$week >= 17 & unmelted.sum$week <= week(today())), subset(unmelted.polls[,-1], unmelted.sum$week >= 17 & unmelted.sum$week <= week(today())))
final.df3 <- cbind(subset(unmelted.sum, unmelted.sum$week >= 18 & unmelted.sum$week <= week(today())), subset(polls.week3[,-1], polls.week3$week >= 18 & polls.week3$week <= week(today())))
names(final.df3)[1] <- "weeknum"

wtf.sub <- subset(pres.data, exp_dat >= as.Date("2012-07-11") & exp_dat <= as.Date("2012-07-25"))
wtf.sub <- wtf.sub[with(wtf.sub, order(-exp_amo)), ]

wtf.sub2<-wtf.sub[,c("exp_amo", "beneful_can", "bucket2")]
wtf_sum <- dcast(melt(wtf.sub2,id=c("beneful_can", "bucket2")), wtf.sub2$bucket2 ~ wtf.sub2$beneful_can, sum)
wtf_sum$both<-wtf_sum$obama + wtf_sum$romney

## For the plot
wtf_wtf <- ddply(wtf_sub2, .(bucket2, beneful_can), summarise, Sum = sum(exp_amo))

twoweeksum <- subset(wtf_wtf, bucket2 == "ad" & beneful_can == "romney")$Sum
overallsum <- subset(sum_exp2_p, bucket2 == "ad" & beneful_can == "romney")$Sum

twoweeksum / overallsum
# vs...
2 / (as.numeric(max(num.weeks$week)) - as.numeric(min(num.weeks$week)))

pres.datam <- pres.data[,c("beneful_can", "exp_amo", "exp_dat")]
pres.datamp <- ddply(pres.datam, .(exp_dat, beneful_can), summarise, day_amo = sum(exp_amo))
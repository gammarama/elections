### Andee Kaplan + Eric Hare
### STAT 579
### Final Project

library(lubridate)
library(XML)
suppressWarnings(library(ggplot2))
library(plyr)
library(reshape2)
library(RColorBrewer)

createBucket <- function(df, text, bucket, entryText = text) {
    empty <- (all(is.na(bucket)))
    if (empty) {
        bucket[grep(text, as.character(df$pur))] <- entryText
    } else {
        text.grep <- grep(text, as.character(df$pur))
        bucket[text.grep] <- ifelse(is.na(bucket[text.grep]), entryText, bucket[text.grep])
    }
    return(bucket)
}

# This function accepts a year and returns the indepedent expenditures for that year
getExpenditures <- function(year) {
    data <- read.csv(paste("http://www.fec.gov/data/IndependentExpenditure.do?format=csv&election_yr=", year, sep = ""))
    
    # Format the dates and expenditure amounts
    data$rec_dat <- as.Date(data$rec_dat,format='%m/%d/%Y')
    data$exp_dat <- as.Date(data$exp_dat,format='%m/%d/%Y')
    
    data$exp_amo <- as.numeric(as.character(gsub(",","",gsub("\\$","",data$exp_amo))))
    data$agg_amo <- as.numeric(as.character(gsub(",","",gsub("\\$","",data$agg_amo))))
    
    dl<-data.frame(lapply(data,function(x) if("factor" %in% class(x)) tolower(x) else x))
    
    spend.data <- dl[grep("obama|romne", dl$can_nam), ]
    spend.data <- subset(spend.data, exp_dat >= "2012-04-25")
    
    spend.data$bucket <- rep(NA, nrow(spend.data))
    
    
    
    bucket <- createBucket(spend.data, "tv|television|production|video", rep(NA, nrow(spend.data)), entryText = "tv")
    bucket <- createBucket(spend.data, "web|internet|online|digital", bucket, "web")
    bucket <- createBucket(spend.data, "radio", bucket)
    bucket <- createBucket(spend.data, "salar", bucket, "salary")
    bucket <- createBucket(spend.data, "email|e\\-mail", bucket, "email")
    bucket <- createBucket(spend.data, "call|phone|cell", bucket, "call")
    bucket <- createBucket(spend.data, "travel|lodging|diem|travel|hotel|airport|flight", bucket, "travel")
    bucket <- createBucket(spend.data, "flyer|flier|palm cards", bucket, "flyer")
    bucket <- createBucket(spend.data, "mail", bucket)
    bucket <- createBucket(spend.data, "^(sign)|banner|billboard", bucket, "sign")
    bucket <- createBucket(spend.data, "design", bucket)
    bucket <- createBucket(spend.data, "yard|sign", bucket, "sign")
    bucket <- createBucket(spend.data, "vehicle|car|truck|van|bus|gas|transport|mileage", bucket, "transport")
    bucket <- createBucket(spend.data, "canvas", bucket, "canvass")
    bucket <- createBucket(spend.data, "media", bucket, "media")
    bucket <- createBucket(spend.data, "printing", bucket, "printing")
    bucket <- createBucket(spend.data, "shirts|button|hat|bumper", bucket, "swag")
    bucket[bucket == "design"] <- NA
    bucket[is.na(bucket)]<-"other"
    
    spend.data$bucket <- bucket
    
    spend.data$bucket2 <- spend.data$bucket
    spend.data[spend.data$bucket2 %in% c("tv", "radio", "media", "web"), "bucket2"] <- "ad"
    spend.data[spend.data$bucket2 %in% c("sign", "flyer", "printing", "swag"), "bucket2"] <- "swag"
    spend.data[spend.data$bucket2 %in% c("travel", "transport"), "bucket2"] <- "transport"
    spend.data[spend.data$bucket2 %in% c("email", "call", "mail", "canvass"), "bucket2"] <- "direct contact"
    
    
    spend.data$can_nam<-factor(spend.data$can_nam)
    obama.flag <- rep(0, nrow(spend.data))
    obama.flag[grep("obama", spend.data$can_nam)] <- 1
    spend.data$oflag<-factor(obama.flag)
    
    #There is one record without sup_opp, we looked up
    #the PAC, and they are conservative, we filled out
    #sup_opp accordingly (oppose Obama)
    spend.data[spend.data$sup_opp==" ","sup_opp"]<-"oppose"
    
    spend.data$beneful_can <- rep(0, nrow(spend.data))
    spend.data$beneful_can[spend.data$oflag==1 & spend.data$sup_opp=="support"] <- "obama"
    spend.data$beneful_can[spend.data$oflag==0 & spend.data$sup_opp=="oppose"] <- "obama"
    spend.data$beneful_can[spend.data$oflag==0 & spend.data$sup_opp=="support"] <- "romney"
    spend.data$beneful_can[spend.data$oflag==1 & spend.data$sup_opp=="oppose"] <- "romney"
    spend.data$beneful_can<-factor(spend.data$beneful_can)
    summary(spend.data$beneful_can)
    
    #get rid of empty levels
    spend.data$sup_opp <- factor(spend.data$sup_opp)
    
    #There is one spe with duplicate names (entry error). This is remedied:
    levels(spend.data$spe_nam)[grep("united food",levels(spend.data$spe_nam))]<-"united food and commerical workers international union"
    spend.data$spe_nam<-factor(spend.data$spe_nam)
    
    
    
    test<-spend.data[,c("spe_nam","sup_opp","oflag","beneful_can")]
    count_pac_can<-ddply(test,.(spe_nam,beneful_can),"nrow"); names(count_pac_can)[1]<-"spe_nam"
    
    
    #There are 77 unique pacs in this dataset, but 82 rows in count_pac_can => there are 5
    #pacs that have mistakes. which ones?
    prob_spe<-count_pac_can$spe_nam[duplicated(count_pac_can$spe_nam)]
    prob_count<-count_pac_can[count_pac_can$spe_nam %in% prob_spe,]
    
    #These pacs have names that show we are right in thinking these are mistakes. 
    #Let's assume that it is a mistake and change beneful_can appropriately.
    
    for(i in 1:(dim(prob_count)[1] - 1)) {
        if(prob_count$spe_nam[i] == prob_count$spe_nam[i+1]) {
            spend.data[spend.data$spe_nam == prob_count$spe_nam[i],"beneful_can"] <- ifelse(prob_count$nrow[i] < prob_count$nrow[i+1],as.character(prob_count$beneful_can[i+1]),as.character(prob_count$beneful_can[i]))
        }
    }
    
    spend.data$beneful_can<-factor(spend.data$beneful_can)
    
    return(spend.data)
}

spend.data <- getExpenditures("2012")

num.weeks <- ddply(spend.data, .(week = factor(week(exp_dat)), beneful_can, bucket2, sup_opp),.drop=FALSE, summarise, WeeklySum = sum(exp_amo))
num.weeks[num.weeks$WeeklySum == 0,"WeeklySum"] = NA
num.weeks$date <- as.Date("1/1/2012",format="%d/%m/%Y") + weeks(as.numeric(num.weeks$week))


df2<-spend.data[,c("exp_amo", "beneful_can", "bucket2")]
sum_exp2 <- dcast(melt(df2,id=c("beneful_can", "bucket2")), df2$bucket2 ~ df2$beneful_can, sum)
sum_exp2$both<-sum_exp2$obama + sum_exp2$romney
sum_exp2_p <- ddply(df2, .(bucket2, beneful_can), summarise, Sum = sum(exp_amo))


sum_exp_spe <- ddply(spend.data, .(spe_nam), summarise, Sum = sum(exp_amo))
sum_exp_spe <- sum_exp_spe[with(sum_exp_spe, order(-Sum)),]
x <- as.character(sum_exp_spe$spe_nam[1:10])
x[11] <- "Other Obama Super PACs"
x[12] <- "Other Romney Super PACs"

test<-spend.data[,c("spe_nam","sup_opp","oflag","beneful_can")]
count_pac_can<-ddply(test,.(spe_nam,beneful_can),"nrow"); names(count_pac_can)[1]<-"spe_nam"

not.top10 <- count_pac_can[with(count_pac_can, order(-nrow)), ][11:nrow(count_pac_can), ]
obama.nottop <- subset(not.top10, beneful_can == "obama")
romney.nottop <- subset(not.top10, beneful_can == "romney")
## NOTE: The top 5 superpacs account for 9149 / 10686 entries!

pres.spe <- subset(spend.data, spe_nam %in% x)
obama.spe <- subset(spend.data, spe_nam %in% as.character(obama.nottop$spe_nam))
obama.spe$spe_nam <- "Other Obama Super PACs"
romney.spe <- subset(spend.data, spe_nam %in% as.character(romney.nottop$spe_nam))
romney.spe$spe_nam <- "Other Romney Super PACs"

pres.spe <- rbind(pres.spe, obama.spe, romney.spe)

df.spe<-pres.spe[,c("exp_amo", "beneful_can", "spe_nam")]
sum_exp.spe <- dcast(melt(df.spe,id=c("beneful_can", "spe_nam")), df.spe$spe_nam ~ df.spe$beneful_can, sum)

sum_exp.spep <- ddply(df.spe, .(spe_nam, beneful_can), summarise, Sum = sum(exp_amo))
sum_exp.spep <- sum_exp.spep[with(sum_exp.spep, order(beneful_can, -Sum)), ]
sum_exp.spep$spe_nam <- factor(sum_exp.spep$spe_nam, levels = sum_exp.spep$spe_nam)

red <- rev(brewer.pal(9, "Reds"))
blue <- rev(brewer.pal(3, "Blues"))


num.weeks.sum <- ddply(subset(num.weeks, week > 17), .(week, beneful_can), summarise, sum = sum(WeeklySum, na.rm = TRUE))
num.weeks.sum$Date <- as.Date("2012-04-28") + (7 * (as.numeric(num.weeks.sum$week) - week(as.Date("2012-04-28"))))
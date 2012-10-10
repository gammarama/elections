# Credit: http://www.r-bloggers.com/building-a-better-word-cloud/

# Load libraries and data
library(tm)
library(ggplot2)

source("SPEDataCleanup.R")

### Step 1: Load in text data, clean, and analyze overlapping terms
write(c("<TEXT>", "<BODY>", as.character(unique(pres.data[pres.data$beneful_can=="obama",]$spe_nam)), "</BODY>", "</TEXT>", "\n"),
          paste(getwd(), "\\Corpus\\obama.xml",sep=""))
write(c("<TEXT>", "<BODY>", as.character(unique(pres.data[pres.data$beneful_can=="romney",]$spe_nam)), "</BODY>", "</TEXT>", "\n"),
          paste(getwd(), "\\Corpus\\romney.xml",sep=""))




pacs<-Corpus(DirSource(paste(getwd(), "\\Corpus\\",sep=""))) 
# Get word counts
obama.wc<-length(unlist(strsplit(pacs[[1]], " ")))
romney.wc<-length(unlist(strsplit(pacs[[2]], " ")))

# Create a Term-Document matrix
add.stops=c("the","political","action","committee","pac","fund")
pacs.control=list(stopwords=c(stopwords(),add.stops), removeNumbers=TRUE, removePunctuation=TRUE)
pacs.matrix<-TermDocumentMatrix(pacs, control=pacs.control)

# Create data frame from matrix
pacs.df<-as.data.frame(inspect(pacs.matrix))
pacs.df<-subset(pacs.df, obama.xml>0 & romney.xml>0)
pacs.df<-transform(pacs.df, freq.dif=obama.xml-romney.xml)    

### Step 2: Create values for even y-axis spacing for each vertical
#           grouping of word frequencies

# Create separate data frames for each frequency type
obama.df<-subset(pacs.df, freq.dif>0)   # Said more often by Obama
romney.df<-subset(pacs.df, freq.dif<0)   # Said more often by Romney
equal.df<-subset(pacs.df, freq.dif==0)  # Said equally

# This function takes some number as spaces and returns a vertor
# of continuous values for even spacing centered around zero
optimal.spacing<-function(spaces) {
  if(spaces>1) {
    spacing<-1/spaces
    if(spaces%%2 > 0) {
      lim<-spacing*floor(spaces/2)
      return(seq(-lim,lim,spacing))
    }
    else {
      lim<-spacing*(spaces-1)
      return(seq(-lim,lim,spacing*2))
    }
  }
  else {
    return(0)
  }
}

# Get spacing for each frequency type
obama.spacing<-sapply(table(obama.df$freq.dif), function(x) optimal.spacing(x))
romney.spacing<-sapply(table(romney.df$freq.dif), function(x) optimal.spacing(x))
equal.spacing<-sapply(table(equal.df$freq.dif), function(x) optimal.spacing(x))

# Add spacing to data frames
obama.optim<-rep(0,nrow(obama.df))
for(n in names(obama.spacing)) {
  obama.optim[which(obama.df$freq.dif==as.numeric(n))]<-obama.spacing[[n]]
}
obama.df<-transform(obama.df, Spacing=obama.optim)

romney.optim<-rep(0,nrow(romney.df))
for(n in names(romney.spacing)) {
  romney.optim[which(romney.df$freq.dif==as.numeric(n))]<-romney.spacing[[n]]
}
romney.df<-transform(romney.df, Spacing=romney.optim)

equal.df$Spacing<-as.vector(equal.spacing)

### Step 3: Create visualization
ggplot(obama.df, aes(x=freq.dif, y=Spacing))+geom_text(aes(size=obama.xml, label=row.names(obama.df), colour=freq.dif))+
  geom_text(data=romney.df, aes(x=freq.dif, y=Spacing, label=row.names(romney.df), size=romney.xml, color=freq.dif))+
  geom_text(data=equal.df, aes(x=freq.dif, y=Spacing, label=row.names(equal.df), size=obama.xml, color=freq.dif))+
  scale_size(range=c(3,9),name="Word Frequency")+scale_colour_gradient(low="darkred", high="darkblue", guide="none")+
  scale_x_continuous(breaks=c(min(romney.df$freq.dif),0,max(obama.df$freq.dif)),labels=c("Said More by Romney","Said Equally","Said More by Obama"))+
  scale_y_continuous(breaks=c(0),labels=c(""))+xlab("")+ylab("")+theme_bw()+
  opts(panel.grid.major=theme_blank(),panel.grid.minor=theme_blank(), title="Super PAC Names (Obama vs. Romney)")
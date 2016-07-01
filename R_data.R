##-----------------------------------##################
#                      #
############################################################

#       format(index(first(AAPL)),"%Y")


library(quantmod)
library(xts)
symb<-c("AMARAJABAT.NS","APOLLOTYRE.NS","BLUEDART.NS")#,"MOTHERSUMI.NS","HCLTECH.NS","SUZLON.NS","MOREPENLAB.NS","WELSPUNIND.NS") 
# list of symblols
for(i in head(symb, length(symb))){ 
  status <- tryCatch(getSymbols(i, src = "yahoo",
                                from = as.Date("2007-01-01")),
                     error = identity)
  if(inherits(status, "error"))
    cat("Symbol '", i, "' not downloadable!\n", sep = "")
}


# merge them all together
nsedata <- data.frame(as.xts(merge(AMARAJABAT.NS,APOLLOTYRE.NS,BLUEDART.NS))) # need to add remaning stocks

# set outcome variable
outcomeSymbol <- 'AMARAJABAT.NS.Volume'
# shift outcome value to be on same line as predictors
nsedata <- xts(nsedata,order.by=as.Date(rownames(nsedata)))
summary(nsedata) # to find NA  in data
nsedata <- as.data.frame(merge(nsedata, lm1=lag(nsedata[,outcomeSymbol],-1)))
nsedata<-na.aggregate(nsedata)
nsedata$outcome <- ifelse(nsedata[,paste0(outcomeSymbol,'.1')] > nsedata[,outcomeSymbol], 1, 0)

# remove shifted down volume field as we don't care by the value
nsedata <- nsedata[,!names(nsedata) %in% c(paste0(outcomeSymbol,'.1'))]

# cast date to true date and order in decreasing order
nsedata$date <- as.Date(row.names(nsedata))
nsedata <- nsedata[order(as.Date(nsedata$date, "%m/%d/%Y"), decreasing = TRUE),]



# calculate all day differences and populate them on same row
GetDiffDays <- function(objDF,days=c(10), offLimitsSymbols=c('outcome'), roundByScaler=3) {
  # needs to be sorted by date in decreasing order
  ind <- sapply(objDF, is.numeric)
  for (sym in names(objDF)[ind]) {
    if (!sym %in% offLimitsSymbols) {
      print(paste('*********', sym))
      objDF[,sym] <- round(scale(objDF[,sym]),roundByScaler)
      
      print(paste('theColName', sym))
      for (day in days) {
        objDF[paste0(sym,'_',day)] <- c(diff(objDF[,sym],lag = day),rep(x=0,day)) * -1
      }
    }
  }
  return (objDF)
}

# call the function with the following differences
nsedata <- GetDiffDays(nsedata, days=c(1,2,3,4,5,10,20), offLimitsSymbols=c('outcome'), roundByScaler=2)

# drop most recent entry as we don't have an outcome
nsedata <- nsedata[2:nrow(nsedata),]

# take a peek at YHOO features:
dput(names(nsedata)[grepl('AMARAJABAT.NS.',names(nsedata))])

# well use POSIXlt to add day of the week, day of the month, day of the year
nsedata$wday <- as.POSIXlt(nsedata$date)$wday
nsedata$yday <- as.POSIXlt(nsedata$date)$mday
nsedata$mon<- as.POSIXlt(nsedata$date)$mon

# remove date field and shuffle data frame
nsedata <- subset(nsedata, select=-c(date))
nsedata <- nsedata[sample(nrow(nsedata)),]


# let's model
library(xgboost)
predictorNames <- names(nsedata)[names(nsedata) != 'outcome']

set.seed(1234)
split <- sample(nrow(nsedata), floor(0.7*nrow(nsedata)))
train <-nsedata[split,]
test <- nsedata[-split,]

bst <- xgboost(data = as.matrix(train[,predictorNames]),
               label = train$outcome,
               verbose=0,
               eta = 0.1,
               gamma = 50, 
               nround = 50,
               colsample_bytree = 0.1,
               subsample = 8.6,
               objective="binary:logistic")

predictions <- predict(bst, as.matrix(test[,predictorNames]), outputmargin=TRUE)

library(pROC)
auc <- roc(test$outcome, predictions) # this value should be .5 to 1(best model)
print(paste('AUC score:', auc$auc))

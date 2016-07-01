#-------------------------------------get data from CSV files
# get desired data from NSE website @ duartion to CSV files
data_Amar<-read.csv("/Users/administrator/Desktop/ML/zzz_Markt/03-04-2014-TO-01-04-2016AMARAJABATALLN.csv")
head(data_Amar)
tail(data_Amar)
sumary (data_Amar)

plot_ly(data = data_Amar, x = data_Amar$Date, y = data_Amar$High.Price, mode = "line")
add_trace(y = data_Amar$Low.Price, x = data_Amar$Date) 
plot(as.Date(data_Amar$Date, "%d-%b-%y"), data_Amar$Close.Price, xlab = "Dates", ylab = "Closing price", type = "l", col = "red", main = "Closing price of AMARAJA")

##----------------------------------------------Metals data from quantmod
getMetals(c('XAU', 'XAG'), from=Sys.Date()-365)
silver = data.frame(XAGUSD)
silver$date = as.Date(rownames(silver))
colnames(silver)[1] = 'price'

ggplot(data=silver, aes(x=date, y=price)) + geom_line() + theme_bw()

##--------------------------------------++++++----Getting DATA
## for looping 
library(quantmod)
symb<-c("AMARAJABAT.NS","APOLLOTYRE.NS","BLUEDART.NS","MOTHERSUMI.NS","HCLTECH.NS","SUZLON.NS","MOREPENLAB.NS","WELSPUNIND.NS") 
# list of symblols
for(i in head(symb, length(symb))){ 
  status <- tryCatch(getSymbols(i, src = "yahoo",
                        from = as.Date("2007-01-01")),
              error = identity)
  if(inherits(status, "error"))
    cat("Symbol '", i, "' not downloadable!\n", sep = "")
}

#------------------------------merge close data to ingle xts
data_close<-Cl(merge(AMARAJABAT.NS,BLUEDART.NS,APOLLOTYRE.NS,HCLTECH.NS,MOREPENLAB.NS,MOTHERSUMI.NS))
anyNA(data_close)
#data_close<-na.aggregate(data_close) # if any NA are present in vector 
cl_max<-apply(data_close,2,max, na.rm=TRUE) # to find max of each colum
data_close<-apply(data_close,1,function(x) x[]/cl_max[]) 
data_close<-t(data_close)
#----------------------stacked plot by dygraph
library(dygraphs)
dygraph(data_close, ylab="Close_PU", main="Close Data normalized to each peak value") %>%
dyRangeSelector()

#----------------------Plot by Plotly 
library(plotly)
data_close<-data.frame(data_close)
data_close$date<-rownames(data_close)
plot_ly(data = data_close, x = data_close$date, y = data_close[,1], mode = "line")
 add_trace(y = data_close[,2],x = data_close$date)
 add_trace(y = data_close[,3],x = data_close$date)
 
 
##--------------------------------to extract colum from the Xts object
index(AMARAJABAT.NS)
dim(AMARAJABAT.NS)   #dimension of te array matrix
Hi(AMARAJABAT.NS)
Lo(AMARAJABAT.NS)
Cl(AMARAJABAT.NS)
OHLC(AMARAJABAT.NS)
colnames(AMARAJABAT.NS)
x<-AMARAJABAT.NS[,"AMARAJABAT.NS.Open"]
##-----------------------------------Charting  select any below one 
candleChart(AMARAJABAT.NS,subset='2015-12::2016',theme='white', type='candles')
lineChart(AMARAJABAT.NS)
barChart(AMARAJABAT.NS)
chartSeries(AMARAJABAT.NS)
chartSeries(c(AMARAJABAT.NS, HCLTECH.NS), subset='last 3 months')
zoomChart("2016-2")
addTA(OpCl(AMARAJABAT.NS))  # adding trace to the chart
##--------------------------------Adding performance computing 
addMACD()      
addSMA()
addBBands(n=20, sd=2) #addBBands(n = 20, sd = 2, ma = "SMA", draw = "bands", on = -1)
# We can add the Bollinger Bands to a plot by using the 
# command: addBBands(n = 20, sd = 2, ma = "SMA", draw = "bands", on = -1)
# where n denotes the number of moving average periods, sd the number of standard deviations and ma the used moving average process.
# -------------------------------------Using Quantmod pakage    #get data from 2007 onwards

z=paste0("AMARAJABAT",".NS") #--------------------Create symbol 
getSymbols(z, src="yahoo")

getSymbols("HDFCBANK.BO", src="yahoo")
getSymbols("AMARAJABAT.NS", src="yahoo")  
getSymbols("BALLARPUR.BO",src="yahoo")
getSymbols("INNOVTEC.BO",src="yahoo")
# GS-> is symbol in work envirment 
GS['2008'] #returns only that year 
GS['2008-01'] #now just January of 2008 
GS['2007-06::2008-01-12'] #Jun of 07 through Jan 12 of 08
GS['::'] # everything in GS 
GS['2008::'] # everything in GS, from 2008 onward 
GS['/2008']  # begning of the data through 2008
non.contiguous <- c('2007-01','2007-02','2007-12') 
GS[non.contiguous]

AMARAJABAT.NS[endpoints(AMARAJABAT.NS, on ="months")] # extract mothly data


#--------------------------------------------Return computation 
vix.returns <- allReturns(AMARAJABAT.NS)
vix.returns.df <- data.frame(date=time(vix.returns),coredata(vix.returns$daily))


#-----------------------------------Linear regression on xts objects
SYM=getSymbols("SUZLON.NS")
DAT<-SUZLON.NS
dt<-"2016"
df <- data.frame(Cl(DAT[dt]),Time=index(DAT[dt]))
head(df)
tail(df)
m <- lm(df[,1] ~ Time, data = df)
plot(df[,2],df[,1])
abline(m)
# plot_ly(data=df,x=Time,y=df[,1])
lines(m$fitted.values ~ index(DAT[dt]))
lines(m$residuals ~ index(DAT[dt]),type="l",col="red")
detrendPrice <- m$residuals
# plot(xts(detrendPrice/mean(DAT[dt]),index(DAT[dt])),type="l")
plot(xts(detrendPrice,index(DAT[dt])),type="l")
# plot(m$fitted.values+detrendPrice-df[,1])


fft_dtrend<-fft(detrendPrice) #--------------------------FFT of the dtrend
barplot(abs(fft_dtrend[1:150])*2/length(detrendPrice))
#--------------------------------------
# Market Microstructure Backtesting Code
#--------------------------------------
is.installed <- function(pkg){ is.element(pkg, installed.packages()[,1])}
#Check if package var has been installed
#if(!is.installed('strucchange')){ install.packages("strucchange",repos="http://lib.stat.cmu.edu/R/CRAN")}
#Calling the Package for Testing, Monitoring, Dating Structural Changes
#library("strucchange")
library("futile.logger")
mripPath<-getwd()
flog.threshold(INFO, name="info_consume")
flog.appender(appender.file(paste(mripPath,"/Microstructure1_info.log",sep="")), name="info_consume")
flog.info("R libraries loaded succesfully", name="info_consume")
# Defining Global Variables#
symbolVec <- c('JPM')
newbid<-0
newask<-0
prevna<-0
prevnb<-0
nac<-0
nbc<-0
askPrice<-0
ask_old<-0
bidPrice<-0
bid_old<-0
min<-0
min_old<-0
PL<-0
cnt<-0
position<-0
gamma<-0.01
t<-0
buysize10<-1
firstCall <- TRUE
microMarket<-function (tradeTime,bidSize,askSize, bidPrice,askPrice,timChk,tick){
  #bidPrice<-as.numeric(bidPrice)
  askPrice<-as.numeric(askPrice)
  #bidSize<-as.numeric(bidSize)
  #askSize<-as.numeric(askSize)
  tickerId<-as.character(tick)
  tradeTime<-as.character(tradeTime)
  timChk<-as.logical(timChk)
  #flog.info(paste("bidSize - ",bidSize," askSize - ",askSize,sep=""),name = "info_consume")
  flog.info(paste("Ticker Symbol - ",tickerId,sep=""),name = "info_consume")
  #flog.info(paste("TIME CHECK ---------------------------------------- ",timChk,sep=""), name="info_consume")
  flog.info(paste("bidPrice - ",bidPrice," askPrice - ",askPrice,sep=""),name = "info_consume")
  #flog.info(paste("TimeStamp - ",tradeTime,sep=""), name="info_consume")
  if (firstCall){
    ask_old<<-askPrice
    bid_old<<-bidPrice
    flog.info(paste("First Ask old is:",ask_old,sep=","),name = "info_consume")
    flog.info(paste("First Ask Price is:",askPrice,sep=","),name = "info_consume")
    flog.info(paste("First bid old is:",bid_old,sep=","),name = "info_consume")
    flog.info(paste("First bid Price is:",bidPrice,sep=","),name = "info_consume")
    min1<<-as.POSIXct(tradeTime,origin = "1970-01-01")
    min<-(as.numeric(format(min1,"%M")))
    flog.info(paste("min is:",min, sep=","),name = "info_consume")
    min_old<<-min
    firstCall <<- FALSE
    #return(paste("ask_old is",ask_old,"bid_old is:",bid_old,sep=","))
    
  }# End of First Call
  else{
    min2<<-as.POSIXct(tradeTime,origin = "1970-01-01") 
    min3<-(as.numeric(format(min2,"%M")))
    t<-difftime(min2,min1,unit="mins")
    flog.info(paste("Min is:",min,sep=","))
    flog.info(paste("Min_old is:",min_old,sep=","))
    t<-as.difftime(min,min_old,unit="mins")
    askPrice <<- askPrice
    bidPrice<<-bidPrice
    min<<-min3
    flog.info(paste("t is:",t, sep=","),name = "info_consume")
    
    if(timChk==TRUE) #Day is near to end
    {
      
      if(position > 0)
      {
        PL <<-(PL +(position*askPrice)) # PL increases as he buys
        flog.info(paste("Todays PNL is:",PL,sep=","),"info_consume") 
        flog.info(paste("BidPrice is:",bidPrice,sep=","),name = "info_consume")
        flog.info(paste("askPrice is:",askPrice,sep=","),name = "info_consume")
        position <<-0
        flog.info(paste("position is:",position,sep=","),name = "info_consume")
        #return(paste("PL is",PL,sep=","))
      }
      else (position < 0)
{
        PL<<- (PL -(position*bidPrice)) # PL decreases as he sells
        flog.info(PL)
        flog.info(paste("BidPrice is:",bidPrice,sep=","),name = "info_consume")
        flog.info(paste("askPrice is:",askPrice,sep=","),name = "info_consume")
        position <<-0
        flog.info(paste("position is:",position,sep=","),name = "info_consume")
        #return(paste("PL is",PL,sep=","))
      }
    }
else{
  
  if ((t<1)&&(min_old==min)){ # computing new ask & previous new ask counts 
    if(ask_old!=askPrice)
    {
      newask<<-newask+1
      flog.info(paste("prevna is:",prevna,sep=","),,name = "info_consume")
      flog.info(paste("newask is:",newask,sep=","),,name = "info_consume")
      flog.info(paste("BidPrice is:",bidPrice,sep=","),,name = "info_consume")
      flog.info(paste("askPrice is:",askPrice,sep=","),,name = "info_consume")
      flog.info(paste("ask_old is:",ask_old,sep=","),,name = "info_consume")
      flog.info(paste("bid_old is:",bid_old,sep=","),,name = "info_consume")
      #return(paste("newask is",newask,"prevna is:",prevna,sep=","))
    }
    else
    {    
      flog.info(paste("BidPrice is:",bidPrice,sep=","),name = "info_consume")
      flog.info(paste("askPrice is:",askPrice,sep=","),name = "info_consume")
      flog.info(paste("ask_old is:",ask_old,sep=","),name = "info_consume")
      flog.info(paste("bid_old is:",bid_old,sep=","),name = "info_consume")
      #return(paste("newask is",newask,"prevna is:",prevna,sep=","))
    }
  }
  else if(t>=1 || (min_old!=min))
    
  { 
    min1<<-min2
    nac<<-prevna
    prevna<<-newask
    newask<<-0 
    cnt<<-cnt+1
    flog.info(paste("cnt is:",cnt,sep=","),name = "info_consume")
    flog.info(paste("prevna is:",prevna,sep=","),name = "info_consume")
    flog.info(paste("newask is:",newask,sep=","),name = "info_consume")
    flog.info(paste("BidPrice is:",bidPrice,sep=","),name = "info_consume")
    flog.info(paste("askPrice is:",askPrice,sep=","),name = "info_consume")
    flog.info(paste("ask_old is:",ask_old,sep=","),name = "info_consume")
    flog.info(paste("bid_old is:",bid_old,sep=","),name = "info_consume")
    #return(paste("newask is",newask,"prevna is:",prevna,sep=","))
  }
  else {
    
    flog.info("Nothing to do")
    #return("ignore")
  }
  
  if ((t<1)&&(min_old==min)) {
    # computing new bid & previous new bid counts   
    if (bid_old!=bidPrice)
    {
      prevnb<<-prevnb
      newbid<<-newbid+1
      #return(paste("newbid is",newbid,"prevnb is:",prevnb,sep=","))
      
    }
    else {
      
      flog.info("Nothing to do")
      #return("ignore")
    }
  }
  
  else if(t>=1)
  {
    flog.info(paste("min2 is:",min2, sep=","))
    flog.info(paste("min1 is:",min1, sep=","))
    min1<<-min2
    nbc<<-prevnb
    prevnb<<-newbid
    newbid<<-0
    flog.info("30")
    #return(paste("newbid is",newbid,"prevnb is:",prevnb,sep=","))
  }
  
  else {
    flog.info("Nothing to do")
    #return("ignore")
  }
  
  if (cnt>=2)
  {
    if (((nbc-prevnb)!=0) && ((bid_old)!=0) && ((bidPrice)!=0))
      
    {
      var1<-abs(1-gamma*(((nbc)*(prevnb))/(nbc-prevnb)))
      var2<-log(var1)
      var3<-(1/gamma)*var1
      
      optbid <<-ifelse(is.nan(bidPrice-var3),0,(bidPrice-var3))
      flog.info(paste("optbid is:",optbid, sep=","))
      #return (paste("Optimal bid price is:",optbid,sep=","))
    }
    
    else
    {
      optbid<<-0
      flog.info(paste("optbid is:",optbid, sep=","))
      #return ("ignore")
    }
    
    if (((nac-prevna)!=0) && ((ask_old)!=0) && ((askPrice)!=0))
      
    {  
      var3<-abs(1-gamma*(((newask)*(prevna))/(newask-prevna)))
      var4<-log(var3)
      var5<-(1/gamma)*var4
      optask <<-ifelse(is.nan(askPrice-var5),0,(askPrice-var5))
      flog.info(paste("optask is:",optask, sep=","))
      #return (paste("Optimal ask price is:",optask,sep=","))
    }
    else
    {
      optask<<-0
      flog.info(paste("optask is:",optask, sep=","))
      #return ("ignore")
    }
    
    
    if(optbid> askPrice)
      
      
    {
      
      #action = 'B' or BUY
      PL<<- PL-askPrice
      position <<- (position +1)
      flog.info(paste("position is:",position,sep=","),name = "info_consume")
      ret = list("symbol" = "JPM",
                 "price" = askPrice,
                 "signal" = "BUY",
                 "size" = buysize10)
      
    }
    
    
    else if(optask < bidPrice)
    {
      
      #action = 'A'or SELL
      PL<<-PL+bidPrice
      position<<-position-1
      flog.info(PL)
      flog.info(paste("position is:",position,sep=","),name = "info_consume")
      #return (paste("PL is:",PL,sep=","))
      ret = list("symbol" = "JPM",
                 "price" = bidPrice,
                 "signal" = "SELL",
                 "size" = buysize10)
      
      
      
    }
    else
    {
      #between bid and ask, if closer to bid, buy, else sell
      if(abs(bidPrice-optbid) > abs(askPrice-optask))
      {
        #action = 'B'
        PL <<-(PL-askPrice)
        position <<-(position+1)
        flog.info(paste("position7 is:",position,sep=","),name = "info_consume")
        #return (paste("PL is:",PL,sep=","))
        ret = list("symbol" = "JPM",
                   "price" = askPrice,
                   "signal" = "BUY",
                   "size" = buysize10)
      }
      else (abs(bidPrice-optbid) > abs(askPrice-optask))
{
        #action = 'A'
        PL <<-(PL+bidPrice)
        position <<-(position-1)
        flog.info(paste("position8 is:",position,sep=","),name = "info_consume")
        #return (paste("PL is:",PL,sep=","))
        ret = list("symbol" = "JPM",
                   "price" = bidPrice,
                   "signal" = "SELL",
                   "size" = buysize10)
      }
    }
ask_old<<-askPrice
bid_old<<-bidPrice
min_old<<-min


  }


else {
  
  
  return("ignore")
}


}
  }
}

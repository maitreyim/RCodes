# ==========================================================================#
#-- Project Name :  Pairs Trading Agent Initial Code                                                     
#-- Task         :  Write a Pairs Trading Strategy     
#-- version      :  1.0
#-- Date         :  01/APRIL/2014
#-- Author       :  Maitreyi Mandal   
#-- SVN Directory:  \xxxx           
# ==========================================================================#
#============================== Get the environment variables===============#
#==============================
mripPath = Sys.getenv("MRIP_HOME")
#===================================
# Common variables across the agency
#===================================
assign("checPkgVec",0, pos=.GlobalEnv)
assign("firstCall",TRUE, pos=.GlobalEnv)
assign("symbolVec",c("JPM","GS"), pos=.GlobalEnv)
#assign("activeMQIp","tcp://10.10.5.40:61616", pos=.GlobalEnv)
#=========================
# Check the logger package
#=========================
if(!is.element("futile.logger", installed.packages())){
  install.packages("futile.logger",repos="http://lib.stat.cmu.edu/R/CRAN")
}
require("futile.logger")
#================== 
# Setting up logger
#==================
flog.threshold(INFO)
#flog.appender(appender.file(paste(mripPath,"/MINTlogs/NewpairTradingProcess10.log",sep="")))
flog.info("Sourcing PairTradingModel.R file - Begin!")
#flog.threshold(INFO)
flog.appender(appender.file("C:/Users/Maitreyi.Mandal/Desktop/R/log9.log"))
flog.info("Sourcing PairTrading.R file - Begin!")
#====================================
# Common Functions across the agency
#====================================
#---------------------------------------------------------------------
# A simple function to check if the required version of R is installed
#---------------------------------------------------------------------
CheckRversion<-function(reqRVersion = '2.15.1'){
  if (getRversion() < reqRVersion){
    flog.error("Upgrade your R!")
    return(10)
  } else {
    return(0)
  }
}

packInfo <- installed.packages ()[ ,c("Package", "Depends", "Version")] 

#-----------------------------------------------------------------------------
# Check if a given package is installed and is >= required version
# Args: 
#   packName: Name of the package as a string
#   reqVersion: Required version as a string
#Returns:
#   0 if the package is already installed/ or was succesfully installed by the
#    function. Else returns 11.
#-----------------------------------------------------------------------------
CheckPackVer <- function(packName, reqVersion) {
  flog.info("Checking package and version for package %s",paste(packName,reqVersion,sep=":"))
  reqVersion <- unlist(strsplit(reqVersion, "[[:punct:][:space:]]+"))
  count <- 1
  tryCatch({
    currVersion <- packInfo[packName,3]
    currVersion <- unlist(strsplit(currVersion,"[[:punct:][:space:]]+"))
    
    if(length(currVersion) > length(reqVersion) ){
      reqVersion = c(reqVersion,rep("0",length(currVersion) - length(reqVersion)))
    }
    if(length(currVersion) < length(reqVersion) ){
      currVersion = c(currVersion,rep("0",length(reqVersion) - length(currVersion)))
    }
    
    if(any(currVersion != reqVersion)){
      chng <- which(currVersion != reqVersion)[1]
      
      if(reqVersion[chng] > currVersion[chng]){
        stop()
      }
      
    }
    flog.info("%s is installed",packName)
    return(0)
  },
  error = function(err){
    flog.error("Error in the package %s",packName)
    flog.error("Error is %s",err)
    
    while(count<4){
      flog.info("%s Installation attemp %d",packName,count)
      install.packages(packName,dependencies=TRUE,repos="http://lib.stat.cmu.edu/R/CRAN")
      packInfo <- installed.packages ()[ ,c("Package", "Depends", "Version")] 
      if(packName %in% packInfo[,1]) break
      count<<-count+1
    }
    if (count == 4 && !(packName %in% packInfo[,1])){
      return(11)
    } else{
      return(0)
    }
  },
  finally={})
}

flog.info("Sourcing PairTradingModel.R file - End!")


#====================================
# Check R version and package version
#====================================

errorCode<-CheckRversion()

checPkgVec[1]<-CheckPackVer("Rjms",'0.0.5')
checPkgVec[2]<-CheckPackVer("WeightedPortTest",'1.0')
if (any(checPkgVec!=0)){
  errorCode<<-11
}
#===========================
# Loading Required Libraries
#===========================
require("Rjms")
require("rjson")
flog.info("R libraries loaded succesfully")

#=====================
# Initialize Variables
#=====================
frameSize <- 100
nSymbols <- length(symbolVec)
waitTime <- 10000 # Time to wait (in millis) for the order to get filled 
dfMatrix <-NULL
fillCount <-NULL
SJPM <-0 # Counter to keep track of number of JPM Shares at the end
SGS<-0 # Counter to keep track of number of GS shares at the end
PNL<-0 # Profit & Loss
value<-0
stoploss<-0


#===============================
# LatencyComputation Variables
#===============================
#latencyFile = paste(mripPath,"/MINTlogs/NewpairTradingProcessLatency10.csv",sep="")

#=========================================
# Function to Initialize dynamic Variables
#=========================================

initParams <- function(){
  dfMatrix <<- matrix(-1,nrow=frameSize,ncol=nSymbols+1)
  fillCount <<- 1
  
}
#=============================
# DataFrame Charging Function
#=============================
dfChargeUsePrev <- function(tickerId, askPrice, tradeTime){
  temp <- gsub('.*:','', tradeTime)
  tradeTime <- sub(paste(':',temp,sep=""),paste(':',temp,sep=""), tradeTime)
  if(!is.na(askPrice)){
    symIndex <- which(symbolVec == tickerId)
    #First fill row no 1 in the matrix
    if(is.element(-1,as.vector(dfMatrix[1,]))){
      dfMatrix[1,symIndex] <<- askPrice
      dfMatrix[1,(nSymbols+1)] <<- as.POSIXct(strptime(tradeTime,"%Y-%m-%d %H:%M:%OS"))
      dFrame = data.frame(dfMatrix)
      colnames(dFrame) <- c(symbolVec,'Time')
      return(list("dFrame"=dFrame,"full"=FALSE))
    }
    #We have a complete row now
    else{
      if(fillCount == frameSize){
        #Pop the first element and insert at bottom
        tVec <- dfMatrix[fillCount[1],]
        dfMatrix[1:frameSize-1,] <<- dfMatrix[2:frameSize,]
        dfMatrix[frameSize,] <<- tVec
        dfMatrix[frameSize,symIndex] <<- askPrice
        dfMatrix[frameSize,(nSymbols+1)] <<- as.POSIXct(strptime(tradeTime,"%Y-%m-%d
                                                                 %H:%M:%OS"))
        dFrame <- data.frame(dfMatrix)
        colnames(dFrame) <- c(symbolVec,'Time')
        return(list("dFrame"=dFrame,"full"=TRUE))
      }
      else{
        #Add the element
        fillCount <<- fillCount + 1
        #Replicate previous value
        dfMatrix[fillCount,] <<- dfMatrix[fillCount-1,]
        dfMatrix[fillCount,symIndex] <<- askPrice
        dfMatrix[fillCount,(nSymbols+1)] <<- as.POSIXct(strptime(tradeTime,"%Y-%m-%d
                                                                 %H:%M:%OS"))
        if(fillCount == frameSize){
          dFrame <- data.frame(dfMatrix)
          colnames(dFrame) <- c(symbolVec,'Time')
          return(list("dFrame"=dFrame,"full"=TRUE))
        }
        else{
          dFrame = data.frame(dfMatrix)
          colnames(dFrame) <- c(symbolVec,'Time')
          return(list("dFrame"=dFrame,"full"=FALSE))
        }
      }
    }
  }
}
############################################### Pair Trading Function ###########################
pairTradingCheck<-function(df)
{
  #colnames(df)<-c("Y","X")
  m <- lm(JPM ~ GS + 0, data=df)
  beta <- coef(m)
  flog.info("Assumed hedge ratio is", beta, "\n")
  return(beta)
}

#==================================
# Function to generate trade signal
#==================================
generateTradeSignal <- function(tick){
  
  tickList <- fromJSON(tick)
  
  marketStatus <- tickList$marketStatus[1]
  
  tradeTime <- as.POSIXlt(tickList$timeStamp)
  
  flog.info(paste("TIMESTAMP is - ", tickList$timeStamp,sep=" "))
  
  if(tradeTime$hour==15 && tradeTime$min>=50)
  {
    if (SJPM!=0){
      
      flog.info(paste("JPM Shares in Hand:",SJPM,sep=""))
      flog.info(paste("GS Shares in Hand:",SGS,sep=""))
      dfList <- lapply(1:length(tickList$tickerId),FUN=function(x){
        return(dfChargeUsePrev(tickList$tickerId[x],
                               tickList$askPrice[x],
                               tickList$timeStamp[x]))})
      var5<-SJPM*(dfList$dFrame[frameSize,1])
      flog.info(paste("var5 is:",var5,sep=""))
      var6<-SGS*(dfList$dFrame[frameSize,2])
      flog.info(paste("var6 is:",var6,sep=""))
      PNL<<-PNL+var5+var6
      flog.info(paste("Today's PNL is:",PNL,sep=""))
      SJPM<<-0
      SGS<<-0
      flog.info("End of trading Day")
      return(paste("PNL is:",PNL,sep=""))
    }
    else
    {
      flog.info("Nothing in hand")
      flog.info(paste("Today's PNL is:",PNL,sep=""))
      return("ignore")
    } 
    # close out all prices as Day has ended
    dfMatrix <<- matrix(-1,nrow=frameSize,ncol=nSymbols+1)
    fillCount <<- 1
    
  }
  
  else{ # Day has not ended yet
    
    flog.info("Day hasnot ended yet")
    dfList <- lapply(1:length(tickList$tickerId),FUN=function(x){
      return(dfChargeUsePrev(tickList$tickerId[x],
                             tickList$askPrice[x],
                             tickList$timeStamp[x]))}) 
    
    dfList <- dfList[[length(dfList)]]
    
    if(is.null(dfList$dFrame)){
      flog.info("Dataframe is still charging")
      return("ignore")
    }
    
    if(dfList$full)    
    {
      flog.info("Data is Full")
      resFrame <- dfList$dFrame
      flog.info(paste("SJPM :",SJPM,sep=""))
      
      # Closing the Position once Open
      if (SJPM!=0){
        flog.info(paste("SJPM not zero:",SJPM,sep=""))
        flog.info(paste("SGS not zero:",SGS,sep=""))
        
        var1=SJPM*resFrame[frameSize,symbolVec[1]]
        flog.info(paste("var1:",var1,sep=""))
        var2=SGS*resFrame[frameSize,symbolVec[2]]
        flog.info(paste("var2:",var2,sep=""))
        stoploss<<-value+var1+var2 # can be thought of as PNL
        flog.info(paste("Stoploss is:",stoploss,sep=""))
        
        
        if( stoploss>=-100.00  ) # Not willing to lose more than $100 per trade
        {
          flog.info(paste("SJPM is:",SJPM,sep=""))
          PNL<<-stoploss
          flog.info("Closing the spread/SJPM less than zero")
          flog.info(paste("Today's PNL is:",PNL,sep=""))
          ret<- list("agency" = list(agency__,agency__),
                     "agent" = list(agent__,agent__),
                     "symbol" = list(symbolVec[1],symbolVec[2]),
                     "action" = list("SELL", "BUY"),
                     
                     "orderType"= "MKT",
                     "waitTime"=waitTime
                     
          )
          
          
          SJPM<<-0
          SGS<<-0
          
          return(ret)
          
        } 
        
        
        else
        {
          flog.info("Wait til spread closes")
          PNL<<-stoploss
          flog.info(paste("Today's PNL is:",PNL,sep=""))
          return("ignore")
        }
      }        
      
      else{ #SJPM=0 here
        beta<-pairTradingCheck(resFrame[,1:2])
        
        flog.info(paste("beta:",beta,sep="")) 
        sprdFrame <- resFrame[,symbolVec[1]] - beta*resFrame[,symbolVec[2]]
        sprd <- resFrame[frameSize,symbolVec[1]] - beta*resFrame[frameSize,symbolVec[2]]
        zscore<-(sprd-mean(sprdFrame))/sd(sprdFrame)
        flog.info(paste("zscore:",zscore,sep=""))
        #Opening a Position
        if(zscore > 2)
        {
          SJPM <<- SJPM-100 #sold JPM
          SGS<<- SGS+ceiling(beta*100) # bought GS
          
          flog.info(paste("SJPM is:",SJPM, sep=""))
          flog.info(paste("SGS is:",SGS, sep=""))
          var3<- -SGS*resFrame[frameSize,symbolVec[2]]
          var4<- -SJPM*resFrame[frameSize,symbolVec[1]]
          flog.info(paste("var3:",var3, sep=""))
          flog.info(paste("var4:",var4, sep=""))
          
          value<<- (-SJPM*resFrame[frameSize,symbolVec[1]]-SGS*resFrame[frameSize,symbolVec[2]])
          # value of the open position, neg sign of SJPM to take care of the fact that we have 
          #shorted SJPM but earned money
          
          flog.info(paste("value is:",value, sep=""))
          ret<- list("agency" = list(agency__,agency__),
                     "agent" = list(agent__,agent__),
                     "symbol" = list(symbolVec[1],symbolVec[2]),
                     "action" = list("SELL", "BUY"),
                     "orderType"= "MKT",
                     "waitTime"=waitTime
          )
          
          return(ret)
          
        }
        else if(zscore < -2 )
        {
          SJPM<<- SJPM+100 # bought JPM
          SGS<<- SGS-ceiling(beta*100) # sold GS
          var3<- -SGS*resFrame[frameSize,symbolVec[2]]
          var4<- -SJPM*resFrame[frameSize,symbolVec[1]]
          flog.info(paste("var3:",var3, sep=""))
          flog.info(paste("var4:",var4, sep=""))
          value<<-(-SGS*resFrame[frameSize,symbolVec[2]]-SJPM*resFrame[frameSize,symbolVec[1]])
          flog.info(paste("SJPM is:",SJPM, sep=""))
          flog.info(paste("SGS is:",SGS, sep=""))
          ret<- list("agency" = list(agency__,agency__),
                     "agent" = list(agent__,agent__),
                     "symbol" = list(symbolVec[1],symbolVec[2]),
                     "action" = list("SELL", "BUY"),
                     "orderType"= "MKT",
                     "waitTime"=waitTime,
                     
          )
          
          
          # value of the open position, neg sign of SGS to take care of the fact that we have 
          #shorted SGS but earned money
          
          flog.info(paste("value is:",value, sep=""))
          return(ret)
        }
        else 
        {
          
          flog.info("No Trade as Zscore not enough")
          
          return("ignore")
          
        }
      }
      
      
    }
    
    
    else {
      flog.info("DF is charging..")
      return("ignore")
    }
    
    
  }
}

#=========================================
# Main function to push signals to a queue 
#=========================================

MainSendSignal <- function(tick){
  
  t1 <- as.character(format(Sys.time(),"%Y-%m-%d %H:%M:%OS6"))
  
  if(firstCall){
    initParams()
    flog.info("Initialized Parameters for the first Call.")
    firstCall <<- FALSE
  }
  
  predSignal <- generateTradeSignal(tick)
  
  # Write the Latency values to a file
  if(predSignal != "ignore")
  {
    output <- toJSON(predSignal)
    #checkLogged <- to.logger(signalLogger,output,asString=T)
    
    # Calculate Latency values of R computation
    t2 <- as.character(format(Sys.time(),"%Y-%m-%d %H:%M:%OS6"))
    latVal = as.double(difftime(t2,t1)[1])
    latencyValues = c(t1,t2,latVal)
    write.table(x=t(latencyValues), file = latencyFile, row.names=FALSE, col.names=FALSE, sep=",",append=TRUE)
    flog.info("Sent a signal to the queue")
  }
}

#===================
# Clean Up Function
#===================

cleanUp <- function(){
  flog.info("The agent %s in agency %s has been killed. Clean up function evoked",
            agent__,agency__, name=logNS)
  
  #destroy.logger(signalLogger)
  rm(list=ls())
  flog.info("Clean up done.")
  
}




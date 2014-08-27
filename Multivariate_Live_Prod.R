# ==========================================================================#
#-- Project Name :  Multivariate Stock Price Movement Live Agent--Initial Model (LONG ONLY)                                                    
#-- Task         :  Write a Multivariate Stock Price Movement Agent    
#-- version      :  1.0
#-- Date         :  18/August/2014
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
assign("symbolVec",c("FAS","SPY","XLE","GLD","USO","HYG","LQD","VIX"), pos=.GlobalEnv)
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
flog.appender(appender.file(paste(mripPath,"/MINTlogs/Multivariate_Live.log",sep="")))
flog.info("Sourcing Multivariate_Live.R file - Begin!")
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

flog.info("Sourcing Multivariate_Live.R file - End!")


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
require("aod")
require("caret")
flog.info("R libraries loaded succesfully")

#=====================
# Initialize Variables
#=====================
frameSize <- 10000
tradeSize<-10
nSymbols <- length(symbolVec)
waitTime <- 1000 # Time to wait (in millis) for the order to get filled 
dfMatrix <-NULL
fillCount <-NULL
SFAS <-0 # Counter to keep track of number of FAS Shares at the end
value<-0
stoploss<-0
tradeId<-0 # to calculate the number of trades to be sent to the protector
startTickDate<-NULL
firstcall<-TRUE
tradeId<-0 # to calculate the number of trades to be sent to the protector
startTickDate<-NULL
#===============================
# LatencyComputation Variables
#===============================
latencyFile = paste(mripPath,"/MINTlogs/Multivariate_live.csv",sep="")

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

### ----------------------------------------------------------------------
###Function For Variable Ranking
###-----------------------------------------------------------------------
library(caret)
#library(FeatureSelection)
variableRanking<-function(x, y, method = c("nnet","rpart","gbm","blackboost",
                                           "glmboost","gamboost","treebag","pls","lm",
                                           "svmRadial","svmPoly","gaussprRadial","gaussprPoly","lasso",
                                           "rf","mars","enet","rvmRadial","rvmPoly","lda","multinom","rda",
                                           "fda","sddaLDA","sddaQDA","bagEarth","ctree","cforest",
                                           "nb","gpls","lvq"),
                          plot=F, ...)
{
  #Resetting an error flag
  errFlag<-0
  
  #Get the method argument in the function
  methodType<-match.arg(method)
  
  if(methodType == "mars")
  {
    methodType<-"earth"
  }
  
  tryCatch({
    modelFit<-train(x,y,method=methodType,...)
    varImportance<-varImp(modelFit)
  },error=function(e)
  {
    #message("The error is:")
    #message(e)
    errFlag<<-1
  })
  if(errFlag==1)
  {
    #message("The Tuning Parameters for the model are not set properly or the dataset provided is not proper. Please refer to the caret package and the specific model packages for details")
    return()
  }
  
  #Check for plotting a dotchart
  if(plot)
  {
    modProfile<-varImportance$importance
    featureVec<-modProfile[order(modProfile[,1],decreasing=T),1]
    names(featureVec)<-row.names(modProfile)[order(modProfile[,1],decreasing=T)]
    par(mgp=c(0.3,1.3,0))
    dotchart(rev(featureVec), cex = 0.4, main = "Variable Ranking values")
  } 
  #Return the importance values of the features in order of importance
  return(varImportance)
}

#==================================
# Function to generate trade signal
#==================================
generateTradeSignal <- function(tick){
  
  tickList <- fromJSON(tick)
  tickDate <- as.Date(tickList$timeStamp)
  
  if(is.null(startTickDate)){
    startTickDate <<- tickDate
  }
  
  if(tickDate > startTickDate){
    tradeId <<- 0
    startTickDate <<- tickDate
  }
  
  marketStatus <- tickList$marketStatus[1]
  
  tradeTime <- as.POSIXlt(tickList$timeStamp)
  
  flog.info(paste("TIMESTAMP is - ", tickList$timeStamp,sep=" "))
  
  dfList <- lapply(1:length(tickList$tickerId),FUN=function(x){
    return(dfChargeUsePrev(tickList$tickerId[x],
                           tickList$askPrice[x],
                           tickList$timeStamp[x]))}) 
  
  dfList <- dfList[[length(dfList)]]
  
  if(is.null(dfList$dFrame)){
    flog.info("Dataframe is still charging..",)
    return("ignore")
  }
  
  if(dfList$full)    
  {
    
    flog.info("Data is Full")
    
    resFrame <- dfList$dFrame
    
    resFrame<-data.frame(resFrame)
    colnames(resFrame)<-c("FAS","SPY","XLE","GLD","USO","HYG","LQD","VIX","Time")
    resFrame$Time1<-as.POSIXct(as.numeric(resFrame$Time),origin = "1970-01-01")
    resFrame$sec<-(as.numeric(format(resFrame$Time1,"%S")))
    
    resFrame <- resFrame[!duplicated(resFrame$sec),]
    resFrame<- subset(resFrame, select = c(1,2,3,4,5,6,7,8,10))
    colnames(resFrame)<-c("FAS","SPY","XLE","GLD","USO","HYG","LQD","VIX","Sec")
    
    for(i in 2:nrow (resFrame))
    {
      #Calculate Return on FAS
      resFrame[i,10]=(resFrame[i,1]-resFrame[i-1,1])/resFrame[i,1]
      #Calculate Lag 1 of Return of FAS
      resFrame[i,11]=(resFrame[i,10]-resFrame[i-1,10])/resFrame[i,10]
      #Calculate Lag 2 of Return of FAS
      resFrame[i,12]=(resFrame[i,11]-resFrame[i-1,11])/resFrame[i,11]
      # Calculate the Indicator Variable
      if (resFrame[i,10]>0)
      {
        
        resFrame[i,13]=1
      }
      else
      {
        resFrame[i,13]=0
      }
      
      #Calculate Return on SPY
      resFrame[i,14]=(resFrame[i,2]-resFrame[i-1,2])/resFrame[i,2]
      #Calculate Lag 1 of Return of SPY
      resFrame[i,15]=(resFrame[i,13]-resFrame[i-1,13])/resFrame[i,13]
      #Calculate Lag 2 of Return of SPY
      resFrame[i,16]=(resFrame[i,14]-resFrame[i-1,14])/resFrame[i,14]
      
      #Calculate Return on XLE
      resFrame[i,17]=(resFrame[i,3]-resFrame[i-1,3])/resFrame[i,3]
      #Calculate Lag 1 of Return of XLE
      resFrame[i,18]=(resFrame[i,16]-resFrame[i-1,16])/resFrame[i,16]
      #Calculate Lag 2 of Return of XLE
      resFrame[i,19]=(resFrame[i,17]-resFrame[i-1,17])/resFrame[i,17]
      
      #Calculate Return on GLD
      resFrame[i,20]=(resFrame[i,4]-resFrame[i-1,4])/resFrame[i,4]
      #Calculate Lag 1 of Return of GLD
      resFrame[i,21]=(resFrame[i,19]-resFrame[i-1,19])/resFrame[i,19]
      #Calculate Lag 2 of Return of GLD
      resFrame[i,22]=(resFrame[i,20]-resFrame[i-1,20])/resFrame[i,20]
      
      
      #Calculate Return on USO
      resFrame[i,23]=(resFrame[i,5]-resFrame[i-1,5])/resFrame[i,5]
      #Calculate Lag 1 of Return of USO
      resFrame[i,24]=(resFrame[i,22]-resFrame[i-1,22])/resFrame[i,22]
      #Calculate Lag 2 of Return of USO
      resFrame[i,25]=(resFrame[i,23]-resFrame[i-1,23])/resFrame[i,23]
      
      # Calculate Ratio of HYG/LQD
      
      resFrame[i,26]=(resFrame[i,6]/resFrame[i,7])
      
      
      # Add Term Structure, Fama-French Factors
      
      resFrame[,27] <- rep(0.07,nrow(resFrame)) 
      resFrame[,28] <- rep(0.22,nrow(resFrame)) 
      resFrame[,29] <- rep(0.22,nrow(resFrame))
      resFrame[,30]<- rep(1.78,nrow(resFrame))
      
      colnames(resFrame)<-c("FAS","SPY","XLE","GLD","USO","HYG","LQD","Time","VIX","Ret_FAS","LAG1_FAS","LAG2_FAS","Ind","Ret_SPY","Lag1_SPY","Lag2_SPY","Ret_XLE",
                            "Lag1_XLE","Lag2_XLE","Ret_GLD","Lag1_GLD","Lag2_GLD","Ret_USO","Lag1_USO","Lag2_USO","Ratio","Risk_prem","SMB","HML","TermS")
      
      # Check for NA & Inf Values etc
      # NA Values
      resFrame[is.na(resFrame)] <- 0
      
      # Inf Values
      is.na(resFrame) <- do.call(cbind,lapply(resFrame, is.infinite))
      
    }
    #write.csv(resFrame,'resFrame.csv')
    
    #### Calling VarImp Function
    if (firstcall==TRUE)
    {
      y<-resFrame[,13]
      y<-as.factor(unlist(y))
      #write.csv(y,'y.csv')
      x <- subset(resFrame, select = c(9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)) # selecting columns
      x[is.na(x)] <- 0
      #write.csv(x,'x.csv')
      v<-variableRanking(x, y, "nb")
      #print(v)
      
      z<-row.names(v$importance[order(v$importance[,1],decreasing = T),])[1:3]
      zd<<-data.frame(z)
      d1<-resFrame[,grepl(paste0(zd[1,1],collapse="|"),colnames(resFrame))]
      d2<-resFrame[,grepl(paste0(zd[2,1],collapse="|"),colnames(resFrame))]
      d3<-resFrame[,grepl(paste0(zd[3,1],collapse="|"),colnames(resFrame))]
      data5<-cbind(resFrame[,13],d1,d2,d3)
      data5<-data.frame(data5)
      mylogit <- glm(data5[,1] ~ data5[,2] + data5[,3] + data5[,4], data = data5, family = "binomial")
      newdata<-data.frame(lapply(resFrame, tail, 1))
      pred<-predict(mylogit, data5, type="response")
      d<-data.frame(pred)
      d[is.na(d)] <- 0
      print (d[,1])
      x<-mean(d[,1])
      print(x)
      firstcall <<-FALSE
    }
    
    
    else{
      
      ##### Run Logistic Regression
      flog.info("hhhh")
      d1<-resFrame[,grepl(paste0(zd[1,1],collapse="|"),colnames(resFrame))]
      d2<-resFrame[,grepl(paste0(zd[2,1],collapse="|"),colnames(resFrame))]
      d3<-resFrame[,grepl(paste0(zd[3,1],collapse="|"),colnames(resFrame))]
      data5<-cbind(resFrame[,13],d1,d2,d3)
      data5<-data.frame(data5)
      mylogit <- glm(data5[,1] ~ data5[,2] + data5[,3] + data5[,4], data = data5, family = "binomial")
      #print(mylogit)
      #write.csv(resFrame,'resFrame.csv')
      newdata<<-data.frame(lapply(resFrame, tail, 1))
      #print(newdata)
      pred<-predict(mylogit, data5, type="response")
      d<-data.frame(pred)
      d[is.na(d)] <- 0
      print (d[,1])
      x<-mean(d[,1])
      print(x)
      
    }
    
    
    if (x>0.01)
    {
      
      SFAS <- tradeSize+SFAS #Bought FAS
      orderType <- "MKT"
      lmtPrice <- 0
      action<-("BUY")
      tradeId<<-tradeId+1
      flog.info("Sending %s signal at askPrice %s and bidPrice %s at %s",action,
                tickList[1]$askPrice, tickList[1]$timeStamp)
      
      flog.info(paste("SFAS is:",SFAS, sep=""))
      flog.info(paste("value is:",value, sep=""))
      ret <- list("agency" = list(agency__),
                  "agent" = list(agent__),
                  "symbol" = list(symbolVec[1]),
                  "action" = list(action),
                  "qty" = list(tradeSize),
                  "orderType"= list(orderType),
                  "lmtPrice"=list(lmtPrice)
      )
      
      return(ret)
      
    }
    
    else 
    {
      flog.info("No Trade as Pred CutOff not enough")
      
      return("ignore")
      
    }
  } 
  
  else {
    flog.info("DF is charging..")
    return("ignore")
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
    
    return(output)
  }
  else {
    return("ignore")
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

library(data.table)
makeSymbols<-function(yearId, userId,marketId, nickname="unknown", amount=100000, from,to, period){
  
  #Remove account and portfolio if run previously
  #.blotter<-NULL
  message(nickname)
  try(rm(list=c(paste("portfolio",userPortf,sep="."),
                paste("account",userAcc,sep=".")),
         pos=.blotter), silent =FALSE)
  
  
  from<- strptime(from, "%d.%m.%Y")
  
  #download user data and trades data
  #userId<-"50175"
  dateId<-"all" # all - all trades, 20141208 - day trades
  #ProfileLink<-"http://investor.moex.com/ru/statistics/2015/portfolio.aspx?traderId="
  #ProfileLink<-paste(ProfileLink, userId, sep="")
  TradesLink<-"ftp://ftp.moex.com/pub/info/stats_contest"
  TradesLink<-paste(TradesLink, yearId,dateId,
                    paste(marketId,"_", userId,".zip", sep=""),
                    sep="/")
  download.file(TradesLink, paste(marketId,"_", userId,".zip", sep=""))
  unzip(paste(marketId,"_", userId,".zip", sep=""))
  
  #Read trades data
  userData<-read.csv(paste(marketId,"_", userId,".csv", sep=""),sep=";", header=FALSE)
  
  #Removing temporary files
  file.remove(paste(marketId,"_", userId,".zip", sep=""))
  file.remove(paste(marketId,"_", userId,".csv", sep=""))
  
  
  #Processing data and declare symbol
  userSymbols<-levels(factor(userData$V2))
  userData$V2<-gsub(" ","",userData$V2)
  
  #Load historical data for the symbol
  #data("tickers")
  #MOEXSymbols<-loadStockListMoex()
  #MOEXSymbols<-data.table(MOEXSymbols, stringsAsFactors=FALSE)
  


  #assign("symbols", symbols,envir=globalenv())
  userSymbols<-gsub(" ","",userSymbols)

    res<-data.table(nickname=nickname, userId=userId, marketId=marketId, usersymbol=userSymbols)
}

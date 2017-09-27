library(data.table)
makePortfolio<-function(yearId, userId,marketId, nickname="unknown", amount=100000, from,to, period){
  
  #Remove account and portfolio if run previously
  #.blotter<-NULL
  message(nickname)
  try(rm(list=c(paste("portfolio",userPortf,sep="."),
                paste("account",userAcc,sep=".")),
         pos=.blotter), silent =FALSE)
  
  fromStart<- strptime("2017-09-21 19:00:00", "%F %H:%M:%S")
  from<- strptime(from, "%d.%m.%Y")
  dayofweek<-as.numeric(format(from,"%u"))
  if(dayofweek==1)
    from<-from-60*60*24*3+60*60*19
  else
    from<-from-60*60*24*1+60*60*19
  to<- strptime(to, "%d.%m.%Y")

  
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
           
  
  fromUser<-as.POSIXct(userData[1,1])
  
  #Load historical data for the symbol
  #data("tickers")
  #MOEXSymbols<-loadStockListMoex()
  #MOEXSymbols<-data.table(MOEXSymbols, stringsAsFactors=FALSE)

  if (marketId==2){
    symbols<-unlist(sapply(paste(userSymbols, " ", sep=""), 
                           searchSymbol, USE.NAMES=FALSE))
    ss<-data.table(cbind(symbols,userSymbols))
    ss[is.na(symbols),symbols:=unlist(sapply(userSymbols,searchSymbol, USE.NAMES=FALSE))]
    symbols<-ss[,symbols]
    rm(ss)
    
  } 
    
  else 
    symbols<-unlist(sapply(gsub(" ","",userSymbols), FUN=function(x) spot[x==gsub(" ","",spot[,1]),2]))#spot[spot[,1] %in% gsub(" ","",userSymbols),2]


  #from<-as.Date(userData[1,1])
  from<-from
  to<-to
  period<-period
  
  for(s in symbols){
      try(getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE))
    }
  
  
  
  #Initialize stocks
  currency("RUB")
  #symbol<-toupper(symbol)
  symbols<-toupper(symbols)
  for(s in symbols){
    if(exists(s)){
      if(length(get(s)[paste(from,"/", sep="")])>0){
        assign(s,get(s)[paste(from,"/", sep="")])
        assign(s, get(s),envir=globalenv())
      }
      else{
        userSymbols<-userSymbols[-match(s,symbols)]
        symbols<-symbols[-match(s,symbols)]
        
      }
        
    }
    else{
      userSymbols<-userSymbols[-match(s,symbols)]
      symbols<-symbols[-match(s,symbols)]
    }
  }
  #assign("symbols", symbols,envir=globalenv())
  userSymbols<-gsub(" ","",userSymbols)
  if(marketId==2)
    symbols.df<-data.frame(symbols, userSymbols,stringsAsFactors=F)
  else
    symbols.df<-data.frame(symbols, userSymbols=as.character(names(symbols)), stringsAsFactors=F)
  
  symbol<-symbols[1]
  
  #stock(symbol,currency="RUB",multiplier=1)
  stock(symbols,currency="RUB",multiplier=1)
  
  
  qnty<-fread("qntFinal.csv")
  #qnty<-data.table(qnty)
  qnty<-qnty[id==userId & seccode %in% gsub(" ","",userSymbols),]
  
  # Initialize the Portfolio
  initDate<-"2010-01-14"
  initEq<-amount
  initPosQty<-rep(0,nrow(symbols.df))
  
  initPosQty[symbols.df$userSymbols %in% qnty$seccode]<-qnty[seccode %in% symbols.df$userSymbols,startPos]
  
  
  #initPortf(userPortf,symbols=symbol,initDate=initDate)
  initPortf(userPortf,
            symbols=symbols,
            initPosQty = 0,#initPosQty, 
            initDate=initDate,
            currency="RUB")
  
  initAcct(userAcc,
           portfolios=userPortf,
           initDate=initDate, 
           initEq=initEq, 
           currency="RUB")
  

  # look at the transactions data
  #symbol.trades
  
  # Add the transactions to the portfolio
  for(s in symbols){
    us<-as.character(symbols.df[symbols.df[,1]==s,2])
    
    if(length(qnty[seccode==us, startPos])){
      neworder<-data.frame(V1=as.character(index(get(s)$Open[1])),
                           V2=us,
                           V3=qnty[seccode==us, startPos],
                           V4=as.numeric(get(s)$Open[1]))
      
      userData<-rbind(neworder, userData)
    }
    
    symbol.trades<-userData[userData$V2==us,]
    symbol.trades<-xts(cbind(symbol.trades$V4,symbol.trades$V3),
                       order.by=as.POSIXct(symbol.trades[,1]))
    
    colnames(symbol.trades)<-c("TxnPrice","TxnQty")
    blotter:::addTxns(userPortf,s,
                      TxnData=symbol.trades,verbose=FALSE)
    
  }
  
  # update the portfolio stats
  updatePortf(userPortf)
  
  # update the account P&L
  updateAcct(userAcc)
  
  # and look at it
  portfolio = getPortfolio(userPortf)
  account = getAccount(userAcc)
  
  
  #lapply(symbols, FUN=function(s) chart.Posn(userPortf, s, theme=theme))
  tStats <- tradeStats(Portfolios = userPortf, inclZeroDays=F)
  #tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)
  #print(data.frame(t(tStats[,-c(1,2, 23)])))
  
  if(is.null(tStats) ||nrow(tStats)==0) return (NULL)
  else{
    
    ss<-data.table(symbols.df)
    setkey(ss, symbols)
    st<-data.table(rownames(tStats))
    setkey(st, V1)
    ss[,userSymbols:=gsub(" ","",userSymbols)]
    
    res<-data.table(nickname=nickname, userId=userId, marketId=marketId, symbol=rownames(tStats),usersymbol=ss[st][,userSymbols],tStats[,-c(1,2)])
    rm(list = symbols, envir =globalenv())
    res
  }
}

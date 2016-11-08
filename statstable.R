setwd("~/repo/shinyLCHI(AlexeyT)/new")
####Table with Statistics############
local<-TRUE
if (!local) encoding<-"" else encoding<-"CP1251"

library(FinancialInstrument)
library(PerformanceAnalytics)
library(rusquant)
library(blotter)
#library(ggplot2)
#library(dplyr)
#library(rmarkdown)
#library(quantmod)
#library(highcharter)
#library(markdown)
library(data.table)
#library(DT) # install.packages('DT')
#library(flexdashboard) # devtools::install_github('rstudio/flexdashboard')
#source("func.my.R",encoding = "UTF-8")
source('makeportfolio.R',encoding = "UTF-8")

data(tickers)
markets<-c("Срочный"=2, "Фондовый"=1, "Валютный"=3)
TradesLink<-"ftp://ftp.moex.com/pub/info/stats_contest"
yearId<-2016
dateId<-"all"
initEq<-100000

if (!exists('.blotter')) .blotter <- new.env()
userPortf<-"user_port"
userAcc<-"user_acc"


spot<-read.table("spot.csv", header=TRUE, sep=";", as.is=TRUE,encoding = "UTF-8")
#write.table(spot,file="spot.csv",sep=";",row.names = F )
#REFRESH

resday<-read.table("ftp://ftp.moex.com/pub/info/stats_contest/2016/result_day.csv",
                   header=TRUE
                   , sep=";", 
                   dec=".",
                   quote="",
                   as.is=TRUE,
                   fileEncoding = encoding,
                   skipNul=TRUE
)


statDT<-data.table(resdayAll)
statDT[as.numeric(count_deal)>100]
statDT<-statDT[, marketId:=ifelse(contype_name=="Срочный", 2, ifelse(contype_name=="Фондовый", 1,3))]

timerStart<-Sys.time()
# resDT<-rbindlist(lapply(1:statDT[,.N], 
#                         FUN = function(x) {makePortfolio(yearId, 
#                                                         statDT[x,trader_id], 
#                                                         statDT[x,marketId], 
#                                                         statDT[x,nik],
#                                                         statDT[x,amount], 
#                                                         strptime(resday$moment[1], "%d.%m.%Y"), 
#                                                         "10min" )
#                           #print(paste(x,allU,sep=" / "))
#                           }
#                         ))

library(parallel)
no_cores <- detectCores()
# Initiate cluster
cl <- makeCluster(no_cores,type="FORK")
resDT<-parLapply(cl,1:statDT[,.N], 
                 fun = function(x) {makePortfolio(yearId, 
                                                  statDT[x,trader_id], 
                                                  statDT[x,marketId], 
                                                  statDT[x,nik],
                                                  statDT[x,amount],
                                                  strptime(statDT[x,date_start], "%d.%m.%Y"),
                                                  strptime(resday$moment[1], "%d.%m.%Y"), 
                                                  "10min" )
                   #print(paste(x,allU,sep=" / "
                   
                 }
)
stopCluster(cl)
resDT<-rbindlist(resDT)
print(Sys.time()-timerStart)


resDT[,nik:=nickname]
resDT[, moment:=resday$moment[1]]

statDT[,skey:=paste(trader_id, contype_name, sep="")]
setkey(statDT,skey)

resDT[,skey:=paste(trader_id, contype_name, sep="")]
setkey(resDT,skey)

resDT<-statDT[resDT]
#resDT[,marketId:=NULL]
#resDT[,symbol:=NULL]
#resDT[,ticker:=usersymbol]
#resDT[,usersymbol:=NULL]
resDT[,tickersqty:=.N, by=trader_id]

cols<-c("moment",
"contype_name", 
"trader_id",
"nik",
#"diler_name",
"date_start",
"dohod",
"amount",
"dohod_rub",
"count_deal",
"qty",
"tickersqty",
#"vol_rub",
#"duel_win",
#"duel_loss",
#"nickname",
"ticker",
"Num.Txns",
"Num.Trades",
"Net.Trading.PL",
"Percent.Positive" ,
"Percent.Negative" ,
"Profit.Factor",
"Max.Drawdown"  , 
"Max.Equity",
"Min.Equity" ,  
"End.Equity")

tempDT<-resDT
resDT<-resDT[,.SD, .SDcols=cols] [order(-Profit.Factor)] 


resDT[,`:=`(dohod=round(dohod,1),
            amount=round(amount,2),
            dohod_rub=round(dohod_rub,2),
            Net.Trading.PL=round(Net.Trading.PL,2),
            Percent.Positive=round(Percent.Positive,1) ,
            Percent.Negative=round(Percent.Negative,1) ,
            Profit.Factor=round(Profit.Factor,1),
            Max.Drawdown=round(Max.Drawdown,2), 
            Max.Equity=round(Max.Equity,2),
            Min.Equity=round(Min.Equity,2),  
            End.Equity=round(End.Equity,2))]


save(resDT, file="resDT.RData")



##################33
# 
# no_cores <- detectCores()
# # Initiate cluster
# cl <- makeCluster(no_cores,type="FORK")
# symbDT<-parLapply(cl,1:statDT[,.N], 
#                  fun = function(x) {makeSymbols(yearId, 
#                                                   statDT[x,trader_id], 
#                                                   statDT[x,marketId], 
#                                                   statDT[x,nik],
#                                                   statDT[x,amount],
#                                                   strptime(statDT[x,date_start], "%d.%m.%Y"),
#                                                   strptime(resday$moment[1], "%d.%m.%Y"), 
#                                                   "10min" )
#                    #print(paste(x,allU,sep=" / "
#                    
#                  }
# )
# stopCluster(cl)
# symbDT<-rbindlist(symbDT)
# 
# symbDT[,skey:=paste(userId, usersymbol, sep="")]
# setkey(symbDT,skey)
# 
# resDT[,skey:=paste(trader_id, ticker, sep="")]
# setkey(resDT,skey)
# 
# symbresDT<-symbDT[resDT]
# symbresDT<-symbresDT[unique(skey)]
# 
# symbresDT[, cor_contype_name:=ifelse(marketId== 2,"Срочный", ifelse(marketId==1,"Фондовый","Валютный"))]
# 
# symbresDT[, contype_name:=cor_contype_name]
# resDT<-symbresDT
# 
# cols<-c("moment",
#         "contype_name", 
#         "trader_id",
#         "nik",
#         "ticker",
#         "tickersqty",
#         "diler_name",
#         "date_start",
#         "dohod",
#         "amount",
#         "dohod_rub",
#         "count_deal",
#         "qty",
#         "vol_rub",
#         "duel_win",
#         "duel_loss",
#         "nickname",
#         "Num.Txns",
#         "Num.Trades",
#         "Net.Trading.PL",
#         "Percent.Positive" ,
#         "Percent.Negative" ,
#         "Profit.Factor",
#         "Max.Drawdown"  , 
#         "Max.Equity",
#         "Min.Equity" ,  
#         "End.Equity")
# 
# 
# resDT<-resDT[,.SD, .SDcols=cols] [order(-Profit.Factor)] 
# save(resDT, file="resDT.RData")


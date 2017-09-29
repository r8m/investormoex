setwd("~/repo/shinyLCHI(AlexeyT)/new")
####Table with Statistics############
local<-TRUE
if (!local) encoding<-"" else encoding<-"CP1251"

#library(FinancialInstrument)
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

tickers<-loadStockListMfd()

markets<-c("Срочный"=2, "Фондовый"=1, "Валютный"=3)
TradesLink<-"ftp://ftp.moex.com/pub/info/stats_contest"
yearId<-2017
dealsTreshhold<-50
#dateId<-"all"
#initEq<-100000

if (!exists('.blotter')) .blotter <- new.env()
userPortf<-"user_port"
userAcc<-"user_acc"

spot<-read.table("spot.csv", header=TRUE, sep=";", as.is=TRUE,encoding = "UTF-8")
#write.table(spot,file="spot.csv",sep=";",row.names = F )
#REFRESH

# resday<-read.table("ftp://ftp.moex.com/pub/info/stats_contest/2017/result_day.csv",
#                    header=TRUE
#                    , sep=";", 
#                    dec=".",
#                    quote="",
#                    as.is=TRUE,
#                    fileEncoding = encoding,
#                    skipNul=TRUE
# )
download.file("ftp://ftp.moex.com/pub/info/stats_contest/2017/result_day.csv", "result_day.csv")
writeLines(iconv(readLines("result_day.csv"), from = "CP1251", to = "UTF8"), 
           file("result_day1.csv", encoding="UTF-8"))

statDT<-fread("result_day1.csv",fill = T)
file.remove("result_day.csv")
file.remove("result_day1.csv")

statDT<-statDT[as.numeric(count_deal)>dealsTreshhold]
statDT<-statDT[, marketId:=ifelse(contype_name=="Срочный", 2, ifelse(contype_name=="Фондовый", 1,3))]

nrec<-statDT[,.N]

 timerStart<-Sys.time()
# resDT<-rbindlist(lapply(1:statDT[,.N],
#                          FUN = function(x) {makePortfolio(x,yearId,
#                                                           statDT[x,trader_id],
#                                                           statDT[x,marketId],
#                                                           statDT[x,nik],
#                                                           statDT[x,amount],
#                                                           statDT[x,date_start],
#                                                           statDT[1,moment],
#                                                           "10min" )
#                            }))
# 

library(foreach)
library(doParallel)
no_cores <- detectCores()
cl<-makeCluster(no_cores, outfile="")
registerDoParallel(cl)


parMakePortfolio <- function (nrec) {
  foreach(x = 1:nrec, 
          #.combine = rbindlist,
          .export = c("spot", "tickers", "statDT", "yearId","makePortfolio","userPortf", "userAcc"), 
          .packages = c("rusquant", "blotter", "PerformanceAnalytics", "data.table"))  %dopar%  
    makePortfolio(x,
                  yearId, 
                  statDT[x,trader_id], 
                  statDT[x,marketId], 
                  statDT[x,nik],
                  statDT[x,amount],
                  statDT[x,date_start],
                  statDT[1,moment], 
                  "10min" )
}

resList<-parMakePortfolio(nrec)
stopCluster(cl)
print(Sys.time()-timerStart)

resDT<-rbindlist(resList)

statDT[,skey:=paste(trader_id, marketId, sep="")]
setkey(statDT,skey)

resDT[,skey:=paste(userId, marketId, sep="")]
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
#"qty",
"tickersqty",
#"vol_rub",
#"duel_win",
#"duel_loss",
#"nickname",
"usersymbol",
"Num.Txns",        
"Num.Trades",
"Net.Trading.PL",
"Avg.Trade.PL",
"Med.Trade.PL",
"Largest.Winner",
"Largest.Loser",
"Gross.Profits",
"Gross.Losses",
"Std.Dev.Trade.PL",
"Percent.Positive",
"Percent.Negative",
"Profit.Factor",
"Avg.Win.Trade",
"Med.Win.Trade",
"Avg.Losing.Trade",
"Med.Losing.Trade",
"Avg.Daily.PL",
"Med.Daily.PL",      
"Std.Dev.Daily.PL",
"Ann.Sharpe",
"Max.Drawdown",
"Profit.To.Max.Draw",
"Avg.WinLoss.Ratio",
"Med.WinLoss.Ratio",
"Max.Equity",
"Min.Equity",
"End.Equity" )





tempDT<-resDT
resDT<-resDT[,.SD, .SDcols=cols] [order(-Profit.Factor)] 


resDT[,`:=`(dohod=round(dohod,1),
            amount=round(amount,2),
            dohod_rub=round(dohod_rub,2),
            Net.Trading.PL=round(Net.Trading.PL,2),
            Avg.Trade.PL=round(Avg.Trade.PL,2),
            Med.Trade.PL=round(Med.Trade.PL,2),
            Largest.Winner=round(Largest.Winner,2),
            Largest.Loser=round(Largest.Loser,2),
            Gross.Profits=round(Gross.Profits,2),
            Gross.Losses=round(Gross.Losses,2),
            Std.Dev.Trade.PL=round(Std.Dev.Trade.PL,2),
            Percent.Positive=round(Percent.Positive,1),
            Percent.Negative=round(Percent.Negative,1),
            Profit.Factor=round(Profit.Factor,1),
            Avg.Win.Trade=round(Avg.Win.Trade,2),
            Med.Win.Trade=round(Med.Win.Trade,2),
            Avg.Losing.Trade=round(Avg.Losing.Trade,2),
            Med.Losing.Trade=round(Med.Losing.Trade,2),
            Avg.Daily.PL=round(Avg.Daily.PL,2),
            Med.Daily.PL=round(Med.Daily.PL,2),      
            Std.Dev.Daily.PL=round(Std.Dev.Daily.PL,2),
            Ann.Sharpe=round(Ann.Sharpe,1),
            Max.Drawdown=round(Max.Drawdown,2),
            Profit.To.Max.Draw=round(Profit.To.Max.Draw,2),
            Avg.WinLoss.Ratio=round(Avg.WinLoss.Ratio,2),
            Med.WinLoss.Ratio=round(Med.WinLoss.Ratio,2),
            Max.Equity=round(Max.Equity,2),
            Min.Equity=round(Min.Equity,2),
            End.Equity=round(End.Equity,2) 
            
            )]

resDT<-resDT[order(-Net.Trading.PL)] 
save(resDT, file="resDT2017.RData")



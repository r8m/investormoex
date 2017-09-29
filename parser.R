library(jsonlite)
library(httr)
library(reshape2)
library(dplyr)
library(data.table)
setwd("~/repo/shinyLCHI(AlexeyT)/new")


#твоя функция не изменная
getInitPos<-function(traderId, startDate){
  resp<-POST(url = "investor.moex.com/ru/statistics/2017/portfolio.aspx/GetPortfolioData", 
             encode = "json", 
             body = list('traderId'=paste0(traderId),'date'=paste0(startDate),'tableId'=6 ))
  stop_for_status(resp)
  json<-content(resp, "text")
  validate(json)
  
  initPos <- fromJSON(fromJSON(txt = json)$d)
  

  return(initPos)
    
}


download.file("ftp://ftp.moex.com/pub/info/stats_contest/2017/result_day.csv", "result_day.csv")
writeLines(iconv(readLines("result_day.csv"), from = "CP1251", to = "UTF8"), 
           file("result_day1.csv", encoding="UTF-8"))

resday<-fread("result_day1.csv",fill = T)
file.remove("result_day.csv")
file.remove("result_day1.csv")

resday %>% filter (dohod_rub!=0)%>%
  select(trader_id,date_start)%>%unique-> resday2

resday2$date_start<-strptime(resday2$date_start, format = "%d.%m.%Y")

qntyDF<-data.frame()

system.time({
for (i in 1:nrow(resday2))
{
#i<-898
  traderId<-resday2[i,1]
  message(traderId)
 # traderId<-148835
#  startDate<-strptime("22.09.2017", format = "%d.%m.%Y")
  
  
  startDate<-resday2[i,2]  
  allTable<-getInitPos(traderId,startDate)
  if(is.null(dim(allTable))) allTable<-data.frame(pos="0 (0)",seccode=0)
  posTable<-allTable[,c("pos","seccode")]#не помню какие колонки, то есть тикер и кол-во
  posTable$id<-traderId
  qntyDF<-rbind(qntyDF,posTable)
}
})

qntyDF$pos2<-sub("(-)","(0)", qntyDF$pos, fixed=TRUE)
tempTable<-colsplit(sub(")","", qntyDF$pos2),"\\(", c("open","delta"))
qntyDF$startPos<-tempTable$open-tempTable$delta


qntFinal<-qntyDF%>%filter(startPos!=0)%>%select(id,seccode,startPos)

#write.csv(qntFinal, "qntFinal.csv", row.names = FALSE, quote=FALSE)

#test<-read.csv("qntFinal.csv", as.is=TRUE)



inst.all<-fread("ftp://ftp.moex.com/pub/info/stats/forts/FORTS_LIST.TXT",
                header=TRUE,
                sep=';'
)



test2<-left_join(qntFinal,inst.all,  by = c("seccode" = "symbol"))
test3<-test2[c(1,2,3,7)]

test3[!is.na(test3$short_symbol),2]<-test3[!is.na(test3$short_symbol),4]

write.csv(test3[,-4], "qntFinal.csv", row.names = FALSE, quote=FALSE)


#########################
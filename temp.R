getSymbolsmfd <-
  function(Symbols,env,return.class='xts',index.class='Date',
           from='2014-01-01',
           to=Sys.Date(),
           adjust=FALSE,
           period='day',
           updatetickers=FALSE,
           ...)
  {
    importDefaults("getSymbols.mfd")
    this.env <- environment()
    for(var in names(list(...))) {
      # import all named elements that are NON formals
      assign(var, list(...)[[var]], this.env)
    }
    
    default.return.class <- return.class
    default.from <- from
    default.to <- to
    
    if(missing(verbose)) verbose <- FALSE
    if(missing(auto.assign)) auto.assign <- FALSE
    
    p <- 0
    
    if ("1min" == period) p <- 1
    if ("5min" == period) p <- 2
    if ("10min" == period) p <- 3
    if ("15min" == period) p <- 4
    if ("30min" == period) p <- 5
    if ("hour" == period) p <- 6
    if ("day" == period) p <- 7
    if ("week" == period) p <- 8
    if ("month" == period) p <- 9
    if ("tick" == period) p <- 0
    
    #if (p==0) {
    #   message(paste("Unkown period ", period))
    #}
    for (i in 1:length(Symbols)) {
      mfd.from <- format(as.Date(from), "%d.%m.%Y")
      mfd.to <- format(as.Date(to), "%d.%m.%Y")
      Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
      Symbols.name <- ifelse(is.null(Symbols.name), Symbols[[i]], 
                             Symbols.name)
      
      if(updatetickers){
        tickers <- loadStockListMfd()
        assign('tickers', tickers, env)
      }
      
      #data("tickers")
      if (!exists("tickers")){
        data("tickers")
      }
      
      
      SYMBOL.GROUP <- tickers[which(tickers[, 4] == Symbols), 1]
      SYMBOL.ID <- tickers[which(tickers[, 4] == Symbols), 3]
      if (length(SYMBOL.ID) == 0) {
        if (verbose) 
          cat("Don't know about", Symbols[[i]], "\n\n")
        next
      }
      mfd.URL <- "http://mfd.ru/export/handler.ashx/Data.txt?"
      stock.URL <- paste(mfd.URL, "TickerGroup=", SYMBOL.GROUP, 
                         "&Tickers=", SYMBOL.ID, "&Alias=false&Period=", p, 
                         "&timeframeValue=1&timeframeDatePart=day&StartDate=", 
                         mfd.from, "&EndDate=", mfd.to, "&SaveFormat=0&SaveMode=0&FileName=Date18112013_23112013.txt&FieldSeparator=%253b&DecimalSeparator=.&DateFormat=yyyyMMdd&TimeFormat=HHmmss&DateFormatCustom=&TimeFormatCustom=&AddHeader=true&RecordFormat=0&Fill=false", 
                         sep = "")
      tmp <- tempfile()
      download.file(stock.URL, destfile = tmp, quiet = TRUE)
      fr <- read.table(tmp, sep = ";", header = TRUE)
      unlink(tmp)
      if(nrow(fr)==0) next
      if(nrow(fr)==1) fr<-rbind(fr,fr)
      if (p %in% 1:6) {
        fr[fr[, 4] < 1e+05 & fr[, 4] >= 10000, 4] <- paste("0", as.character(fr[fr[, 4] < 1e+05 & fr[, 4] >= 10000, 4]), sep = "")
        fr[as.double(fr[, 4]) < 10000 & as.double(fr[, 4]) >  0, 4] <- paste("00", (fr[as.double(fr[, 4]) <  10000 & as.double(fr[, 4]) > 0, 4]), sep = "")
        fr[fr[, 4] == "0", 4] <- paste("00000", (fr[fr[,  4] == "0", 4]), sep = "")
        fr <- xts(apply(as.matrix(fr[, (5:10)]), 2, as.numeric), 
                  as.POSIXct(strptime(paste(fr[, 3], fr[, 4]), 
                                      "%Y%m%d %H%M%S")), unique = T,src = "mfd", updated = Sys.time())
        if(!is.null(colnames(fr)))
          colnames(fr) <- c("Open", "High", "Low", "Close", "Volume", "OPEN_INTEREST")
      }
      if (p %in% 7:9) {
        fr <- xts(apply(as.matrix(fr[, (5:10)]), 2, as.numeric), 
                  as.Date(strptime(fr[, 3], "%Y%m%d")), src = "mfd", updated = Sys.time())
        
        if(!is.null(colnames(fr)))
          colnames(fr) <- c("Open", "High", "Low", "Close", "Volume", "OPEN_INTEREST")
      }
      
      
      #<TICKER>;<PER>;<DATE>;<TIME>;<CLOSE>;<VOL>
      if (p == 0) {
        fr[fr[, 4] < 1e+05 & fr[, 4] >= 10000, 4] <- paste("0", 
                                                           as.character(fr[fr[, 4] < 1e+05 & fr[, 4] >= 
                                                                             10000, 4]), sep = "")
        fr[as.double(fr[, 4]) < 10000 & as.double(fr[, 4]) > 
             0, 4] <- paste("00", (fr[as.double(fr[, 4]) < 
                                        10000 & as.double(fr[, 4]) > 0, 4]), sep = "")
        fr[fr[, 4] == "0", 4] <- paste("00000", (fr[fr[, 
                                                       4] == "0", 4]), sep = "")
        fr <- xts(apply(as.matrix(fr[, (5:6)]), 2, as.numeric), 
                  as.POSIXct(strptime(paste(fr[, 3], fr[, 4]), 
                                      "%Y%m%d %H%M%S")), src = "mfd", updated = Sys.time())
        colnames(fr) <-  c("Close", "Volume")
      }
      
      fr <- convert.time.series(fr = fr, return.class = return.class)
      fr<-fr[,-6]
      if (is.xts(fr) && p > 7) 
        indexClass(fr) <- index.class
      Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
      if (auto.assign) 
        assign(Symbols[[i]], fr, env)
      if (i >= 5 && length(Symbols) > 5) {
        message("pausing 1 second between requests for more than 5 symbols")
        Sys.sleep(1)
      }
    }
    if (auto.assign) 
      return(Symbols)
    if (exists("fr")) 
      return(fr)
    
  }
s<-"BR60BI6 (09.2016)"
from<-as.Date("2016-09-15")
to<-as.Date("2016-11-05")

getSymbolsmfd(s, from=from, to=to, period="10min", src='mfd',adjust=TRUE, auto.assign=TRUE, verbose=T)
#getSymbols(s, from=from, to=to, period="10min", src='mfd',adjust=TRUE, auto.assign=TRUE)

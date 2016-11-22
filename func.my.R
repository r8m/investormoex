charts.PerformanceSummary.my<-
function (R, Rf = 0, main = NULL, geometric = TRUE, methods = "none", 
          width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, 
          gap = 12, begin = c("first", "axis"), legend.loc = "topleft", 
          p = 0.95, ...) 
{
  begin = begin[1]
  x = checkData(R)
  colnames = colnames(x)
  ncols = ncol(x)
  length.column.one = length(x[, 1])
  start.row = 1
  start.index = 0
  while (is.na(x[start.row, 1])) {
    start.row = start.row + 1
  }
  x = x[start.row:length.column.one, ]
  if (ncols > 1) 
    legend.loc = legend.loc
  else legend.loc = NULL
  if (is.null(main)) 
    main = paste(colnames[1], "Performance", sep = " ")
  if (ylog) 
    wealth.index = TRUE
  op <- par(no.readonly = TRUE)
  layout(matrix(c(1, 2, 3)), heights = c(2, 1, 1.3), widths = 1)
  par(mar = c(1, 4, 4, 2))
  chart.CumReturns(x, main = main, xaxis = FALSE, legend.loc = legend.loc, 
                   event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, 
                   begin = begin, geometric = geometric, ylab = "Кумул доходность", 
                   ...)
  par(mar = c(1, 4, 0, 2))

  chart.BarVaR(x, main = "", xaxis = FALSE, width = width, 
               ylab = "Дневная доходность", methods = methods, 
               event.labels = NULL, ylog = FALSE, gap = gap, p = p, 
               ...)
  par(mar = c(5, 4, 0, 2))
  chart.Drawdown(x, geometric = geometric, main = "", ylab = "Просадка", 
                 event.labels = NULL, ylog = FALSE, ...)
  par(op)
}

chart.Posn.my<-function (Portfolio, Symbol, Dates = NULL, userName=NULL, ..., TA = NULL){
  pname <- Portfolio
  Portfolio <- getPortfolio(pname)
  if (missing(Symbol)) Symbol <- ls(Portfolio$symbols)[[1]]
  else Symbol <- Symbol[1]
  Prices = get(Symbol)
  if (!is.OHLC(Prices)) {
    if (hasArg(prefer)) prefer = eval(match.call(expand.dots = TRUE)$prefer)
    else prefer = NULL
    Prices = getPrice(Prices, prefer = prefer)
  }
  freq = periodicity(Prices)
  switch(freq$scale, 
         seconds = {mult = 1}, 
         minute = {mult = 60}, 
         hourly = {mult = 3600}, 
         daily = {mult = 86400}, 
                {mult = 86400}
         )
  if (!isTRUE(freq$frequency * mult == round(freq$frequency,0) * mult)) 
    n = round((freq$frequency/mult), 0) * mult
  else 
    n = mult
  tzero = xts(0, order.by = index(Prices[1, ]))
  if (is.null(Dates)) Dates <- paste(first(index(Prices)), last(index(Prices)), sep = "::")
  Portfolio$symbols[[Symbol]]$txn <- Portfolio$symbols[[Symbol]]$txn[Dates]
  Portfolio$symbols[[Symbol]]$posPL <- Portfolio$symbols[[Symbol]]$posPL[Dates]
  Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
  Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades > 0)]
  Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades < 0)]
  Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty

  if (as.POSIXct(first(index(Prices))) < as.POSIXct(first(index(Position)))) 
    Position <- rbind(xts(0, order.by = first(index(Prices) - 1)), Position)
   Positionfill = na.locf(merge(Position, index(Prices)))
  #Positionfill<-Position
  #Position<-Position[-1]
  
  CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
  if (length(CumPL) > 1) CumPL = na.omit(na.locf(merge(CumPL, index(Prices))))
  else CumPL = NULL
  if (!is.null(CumPL)) {
    CumMax <- cummax(CumPL)
    Drawdown <- -(CumMax - CumPL)
    Drawdown <- rbind(xts(-max(CumPL), order.by = first(index(Drawdown) - 
                                                          1)), Drawdown)
    Drawdown<-Drawdown[-1]
  }
  else {
    Drawdown <- NULL
  }
  if (!is.null(Dates)) Prices = Prices[Dates]
 
  highchart() %>%
    hc_plotOptions(series=list(dataGrouping=list(enabled=FALSE)))%>%
    hc_title(text = paste("Участник:",userName,"Инструмент:",Symbol, "Покупок:", nrow(Buys), " / Продаж:",nrow(Sells)))%>%
    
    hc_yAxis_multiples(
      list(title = list(text = "Сделки"), height = "60%", top = "0%"),
      list(title = list(text = "Позиция"), height = "10%",top = "67%"),
      list(title = list(text = "Эквити/Просадка"), height = "20%",top = "80%")
    #hc_yAxis_multiples(
    #     list(title = list(text = NULL), height = "50%", top = "0%"),
    #     list(title = list(text = NULL), height = "24%", top = "51%",opposite = TRUE),
    #     list(title = list(text = NULL), height = "24%", top = "76%")
   ) %>%
    hc_add_series_ohlc(Prices, yAxis = 0,name=Symbol) %>%
    hc_add_series_xts(Positionfill, type="column", color = "blue",
                      name="Positionfill", yAxis=1)%>%
    hc_add_series_xts(CumPL, color = "darkgreen", type = "line", 
                      name="CumPL", yAxis=2)%>%
    hc_add_series_xts(Drawdown, color = "darkred",type = "line", 
                      name="Drawdown", yAxis=2)%>%
    hc_add_series_flags(as.Date(c("2016-10-03"), format = "%Y-%m-%d"),
                      title = c("Новые тарифы"), 
                      text = c("Введены новые тарифы"))%>% 
    hc_add_theme(hc_theme_ft())->hc
  
  if (nrow(Buys)!=0) hc<-hc%>%hc_add_series_xts(Buys, color = "green",
                    type="scatter", marker = list(symbol = "triangle"),
                    enableMouseTracking = FALSE, yAxis = 0)
  
  if(nrow(Sells)!=0) hc<-hc%>%hc_add_series_xts(Sells, color = "red",
                             type="scatter", marker = list(symbol = "triangle-down"),
                             enableMouseTracking = FALSE, yAxis = 0)
  hc<-hc %>% 
    #hc_add_theme(hc_theme_flat())
    #hc_add_theme(hc_theme_smpl())
    #Cool hc_add_theme(hc_theme_538())
    hc_add_theme(hc_theme_gridlight())
  return (hc)

}







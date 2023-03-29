require("httr")
require("jsonlite")
library(xts)
library(zoo)
library(quantmod)
library(tseries)
library(forecast)
library(rugarch)
library(RODBC)

CalculateGarch<-function(symbol,yahoo_symbol,final_xts){
  apikey <- "7BNQRFGNAKJL5XFOAGZE2LIUSWFJGE5G"
  now<-Sys.Date()
  
  #Get Last Price
  call_tick <- sprintf("https://api.tdameritrade.com/v1/marketdata/%s/quotes?apikey=%s",symbol,apikey)
  get_tick <- GET(call_tick)
  get_tick_result <- content(get_tick, "text")
  get_tick_json <- fromJSON(get_tick_result, flatten = TRUE)
  lastPrice<-get_tick_json[[symbol]]$lastPrice
  
  #Get History Data
  ffrom<-as.Date(now) - as.difftime(730,unit="days")
  getSymbols(yahoo_symbol,src="yahoo",from=ffrom,to=now)
  data<-get(yahoo_symbol)
  stock_prices<-na.omit(data, na.action = "omit", fill = NULL)
  
  #Bind Last Price to Historical Data
  matrix<-matrix(c(0,0,0,lastPrice,0,0),nrow=1,ncol=6,byrow = TRUE,dimnames=list(c(paste(now)),c("Open","High","Love","Close","Volume","Adjusted")))
  xts<-as.xts(matrix,order.by = now)
  stock_prices_with_tick<-rbind(stock_prices,xts)
  print(tail(stock_prices_with_tick))
  
  #Daily Return Calculation
  returns = diff(log(Cl(stock_prices_with_tick)))
  returns[as.character(head(index(Cl(stock_prices_with_tick)),1))] = 0
  
  
  
  #Get first {windowLength} daily return
  windowLength = 500
  returns_length=length(returns)
  first=returns_length-500
  dailyReturnsSubSet = returns[first:returns_length]
  
  #Calculate arma order
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:5) for (q in 0:5) {
    if ( p == 0 && q == 0) {
      next
    }
    
    arimaFit = tryCatch( arima(dailyReturnsSubSet, order=c(p, 0, q)),
                         error=function( err ) FALSE,
                         warning=function( err ) FALSE )
    
    if( !is.logical( arimaFit ) ) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) {
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        final.arima <- arima(dailyReturnsSubSet, order=final.order)
      }
    } else {
      next
    }
  }
  xts$symbol<-symbol
  xts$sGarchResult<-0
  xts$eGarchResult<-0
  xts$fGarchResult<-0
  xts$iGarchResult<-0
  
  xts[now,7]=symbol
  #BAD NEWS
  #eGarch(1,1)
  #egarch11_spec <- ugarchspec(variance.model = list(model="eGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(final.order[1], final.order[3]),include.mean=T),distribution.model="sged")
  #egarch11_fit<- tryCatch(ugarchfit(spec=egarch11_spec, data=dailyReturnsSubSet,solver = 'hybrid'), error=function(e) e, warning=function(w) w)
  
  #if(!is(egarch11_fit, "warning")) 
  #{
  #  forecasted = ugarchforecast(egarch11_fit, n.ahead=1)
  #  result = forecasted@forecast$seriesFor
  #  xts[now,9]=result[1]
    #eGarchResult=result[1]
  #}
  
  #sGarch(1,1)
  sgarch11_spec <- ugarchspec(variance.model = list(model="sGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(final.order[1], final.order[3]),include.mean=T),distribution.model="sged")
  sgarch11_fit<- tryCatch(ugarchfit(spec=sgarch11_spec, data=dailyReturnsSubSet,solver = 'hybrid'), error=function(e) e, warning=function(w) w)
  if(!is(sgarch11_fit, "warning")) 
  {
    forecasted = ugarchforecast(sgarch11_fit, n.ahead=1)
    result = forecasted@forecast$seriesFor
    xts[now,8]=result[1]
  }
  #fGarch(1,1)
  #fgarch11_spec <- ugarchspec(variance.model = list(model="fGARCH", garchOrder = c(1, 1),submodel = "APARCH"),mean.model = list(armaOrder = c(final.order[1], final.order[3]),include.mean=T),distribution.model="sged")
  #fgarch11_fit<- tryCatch(ugarchfit(spec=fgarch11_spec, data=dailyReturnsSubSet,solver = 'hybrid'), error=function(e) e, warning=function(w) w)
  #if(!is(fgarch11_fit, "warning")) 
  #{
  #  forecasted = ugarchforecast(fgarch11_fit, n.ahead=1)
  #  result = forecasted@forecast$seriesFor
  #  xts[now,10]=result[1]
  #}
  
  #iGarch(1,1)
  #igarch11_spec <- ugarchspec(variance.model = list(model="iGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(final.order[1], final.order[3]),include.mean=T),distribution.model="sged")
  #igarch11_fit<- tryCatch(ugarchfit(spec=igarch11_spec, data=dailyReturnsSubSet,solver = 'hybrid'), error=function(e) e, warning=function(w) w)
  #if(!is(igarch11_fit, "warning")) 
  #{
  #  forecasted = ugarchforecast(igarch11_fit, n.ahead=1)
  #  result = forecasted@forecast$seriesFor
  #  xts[now,11]=result[1]
  #}
  
  final_xts<-rbind(final_xts,xts)
  return(final_xts)
  
}

con <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=Investing;trusted_connection=true')
res <- sqlQuery(con, 'SELECT * FROM dbo.Symbol')

initial<-matrix(c(0,0,0,0,0,0,"",0,0,0,0),nrow=1,ncol=11,byrow = TRUE,dimnames=list(c(paste(Sys.Date())),c("Open","High","Love","Close","Volume","Adjusted","symbol","sGarchResult","eGarchResult","fGarchResult","iGarchResult")))
final_xts<-as.xts(initial)

for(row in 1:nrow(res)){
  code <- res[row, "SymbolCode"]
  yahoo_symbol <- res[row, "YahooCode"]
  description <- res[row, "Description"]
  if(code!="RDSa" && code!="ZM")
  {
  print(paste("******************",description,"Start!**************************"))
  final_xts<-CalculateGarch(code,yahoo_symbol,final_xts)
  print(paste("******************",description,"End!  **************************"))
  }
  
}
odbcCloseAll()
write.csv(as.data.frame(final_xts), file="final_xts_16Nov.csv", row.names=TRUE)
GetAllOptions <- function (){
  return( c("Flat", "Buy", "Sell"))
}

EstimateProfit <- function(quotes, openIndex, closeIndex, userOption){

  rv <-list( Answer=userOption, ActualProfit=1, MaxProfit=1, MinProfit=1, OpenIndex=openIndex)
  
  buyProfit <- round(10000 * (quotes[closeIndex, open] - quotes[openIndex, open]), 1)
  sellProfit <- -buyProfit
    
  if( userOption == "Buy"){
    rv$ActualProfit <- buyProfit
  }else if(userOption == "Sell"){
    rv$ActualProfit <- sellProfit
  }else if(userOption == "Flat"){
    rv$ActualProfit <- 0
  }
  
  rv$MaxProfit <- max(buyProfit, sellProfit)
  rv$MinProfit <- min(buyProfit, sellProfit)
  
  rv
}
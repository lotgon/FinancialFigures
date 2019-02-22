require(foreach)
require(tidystats)
#source("BuildClusters.R")

CalculateClosestCenters <- function( quotes, qIndex, cTable){
  mean_window <- cTable[1, mean_window]
  window_size <- cTable[1, window_size]
  
  meanPredictedWindow <- quotes[qIndex:(qIndex+mean_window-1), mean(open)]
  sampleQuotes <- quotes[qIndex:(qIndex+mean_window-1), open/meanPredictedWindow]
  
  distTable <- cTable[index<=mean_window,.(distance=sum(abs(value - sampleQuotes))), by=.(clusterIndex, clusterPower)]
  distTable <- distTable[distance < min(distance)*1.1]
  expectedPrice <- cTable[clusterIndex %in% distTable$clusterIndex,][index==window_size, sum(value*clusterPower)/sum(clusterPower)]
  distTable[,expectedPrice := expectedPrice]
  distTable$clusterExpectedPrice <- cTable[clusterIndex %in% distTable$clusterIndex &  index==window_size, value]
  list(sampleQuotes = sampleQuotes, distTable=distTable)
}

ModelEstimator1 <- function(quotes, qIndex, cTable, tradeThreshold=0.005){
  
  r <- CalculateClosestCenters(quotes, qIndex, cTable)
  distTable <- r$distTable
  sampleQuotes <- r$sampleQuotes
  
  window_size <- cTable[1,window_size]
  mean_window <- cTable[1,mean_window]
  
  
  expectedPrice <- distTable[1,expectedPrice]
  profit <- 0
  
  if( sampleQuotes[mean_window] - expectedPrice > tradeThreshold & all(sampleQuotes[mean_window] > distTable$clusterExpectedPrice) & distTable[,.N]>0)#sell
    profit <- quotes[qIndex+mean_window] - quotes[qIndex + window_size]
  else if( expectedPrice - sampleQuotes[mean_window] > tradeThreshold & all(sampleQuotes[mean_window] < distTable$clusterExpectedPrice)& distTable[,.N]>0) #buy
    profit <- quotes[qIndex + window_size] - quotes[qIndex+mean_window]
  
  profit
}

SimulateTrades <- function(chosenSymbol, tradeStep = 0, tradeThreshold = 0.006, cTableName = "cTable", from=ISOdate(2018, 1, 1, tz = "UTC"), to=ISOdate(2018, 11, 1, tz = "UTC")){
  print(chosenSymbol)
  quotes <- GetQuotes(chosenSymbol, from, to)[,.(open)]
  cTable <- readRDS(cTableName)[symbol==chosenSymbol] #table(cTable <- readRDS("cTable")$symbol)
  window_size <- cTable[1,window_size]
  mean_window <- cTable[1,mean_window]
  
  if( tradeStep == 0)
    tradeStep <- window_size- mean_window 
  
  n <- quotes[,.N-window_size]
  step <- 50
  i <- 1
  r <- numeric()
  while(i<n){
    profit <- ModelEstimator1(quotes, i, cTable, tradeThreshold)
    if(profit != 0){
      r <- c(r, profit)
      i <- i + tradeStep
    } else {
      i <- i + step
    }
  }

  if( length(r) < 1)
    return(data.table())
  
  return( 
    data.table(value=unlist(r), symbol=chosenSymbol, tradeStep=tradeStep, window_size=window_size, mean_window=mean_window, tradeThreshold=tradeThreshold)
    )

  #result <- as.data.table(tidy_stats(ttest))
}

# [1] c("AUDUSD", "EURJPY", "GBPCHF", "GBPUSD", "NZDUSD", "USDNOK")
# r[, t.test(value)$p.value,by=.(symbol, tradeStep, tradeThreshold)][order(symbol)][V1<0.05]

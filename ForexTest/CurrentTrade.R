source("DownloadQuotes.R")
source("Estimator.R")
require(ggplot2)
library(directlabels)
library("rTTRatesHistory", lib.loc="~/R/win-library/3.5")
library(lubridate)

PlotMeans <- function(quotes, qIndex, cTable){
    
  r <- CalculateClosestCenters(quotes, qIndex, cTable)
  distTable <- r$distTable
  sampleQuotes <- r$sampleQuotes
  
  mean_window <- cTable[1, mean_window]
  window_size <- cTable[1, window_size]
  cluster_number <- cTable[1, cluster_number]
  sampleQuotesFull <- c(sampleQuotes, rep(tail(sampleQuotes, 1),  window_size-mean_window))
  
  graphTable <- rbind( data.table(clusterIndex=0, index=1:window_size, clusterPower=100, value=sampleQuotesFull, cluster_number=cluster_number), 
                         cTable[clusterIndex %in% distTable$clusterIndex,], use.names = T, fill=T)
  graphTable[clusterIndex==0, clusterPower:=min(graphTable$clusterPower)/5]
    
  list(expectedDiff = distTable[1,expectedPrice] - tail(sampleQuotesFull, 1),
       ggplot= ggplot(graphTable, aes(x=index, y=value, group=clusterIndex, size=clusterPower, colour=clusterIndex)) + 
         geom_point() +
         geom_dl(aes(label = clusterIndex), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.8)) +
         geom_hline(yintercept = distTable[1,expectedPrice]) + 
         ggtitle(cTable[1,symbol])
  )
}
#symbols <-  c("AUDUSD", "EURJPY", "GBPUSD", "NZDUSD", "USDNOK")
#AnalyseToday("AUDUSD", threshold=0.006)
AnalyseToday <- function(symbols, threshold=0.006){
  #cTableTotal <- readRDS("cTable_latest")
  cTableTotal <- readRDS("cTable")
  currentTime <- Sys.time()
  dirOutput <- file.path("output", format(currentTime, "%Y%m%d"))
  if( !file.exists(dirOutput))
    dir.create(dirOutput, recursive = T)
  
  for(chosenSymbol in symbols){
    cTable <- cTableTotal[symbol==chosenSymbol]
    #quotes <- GetQuotes(chosenSymbol, startDate=ISOdate(2018, 11, 5, tz = "UTC"), endDate=ISOdate(2019, 01, 10, tz = "UTC"))[,.(open)]
    quotes <- GetLastQuotes(chosenSymbol, cTable[1, mean_window])

    expectedDiff <- PlotMeans(quotes, quotes[,.N-cTable[1, mean_window]+1], cTable)$expectedDiff
    if( abs(expectedDiff) >= threshold )
      ggsave(file.path(dirOutput, paste0(chosenSymbol, format(currentTime, "%H%M"), ".png")), device="png")
  }
}





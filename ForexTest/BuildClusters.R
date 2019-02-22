source("DownloadQuotes.R")

#symbols <- sort(strsplit("EURUSD,GBPUSD,AUDUSD,USDJPY,USDCAD,USDCNH,USDRUB,NZDUSD,USDCHF,AUDCAD,AUDCHF,AUDJPY,AUDNZD,CADJPY,CHFJPY,EURAUD,EURCAD,EURCHF,EURGBP,EURJPY,EURNZD,GBPAUD,GBPCHF,GBPJPY,NZDJPY,USDDKK,USDHKD,USDMXN,USDNOK,USDPLN,USDSEK,USDSGD,USDTRY,EURDKK,EURHKD,EURNOK,EURPLN,EURSEK,EURTRY,GBPCAD,GBPNZD,GBPSGD,CADCHF,HKDJPY,NOKJPY,NOKSEK,NZDCAD,NZDCHF,NZDSGD,SGDJPY,XAUUSD,XAGUSD", ",")[[1]])
#BuildCenterLines(symbols, ISOdate(2017, 1, 1, tz = "UTC"), ISOdate(2018, 1, 1, tz = "UTC"), "M15", 100, 1500, 1200, outputName="cTable1200_allEuroSymbols")
BuildCenterLines <- function(symbols, startDate, endDate, periodicity="M15", cluster_number=100, window_size=3000, mean_window=2300, 
                             outputName){
  #for(curr_symbol in symbols){
    
    curr_symbol <- Reduce(paste, symbols)
    m <- NULL
    
    for(j in seq_along(symbols)){
      quotes <- GetQuotes(symbols[[j]], startDate, endDate, periodicity, "Bids")[,.(open)]
      startIndices <- seq.int(1, nrow(quotes) - window_size+1, by=10)
      m_part <- matrix(0, nrow = length(startIndices), ncol = window_size)
      for(i in seq_along(startIndices)){
        meanPredictedWindow <- quotes[startIndices[i]:(startIndices[i]+mean_window-1), mean(open)]
        m_part[i,] <- quotes[startIndices[i]:(startIndices[i]+window_size-1), open/meanPredictedWindow] 
      }
      m <- rbind(m_part, m)
    }
    r <- kmeans(m, cluster_number, iter.max = 40)
    
    centers <- as.data.table(r$centers)
    centers [,clusterIndex:= seq_len(.N)]
    centers[,clusterPower:=r$size/sum(r$size)]

    
    cTable <- melt(centers, id.vars = c("clusterIndex", "clusterPower"))
    cTable[,index := as.numeric(variable) ]
    cTable[,variable:=NULL]

    cTable[,mean_window:=mean_window]
    cTable[,window_size:=window_size]
    cTable[,cluster_number:=cluster_number]
    cTable[,symbol:=curr_symbol]
    cTable[,startDate:=startDate]
    cTable[,endDate:=endDate]
    
    diskTable <- data.table()
    if(file.exists(outputName)){
      diskTable <- readRDS(outputName)
      ws <- window_size
      mw <- mean_window
      diskTable <- diskTable[!(symbol==curr_symbol&mean_window==mw&window_size==ws),]
    }
    diskTable <- rbind(diskTable, cTable)
    
    saveRDS(diskTable, outputName)
  #}
}

#export data for mt5
#ExportCSV2Mt5(NULL, "cTable1200_allSymbols")
ExportCSV2Mt5 <- function(chosenSymbol, cTableName = "cTable")
{
  print(chosenSymbol)
  cTable <- readRDS(cTableName)
  if( !is.null( chosenSymbol) )
    cTable <- cTable[symbol==chosenSymbol] 
  #cTable <- cTable[index<=500]
  t<-dcast(cTable, clusterIndex ~ index, drop=FALSE)
  t[,clusterIndex:=NULL]
  fwrite(t, paste0(chosenSymbol, cTableName, ".csv"), col.names=FALSE, row.names = FALSE)
}

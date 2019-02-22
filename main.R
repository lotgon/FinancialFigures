require(data.table)
require(gridExtra)
require(foreach)
require(ggplot2)
require(directlabels)

window_size <- 3000
predicted_window <- 2300
cluster_number <- 100
filePath <- "c:\\Users\\andrei.pazniak\\Documents\\Quotes\\EURUSD Ask M15  20100101  20180101.txt"
quotes <- fread(filePath, select = c(3), col.names = c("open"))
#quotes<-quotes[1:10000]
startIndices <- seq.int(1, nrow(quotes) - window_size+1, by=10)
m <- matrix(0, nrow = length(startIndices), ncol = window_size)
for(i in seq_along(startIndices)){
  meanPredictedWindow <- quotes[startIndices[i]:(startIndices[i]+predicted_window-1), mean(open)]
  m[i,] <- quotes[startIndices[i]:(startIndices[i]+window_size-1), open/meanPredictedWindow] 
}

r <- kmeans(m, cluster_number, iter.max=30)
r$withinss
#fwrite(as.data.table(r$centers), paste0("GBPUSD15_", window_size, ".csv"), col.names = FALSE)

centers <- as.data.table(r$centers)
centers [,clusterIndex:= as.factor(.I)]
centers[,window_size:=window_size]
centers[,cluster_number:=cluster_number]
centers[,clusterPower:=r$size/sum(r$size)]

cTable <- melt(centers, id.vars = c("clusterIndex","window_size","cluster_number", "clusterPower"))
cTable[,index := as.numeric(variable) ]
cTable[,variable:=NULL]
saveRDS(cTable, paste0("cTable_", cluster_number, "_", "EURUSD_test"))
  
##
startIndexSampleQuotes <- 15000
sampleQuotes <- quotes[startIndexSampleQuotes:(startIndexSampleQuotes+predicted_window-1), open/mean(open)]
sampleQuotesFull <- quotes[startIndexSampleQuotes:(startIndexSampleQuotes+window_size-1), open/mean(open)]
distTable <- cTable[index<=predicted_window,.(distance=sum(abs(value - sampleQuotes))), by=.(clusterIndex, clusterPower)]
distTable <- distTable[distance < min(distance)*1.1]

#
graphTable <- rbind( data.table(clusterIndex=0, index=1:window_size, clusterPower=100, value=sampleQuotesFull, window_size=window_size, comment="", cluster_number=cluster_number), 
                         cTable[clusterIndex %in% distTable$clusterIndex,], use.names = T, fill=T)
graphTable[clusterIndex==0, clusterPower:=min(graphTable$clusterPower)]
expectedPrice <- graphTable[index==window_size & clusterIndex!=0, sum(value*clusterPower)/sum(clusterPower)]

  ggplot(graphTable, aes(x=index, y=value, group=clusterIndex, size=clusterPower, colour=clusterIndex)) + 
  geom_point() +
  geom_dl(aes(label = clusterIndex), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.8))  + 
  geom_hline(yintercept = expectedPrice)

distTable


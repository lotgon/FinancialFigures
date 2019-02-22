source("Estimator.R", local = T)

cluster_number <- 100

GetIndicatorsList <- function(){
  list("None", "CenterIndicator")
}

cTable <<- data.table()
GetCenterTable <- function(){
  if( nrow(cTable)==0){
    cTable <<- readRDS("cTable")
    currSymbol <- symbol
    cTable <<- cTable[symbol==currSymbol]
  }
  cTable
}

QuestionPlot <- function(qoutes, indicatorName){
  if("CenterIndicator" == indicatorName ){
    return( PlotCenterIndicator(quotes, -1)) 
  }
  qIndex <- GenerateNextRandomQuestionIndex()
  meanPredictedWindow <- quotes[qIndex:(qIndex+predicted_window-1), mean(open)]
  list(qIndex=qIndex, 
    ggplot=ggplot(quotes[qIndex:(qIndex+predicted_window-1)], 
                 aes(x=1:predicted_window, y=open/meanPredictedWindow)) + geom_point(colour = 'red') 
  )
}

GenerateNextRandomQuestionIndex <- function(){
  round(runif(1, 1, quotes[,.N]-window_size))
}

#PlotCenterIndicator(quotes, 1)
PlotCenterIndicator <- function( quotes, qIndex=-1){

  isQuestion <- TRUE
  if( qIndex >=0 )
    isQuestion <- FALSE

  repeat{
    if( isQuestion )
      qIndex <- NextRandomQuestionIndex()
    
    meanPredictedWindow <- quotes[qIndex:(qIndex+predicted_window-1), mean(open)]
    #sampleQuotes <- quotes[qIndex:(qIndex+predicted_window-1), open/meanPredictedWindow]
    cTable <- GetCenterTable()
    #distTable <- cTable[index<=predicted_window,.(distance=sum(abs(value - sampleQuotes))), by=.(clusterIndex, clusterPower)]
    #distTable <- distTable[distance < min(distance)*1.1]
    r <- CalculateClosestCenters(quotes, qIndex, cTable)
    sampleQuotes <- r$sampleQuotes
    distTable <- r$distTable
    
    if( isQuestion==FALSE)
      sampleQuotesFull <- quotes[qIndex:(qIndex+window_size-1), open/meanPredictedWindow]
    else
      sampleQuotesFull <- c(sampleQuotes, rep(tail(sampleQuotes, 1),  window_size-predicted_window))
      
    graphTable <- rbind( data.table(clusterIndex=0, index=1:window_size, clusterPower=100, value=sampleQuotesFull, cluster_number=cluster_number), 
                         cTable[clusterIndex %in% distTable$clusterIndex,], use.names = T, fill=T)
    graphTable[clusterIndex==0, clusterPower:=min(graphTable$clusterPower)/5]
    
    expectedPrice <- distTable[1,expectedPrice]
      
    if( abs(expectedPrice-sampleQuotesFull[predicted_window]) > 0.006 )
      break
  }
  list(qIndex= qIndex, 
    ggplot=ggplot(graphTable, aes(x=index, y=value, group=clusterIndex, size=clusterPower, colour=clusterIndex)) + 
      geom_point() +
      geom_dl(aes(label = clusterIndex), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.8)) +
      geom_hline(yintercept = expectedPrice)
  )
}

AnswerPlot <- function(qoutes, indicatorName, qIndex){
  if("CenterIndicator" == indicatorName ){
    return( PlotCenterIndicator(quotes, qIndex)) 
  }
  
  meanPredictedWindow <- quotes[qIndex:(qIndex+predicted_window-1), mean(open)]
  list(qIndex=qIndex, 
       ggplot=ggplot(quotes[qIndex:(qIndex+window_size-1)], 
         aes(x=1:window_size, y=open/meanPredictedWindow)) + geom_point(colour = 'red', size=1) + geom_hline(yintercept = quotes[qIndex+predicted_window-1,open/meanPredictedWindow] )
  )
}

  
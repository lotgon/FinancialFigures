library(shiny)
library(data.table)
library(ggplot2)
library(directlabels)
source("..//shinyModel.R", local = T)
source("PlotModel.R", local = T)

window_size <<- 3000
qNumber <<- 13
predicted_window <<- 2300
symbol <<- "AUDCHF"
quotes <<- fread(".\\Quotes\\AUDCHF Ask M15  20180101  20181101.csv", select = c(3), col.names = c("open"))
#GetQuotes(symbol, ISOdate(2018, 1, 1, tz = "UTC"), ISOdate(2018, 12, 1, tz = "UTC"))[,.(open)]
function(input, output, session) {
  
  results <- data.table()
  questions <- numeric(qNumber) 
  #questions <- round(runif(1, 1, quotes[,.N]-2*window_size)) + (0:qNumber)*100
  balance <- 1000

  # Hit counter
  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) counter <- 0
      if (file.exists("counter.Rdata")) load(file="counter.Rdata")
      counter <- counter <<- counter + 1
      
      save(counter, file="counter.Rdata")     
      paste0("Hits: ", counter)
    })
  
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  
  output$header <- renderUI({
    reactive({
      if (input$Click.Counter>0){
        return( list(h4(textOutput("profit")) ))
      }
    }) ()
  })
  output$profit <- renderText({ 
      qi <- ceiling(input$Click.Counter/2)
      if(input$Click.Counter%%2 == 0)
        paste0("Current Balance: ", balance, ". Last Profit: ", results[1,ActualProfit])
      else
        paste0("Current Balance: ", balance, ". Chosen indicator: ", input$InputIndicator)
      })

  dynamicUi <- reactive({
    if (input$Click.Counter==0) 
      return(
        list(
          h5("Welcome to Forex Test Tool!"),
          h6("by Andrei Pazniak"), 
          textInput("Nickname", "Your name", ""),
          selectInput("InputIndicator", "Indicator", GetIndicatorsList())
          )
      )
    
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter>0 & input$Click.Counter<=2*qNumber & input$Click.Counter%%2==1)  
      return(
        list(
          h5(textOutput("question")),
          plotOutput("plotQuestion"),
          radioButtons("survey", "Please Select:", option.list())
        )
      )
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter>0 & input$Click.Counter<=2*qNumber & input$Click.Counter%%2==0)  
      return(
        list(
          h5(textOutput("answer")),
          plotOutput("plotAnswer")
        )
      )
    
    # Finally we see results of the survey as well as a
    # download button.
    if (input$Click.Counter>2*qNumber)
      return(
        list(
          h4("View aggregate results"),
          tableOutput("surveyresults"),
          h4(textOutput("summaryResult")),
          "Thanks for taking the survey!"
          #actionButton('saveRecord', 'saveRecord'),
        )
      )    
  })
  
  output$save.results <- renderText({
    qi <- ceiling(input$Click.Counter/2)
    
    if( (input$Click.Counter>0) & (input$Click.Counter<=2*qNumber) & input$Click.Counter%%2 ==0){
      res <- EstimateProfit(quotes, questions[qi] + predicted_window, questions[qi] + window_size, input$survey)
      balance <<- balance+res$ActualProfit
      results <<- rbind(res, results)
    }
    
    if( input$Click.Counter == 2*qNumber + 1){
      oldResult <- data.table()
      if( file.exists("oldresults"))
        oldResult <- readRDS(file="oldresults")
      
      results[,Player:=input$Nickname]
      results[, Id:=counter]
      results[,time:=Sys.time()]
      results[,sumProfit:=sum(ActualProfit)]
      results[,Performance:=round(100*results[, (sumProfit-sum(MinProfit))/(sum(MaxProfit)-sum(MinProfit))], 1)]
      results[,Indicator:=input$InputIndicator]
      results <- rbind(results, oldResult)
      saveRDS(results, file="oldresults")
    }
    
    
    ""
  })
  
  # This function renders the table of results from the
  # survey.
  output$surveyresults <- renderTable({
    results
  })
  output$summaryResult <- renderText({
    paste0("Your profit: ", results[,sum(ActualProfit)], 
           ". Your profit / (best profit-worst profit): ", round(100*results[, (sum(ActualProfit)-sum(MinProfit))/(sum(MaxProfit)-sum(MinProfit))], 1), "%"
    )
  })
  
  # This renders the data downloader
  output$downloadData <- downloadHandler(
    filename = "IndividualData.csv",
    content = function(file) {
      write.csv(presults, file)
    }
  )
  
  option.list <- reactive({
    GetAllOptions()
  })
  
  output$question <- renderText({
    qi <- ceiling(input$Click.Counter/2)
    paste0("Q", qi,":")
  })
  output$answer <- renderText({
    qi <- ceiling(input$Click.Counter/2)
    paste0("Answer", qi,":")
  }) 
  output$plotQuestion <- renderPlot({
    if( input$Click.Counter%%2!=1 )
      return()
    
    qi <- ceiling(input$Click.Counter/2)
    r <- QuestionPlot(quotes, input$InputIndicator)
    questions[qi] <<- r$qIndex
    r$ggplot
    
  })
  output$plotAnswer <- renderPlot({
    if( input$Click.Counter%%2!=0 )
      return()
    qi <- ceiling(input$Click.Counter/2)
    r <- AnswerPlot(quotes, input$InputIndicator, questions[qi])
    r$ggplot
    
  })
  
  
}
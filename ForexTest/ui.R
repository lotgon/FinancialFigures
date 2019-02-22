library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Forex Test Tool v0.1"),
  
  sidebarPanel(
    h6(textOutput("save.results")),
    h5(textOutput("counter"))
    
    
  ),
  
  
  # Show a table summarizing the values entered
  mainPanel(
    # Main Action is where most everything is happenning in the
    # object (where the welcome message, survey, and results appear)
    uiOutput("header"),
    uiOutput("MainAction"),
    # This displays the action putton Next.
    actionButton("Click.Counter", "Next")    
  )
))
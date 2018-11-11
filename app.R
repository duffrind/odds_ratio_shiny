#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
library(ggplot2)
#library(shinyEvents)

DF = data.frame(Lab_Test = c('Potassium', 'Sodium', 'Chloride', 'Creatine'),
                Value = c(-1, -1, -1, -1),
                Units = c('mmol/L', 'mmol/L', 'mmol/L', 'mg/dL'),
                stringsAsFactors = FALSE)

#disease = c('COPD')
#test = c('Potassium')
#interval = c('20')

#disease = 'KidneyDisease'
#test = 'Potassium'
#interval = '18'

#disease = 'Dementia'
#test = 'Potassium'
#interval = '11'

#disease = 'Cancer'
#test = 'Potassium'
#interval = '17'

#disease = 'Diabetes'
#test = 'Potassium'
#interval = '20'

#disease = 'Diabetes'
#test = 'Glucose'
#interval = '80'


#intervals = c(read.csv(paste0('medical_data/', interval, '_intervals_', test, '_', disease, '.csv')),
#              read.csv(paste0('medical_data/', interval, '_intervals_', test, '_', disease, '.csv')),
#              read.csv(paste0('medical_data/', interval, '_intervals_', test, '_', disease, '.csv')),
#              read.csv(paste0('medical_data/', interval, '_intervals_', test, '_', disease, '.csv')))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Patient Odds Ratios"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
         tags$hr(),
         textInput("patName", "Patient Name"),
         rHandsontableOutput("lab"),
         checkboxInput("refShow",
                     "Show Reference Ranges?"),
         selectInput('comorbidity', 'Disease Dropdown', c('Diabetes', 'Kidney Disease', 'Cancer', 'Alzheimers'), selected = NULL, multiple = FALSE,
                     selectize = TRUE, width = NULL, size = NULL),
         actionButton("makePlots", "Plot!")
      ),
      
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output, session) {
  #rhandsontable(DF) %>% hot_col("Lab_Test", readOnly = TRUE) %>% hot_col("Units", readOnly = TRUE)
  output$lab=renderRHandsontable(rhandsontable(DF,readOnly=FALSE) %>% hot_col("Lab_Test", readOnly = TRUE) %>% hot_col("Units", readOnly = TRUE))
  
  observeEvent(input$makePlots, {doPlot()})
  
  doPlot <- eventReactive(input$makePlots, {
    labs = hot_to_r(input$lab)
    print(c(input$refShow,input$comorbidity,labs$Value))
    output$distPlot <- renderPlot({ 
      par(mfrow=c(2,2))
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = 30)
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
  })
  
  observeEvent(input$file1, {
    req(input$file1)
    
    tryCatch(
      {
        req(input$file1)
        file_df <- read.csv(input$file1$datapath, header=FALSE)
        labs_df <- hot_to_r(input$lab) # DF # (if you want to set all missing to -1)
          for (i in 1:nrow(file_df)) {
            switch(as.character(file_df$V1[i]),
                   'Disease' = updateSelectInput(session, 'comorbidity', selected=as.character(file_df$V2[i])),
                   'Name' = updateTextInput(session, 'patName', value = as.character(file_df$V2[i])),#output$patName <- as.character(file_df$V2[i]),
                   'Potassium' = labs_df$Value[1] <- as.numeric(as.character(file_df$V2[i])),
                   'Sodium' = labs_df$Value[2] <- as.numeric(as.character(file_df$V2[i])),
                   'Chloride' = labs_df$Value[3] <- as.numeric(as.character(file_df$V2[i])),
                   'Creatine' = labs_df$Value[4] <- as.numeric(as.character(file_df$V2[i])))}
        output$lab = renderRHandsontable({rhandsontable(labs_df,readOnly=FALSE) %>% hot_col("Lab_Test", readOnly = TRUE) %>% hot_col("Units", readOnly = TRUE)})
        
      },
      error = function(e) {
        stop(safeError(e))
      }
  )})
  
}

# Run the application 
shinyApp(ui = ui, server = server)


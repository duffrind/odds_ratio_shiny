library(shiny)
library(rhandsontable)
library(ggplot2)
library(gridExtra)
library(grid)

patients = read.csv('PatientList1.csv')

DF = data.frame(Lab_Test = c('BUN', 'Chloride', 'Creatinine', 'Glucose', 'Potassium'),#, 'Sodium'),
                Value = c(-1, -1, -1, -1, -1),#, -1),
                Units = c('mg/dL', 'mmol/L', 'mg/dL', 'mg/dL', 'mmol/L'),#, 'mmol/L'),
                stringsAsFactors = FALSE)

cancer_bun <- read.csv('cancer/bun.csv')
cancer_chloride <- read.csv('cancer/chloride.csv')
cancer_creatinine <- read.csv('cancer/creatinine.csv')
cancer_glucose <- read.csv('cancer/glucose.csv')
cancer_potassium <- read.csv('cancer/potassium.csv')
cancer_sodium <- read.csv('cancer/sodium.csv')

copd_bun <- read.csv('copd/bun.csv')
copd_chloride <- read.csv('copd/chloride.csv')
copd_creatinine <- read.csv('copd/creatinine.csv')
copd_glucose <- read.csv('copd/glucose.csv')
copd_potassium <- read.csv('copd/potassium.csv')

dementia_bun <- read.csv('dementia/bun.csv')
dementia_chloride <- read.csv('dementia/chloride.csv')
dementia_creatinine <- read.csv('dementia/creatinine.csv')
dementia_glucose <- read.csv('dementia/glucose.csv')
dementia_potassium <- read.csv('dementia/potassium.csv')

diabetes_bun <- read.csv('diabetes/bun.csv')
diabetes_chloride <- read.csv('diabetes/chloride.csv')
diabetes_glucose <- read.csv('diabetes/glucose.csv')
diabetes_potassium <- read.csv('diabetes/potassium.csv')
diabetes_sodium <- read.csv('diabetes/sodium.csv')

kidney_bun <- read.csv('kidney/bun.csv')
kidney_chloride <- read.csv('kidney/chloride.csv')
kidney_creatinine <- read.csv('kidney/creatinine.csv')
kidney_glucose <- read.csv('kidney/glucose.csv')
kidney_potassium <- read.csv('kidney/potassium.csv')

plotFunc <- function(x_given, intervals, refBars, test, disease, measurement, bin_size) {
  switch(test,
         'Potassium' = {left <- 3.5; right <- 5.1},
         'BUN' = {left <- 7; right <- 20},#{left <- 22; right <- 46},
         'Chloride' = {left <- 94; right <- 110},#{left <- 96; right <- 106},
         'Creatinine' = {left <- 0.8; right <- 1.3},#{left <- 7.5; right <- 15},
         'Glucose' = {left <- 0; right <- 140},#{left <- 70; right <- 110},
         'Sodium' = {left <- 136; right <- 145})#{left <- 135; right <- 145})
  if (disease == 'Kidney') {
    disease <- 'Kidney Disease'
  }
  intervals$constant = rep(1, length(intervals$mean))
  point = approx(x = intervals$mean, y = intervals$cc_or, xout = x_given)
  border_colour = 'black'
  point_colour = 'black'
  if (is.na(point$y) | point$y > 1) {
    point_colour = 'red'
  }
  if ((is.na(point$y) | point$y > 1) & (point$x < left | point$x > right)) {
    border_colour = 'red'
  } else if (is.na(point$y) | point$y > 1 | (point$x < left | point$x > right)) {
    border_colour = 'yellow'
  }
  if (refBars) {
    intervals['plot_n'] <- intervals$n / max(intervals$n) * 5
    #width <- intervals$plot_n / sum(intervals$plot_n)
    #loc <- cumsum(width) - width/2
  }
  p <- ggplot(intervals, aes(mean)) + geom_line(aes(y=cc_or), colour='blue') +
    geom_line(aes(y=cc_ci_l, color='cc_ci_l'), linetype='dashed', colour='purple') +
    scale_y_continuous(breaks=seq(0, 10, 1)) +
    geom_line(aes(y=cc_ci_r), linetype='dashed', colour='purple') +
    geom_abline(slope=0, intercept=1, size=1) + coord_cartesian(ylim=c(0,6), xlim=c(min(intervals$mean),max(intervals$mean))) +
    theme_bw() + geom_point(aes(x=point$x, y=point$y), size=5, color=point_colour) +
    theme(panel.border = element_rect(colour = border_colour, fill=NA, size=5)) +
    labs(x=paste0(test, ', ', measurement), y='Mortality Odds Ratio', title=paste0(test, ' Odds Ratios +/- 95% C.I. for ', disease))
  if (bin_size) {
    #p <- p + geom_bar(stat='identity', aes(y=plot_n), colour='lightblue', fill='lightblue', alpha=0.5, width=width) #+ scale_x_continuous(breaks = loc)
    p <- p + geom_area(aes(y=plot_n), colour='lightblue', fill='lightblue', alpha=0.1)
  }
  if (refBars) {
    p <- p + geom_vline(xintercept=left, linetype='longdash') + geom_vline(xintercept=right, linetype='longdash') + geom_rect(aes(xmin=left, xmax=right, ymin=-Inf, ymax=Inf), alpha=.01)
  }
  #p <- p + theme(plot.background = element_rect(fill = "#C4E7FF"),
  #               #panel.background = element_blank(),
  #               plot.margin = margin(20, 20, 20, 20))
  #g <- ggplotGrob(p)
  #bg <- g$grobs[[1]]
  #round_bg <- roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
  #                          r=unit(0.1, "snpc"),
  #                          just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
  #g$grobs[[1]] <- round_bg
  return(p)#g
}

ui <- fluidPage(
   titlePanel("Patient Mortality Odds Ratios"),
   
   sidebarLayout(
      sidebarPanel(
         #h1('Mortality Odds Ratios'),
         #hr(),
         selectizeInput('patName', 'Patient Name', choices=patients$PatientName, selected = patients$PatientName[1], multiple = FALSE, options = NULL),
         rHandsontableOutput("lab"),
         checkboxInput("refShow",
                     "Show Reference Ranges?", value=TRUE),
         checkboxInput("bin_size",
                       "Show Population Distribution?", value=FALSE),
         selectInput('comorbidity', 'Disease Dropdown', c('Diabetes', 'Kidney Disease', 'Cancer', 'Dementia', 'COPD'), selected = as.character(patients$CC[1]), multiple = FALSE,
                     selectize = TRUE, width = NULL, size = NULL),
         hr(),
         actionButton("makePlots", "Plot!"),
         hr(),
         h5('Legend:'),
         #p('Mortality Odds Ratios - Description will go here'),
         HTML('<style>.rdot {height: 12px;width: 12px;background-color: red;border-radius: 50%;display: inline-block;}.bdot {height: 12px;width: 12px;background-color: black;border-radius: 50%;display: inline-block;}</style>'),
         HTML('<p><small><span class="bdot"></span> Mortality Odds below 1</small></p>'),
         HTML('<p><small><span class="rdot"></span> Mortality Odds above 1</small></p>'),
         HTML('<p><small><svg width="12" height="12"><rect width="12" height="12" style="fill:rgb(255,255,255);stroke-width:3;stroke:rgb(0,0,0)" /></svg> Mortality Odds below 1 and within Reference Intervals</small></p>'),
         HTML('<p><small><svg width="12" height="12"><rect width="12" height="12" style="fill:rgb(255,255,255);stroke-width:3;stroke:yellow" /></svg> Mortality Odds above 1 or outside Reference Intervals</small></p>'),
         HTML('<p><small><svg width="12" height="12"><rect width="12" height="12" style="fill:rgb(255,255,255);stroke-width:3;stroke:red" /></svg> Mortality Odds above 1 and outside Reference Intervals</small></p>'),
         HTML('<p><small><svg width="12" height="12"><rect width="12" height="12" style="fill:rgb(175,175,175);stroke-width:3;stroke:rgb(175,175,175)" /></svg> Shading inside of the Reference Interval</small></p>'),
         HTML('<p><small><svg width="12" height="12"><rect width="12" height="12" style="fill:rgb(240,255,255);stroke-width:3;stroke:rgb(240,255,255)" /></svg> Shading showing the population distribution</small></p>')
      ),
      
      mainPanel(
         plotOutput("distPlot", height='90vh')
      )
   )
)

server <- function(input, output, session) {
  labs_df <- DF
  labs_df$Value <- c(patients$BUN[1], patients$Chloride[1], patients$Creatinine[1], patients$Glucose[1], patients$Potassium[1])#, patients$Sodium[1])
  output$lab=renderRHandsontable(rhandsontable(labs_df,readOnly=FALSE) %>% hot_col("Lab_Test", readOnly = TRUE) %>% hot_col("Units", readOnly = TRUE))
  
  observeEvent(input$makePlots, {
    output$distPlot <- renderPlot({ 
      labs_df <- hot_to_r(isolate(input$lab))
      refBars <- FALSE
      if (isolate(input$refShow)) {
        refBars <- TRUE
      }
      showBin <- FALSE
      if (isolate(input$bin_size)) {
        showBin <- TRUE
      }
      comorb <- isolate(input$comorbidity)
      if (comorb == 'COPD') {
        p1 <- plotFunc(labs_df$Value[1], copd_bun, refBars, 'BUN', 'COPD', 'mg/dL', showBin)
        p2 <- plotFunc(labs_df$Value[2], copd_chloride, refBars, 'Chloride', 'COPD', 'mmol/L', showBin)
        p3 <- plotFunc(labs_df$Value[3], copd_creatinine, refBars, 'Creatinine', 'COPD', 'mg/dL', showBin)
        #p4 <- plotFunc(labs_df$Value[4], copd_glucose, refBars, 'Glucose', 'COPD', 'mg/dL', showBin)
        p5 <- plotFunc(labs_df$Value[5], copd_potassium, refBars, 'Potassium', 'COPD', 'mmol/L', showBin)
        grid.arrange(p1, p2, p3, p5, ncol=2)
      } else if (comorb == 'Diabetes') {
        p1 <- plotFunc(labs_df$Value[1], diabetes_bun, refBars, 'BUN', 'Diabetes', 'mg/dL', showBin)
        p2 <- plotFunc(labs_df$Value[2], diabetes_chloride, refBars, 'Chloride', 'Diabetes', 'mmol/L', showBin)
        p3 <- plotFunc(labs_df$Value[4], diabetes_glucose, refBars, 'Glucose', 'Diabetes', 'mg/dL', showBin)
        p4 <- plotFunc(labs_df$Value[5], diabetes_potassium, refBars, 'Potassium', 'Diabetes', 'mmol/L', showBin)
        #p5 <- plotFunc(labs_df$Value[6], diabetes_sodium, refBars, 'Sodium', 'Diabetes', 'mmol/L', showBin)
        grid.arrange(p1, p2, p3, p4, ncol=2)
      } else if (comorb == 'Kidney Disease') {
        p1 <- plotFunc(labs_df$Value[1], kidney_bun, refBars, 'BUN', 'Kidney', 'mg/dL', showBin)
        p2 <- plotFunc(labs_df$Value[2], kidney_chloride, refBars, 'Chloride', 'Kidney', 'mmol/L', showBin)
        p3 <- plotFunc(labs_df$Value[3], kidney_creatinine, refBars, 'Creatinine', 'Kidney', 'mg/dL', showBin)
        #p4 <- plotFunc(labs_df$Value[4], kidney_glucose, refBars, 'Glucose', 'Kidney', 'mg/dL', showBin)
        p5 <- plotFunc(labs_df$Value[5], kidney_potassium, refBars, 'Potassium', 'Kidney', 'mmol/L', showBin)
        grid.arrange(p1, p2, p3, p5, ncol=2)
      } else if (comorb == 'Cancer') {
        p1 <- plotFunc(labs_df$Value[1], cancer_bun, refBars, 'BUN', 'Cancer', 'mg/dL', showBin)
        p2 <- plotFunc(labs_df$Value[2], cancer_chloride, refBars, 'Chloride', 'Cancer', 'mmol/L', showBin)
        p3 <- plotFunc(labs_df$Value[3], cancer_creatinine, refBars, 'Creatinine', 'Cancer', 'mg/dL', showBin)
        #p4 <- plotFunc(labs_df$Value[4], cancer_glucose, refBars, 'Glucose', 'Cancer', 'mg/dL', showBin)
        p5 <- plotFunc(labs_df$Value[5], cancer_potassium, refBars, 'Potassium', 'Cancer', 'mmol/L', showBin)
        #p6 <- plotFunc(labs_df$Value[6], cancer_sodium, refBars, 'Sodium', 'Cancer', 'mmol/L', showBin)
        grid.arrange(p1, p2, p3, p5, ncol=2)
      } else if (comorb == 'Dementia') {
        p1 <- plotFunc(labs_df$Value[1], dementia_bun, refBars, 'BUN', 'Dementia', 'mg/dL', showBin)
        p2 <- plotFunc(labs_df$Value[2], dementia_chloride, refBars, 'Chloride', 'Dementia', 'mmol/L', showBin)
        p3 <- plotFunc(labs_df$Value[3], dementia_creatinine, refBars, 'Creatinine', 'Dementia', 'mg/dL', showBin)
        #p4 <- plotFunc(labs_df$Value[4], dementia_glucose, refBars, 'Glucose', 'Dementia', 'mg/dL', showBin)
        p5 <- plotFunc(labs_df$Value[5], dementia_potassium, refBars, 'Potassium', 'Dementia', 'mmol/L', showBin)
        grid.arrange(p1, p2, p3, p5, ncol=2)
      }
    })
  })
  
  observeEvent(input$patName, {
    req(input$patName)
    
    tryCatch(
      {
        labs_df <- DF
        index <- match(input$patName, patients$PatientName)
        labs_df$Value <- c(patients$BUN[index], patients$Chloride[index], patients$Creatinine[index], patients$Glucose[index], patients$Potassium[index])#, patients$Sodium[index])
        output$lab = renderRHandsontable({rhandsontable(labs_df,readOnly=FALSE) %>% hot_col("Lab_Test", readOnly = TRUE) %>% hot_col("Units", readOnly = TRUE)})
        updateSelectInput(session, 'comorbidity', selected=as.character(patients$CC[index]))
        req(input$patName)
      },
      error = function(e) {
        stop(safeError(e))
      }
  )})
  
}

shinyApp(ui = ui, server = server)


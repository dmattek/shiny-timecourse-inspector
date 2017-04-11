

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(plotly)

shinyUI(fluidPage(
  useShinyjs(),
  # Include shinyjs
  
  # Application title
  title = "Timecourse Inspector",
  
  sidebarLayout(
    sidebarPanel(
      h4("Load data files"),
      
      #Selector for file upload
      fileInput(
        'inFileLoadNuc',
        'Select file (e.g. tCoursesSelected.csv) and press "Load Data"',
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      actionButton("inButLoadNuc", "Load Data"),
      actionButton("butReset", "Reset file input"),
      actionButton('inDataGen1', 'Generate artificial dataset'),
      tags$hr(),
      uiOutput('varSelSite'),
      uiOutput('varSelTrackLabel'),
      uiOutput('varSelGroup'),
      uiOutput('varSelTime'),
      uiOutput('varSelMeas1'),
      radioButtons(
        'inSelMath',
        'Math operation 1st and 2nd meas.:',
        c(
          'None' = '',
          'Divide' = " / ",
          'Sum' = " + ",
          'Multiply' = " * ",
          'Subtract' = ' - ',
          '1 / X' = '1 / '
        )
      ),
      uiOutput('varSelMeas2')
    ),
    
    mainPanel(tabsetPanel(
      tabPanel(
        "Time courses",
        br(),
        fluidRow(
          column(
            4,
            numericInput(
              'inPlotTrajFacetNcol',
              '#Columns:',
              value = 4,
              min = 1,
              width = '100px',
              step = 1
            )
          ),
          column(
            4,
            numericInput(
              'inPlotTrajHeight',
              'Height [px]:',
              value = 800,
              min = 100,
              width = '100px',
              step = 50
            )
          ),
          column(
            4,
            numericInput(
              'inPlotTrajWidth',
              'Width [%]:',
              value = 100,
              min = 10,
              max = 100,
              width = '100px',
              step = 10
            )
          )
        ),
        checkboxInput('chBhighlightTraj', 'Highlight trajectories?', FALSE),
        uiOutput('varSelHighlight'),
        br(),
        actionButton('butPlotTraj', 'Plot!'),
        uiOutput('uiPlotTraj')
      ),
      tabPanel("Box-plots",
               br(),
               fluidRow(
                 column(
                   6,
                   checkboxInput('inPlotBoxNotches', 'Box plot notches?', FALSE),
                   checkboxInput('inPlotBoxOutliers', 'Box plot outliers?', TRUE)
                 ),
                 column(
                   6,
                   selectInput('selPlotBoxLegendPos', 
                               label = 'Select legend position',
                               choices = list(
                                 "Top" = 'top',
                                 "Right" = 'right',
                                 "Bottom" = 'bottom'
                               ),
                               selected = 'top')
                 )
               ),
               
               uiOutput('varSelTpts'),
               
               actionButton('butPlotBox', 'Plot!'),
               plotOutput('outPlotBox', height = 800),
               h4('Download plot'),
               fluidRow(
                 column(
                   3,
                   numericInput(
                     'inPlotBoxWidth',
                     "Width",
                     10,
                     min = 1,
                     width = 100
                   )
                 ),
                 column(
                   3,
                   numericInput(
                     'inPlotBoxHeight',
                     "Height",
                     7,
                     min = 1,
                     width = 100
                   )
                 ),
                 column(6,
                        downloadButton('downPlotBox', 'PDF'))
               )
               #uiOutput('uiPlotBox')
      )
    ))
  )
))
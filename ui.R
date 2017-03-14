
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(plotly)

shinyUI(fluidPage(
  useShinyjs(), # Include shinyjs
  
  # Application title
  title = "Timecourse Inspector",
  
  fluidRow(
    column(3,
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
           
           # fileInput(
           #   'inFileStimLoad',
           #   'Choose CSV file with stimulation times, e.g. stimT.csv',
           #   accept = c('text/csv', 'text/comma-separated-values,text/plain')
           # ),
           
           h4("Plot format"),
           fluidRow(
             column(4, 
                    numericInput('inFacetNcol', '#Columns:', value = 4, min = 1, width = '100px', step = 1)),
             column(4, 
                    numericInput('inPlotHeight', 'Height [px]:', value = 800, min = 100, width = '100px', step = 50)),
             column(4, 
                    numericInput('inPlotWidth', 'Width [%]:', value = 100, min = 10, max = 100, width = '100px', step = 10))
           ),
           
           actionButton('butGo', 'Plot!')),
    
    column(3, offset = 1,
           uiOutput('varSelSite'),
           uiOutput('varSelTrackLabel'),
           uiOutput('varSelTime'),
           uiOutput('varSelMeas1'),
           radioButtons('inSelMath', 'Math operation 1st and 2nd meas.:', c('None' = '', 
                                                                            'Divide' = " / ", 
                                                                            'Sum' = " + ", 
                                                                            'Multiply' = " * ", 
                                                                            'Subtract' = ' - ',
                                                                            '1 / X' = '1 / ')),
           uiOutput('varSelMeas2')),
  
    column(3, offset = 1,
           uiOutput('varSelGroup'))
    ),
  br(),
  uiOutput('uiPlot')
))


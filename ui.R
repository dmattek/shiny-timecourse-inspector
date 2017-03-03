
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
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
             'inFileNucLoad',
             'Choose CSV file with measurement data, e.g. tCoursesSelected.csv',
             accept = c('text/csv', 'text/comma-separated-values,text/plain')
           ),
           
           fileInput(
             'inFileStimLoad',
             'Choose CSV file with stimulation times, e.g. stimT.csv',
             accept = c('text/csv', 'text/comma-separated-values,text/plain')
           ),
           
           actionButton("butReset", "Reset file input"),
           actionButton('butDataGen', 'Generate artificial dataset'),
           actionButton('butGo', 'Go!')),
    
    column(4, offset = 1,
           uiOutput('varSelSite'),
           uiOutput('varSelTrackLabel'),
           uiOutput('varSelTime'),
           uiOutput('varSelMeas1'),
           uiOutput('varSelRatio'),
           uiOutput('varSelMeas2')),
  
    column(2, offset = 1,
           numericInput('inFacetNcol', 'No. of plot columns:', value = 4, min = 1, width = '150px', step = 1),
           numericInput('inPlotHeight', 'Plot Height [px]:', value = 400, min = 100, width = '150px', step = 50),
           numericInput('inPlotWidth', 'Plot Width [%]:', value = 100, min = 10, max = 100, width = '150px', step = 10))
  ),
  br(),
  uiOutput('outPlot')
))


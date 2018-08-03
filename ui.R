

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
      #Selector for file upload
      fileInput(
        'inFileLoadNuc',
        'Select data file (e.g. tCoursesSelected.csv) and press "Load Data"',
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      actionButton("inButLoadNuc", "Load Data"),
      actionButton("butReset", "Reset file input"),
      actionButton('inDataGen1', 'Generate artificial dataset'),
      
      tags$hr(),
      checkboxInput('chBtrajRem', 'Upload IDs to remove'),
      helpPopup(
        title = 'Remove time series',
        content = help.text[1],
        placement = 'right',
        trigger = 'hover'
      ),
      
      uiOutput('uiFileLoadTrajRem'),
      uiOutput('uiButLoadTrajRem'),
      
      tags$hr(),
      checkboxInput('chBtrajInter', 'Interpolate NAs and missing data?', value = T),
      helpPopup(
        title = 'Interpolation of NAs and missing data',
        content = help.text[3],
        placement = 'right',
        trigger = 'hover'
      ),
      
      uiOutput('varSelTimeFreq'),
      checkboxInput('chBtrackUni', 'Create unique TrackLabel', T),
      helpPopup(
        title = 'Create unique cell ID',
        content = help.text[2],
        placement = 'right',
        trigger = 'hover'
      ),
      uiOutput('varSelSite'),
      uiOutput('varSelTrackLabel'),
      
      tags$hr(),
      checkboxInput('chBgroup', 'Dataset contains grouping column (e.g. treatment, condition)', TRUE),                
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
      uiOutput('varSelMeas2'),
      
      tags$hr(),
      checkboxInput('chBtimeTrim', 'Trim x-axis', FALSE),
      uiOutput('uiSlTimeTrim'),
      tags$hr(),
      checkboxInput('chBnorm', 'Normalization', FALSE),
      uiOutput('uiChBnorm'),
      uiOutput('uiSlNorm'),
      uiOutput('uiChBnormRobust'),
      uiOutput('uiChBnormGroup'),
      tags$hr(),
      checkboxInput('chBoutliers', 'Remove outliers', FALSE),
      uiOutput('uiSlOutliers'),
      uiOutput("uiTxtOutliers"),
      downloadButton('downloadDataClean', 'Download mod\'d data')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Time series",
          h4(
            "Plot time series: means per group or individual"
          ),
          br(),
          
          tabsetPanel(
            tabPanel("Means",
                     br(),
                     modTrajRibbonPlotUI('modTrajRibbon')
            ),
            
            tabPanel(
              "Individual",
              br(),
              checkboxInput('chBhighlightTraj', 'Highlight trajectories?', FALSE),
              uiOutput('varSelHighlight'),
              br(),
              modTrajPlotUI('modTrajPlot')
            )
          )
        ),
        
        tabPanel(
          "AUC",
          modAUCplotUI('tabAUC')
        ),
        
        tabPanel(
          "Box-plots",
          tabBoxPlotUI('tabBoxPlot')
        ),
        
        
        # scatter plot
        tabPanel(
          'Scatter',
          tabScatterPlotUI('tabScatter')
        ),
        
        tabPanel(
          'Hierarchical',
          clustHierUI('tabClHier')
        )
        
        # sparse hierarchical clustering package sparcl temporarily unavailable from CRAN
        # tabPanel(
        #   'Hierarchical Sparse',
        #   clustHierSparUI('tabClHierSpar')
        #)
      ))
  )
))
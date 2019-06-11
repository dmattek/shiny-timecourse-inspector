#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This is the user-interface definition for a Shiny web application.
#


library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(shinyBS)

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
        'Select main data file and press "Load Data"',
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      actionButton("inButLoadNuc", "Load Data"),
      actionButton("butReset", "Reset file input"),
      actionButton('inDataGen1', 'Synthetic data'),
      
      tags$hr(),
      checkboxInput('chBtrajRem', 'Upload IDs to remove'),
      bsTooltip('chBtrajRem', help.text.short[1], placement = "right", trigger = "hover", options = NULL),
      
      uiOutput('uiFileLoadTrajRem'),
      uiOutput('uiButLoadTrajRem'),
      
      #tags$hr(),
      checkboxInput('chBstim', 'Upload stimulation pattern'),
      bsTooltip('chBstim', help.text.short[4], placement = "right", trigger = "hover", options = NULL),
      
      uiOutput('uiFileLoadStim'),
      uiOutput('uiButLoadStim'),
      
      #tags$hr(),
      checkboxInput('chBtrajInter', 'Interpolate NAs and missing data', value = F),
      bsTooltip('chBtrajInter', help.text.short[3], placement = "right", trigger = "hover", options = NULL),
      
      uiOutput('varSelTimeFreq'),

      checkboxInput('chBtrackUni', 'Create unique track ID', F),
      bsTooltip('chBtrackUni', help.text.short[2], placement = "right", trigger = "hover", options = NULL),
      
      tags$hr(),

      uiOutput('varSelSite'),
      uiOutput('varSelTrackLabel'),
      
      checkboxInput('chBgroup', 'Select grouping column', F),                
      bsTooltip('chBgroup', help.text.short[5], placement = "right", trigger = "hover", options = NULL),

      uiOutput('varSelGroup'),
      uiOutput('varSelTime'),
      uiOutput('varSelMeas1'),
      radioButtons(
        'inSelMath', width = '50%',
        'Math on 1st and 2nd meas.:',
        c(
          'None' = '',
          'Divide' = " / ",
          'Sum' = " + ",
          'Multiply' = " * ",
          'Subtract' = ' - ',
          '1 / X' = '1 / '
        )
      ),
      bsTooltip('inSelMath', help.text.short[6], placement = "right", trigger = "hover", options = NULL),
      
      uiOutput('varSelMeas2'),
      
      tags$hr(),
      checkboxInput('chBtimeTrim', 'Trim x-axis', FALSE),
      bsTooltip('chBtimeTrim', help.text.short[7], placement = "right", trigger = "hover", options = NULL),
      uiOutput('uiSlTimeTrim'),

      checkboxInput('chBnorm', 'Normalization', FALSE),
      bsTooltip('chBnorm', help.text.short[8], placement = "right", trigger = "hover", options = NULL),
      uiOutput('uiChBnorm'),
      uiOutput('uiSlNorm'),
      uiOutput('uiChBnormRobust'),
      uiOutput('uiChBnormGroup'),

      tags$hr(),
      downloadButton('downloadDataClean', 'Download mod\'d data'),
      bsTooltip('downloadDataClean', help.text.short[9], placement = "right", trigger = "hover", options = NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Time series",
          h4(
            "Plot time series: means per group or individual"
          ),
          br(),
          modSelOutliersUI('returnOutlierIDs'),
          tabsetPanel(
            tabPanel("Means",
                     br(),
                     modTrajRibbonPlotUI('modTrajRibbon')
            ),
            
            tabPanel(
              "Individual",
              br(),
              checkboxInput('chBhighlightTraj', 'Highlight trajectories', FALSE),
              uiOutput('varSelHighlight'),
              br(),
              modTrajPlotUI('modTrajPlot')
            ),
            
            tabPanel(
              "Power Spectral Density",
              br(),
              modPSDPlotUI('modPSDPlot')
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
        ),
        
        # sparse hierarchical clustering package sparcl temporarily unavailable from CRAN
         tabPanel(
           'Hierarchical Sparse',
           clustHierSparUI('tabClHierSpar')
        )
      ))
  )
))
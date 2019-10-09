#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This is the user-interface definition for a Shiny web application.
#


library(shiny)
library(shinyjs) # http://deanattali.com/shinyjs/
library(shinyBS) # for tooltips
library(shinycssloaders) # for loader animations

shinyUI(fluidPage(
  useShinyjs(),
  # Include shinyjs
  
  # Application title
  title = "Timecourse Inspector",
  
  sidebarLayout(
    sidebarPanel(width = 3,
      #Selector for file upload
      fileInput(
        'inFileLoadNuc',
        "Select data file and click Load Data",
        accept = c("text/csv", 
                   "text/comma-separated-values,text/plain", 
                   "application/gzip", 
                   "application/bz2"), 
      ),

      radioButtons("inRbutLongWide", actionLink("alDataFormat", "Data Format:"), c("Long" = 0, "Wide" = 1), inline = T),
      bsAlert("alertAnchorSidePanelDataFormat"),
      
      actionButton("inButLoadNuc", "Load Data"),
      actionButton("butReset", "Reset file input"),
      
      actionButton('inDataGen1', 'Synthetic data'),
      bsTooltip('inDataGen1', helpText.server[["inDataGen1"]], placement = "top", trigger = "hover", options = NULL),
      
      tags$hr(),
      checkboxInput('chBtrajRem', 'Upload tracks to remove'),
      bsTooltip('chBtrajRem', helpText.server[["chBtrajRem"]], placement = "top", trigger = "hover", options = NULL),
      
      uiOutput('uiFileLoadTrajRem'),
      uiOutput('uiButLoadTrajRem'),
      
      #tags$hr(),
      checkboxInput('chBstim', 'Upload stimulation pattern'),
      bsTooltip('chBstim', helpText.server[["chBstim"]], placement = "top", trigger = "hover", options = NULL),
      
      uiOutput('uiFileLoadStim'),
      uiOutput('uiButLoadStim'),
      
      #tags$hr(),
      checkboxInput('chBtrajInter', 'Interpolate NAs and missing data', value = F),
      bsAlert("alertAnchorSidePanelNAsPresent"),
      bsTooltip('chBtrajInter', helpText.server[["chBtrajInter"]], placement = "top", trigger = "hover", options = NULL),
      uiOutput('varSelTimeFreq'),

      checkboxInput('chBtrackUni', 'Create unique track ID', F),
      bsTooltip('chBtrackUni', helpText.server[["chBtrackUni"]], placement = "top", trigger = "hover", options = NULL),
      uiOutput('varSelSite'),
      
      tags$hr(),

      uiOutput('varSelTrackLabel'),
      
      checkboxInput('chBgroup', 'Group data', F),                
      bsTooltip('chBgroup', helpText.server[["chBgroup"]], placement = "top", trigger = "hover", options = NULL),

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
      bsTooltip('inSelMath', helpText.server[["inSelMath"]], placement = "top", trigger = "hover", options = NULL),
      
      uiOutput('varSelMeas2'),
      
      tags$hr(),
      checkboxInput('chBtimeTrim', 'Trim X-axis', FALSE),
      bsTooltip('chBtimeTrim', helpText.server[["chBtimeTrim"]], placement = "top", trigger = "hover", options = NULL),
      uiOutput('uiSlTimeTrim'),

      checkboxInput('chBnorm', 'Normalization', FALSE),
      bsTooltip('chBnorm', helpText.server[["chBnorm"]], placement = "top", trigger = "hover", options = NULL),
      uiOutput('uiChBnorm'),
      uiOutput('uiSlNorm'),
      uiOutput('uiChBnormRobust'),
      uiOutput('uiChBnormGroup'),

      tags$hr(),
      downloadButton('downloadDataClean', 'Download processed data'),
      bsTooltip('downloadDataClean', helpText.server[["downloadDataClean"]], placement = "top", trigger = "hover", options = NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Time series",
          h4(
            "Plot time series: averages per group or individual"
          ),
          br(),
          modSelOutliersUI('returnOutlierIDs'),
          tabsetPanel(
            tabPanel("Averages",
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
          tabAUCplotUI('tabAUC')
        ),
        
        tabPanel(
          "Distributions",
          tabDistPlotUI('tabDistPlot')
        ),
        
        
        # scatter plot
        tabPanel(
          'Scatter',
          tabScatterPlotUI('tabScatter')
        ),
        
        # hierarchical clustering
        tabPanel(
          'Hierarchical',
          clustHierUI('tabClHier')
        ),
        
        # sparse hierarchical clustering package sparcl temporarily unavailable from CRAN
        tabPanel(
           'Hierarchical Sparse',
           clustHierSparUI('tabClHierSpar')
        ),
        
        # cluster validation
        tabPanel(
          'Validation',
          clustValidUI('tabClValid')
        )
      ))
  )
))
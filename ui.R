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
  # Include shinyjs
  useShinyjs(),
  
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
      
      conditionalPanel(
        condition = "input.chBtrajRem",
        
        fileInput(
          'inFileLoadTrajRem',
          "Select file and click Load Data",
          accept = c("text/csv", 
                     "text/comma-separated-values,text/plain", 
                     "application/gzip", 
                     "application/bz2"), 
        ),
        actionButton("inButLoadTrajRem", "Load Data")
      ),
      
      checkboxInput('chBstim', 'Upload stimulation pattern'),
      bsTooltip('chBstim', helpText.server[["chBstim"]], placement = "top", trigger = "hover", options = NULL),
      
      conditionalPanel(
        condition = "input.chBtrajRem",
        
        fileInput(
          'inFileLoadStim',
          "Select file and click Load Data",
          accept = c("text/csv", 
                     "text/comma-separated-values,text/plain", 
                     "application/gzip", 
                     "application/bz2"), 
        ),
        actionButton("inButLoadStim", "Load Data")
      ),
      
      tags$hr(),

      uiOutput('varSelTrackLabel'),
      checkboxInput('chBtrackUni', 'Create unique track ID', F),
      bsTooltip('chBtrackUni', helpText.server[["chBtrackUni"]], placement = "top", trigger = "hover", options = NULL),
      uiOutput('varSelSite'),
      
      checkboxInput('chBgroup', 'Group data', F),                
      bsTooltip('chBgroup', helpText.server[["chBgroup"]], placement = "top", trigger = "hover", options = NULL),

      uiOutput('varSelGroup'),
      uiOutput('varSelTime'),
      uiOutput('varSelMeas1'),
      radioButtons(
        'inSelMath', width = '50%',
        'Math on 1st and 2nd meas.',
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
      
      conditionalPanel(
        condition = "input.chBtimeTrim",
        
        uiOutput('uiSlTimeTrim')
      ),

      checkboxInput('chBtrajInter', 'Interpolate NAs and missing data', value = F),
      bsAlert("alertAnchorSidePanelNAsPresent"),
      bsTooltip('chBtrajInter', helpText.server[["chBtrajInter"]], placement = "top", trigger = "hover", options = NULL),
      
      conditionalPanel(
        condition = "input.chBtrajInter",
        
        numericInput(
          'inSelTimeFreq',
          'Interval between 2 time points',
          min = 0,
          step = 1,
          width = '100%',
          value = 1
        )
      ),
      
      checkboxInput('chBnorm', 'Normalization', FALSE),
      bsTooltip('chBnorm', helpText.server[["chBnorm"]], placement = "top", trigger = "hover", options = NULL),
      
      conditionalPanel(
        condition = "input.chBnorm",
        
        # select normalisation method
        # - fold-change calculates fold change with respect to the mean
        # - z-score calculates z-score of the selected region of the time series
        radioButtons(
          'rBnormMeth',
          label = 'Method',
          choices = list('fold-change' = 'mean', 'z-score' = 'z.score'),
          width = "40%"
        ),
        bsTooltip('rBnormMeth', helpText.server[["rBnormMeth"]], placement = "top", trigger = "hover", options = NULL),
        
        # slider for selecting normalisation range
        uiOutput('uiSlNorm'),
        
        # use robust stats (median instead of mean, mad instead of sd)
        checkboxInput('chBnormRobust',
                      label = 'Robust stats',
                      FALSE, 
                      width = "40%"),
        bsTooltip('chBnormRobust', helpText.server[["chBnormRobust"]], placement = "top", trigger = "hover", options = NULL),
        
        # choose whether normalisation should be calculated for the entire dataset, group, or trajectory
        radioButtons('chBnormGroup',
                     label = 'Grouping',
                     choices = list('Entire dataset' = 'none', 'Per group' = 'group', 'Per trajectory' = 'id'), 
                     width = "40%"),
        bsTooltip('chBnormGroup', helpText.server[["chBnormGroup"]], placement = "top", trigger = "hover", options = NULL)
      ),
      
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
                     selectInput(
                       "selColPal",
                       label = "Colour palette",
                       choices = l.col.pal.dend.2,
                       selected = 'Tableau 20'
                     ),
                     modTrajRibbonPlotUI('modTrajRibbon')
            ),
            
            tabPanel(
              "Individual",
              br(),
              checkboxInput('chBhighlightTraj', 'Highlight trajectories', FALSE),
              conditionalPanel(
                condition = "input.chBhighlightTraj",
                
                uiOutput('varSelHighlight')
              ),
              
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
        
        # scatter plot
        tabPanel(
          'Scatter',
          tabScatterPlotUI('tabScatter')
        ),
        
        # area under the curve
        tabPanel(
          "AUC",
          tabAUCplotUI('tabAUC')
        ),
        
        # distributions at time points
        tabPanel(
          "Distributions",
          tabDistPlotUI('tabDistPlot')
        ),
        
        # hierarchical clustering
        tabPanel(
          'Hierarchical',
          tabClHierUI('tabClHier')
        ),
        
        # sparse hierarchical clustering package sparcl temporarily unavailable from CRAN
        tabPanel(
           'Hier. Sparse',
           clustHierSparUI('tabClHierSpar')
        ),
        
        # cluster validation
        tabPanel(
          'Validation',
          tabClValidUI('tabClValid')
        )
      ))
  )
))
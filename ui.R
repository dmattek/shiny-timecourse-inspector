

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
        uiOutput('uiPlotTraj'),
        downPlotUI('downPlotTraj', "Download PDF")
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
               downPlotUI('downPlotBox', "Download PDF")
      ),
      

      # scatter plot
      tabPanel(
        'Scatter',
        tabScatterPlotUI('tabScatter')
      ),
      
      tabPanel(
        'Hierarchical',
        br(),
        fluidRow(
          column(4,
            selectInput(
              "selectPlotHierLinkage",
              label = ("Select linkage method:"),
              choices = list(
                "Ward" = 1,
                "Ward D2" = 2,
                "Single" = 3,
                "Complete" = 4,
                "Average" = 5,
                "McQuitty" = 6,
                "Centroid" = 7
              ),
              selected = 2
            ),
            selectInput(
              "selectPlotHierDiss",
              label = ("Select type of dissimilarity measure:"),
              choices = list("Euclidean" = 1,
                             "Maximum" = 2,
                             "Manhattan" = 3,
                             "Canberra" = 4,
                             "Binary" = 5,
                             "Minkowski" = 6),
              selected = 1
            )
          ),
          column(4,
                 sliderInput(
                   'inPlotHierNclust',
                   '#dendrogram branches to colour',
                   min = 1,
                   max = 20,
                   value = 1,
                   step = 1,
                   ticks = TRUE,
                   round = TRUE
                 ),
                 checkboxInput('chBPlotHierClSel', 'Manually select clusters to display'),
                 uiOutput('uiPlotHierClSel'),
                 #downCellClUI('downDataHier', "Download Data")
                 downloadButton('downCellCl', 'Download CSV with cell IDs and cluster no.')
          )
        ),
        br(),
        #checkboxInput('inPlotHierSparInteractive', 'Interactive Plot?',  value = FALSE),
        
        tabsetPanel(
          tabPanel('Heat-map',
                   fluidRow(
                     column(3,
                            checkboxInput('selectPlotHierDend', 'Plot dendrogram and re-order samples', TRUE),
                            selectInput(
                              "selectPlotHierPalette",
                              label = "Select colour palette:",
                              choices = l.col.pal,
                              selected = 'Spectral'
                            ),
                            checkboxInput('inPlotHierRevPalette', 'Reverse colour palette', TRUE),
                            checkboxInput('selectPlotHierKey', 'Plot colour key', TRUE)
                     ),
                     column(3,
                            sliderInput(
                              'inPlotHierNAcolor',
                              'Shade of grey for NA values (0 - black, 1 - white)',
                              min = 0,
                              max = 1,
                              value = 0.8,
                              step = .1,
                              ticks = TRUE
                            ),
                            numericInput('inPlotHierHeatMapHeight', 
                                         'Display plot height [px]', 
                                         value = 600, 
                                         min = 100,
                                         step = 100)
                     ),
                     column(6,
                            h4('Classic hierarchical clustering')
                     )
                   ),
                   
                   fluidRow(
                     column(
                       3,
                       numericInput(
                         'inPlotHierMarginX',
                         'Margin below x-axis',
                         5,
                         min = 1,
                         width = 100
                       )
                     ),
                     column(
                       3,
                       numericInput(
                         'inPlotHierMarginY',
                         'Margin right of y-axis',
                         20,
                         min = 1,
                         width = 100
                       )
                     ),
                     column(
                       3,
                       numericInput(
                         'inPlotHierFontX',
                         'Font size row labels',
                         1,
                         min = 0,
                         width = 100,
                         step = 0.1
                       )
                     ),
                     column(
                       3,
                       numericInput(
                         'inPlotHierFontY',
                         'Font size column labels',
                         1,
                         min = 0,
                         width = 100,
                         step = 0.1
                       )
                     )
                   ),
                   br(),
                   
                   downPlotUI('downPlotHier', "Download PDF"),
                   actionButton('butPlotHierHeatMap', 'Plot!'),
                   plotOutput('outPlotHier')
          ),
          # tabPanel('Heat-map int.',
          #          helpText("Choose your settings 2")),
          tabPanel('Time-courses',
                   downPlotUI('downPlotHierTraj', "Download PDF"),
                   actionButton('butPlotHierTraj', 'Plot!'),
                   plotOutput('outPlotHierTraj')),
          tabPanel('Cluster dist.',
                   downPlotUI('downPlotHierClDist', "Download PDF"),
                   actionButton('butPlotHierClDist', 'Plot!'),
                   plotOutput('outPlotHierClDist'))
        )
      ),
      
      
      tabPanel(
        'Hier. Sparse',
        br(),
        fluidRow(
          column(
            6,
            selectInput(
              "selectPlotHierSparLinkage",
              label = ("Select linkage method:"),
              choices = list(
                "Average" = 1,
                "Complete" = 2,
                "Single" = 3,
                "Centroid" = 4
              ),
              selected = 1
            ),
            selectInput(
              "selectPlotHierSparDiss",
              label = ("Select type of dissimilarity measure:"),
              choices = list("Squared Distance" = 1,
                             "Absolute Value" = 2),
              selected = 1
            ),
            sliderInput(
              'inPlotHierSparNclust',
              '#dendrogram branches to colour',
              min = 1,
              max = 30,
              value = 1,
              step = 1,
              ticks = TRUE,
              round = TRUE
            )
          ),
          
          column(
            6,
            checkboxInput('inHierSparAdv',
                          'Advanced options',
                          FALSE),
            uiOutput(
              'uiPlotHierSparNperms'
            ),
            uiOutput(
              'uiPlotHierSparNiter'
            ),
            checkboxInput('chBPlotHierSparClSel', 'Manually select clusters to display'),
            uiOutput('uiPlotHierSparClSel'),
            downloadButton('downCellClSpar', 'Download CSV with cell IDs and cluster no.')
          )
        ),
        
        
        br(),
        #checkboxInput('inPlotHierSparInteractive', 'Interactive Plot?',  value = FALSE),
        
        tabsetPanel(
          tabPanel('Heat-map',
                   fluidRow(
                     column(3,
                            radioButtons(
                              "selectPlotHierSparDend",
                              label = 'Dendrogram',
                              choices = list(
                                'Plot dendrogram; order samples accordingly' = 1,
                                'Don\'t plot dendrogram; retain original ordering' = 2
                              ),
                              selected = 1
                            ),
                            selectInput(
                              "selectPlotHierSparPalette",
                              label = "Select colour palette:",
                              choices = l.col.pal,
                              selected = 'Spectral'
                            ),
                            checkboxInput('inPlotHierSparRevPalette', 'Reverse colour palette', TRUE),
                            checkboxInput('selectPlotHierSparKey', 'Plot colour key', TRUE)
                     ),
                     column(3,
                            sliderInput(
                              'inPlotHierSparNAcolor',
                              'Shade of grey for NA values (0 - black, 1 - white)',
                              min = 0,
                              max = 1,
                              value = 0.8,
                              step = .1,
                              ticks = TRUE
                            )
                     ),
                     column(6,
                            br(),
                            h4(
                              "Sparse hierarchical clustering using ",
                              a("sparcl", href = "https://cran.r-project.org/web/packages/sparcl/")
                            ),
                            p(
                              'Column labels in the heat-map are additionally labeld according to their \"importance\":'
                            ),
                            tags$ol(
                              tags$li("Black - not taken into account"),
                              tags$li("Blue with \"*\" - low importance (weight factor in (0, 0.1]"),
                              tags$li("Green with \"**\" - medium importance (weight factor in (0.1, 0.5]"),
                              tags$li("Red with \"***\" - high importance (weight factor in (0.5, 1.0]")
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(
                       3,
                       numericInput(
                         'inPlotHierSparMarginX',
                         'Margin below x-axis',
                         5,
                         min = 1,
                         width = 100
                       )
                     ),
                     column(
                       3,
                       numericInput(
                         'inPlotHierSparMarginY',
                         'Margin right of y-axis',
                         20,
                         min = 1,
                         width = 100
                       )
                     ),
                     column(
                       3,
                       numericInput(
                         'inPlotHierSparFontX',
                         'Font size row labels',
                         1,
                         min = 0,
                         width = 100,
                         step = 0.1
                       )
                     ),
                     column(
                       3,
                       numericInput(
                         'inPlotHierSparFontY',
                         'Font size column labels',
                         1,
                         min = 0,
                         width = 100,
                         step = 0.1
                       )
                     )
                   ),
                   br(),
                   
                   
                   downPlotUI('downPlotHierSparHM', "Download PDF"),

                   numericInput('inPlotHierSparHeatMapHeight', 
                                'Display plot height [px]', 
                                value = 600, 
                                min = 100,
                                step = 100),
                   
                   actionButton('butPlotHierSparHeatMap', 'Plot!'),
                   plotOutput('outPlotHierSpar')
          ),
          # tabPanel('Heat-map int.',
          #          helpText("Choose your settings 2")),
          tabPanel('Time-courses',
                   downPlotUI('downPlotHierSparTraj', "Download PDF"),
                   actionButton('butPlotHierSparTraj', 'Plot!'),
                   plotOutput('outPlotHierSparTraj')),
          tabPanel('Cluster dist.',
                   downPlotUI('downPlotHierSparClDist', "Download PDF"),
                   actionButton('butPlotHierSparClDist', 'Plot!'),
                   plotOutput('outPlotHierSparClDist'))
        )
      )
    ))
  )
))
#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for plotting time series as a heatmap

## UI ----
modPlotHMUI <- function(id, label = "Heatmap") {
  ns <- NS(id)
  
  tagList(
    checkboxInput(ns('chBplotStyle'),
                  'Appearance',
                  FALSE),
    conditionalPanel(
      condition = "input.chBplotStyle",
      ns = ns,
      fluidRow(
        column(3,
               selectInput(
                 ns("selectPlotHMPalette"),
                 label = "Heatmap\'s colour palette",
                 choices = l.col.pal,
                 selected = 'Spectral'
               ),
               checkboxInput(ns('inPlotHMRevPalette'), 'Reverse heatmap\'s colour palette', TRUE),
               checkboxInput(ns('selectPlotHMyaxisLabels'), 'Plot y-axis labels', FALSE),
        ),
        column(3,
               sliderInput(
                 ns('inPlotHMBGcolor'),
                 'Shade of grey for background',
                 min = 0,
                 max = 1,
                 value = 0.8,
                 step = .1,
                 ticks = TRUE
               ),
        ),
        column(3,
               numericInput(
                 ns('inPlotTrajFacetNcol'),
                 '#columns',
                 value = PLOTNFACETDEFAULT,
                 min = 1,
                 width = '100px',
                 step = 1
               ),
               numericInput(ns('inPlotHMHeight'), 
                            'Display plot height [px]', 
                            value = 600, 
                            min = 100,
                            step = 50,
                            width = "180px")
               
        ),
        column(3,
        )
      )
    ),
    
    checkboxInput(ns('chBdownload'),
                  'Download',
                  FALSE),
    conditionalPanel(
      condition = "input.chBdownload",
      ns = ns,
      
      downPlotUI(ns('downPlotHM'), "")
    ),
    
    actionButton(ns('butPlot'), 'Plot!'),
    withSpinner(plotOutput(ns('outPlotHM')))
  )
}

## SERVER ----
modPlotHM <- function(input, output, session, 
                      in.data, 
                      in.fname) {
  
  ns <- session$ns
  
  ## UI rendering ----
  
  ## Processing ----
  
  ## Plotting ----
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to download a pdf
  PlotHM <- function() {
    cat(file = stderr(), 'modPlotHM:PlotHM: in\n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlot
    
    # Check if data exists
    locDT = in.data()
    
    shiny::validate(
      shiny::need(!is.null(locDT), "Nothing to plot. Load data first!")
    )
    
    if (is.null(locDT)) {
      return(NULL)
    }
    
    loc.p = LOCplotHM(locDT,
                      x.arg = COLRT,
                      y.arg = COLID, 
                      z.arg = COLY,
                      facet.arg = COLGR,
                      facet.ncol.arg = input$inPlotTrajFacetNcol,
                      xlab.arg = "Time",
                      ylab.arg = "Tracks",
                      palette.arg = input$selectPlotHMPalette, 
                      palette.rev.arg = input$inPlotHMRevPalette) +
      theme(
        panel.background = element_rect(fill = grey(input$inPlotHMBGcolor),
                                        colour = grey(input$inPlotHMBGcolor),
                                        size = 0.5, linetype = "solid")
      )
    
    if (!input$selectPlotHMyaxisLabels)
      loc.p = loc.p + 
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
    
    return(loc.p)
  }
  
  #  Display heatmap
  getPlotHMHeight <- function() {
    return (input$inPlotHMHeight)
  }
  
  output$outPlotHM <- renderPlot({
    
    PlotHM()
  }, height = getPlotHMHeight)
  
  
  ## Download ----
  
  #  Heatmap - download pdf
  callModule(downPlot, 
             "downPlotHM", 
             in.fname, 
             PlotHM, 
             TRUE)
  
}
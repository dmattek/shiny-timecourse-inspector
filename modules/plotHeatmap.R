#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for hierarchical clustering (base R hclust + dist)

## Help text ----
helpText.plotHeatmap = c(downCellCl = "Download a CSV with cluster assignments to time series ID",
                         downDend = "Download an RDS file with dendrogram object. Read later with readRDS() function.")


## UI ----
plotHeatmapUI <- function(id, label = "Hierarchical Clustering") {
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
                 ns("selectPlotHierPalette"),
                 label = "Heatmap\'s colour palette",
                 choices = l.col.pal,
                 selected = 'Spectral'
               ),
               checkboxInput(ns('inPlotHierRevPalette'), 'Reverse heatmap\'s colour palette', TRUE),
               checkboxInput(ns('selectPlotHierKey'), 'Plot colour key', TRUE),
               checkboxInput(ns('chBsetColBounds'), 'Set bounds for colour scale', FALSE),
               
               fluidRow(
                 column(6,
                        uiOutput(ns('uiSetColBoundsLow'))
                 ),
                 column(6,
                        uiOutput(ns('uiSetColBoundsHigh'))
                 )
               )
        ),
        column(3,
               sliderInput(
                 ns('inPlotHierNAcolor'),
                 'Shade of grey for NA values',
                 min = 0,
                 max = 1,
                 value = 0.8,
                 step = .1,
                 ticks = TRUE
               ),
               checkboxInput(ns('selectPlotHierDend'), 'Plot dendrogram and re-order samples', TRUE)
        ),
        column(3,
               numericInput(
                 ns('inPlotHierMarginX'),
                 'Bottom margin',
                 5,
                 min = 1,
                 width = "120px"
               ),
               numericInput(
                 ns('inPlotHierFontY'),
                 'Font size column labels',
                 1,
                 min = 0,
                 width = "180px",
                 step = 0.1
               ),
               numericInput(ns('inPlotHierHeatMapHeight'), 
                            'Display plot height [px]', 
                            value = 600, 
                            min = 100,
                            step = 50,
                            width = "180px")
               
        ),
        column(3,
               numericInput(
                 ns('inPlotHierMarginY'),
                 'Right margin',
                 20,
                 min = 1,
                 width = "120px"
               ),
               numericInput(
                 ns('inPlotHierFontX'),
                 'Font size row labels',
                 1,
                 min = 0,
                 width = "180px",
                 step = 0.1
               )
        )
      )
    ),
    
    checkboxInput(ns('chBdownload'),
                  'Download',
                  FALSE),
    conditionalPanel(
      condition = "input.chBdownload",
      ns = ns,
      
      downPlotUI(ns('downPlotHier'), "")
    ),
    
    actionButton(ns('butPlotHierHeatMap'), 'Plot!'),
    withSpinner(plotOutput(ns('outPlotHier')))
  )
}

## SERVER ----
plotHeatmap <- function(input, output, session, 
                           inDataWide, 
                           inDend,
                           inMeth) {
  
  ns <- session$ns
  
  ## UI rendering ----
  
  # UI for setting lower and upper bounds for the heatmap colour scale  
  output$uiSetColBoundsLow = renderUI({
    
    if(input$chBsetColBounds) {
      
      locDT = inDataWide()
      if (is.null(locDT))
        return(NULL)
      
      numericInput(
        ns('inSetColBoundsLow'),
        label = 'Lower',
        step = 0.1, 
        value = signif(min(locDT, na.rm = T), digits = 3)
      )
    }
  })
  
  
  output$uiSetColBoundsHigh = renderUI({
    
    if(input$chBsetColBounds) {
      
      locDT = inDataWide()
      if (is.null(locDT))
        return(NULL)
      
      numericInput(
        ns('inSetColBoundsHigh'),
        label = 'Upper',
        step = 0.1, 
        value = signif(max(locDT, na.rm = T), digits = 3)
      )
    }
  })
  
  ## Processing ----
  
  ## Plotting ----
  
  # Create the string for the plot title based on distance and linkage methods
  createPlotTitle = reactive({
    
    locMeth = inMeth()
    
    paste0(
      "Distance measure: ",
      locMeth$diss,
      "\nLinkage method: ",
      locMeth$link
    )
  })
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to download a pdf
  plotHier <- function() {
    cat(file = stderr(), 'plotHeatmap:plotHier: in\n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlotHierHeatMap
    
    # Check if main data exists
    # Thanks to isolate all mods in the left panel are delayed 
    # until clicking the Plot button
    locDM = shiny::isolate(inDataWide())
    locDend = shiny::isolate(inDend())

    shiny::validate(
      shiny::need(!is.null(locDM), "Nothing to plot. Load data first!"),
      shiny::need(!is.null(locDend), "Could not create dendrogram")
    )
    
    if (is.null(locDM)) {
      return(NULL)
    }

    if (is.null(locDend)) {
      return(NULL)
    }
        
    # Dummy dependency to redraw the heatmap without clicking Plot
    # when changing the number of clusters to highlight
    #loc.k = returnNclust()
    
    loc.col.bounds = NULL
    if (input$chBsetColBounds)
      loc.col.bounds = c(input$inSetColBoundsLow, 
                         input$inSetColBoundsHigh)
    else 
      loc.col.bounds = NULL
    
    
    loc.p = LOCplotHeatmap(locDM,
                           locDend, 
                           palette.arg = input$selectPlotHierPalette, 
                           palette.rev.arg = input$inPlotHierRevPalette, 
                           dend.show.arg = input$selectPlotHierDend, 
                           key.show.arg = input$selectPlotHierKey, 
                           margin.x.arg = input$inPlotHierMarginX, 
                           margin.y.arg = input$inPlotHierMarginY, 
                           nacol.arg = input$inPlotHierNAcolor, 
                           font.row.arg = input$inPlotHierFontX, 
                           font.col.arg = input$inPlotHierFontY, 
                           breaks.arg = loc.col.bounds,
                           title.arg = createPlotTitle())
    
    return(loc.p)
  }
  
  #  Hierarchical - display heatmap
  getPlotHierHeatMapHeight <- function() {
    return (input$inPlotHierHeatMapHeight)
  }
  
  output$outPlotHier <- renderPlot({
    
    plotHier()
  }, height = getPlotHierHeatMapHeight)
  
  
  ## Download ----
  
  # Create the string for the file name based on distance and linkage methods
  createPlotFname = reactive({
    
    locMeth = inMeth()
    
    paste0('clust_hier_heatmap_',
           locMeth$diss,
           '_',
           locMeth$link, 
           '.png')
  })
  
  #  Hierarchical - Heat Map - download pdf
  callModule(downPlot, 
             "downPlotHier", 
             createPlotFname, 
             plotHier)
  
}
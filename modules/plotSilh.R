#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Marc-Antoine Jacques
#
# This module is for plotting silhouette information according to a given clustering in k clusters
# Requires:
# - distance matrix
# - number of clusters
# - a table with cluster numbers matched to colours
# - list with names of distance and linkage methods

## Help text ----
helpText.silh = c(alLearnMore = paste0("<p>Display <a href=https://en.wikipedia.org/wiki/Silhouette_(clustering) target=\"_blank\" title=\"External link\">silhouette</a> ",
                                       "information according to a given clustering in k clusters.<p>"))


# UI ----
plotSilhUI =  function(id, label = "Plot Silhouette") {
  ns <- NS(id)
  
  tagList(
    actionLink(ns("alLearnMore"), "Learn more"),
    checkboxInput(ns('chBplotStyle'),
                  'Appearance',
                  FALSE),
    conditionalPanel(
      condition = "input.chBplotStyle",
      ns = ns,
      
      fluidRow(
        column(
          2,
          numericInput(
            ns('inPlotSilhWidth'),
            'Width [%]',
            value = 100,
            min = 10,
            width = '100px',
            step = 5
          )
        ),
        column(
          2,
          numericInput(
            ns('inPlotSilhHeight'),
            'Height [px]',
            value = PLOTPSDHEIGHT,
            min = 100,
            width = '100px',
            step = 50
          )
        )
      )),
    
    fluidRow(
      column(2,
             actionButton(ns('butPlot'), 'Plot!')),
      column(2,
             checkboxInput(ns('chBplotInt'), 'Interactive'))),
    
    uiOutput(ns('uiPlotSilh')),
    
    checkboxInput(ns('chBdownload'),
                  'Download',
                  FALSE),
    conditionalPanel(
      condition = "input.chBdownload",
      ns = ns,
      
      downPlotUI(ns('downPlotSilh'), "")
    )
  )
}

# Server ----
# Requires:
# inDist - distance matrix
# inNclust- number of clusters
# inClWithCol- a table with cluster numbers matched to colours
# inMeth - list with names of distance and linkage methods
plotSilh = function(input, output, session, 
                       inDist,
                       inNclust,
                       inClWithCol,
                       inMeth
) {
  
  ns <- session$ns
  
  ## UI rendering ----
  
  ## Processing ----
  
  # calculate dendrogram for a chosen number of clusters and the linkage method
  # using factoextra::hcut
  calcDendCut = reactive({
    if (DEB) {
      cat(file = stderr(), 'plotSilh:calcDendCut in\n')
    }
    
    locDist = inDist()
    
    if (is.null(locDist)) {
      return(NULL)
    }
    
    if (DEB) {
      cat(file = stderr(), 'plotSilh:calcDendCut locDist not NULL\n')
    }
    
    locNclust = inNclust()
    
    if (locNclust < 2) {
      return(NULL)
    }
    
    return(LOChcut(x = locDist,
                   k = inNclust(),
                   hc_func = "hclust",
                   hc_method = inMeth()$link
    ))    
  })
  
  ## Plotting ----
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to download a pdf
  
  output$uiPlotSilh = renderUI({
    if (input$chBplotInt) {
      plotlyOutput(
        ns("outPlotSilhInt"),
        width = paste0(input$inPlotSilhWidth, '%'),
        height = paste0(input$inPlotSilhHeight, 'px'))      
    } else {
      plotOutput(
        ns("outPlotSilh"),
        width = paste0(input$inPlotSilhWidth, '%'),
        height = paste0(input$inPlotSilhHeight, 'px'))
    }
  })
  
  
  output$outPlotSilh <- renderPlot({
    
    loc.p = plotClSilh()
    if(is.null(loc.p))
      return(NULL)
    
    return(loc.p)
  })
  
  
  output$outPlotSilhInt <- renderPlotly({
    # This is required to avoid
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    if (names(dev.cur()) != "null device")
      dev.off()
    pdf(NULL)
    
    loc.p = plotClSilh()
    if(is.null(loc.p))
      return(NULL)
    
    return(plotly_build(loc.p))
  })
  
  # Silh visualization of partitioning methods 
  plotClSilh <- function() {
    if (DEB)
      cat(file = stderr(), 'plotSilh:plotClSilh: in\n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlot
    
    # until clicking the Plot button
    locDend = calcDendCut()
    locClWithCol = inClWithCol()
    
    shiny::validate(
      shiny::need(!is.null(locDend), "Nothing to plot. Load data first or set the dendrogram cut > 1!"),
      shiny::need(!is.null(locClWithCol), "Cl ~ Color assignments missing.")
    )    
    
    if (is.null(locClWithCol)) {
      return(NULL)
    } else {
      locColors = locClWithCol[["gr.col"]]
      names(locColors) = locClWithCol[["gr.no"]]
    }
    
    locP = factoextra::fviz_silhouette(locDend, 
                                       print.summary = FALSE, 
                                       label = F,
                                       main = "Silhouette") +
      xlab("Time series") +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(name = "Cluster",
                        values = locColors) +
      scale_colour_manual(name = "Cluster",
                          values = locColors)
    
    return(locP)
  }
  
  ## Download plots ----
  
  # Create the string for the file name based on distance and linkage methods
  createPlotFname = reactive({
    
    locMeth = inMeth()
    
    paste0('clust_hier_silh_',
           locMeth$diss,
           '_',
           locMeth$link, 
           '.pdf')
  })
  
  
  # Silh plot - download pdf
  callModule(downPlot, "downPlotSilh", 
             in.fname = createPlotFname,
             plotClSilh, TRUE)
  
  ## Pop-overs ----
  addPopover(session, 
             ns("alLearnMore"),
             title = "Silhouette",
             content = helpText.silh[["alLearnMore"]],
             trigger = "click")
  
}
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

#' Silhoouette plot
#'
#' @param input 
#' @param output 
#' @param session 
#' @param inDist a distance matrix
#' @param inColWithCl a list with names of distance and linkage methods
#' @param inMeth 
#'
#' @return
#' @export
#'
#' @examples
plotSilh = function(input, output, session, 
                    inDist,
                    inColWithCl,
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
    
    #locNclust = inNclust()
    
    # Get the number of clusters from the named colour vector
    locNclust = length(inColWithCl())
    
    if (locNclust < 2) {
      return(NULL)
    }
    
    ## HERE!!!
    ## The resulting cluster numbers are different from those in other modules???
    locHcut = factoextra::hcut(
      x = locDist,
      k = locNclust,
      isdiss = TRUE,
      hc_func = "hclust",
      hc_method = inMeth()$link
    )
    
    cat("calcDendCut\n")
    print(locHcut$cluster)
    
    return(locHcut)    
  })
  
  calcSilh <- reactive({
    if (DEB) {
      cat(file = stderr(), 'plotSilh:calcSilh in\n')
    }
    
    locDist = inDist()
    
    if (is.null(locDist)) {
      return(NULL)
    }
    
    locHclust = hclust(locDist, 
                       method = inMeth()$link)
    
    # Get the number of clusters from the named colour vector
    locNclust = length(inColWithCl())
    
    locClust = dendextend::cutree(locHclust,
                                  k = locNclust,
                                  order_clusters_as_data = F)

    locClust = locClust[sort(names(locClust))]

    locSilh = LOCcalcSil(inCluster = locClust,
                         inDiss = locDist)
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
    locSilh = calcSilh()
    locColWithCl = inColWithCl()
    
    shiny::validate(
      shiny::need(!is.null(locSilh), "Nothing to plot. Load data first or set the dendrogram cut > 1!"),
      shiny::need(!is.null(locColWithCl), "Cl ~ Color assignments missing.")
    )    
    
    if (is.null(locSilh))
      return(NULL)
    
    if (is.null(locColWithCl))
      return(NULL)

    locP = ggplot(locSilh,
                  aes_string(x = COLID,
                             y = COLSILW)) +
      geom_bar(aes_string(color = COLCL,
                          fill = COLCL),
               stat = "identity") +
      geom_hline(yintercept = mean(locSilh[[COLSILW]]),
                 linetype = "dashed", 
                 color = "red") +
      xlab("Time series") +
      ylab("Silhouette width") +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      scale_fill_manual(name = "Cluster",
                        values = locColWithCl) +
      scale_colour_manual(name = "Cluster",
                          values = locColWithCl)
    
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
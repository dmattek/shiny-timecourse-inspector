#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This is a module of a Shiny web application.
# Selection of tracks by length
#
# Input:
# - time-series data in long format
#
# Output
# - time-series data in long format with tracks with selected lengths

helpText.selTrackLen = c(chbSelTrackLen = "Select tracks by their length.")

# UI-select-tracks-lengths ----
modSelTrackLenUI = function(id, label = "Select tracks by length") {
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  
  tagList(
    checkboxInput(ns('chbSelTrackLen'), 'Select tracks by length', value = F),
    bsTooltip(ns('chbSelTrackLen'), helpText.selTrackLen[["chbSelTrackLen"]], placement = "top", trigger = "hover", options = NULL),
    
    conditionalPanel(
      condition = "input.chbSelTrackLen",
      ns = ns,
      
      fluidRow(
        column(4, 
               uiOutput(ns('varSelMinTrackLen'))
        ),
        column(4, 
               uiOutput(ns('varSelMaxTrackLen'))
        ),
      ),
      
      plotOutput(ns("plotDist"))
    )
  )
}

# Server-tracks-lengths ----

#' Select tracks by length
#' 
#' Server part of track selection
#'
#' @param input 
#' @param output 
#' @param session 
#' @param in.data a data.table with time series in long format.
#'
#' @return
#' @export
#'
#' @examples
modSelTrackLen = function(input, output, session, in.data) {
  
  ns = session$ns
  
  # reactive counter to hold number of tracks before and after removal
  nTracksCounter <- reactiveValues(
    nTracksOrig = 0,
    nTracksAfter = 0,
  )
  
  # Display number of tracks  
  output$txtOutliersPerc <- renderText({ 
    cat(file = stdout(), 'modSelTrackLen: txtTrackssPerc\n')
    
    sprintf('<b>%d total track(s)<br>%d removed track(s)</b><br>', 
            nTracksCounter[['nTracksOrig']], 
            nTracksCounter[['nTracksOrig']] - nTracksCounter[['nTracksAfter']])
  })
  
  # Return a dt with track lengths
  dtTrackLen <- reactive({
    
    loc.dt = in.data()
    
    if (is.null(loc.dt)) {
      loc.tracklen = NULL
    } else {
      loc.tracklen = loc.dt[,
                            .(tlen = max(get(COLRT)) - min(get(COLRT))),
                            by = c(COLID)]
    }
    
    return(loc.tracklen)
  })
  
  # Return a 2-element vector with min and max track lengths
  calcMinMaxTrackLen <- reactive({
    loc.dt = dtTrackLen()
    
    if (is.null(loc.dt)) {
      return(NULL)
    } else {
      return(c(min(loc.dt[['tlen']]),
               max(loc.dt[['tlen']])))
    }
  })
  
  
  output$varSelMinTrackLen <- renderUI({
    ns <- session$ns
    
    if (input$chbSelTrackLen) {
      
      loc.minmax = calcMinMaxTrackLen()
      if(is.null(loc.minmax))
        return(NULL)
      
      numericInput(ns('numMinTrackLen'),
                   label = 'Min track length',
                   min = loc.minmax[1],
                   max = loc.minmax[2],
                   value = loc.minmax[1], 
                   step = 1, width = '100px')
    }
  })
  
  output$varSelMaxTrackLen <- renderUI({
    ns <- session$ns
    
    if (input$chbSelTrackLen) {
      
      loc.minmax = calcMinMaxTrackLen()
      if(is.null(loc.minmax))
        return(NULL)
      
      numericInput(ns('numMaxTrackLen'),
                   label = 'Max track length',
                   min = loc.minmax[1],
                   max = loc.minmax[2],
                   value = loc.minmax[2], 
                   step = 1, width = '100px')
      
    }
  })
  
  # Plot of value distribution
  output$plotDist <- renderPlot({
    cat(file = stderr(), 'tabSelOutliers:plotDist: in\n')
    
    loc.dt = dtTrackLen()
    
    shiny::validate(
      shiny::need(!is.null(loc.dt), "Nothing to plot. Load data first!")
    )
    
    if (is.null(loc.dt)) {
      return(NULL)
    }
    
    # main histogram plot
    locP = ggplot(loc.dt, 
                  aes(tlen)) +
      geom_histogram() +
      geom_vline(xintercept = input$numMinTrackLen,
                 linetype = "dashed",
                 color = 'darkred') +
      geom_vline(xintercept = input$numMaxTrackLen,
                 linetype = "dashed",
                 color = 'darkred') +
      xlab('Track lengths') +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND)
    
    locP
  })
  
  # Identify outliers
  # Outlier values are identified based on a distribution of pooled data from COLY column.
  # There's an option to identify outliers at the left, right, or both ends of the distribution.
  # Time points with outlier measurements are removed entirely.
  # Depending on the length of a gap created by outlier removal, entire trajectories can be removed.
  # The resulting gaps can be interpolated.
  dtReturn = reactive({ 
    cat(file = stdout(), 'modSelOutliers:dtReturn\n')
    
    loc.out = in.data()
    
    if (is.null(loc.out)) {
      return(NULL)
    }
    
    if (!input$chbSelTrackLen) {
      return(loc.out)
    }
    
    # store the number of trajectories before prunning
    nTracksCounter[['nTracksOrig']] = length(unique(loc.out[[COLID]]))
    
    loc.tracklen = dtTrackLen()
    loc.tracklen = loc.tracklen[tlen %between% c(input$numMinTrackLen,
                                                 input$numMaxTrackLen)]
    
    loc.out = loc.out[get(COLID) %in% loc.tracklen[[COLID]]]
    
    # count number of trajectories after removing outlier tracks
    nTracksCounter[['nTracksAfter']] = length(unique(loc.out[[COLID]]))
    
    # return cleaned dt
    if (nrow(loc.out) < 1)
      return(NULL) else
        return(loc.out)
  })
  
  return(dtReturn)
}
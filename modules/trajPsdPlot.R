require(DT)

 # UI ----
modPSDPlotUI =  function(id, label = "Plot PSD of average trajectory.") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        3,
        checkboxInput(ns('chBplotTrajInt'), 'Interactive Plot'),
        radioButtons(ns('rBlegendPos'), 'Legend placement:', list('top' = 'top', 'right' = 'right')),
        actionButton(ns('butPlotTraj'), 'Plot!')
      ),
      column(
        3,
        sliderInput(ns('sliPlotTrajSkip'), 'Plot every n-th point:', min = 1, max = 10, value = 1, step = 1)
      ),
      column(
        3,
        numericInput(
          ns('inPlotTrajWidth'),
          'Width [%]:',
          value = 100,
          min = 10,
          max = 100,
          width = '100px',
          step = 10
        ),
        numericInput(
          ns('inPlotTrajHeight'),
          'Height [px]:',
          value = 800,
          min = 100,
          width = '100px',
          step = 50
        )
      )
    ),
    uiOutput(ns('uiPlotTraj')),
    br(),
    modTrackStatsUI(ns('dispTrackStats')),
    
    downPlotUI(ns('downPlotTraj'), "Download PDF")
  )
}

# Server ----

modPSDPlot = function(input, output, session, 
                             in.data,
                             in.fname,
                             in.facet = 'group', 
                             in.facet.color = NULL
                             ) {
  
  ns <- session$ns
  
  output$uiPlotTraj = renderUI({
    if (input$chBplotTrajInt)
      plotlyOutput(
        ns("outPlotTrajInt"),
        width = paste0(input$inPlotTrajWidth, '%'),
        height = paste0(input$inPlotTrajHeight, 'px')
      ) else
        plotOutput(
          ns("outPlotTraj"),
          width = paste0(input$inPlotTrajWidth, '%'),
          height = paste0(input$inPlotTrajHeight, 'px')
        )
  })
  
  
  callModule(modTrackStats, 'dispTrackStats',
             in.data = in.data)
  
  
  output$outPlotTraj <- renderPlot({
    
    loc.p = plotTraj()
    if(is.null(loc.p))
      return(NULL)
    
    return(loc.p)
  })
  
  
  output$outPlotTrajInt <- renderPlotly({
    # This is required to avoid
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    if (names(dev.cur()) != "null device")
      dev.off()
    pdf(NULL)
    
    loc.p = plotTraj()
    if(is.null(loc.p))
      return(NULL)
    
    return(plotly_build(loc.p))
  })
  
  
  
  # PSD plot - download pdf
  callModule(downPlot, "downPlotTraj", 
             in.fname = in.fname, 
             plotTraj, TRUE)
  
  plotTraj <- function() {
    cat(file = stderr(), 'plotPSD: in\n')
    locBut = input$butPlotTraj
    
    if (locBut == 0) {
      cat(file = stderr(), 'plotPSD: Go button not pressed\n')
      
      return(NULL)
    }
    
    # check if main data exists
    loc.dt = isolate(in.data())
    
    cat("plotPSD: on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'plotPSD: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'plotPSD: dt not NULL\n')
    
    
    
    # Future: change such that a column with colouring status is chosen by the user
    # colour trajectories, if dataset contains mid.in column
    # with filtering status of trajectory
    if (sum(names(loc.dt) %in% 'mid.in') > 0)
      loc.line.col.arg = 'mid.in'
    else
      loc.line.col.arg = NULL
    
    # select every other point for plotting
    loc.dt = loc.dt[, .SD[seq(1, .N, input$sliPlotTrajSkip)], by = id]
    
    # check if columns with XY positions are present
    if (sum(names(loc.dt) %like% 'pos') == 2)
      locPos = TRUE
    else
      locPos = FALSE
    
    # check if column with ObjectNumber is present
    if (sum(names(loc.dt) %like% 'obj.num') == 1)
      locObjNum = TRUE
    else
      locObjNum = FALSE
    
    
    
    # If in.facet.color present,
    # make sure to include the same number of colours in the palette,
    # as the number of groups in dt.
    # in.facet.color is typically used when plotting time series within clusters.
    # Then, the number of colours in the palette has to be equal to the number of clusters (facetted according to in.facet variable).
    # This might differ if the user selects manually clusters to display.
    if (is.null(in.facet.color)) 
      loc.facet.col = NULL 
    else {
      # get group numbers in dt; 
      # loc.dt[, c(in.facet), with = FALSE] returns a data table with a single column
      # [[1]] at the end extracts the first column and returns as a vector
      loc.groups = unique(loc.dt[, c(in.facet), with = FALSE][[1]])
      
      # get colour palette
      # the length is equal to the number of groups in the original dt.
      # When plotting time series within clusters, the length equals the number of clusters.
      loc.facet.col = in.facet.color()$cl.col
      loc.facet.col = loc.facet.col[loc.groups]
    }
    

    loc.dt.aggr <- LOCcalcPSD(in.dt = loc.dt,
                              in.col.meas = 'y',
                              in.col.id = 'id',
                              in.col.by = in.facet,
                              in.method = 'pgram',
                              in.return.period = TRUE
                              )
    loc.dt.aggr[, (in.facet) := as.factor(get(in.facet))]
    
    x_arg <- ifelse('period' %in% colnames(loc.dt.aggr), 'period', 'frequency')
    p.out <- LOCplotPSD(dt.arg = loc.dt.aggr,
                        x.arg = x_arg,
                        y.arg = 'spec',
                        group.arg = in.facet,
                        xlab.arg = x_arg,
                        ylab.arg = '') +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND) + 
      theme(legend.position = input$rBlegendPos)
    
    return(p.out)
  }
}
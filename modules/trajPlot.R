require(DT)

modTrajPlotUI =  function(id, label = "Plot Individual Time Series") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        3,
        numericInput(
          ns('inPlotTrajFacetNcol'),
          '#Columns:',
          value = 4,
          min = 1,
          width = '100px',
          step = 1
        ),
        checkboxInput(ns('chBplotTrajInt'), 'Interactive Plot?'),
        actionButton(ns('butPlotTraj'), 'Plot!')
      ),
      column(
        3,
        checkboxGroupInput(ns('chBPlotTrajStat'), 'Stats:', list('Mean' = 'mean', '95% conf. interv.' = 'CI', 'Std. error' = 'SE'))
      ),
      column(
        3,
        sliderInput(ns('sliPlotTrajSkip'), 'Plot every n-th point:', min = 1, max = 10, value = 1, step = 1),
        uiOutput(ns('uiSlYTrim'))
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


modTrajPlot = function(input, output, session, 
                       in.data, 
                       in.fname,
                       in.facet = 'group', 
                       in.facet.color = NULL) {
  
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
  

  # UI for trimming y-axis
  output$uiSlYTrim = renderUI({
    cat(file = stderr(), 'UI uiSlYTrim\n')
    
    loc.dt = in.data()
    
      locYmin = signif(min(loc.dt$y, na.rm = T), 4)
      locYmax = signif(max(loc.dt$y, na.rm = T), 4)

      sliderInput(
        ns('slYTrim'),
        label = 'Trim y-axis',
        min = locYmin,
        max = locYmax,
        value = c(locYmin, locYmax)
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
    
    return(ggplotly(loc.p))
  })
  
  
  
  # Trajectory plot - download pdf
  callModule(downPlot, "downPlotTraj", in.fname, plotTraj, TRUE)
  
  plotTraj <- function() {
    cat(file = stderr(), 'plotTraj: in\n')
    locBut = input$butPlotTraj
    
    if (locBut == 0) {
      cat(file = stderr(), 'plotTraj: Go button not pressed\n')
      
      return(NULL)
    }
    
    loc.dt = isolate(in.data())
    
    cat("plotTraj: on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'plotTraj: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'plotTraj: dt not NULL\n')
    
    
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

    
    p.out = myGgplotTraj(
      dt.arg = loc.dt,
      x.arg = 'realtime',
      y.arg = 'y',
      group.arg = "id",
      facet.arg = in.facet,
      facet.ncol.arg = input$inPlotTrajFacetNcol,
      facet.color.arg = loc.facet.col,
      xlab.arg = 'Time (min)',
      line.col.arg = loc.line.col.arg,
      aux.label1 = if (locPos) 'pos.x' else NULL,
      aux.label2 = if (locPos) 'pos.y' else NULL,
      aux.label3 = if (locObjNum) 'obj.num' else NULL,
      stat.arg = input$chBPlotTrajStat,
      ylim.arg = input$slYTrim
    )
    
    return(p.out)
  }
}
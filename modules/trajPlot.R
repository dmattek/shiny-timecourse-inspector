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
    downPlotUI(ns('downPlotTraj'), "Download PDF")
  )
}


modTrajPlot = function(input, output, session, in.data, in.facet = 'group', in.facet.color = NULL, in.fname = 'tCourses.pdf') {
  
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
    # colour trajectories, if dataset contains mi.din column
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
    
    p.out = myGgplotTraj(
      dt.arg = loc.dt,
      x.arg = 'realtime',
      y.arg = 'y',
      group.arg = "id",
      facet.arg = in.facet,
      facet.ncol.arg = input$inPlotTrajFacetNcol,
      facet.color.arg = if (is.null(in.facet.color)) NULL else in.facet.color()$cl.col,
      xlab.arg = 'Time (min)',
      line.col.arg = loc.line.col.arg,
      aux.label1 = if (locPos) 'pos.x' else NULL,
      aux.label2 = if (locPos) 'pos.y' else NULL,
      stat.arg = input$chBPlotTrajStat
    )
    
    return(p.out)
  }
}
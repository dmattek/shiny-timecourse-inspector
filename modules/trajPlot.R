#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for plotting individual time series
#


# UI ----

modTrajPlotUI =  function(id, label = "Plot Individual Time Series") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        2,
        numericInput(
          ns('inPlotTrajFacetNcol'),
          '#Columns',
          value = PLOTNFACETDEFAULT,
          min = 1,
          width = '80px',
          step = 1
        ),
        checkboxInput(ns('chBplotTrajInt'), 'Interactive Plot'),
        actionButton(ns('butPlotTraj'), 'Plot!')
      ),
      column(
        2,
        checkboxGroupInput(ns('chBPlotTrajStat'), 'Add', list('Mean' = 'mean', 
                                                                   '95% conf. interv.' = 'CI', 
                                                                   'Std. error' = 'SE'))
      ),
      column(
        3,
        sliderInput(ns('sliPlotTrajSkip'), 'Plot every n-th point', 
                    min = 1, max = 10, value = 1, step = 1),
        
        checkboxInput(ns('chBsetXbounds'), 'Set bounds for x-axis', FALSE),
        fluidRow(
          column(6,
                 uiOutput(ns('uiSetXboundsLow'))
          ),
          column(6,
                 uiOutput(ns('uiSetXboundsHigh'))
          )),
        
        checkboxInput(ns('chBsetYbounds'), 'Set bounds for y-axis', FALSE),
        fluidRow(
          column(6,
                 uiOutput(ns('uiSetYboundsLow'))
          ),
          column(6,
                 uiOutput(ns('uiSetYboundsHigh'))
          ))
        
        
      ),
      column(
        2,
        numericInput(
          ns('inPlotTrajWidth'),
          'Width [%]',
          value = PLOTWIDTH,
          min = 10,
          width = '100px',
          step = 10
        ),
        numericInput(
          ns('inPlotTrajHeight'),
          'Height [px]',
          value = PLOTTRAJHEIGHT,
          min = 100,
          width = '100px',
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
modTrajPlot = function(input, output, session, 
                       in.data, 
                       in.data.stim,
                       in.fname,
                       in.facet = COLGR, 
                       in.facet.color = NULL,
                       in.ylab = NULL) {
  
  ns <- session$ns
  
  output$uiPlotTraj = renderUI({
    if (input$chBplotTrajInt)
      withSpinner(plotlyOutput(
        ns("outPlotTrajInt"),
        width = paste0(input$inPlotTrajWidth, '%'),
        height = paste0(input$inPlotTrajHeight, 'px'))
      ) else
        withSpinner(plotOutput(
          ns("outPlotTraj"),
          width = paste0(input$inPlotTrajWidth, '%'),
          height = paste0(input$inPlotTrajHeight, 'px'))
        )
  })
  
  # UI for bounding the x-axis ====
  output$uiSetXboundsLow = renderUI({
    ns <- session$ns
    
    if(input$chBsetXbounds) {
      
      loc.dt = in.data()
      
      if (is.null(loc.dt)) {
        cat(file = stderr(), 'uiSetXboundsLow: dt is NULL\n')
        return(NULL)
      }
      
      numericInput(
        ns('inSetXboundsLow'),
        label = 'Lower',
        step = 0.1, 
        value = floor(min(loc.dt[[COLRT]], na.rm = T))
      )
    }
  })
  
  
  output$uiSetXboundsHigh = renderUI({
    ns <- session$ns
    
    if(input$chBsetXbounds) {
      
      loc.dt = in.data()
      
      if (is.null(loc.dt)) {
        cat(file = stderr(), 'uiSetXboundsHigh: dt is NULL\n')
        return(NULL)
      }
      
      numericInput(
        ns('inSetXboundsHigh'),
        label = 'Upper',
        step = 0.1, 
        value = ceil(max(loc.dt[[COLRT]], na.rm = T))
      )
    }
  })
  
  
  # UI for bounding the y-axis ====
  output$uiSetYboundsLow = renderUI({
    ns <- session$ns
    
    if(input$chBsetYbounds) {
      
      loc.dt = in.data()
      
      if (is.null(loc.dt)) {
        cat(file = stderr(), 'uiSetYboundsLow: dt is NULL\n')
        return(NULL)
      }
      
      numericInput(
        ns('inSetYboundsLow'),
        label = 'Lower',
        step = 0.1, 
        value = floor(min(loc.dt[[COLY]], na.rm = T))
      )
    }
  })
  
  
  output$uiSetYboundsHigh = renderUI({
    ns <- session$ns
    
    if(input$chBsetYbounds) {
      
      loc.dt = in.data()
      
      if (is.null(loc.dt)) {
        cat(file = stderr(), 'uiSetYboundsHigh: dt is NULL\n')
        return(NULL)
      }
      
      numericInput(
        ns('inSetYboundsHigh'),
        label = 'Upper',
        step = 0.1, 
        value = ceil(max(loc.dt[[COLY]], na.rm = T))
      )
    }
  })
  
  # Plotting ====
  
  callModule(modTrackStats, 'dispTrackStats',
             in.data = in.data,
             in.bycols = in.facet)
  
  
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
    
    # check main data exists
    loc.dt = isolate(in.data())
    
    cat("plotTraj: on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'plotTraj: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'plotTraj: dt not NULL\n')
    
    
    # check if stim data exists
    loc.dt.stim = isolate(in.data.stim())
    
    if (is.null(loc.dt.stim)) {
      cat(file = stderr(), 'plotTraj: dt.stim is NULL\n')
    } else {
      cat(file = stderr(), 'plotTraj: dt.stim not NULL\n')
    }
    
    
    
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
    
    
    loc.xlim.arg = NULL
    if(input$chBsetXbounds) {
      loc.xlim.arg = c(input$inSetXboundsLow, input$inSetXboundsHigh)
    } 
    
    loc.ylim.arg = NULL
    if(input$chBsetYbounds) {
      loc.ylim.arg = c(input$inSetYboundsLow, input$inSetYboundsHigh)
    } 
    
    p.out = LOCplotTraj(
      dt.arg = loc.dt,
      x.arg = COLRT,
      y.arg = COLY,
      group.arg = COLID,
      facet.arg = in.facet,
      facet.ncol.arg = input$inPlotTrajFacetNcol,
      facet.color.arg = loc.facet.col, 
      dt.stim.arg = loc.dt.stim, 
      x.stim.arg = c('tstart', 'tend'),
      y.stim.arg = c('ystart', 'yend'), 
      stim.bar.width.arg = 1,
      xlab.arg = 'Time',
      line.col.arg = loc.line.col.arg,
      aux.label1 = if (locPos) COLPOSX else NULL,
      aux.label2 = if (locPos) COLPOSY else NULL,
      aux.label3 = if (locObjNum) COLOBJN else NULL,
      stat.arg = input$chBPlotTrajStat,
      ylim.arg = loc.ylim.arg,
      xlim.arg = loc.xlim.arg
    )
    
    return(p.out)
  }
}
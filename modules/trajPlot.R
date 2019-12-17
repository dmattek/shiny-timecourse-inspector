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
        3,
        numericInput(
          ns('inPlotTrajFacetNcol'),
          '#columns',
          value = PLOTNFACETDEFAULT,
          min = 1,
          width = '100px',
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
        
        checkboxInput(ns('chBsetXbounds'), 'Bounds for X', FALSE),
        fluidRow(
          column(6,
                 uiOutput(ns('uiSetXboundsLow'))
          ),
          column(6,
                 uiOutput(ns('uiSetXboundsHigh'))
          )),
        
        checkboxInput(ns('chBsetYbounds'), 'Bounds for Y', FALSE),
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
    br(),
    downPlotUI(ns('downPlotTraj'), "Download Plot")
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
        value = min(loc.dt[[COLY]], na.rm = T)
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
        value = max(loc.dt[[COLY]], na.rm = T)
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
    
    # make the f-n dependent on the button click
    locBut = input$butPlotTraj
    
    # Check if main data exists
    # Thanks to solate all mods in the left panel are delayed 
    # until clicking the Plot button
    loc.dt = isolate(in.data())
    shiny::validate(
      shiny::need(!is.null(loc.dt), "Nothing to plot. Load data first!")
    )
    
    cat(file = stderr(), 'plotTraj: dt not NULL\n')
    
    # check if stim data exists
    loc.dt.stim = shiny::isolate(in.data.stim())
    
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
    
    # in.facet.color is typically used when plotting time series per clusters.
    # The number of colours in the palette has to be equal to the number of groups.
    # This might differ if the user selects manually groups (e.g. clusters) to display.
    if (is.null(in.facet.color)) {
      loc.facet.color = NULL
    } else {
      # get existing groups in dt;
      loc.facets = unique(loc.dt[, ..in.facet])
      
      # subset group-color assignments with existing groups
      loc.facet.color = in.facet.color()[loc.facets][["gr.col"]]
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
      facet.color.arg = loc.facet.color, 
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
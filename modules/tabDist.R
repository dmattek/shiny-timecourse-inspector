#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for plotting box-plots at selected time points
#

# UI ----
tabDistPlotUI =  function(id, label = "Snapshots at time points") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Box-/dot-/violin plots at selected time points"
    ),
    br(),
    
    uiOutput(ns('varSelTpts')),
    
    # This is an experimental feature to re-normalise data points with respect to a selected time point
    # Current implementation is limited; in the future slider should be replaced by an input field or a choice list.
    # currenlty, if the selected time point is larger than the smallest time point for snapshot plotting, error appears.
    #checkboxInput(ns('chBfoldCh'), 'Fold change w.r.t. t-point:'),
    #uiOutput(ns('uiSlFoldChTp')),
    
    modStatsUI(ns('dispStats')),
    br(),
   
    modDistPlotUI(ns('distPlot')) 
  )
}

# SERVER ----
tabDistPlot = function(input, output, session, in.data, in.fname) {
  
  callModule(modStats, 'dispStats',
             in.data = data4boxPlot,
             in.meascol = 'y',
             in.bycols = c(COLRT, COLGR),
             in.fname = 'individualsTP.csv')
  
  callModule(modDistPlot, 'distPlot', 
             in.data = data4boxPlot, 
             in.cols = list(meas.x = COLRT,
                            meas.y = COLY,
                            group = COLGR,
                            id = COLID),
             in.labels = list(x = "Time points", y = "", legend = "Groups:"),
             in.fname = in.fname)
  
  # return all unique time points (real time)
  # This will be used to display in UI for box-plot
  # These timepoints are from the original dt and aren't affected by trimming of x-axis
  getDataTpts <- reactive({
    cat(file = stderr(), 'getDataTpts\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[[COLRT]])) # column name specified in data4trajPlot
  })

  output$uiSlFoldChTp = renderUI({
    ns <- session$ns

    if(input$chBfoldCh)
      sliderInput(ns('slFoldChTp'), 'Time before:', min = 0, max = 10, value = 1)
    
  })
    
  # prepare data for plotting boxplots
  # uses the same dt as for trajectory plotting
  # returns dt with these columns:
  data4boxPlot <- reactive({
    cat(file = stderr(), 'data4boxPlot\n')
    
    loc.dt = in.data()
    if (is.null(loc.dt))
      return(NULL)

    # This is part of re-nromalisation with respect to a time point.
    # Test version here; works but needs improvements; see UI section
    # if(input$chBfoldCh) {
    #   out.dt = loc.dt[get(COLRT) %in% input$inSelTpts]
    #   loc.dt.aux = loc.dt[get(COLRT) %in% c(as.numeric(input$inSelTpts) - input$slFoldChTp)]
    #   loc.y.prev = loc.dt.aux[, y]
    # 
    #   out.dt[, y.prev := loc.y.prev]
    # 
    #   out.dt[, y := abs(y / y.prev)]
    # 
    #   out.dt[, y.prev := NULL]
    # 
    # } else
      out.dt = loc.dt[get(COLRT) %in% input$inSelTpts]
    
    
    return(out.dt)
  })
  
  output$varSelTpts = renderUI({
    cat(file = stderr(), 'UI varSelTpts\n')
    
    ns <- session$ns
    
    loc.v = getDataTpts()
    
    if (!is.null(loc.v)) {
      selectInput(
        ns('inSelTpts'),
        'Select one or more time points:',
        loc.v,
        width = '100%',
        selected = loc.v[[1]],
        multiple = TRUE
      )
    }
  })

 
}
#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for plotting Area Under Curve
#
# Calculates area under curve (AUC) for every single time course provided in the input

require(pracma) # for trapz

# UI ----
modAUCplotUI =  function(id, label = "Plot Area Under Curves") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Calculate area under curve and plot per group"
    ),
    br(),
    
    uiOutput(ns('uiSlTimeTrim')),
    modStatsUI(ns('dispStats')),
    br(),
    modBoxPlotUI(ns('boxPlot')
    )
  )
}

# SERVER ----
modAUCplot = function(input, output, session, in.data, in.fname) {
  
  ns <- session$ns
  
  # return all unique time points (real time)
  # This will be used to display in UI for box-plot
  # These timepoints are from the original dt and aren't affected by trimming of x-axis
  getDataTpts <- reactive({
    cat(file = stderr(), 'getDataTpts\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[['realtime']]))
  })
  
  # UI for trimming x-axis (time)
  output$uiSlTimeTrim = renderUI({
    cat(file = stderr(), 'UI uiSlTimeTrim\n')
    
    locTpts  = getDataTpts()
    
    if(is.null(locTpts))
      return(NULL)
    
    locRTmin = min(locTpts)
    locRTmax = max(locTpts)
    
    sliderInput(
      ns('slTimeTrim'),
      label = 'Select time range for AUC calculation',
      min = locRTmin,
      max = locRTmax,
      value = c(locRTmin, locRTmax),
      step = 1
    )
    
  })
  
  AUCcells = reactive({
    cat(file = stderr(), 'AUCcells\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    else {
      loc.res = loc.dt[realtime >= input$slTimeTrim[1] & realtime <= input$slTimeTrim[2], .(AUC = trapz(realtime, y)), by = .(group, id)]
      return(loc.res)
    }
  })
  
  callModule(modStats, 'dispStats',
             in.data = AUCcells,
             in.meascol = 'AUC',
             in.bycols = c('group'),
             in.fname = 'data4boxplotAUC.csv')
  
  callModule(modBoxPlot, 'boxPlot', 
             in.data = AUCcells, 
             in.cols = list(meas.x = 'group',
                            meas.y = 'AUC',
                            group = 'group',
                            id = 'id'),
             in.fname = in.fname)
  
  
}







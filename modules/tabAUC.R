#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for plotting Area Under Curve
#
# Calculates area under curve (AUC) for every single time course provided in the input

helpText.tabAUC = c("Calculate area under curve (AUC) for every time series using trapezoidal rule." #1
)

# UI ----
tabAUCplotUI =  function(id, label = "Plot Area Under Curves") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Area under curve (AUC)"
    ),
    actionLink(ns("alAUC"), "Learn more"),
    br(),
    
    uiOutput(ns('uiSlTimeTrim')),
    modStatsUI(ns('dispStats')),
    br(),
    modAUCplotUI(ns('aucPlot')
    )
  )
}

# SERVER ----
tabAUCplot = function(input, output, session, in.data, in.fname) {
  
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
      return(unique(loc.dt[[COLRT]]))
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
      loc.res = loc.dt[get(COLRT) >= input$slTimeTrim[1] & get(COLRT) <= input$slTimeTrim[2], .(AUC = trapz(get(COLRT), get(COLY))), by = c(COLGR, COLID)]
      return(loc.res)
    }
  })
  
  callModule(modStats, 'dispStats',
             in.data = AUCcells,
             in.meascol = 'AUC',
             in.bycols = COLGR,
             in.fname = 'data4boxplotAUC.csv')
  
  callModule(modAUCplot, 'aucPlot', 
             in.data = AUCcells, 
             in.cols = list(meas.x = COLGR,
                            meas.y = 'AUC',
                            group = COLGR,
                            id = COLID),
             in.labels = list(x = "Groups", y = "", legend = ""),
             in.fname = in.fname)
  
  addPopover(session, 
             id = ns("alAUC"), 
             title = "AUC",
             content = helpText.tabAUC[1],
             trigger = "click")
  
}







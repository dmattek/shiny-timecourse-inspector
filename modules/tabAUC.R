#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for plotting Area Under Curve
#
# Calculates area under curve (AUC) for every single time course provided in the input

## Help text ----
helpText.tabAUC = c(alAUC = paste0("Calculate area under curve (AUC) for every time series using trapezoidal rule. ",
                                   "Display the result as a box-, violin-, or a dot-plot. ",
                                   "The interval used for AUC calculation can be altered using the slider below.")
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
    modAUCplotUI(ns('aucPlot')),
    modStatsUI(ns('dispStats'))
  )
}

# SERVER ----
tabAUCplot = function(input, output, session, in.data, in.fname) {
  
  ns <- session$ns
  
  ## UI rendering ----
  
  # UI for trimming x-axis (time)
  output$uiSlTimeTrim = renderUI({
    if (DEB) {
      cat(file = stderr(), 'tabAUC:uiSlTimeTrim\n')
    }
    
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
  
  ## Processing ----
  
  # return all unique time points (real time)
  # This will be used to display in UI for box-plot
  # These timepoints are from the original dt and aren't affected by trimming of x-axis
  getDataTpts <- reactive({
    if (DEB) {
      cat(file = stderr(), 'tabAUC:getDataTpts\n')
    }
    
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[[COLRT]]))
  })
  
  # Calculate the AUC under time series,
  # return a dt ready for plotting and data summary
  AUCcells = reactive({
    if (DEB) {
      cat(file = stderr(), 'tabAUC:AUCcells\n')
    }
    
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    else {
      loc.res = loc.dt[get(COLRT) >= input$slTimeTrim[1] & 
                         get(COLRT) <= input$slTimeTrim[2], 
                       .(AUC = trapz(get(COLRT), 
                                     get(COLY))), 
                       by = c(COLGR, COLID)]
      return(loc.res)
    }
  })
  
  ## Modules ----
  
  callModule(modStats, 'dispStats',
             in.data = AUCcells,
             in.meascol = 'AUC',
             in.bycols = COLGR,
             in.fname = 'individualsAUC.csv')
  
  callModule(modAUCplot, 'aucPlot', 
             in.data = AUCcells, 
             in.cols = list(meas.x = COLGR,
                            meas.y = 'AUC',
                            group = COLGR,
                            id = COLID),
             in.labels = list(x = "Groups",
                              y = "", 
                              legend = ""),
             in.fname = in.fname)
  
  ## Pop-overs ----
  addPopover(session, 
             id = ns("alAUC"), 
             title = "AUC",
             content = helpText.tabAUC[["alAUC"]],
             trigger = "click")
  
}
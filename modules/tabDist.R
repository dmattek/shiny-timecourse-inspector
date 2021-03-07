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
    modDistPlotUI(ns('distPlot')),
    modStatsUI(ns('dispStats'))
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
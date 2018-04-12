tabBoxPlotUI =  function(id, label = "Comparing t-points") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Box-/dot-/violin plot at selected time points"
    ),
    br(),
    
    uiOutput(ns('varSelTpts')),
    
    checkboxInput(ns('chBfoldCh'), 'Fold change w.r.t. t-point:'),
    uiOutput(ns('uiSlFoldChTp')),
    
    modStatsUI(ns('dispStats')),
    br(),
   
    modBoxPlotUI(ns('boxPlot')) 
  )
}

####
## server box-plot
tabBoxPlot = function(input, output, session, in.data, in.fname) {
  
  callModule(modStats, 'dispStats',
             in.data = data4boxPlot,
             in.meascol = 'y',
             in.bycols = c('realtime', 'group'),
             in.fname = 'data4boxplotTP.csv')
  
  callModule(modBoxPlot, 'boxPlot', 
             in.data = data4boxPlot, 
             in.cols = list(meas.x = 'realtime',
                            meas.y = 'y',
                            group = 'group',
                            id = 'id'),
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
      return(unique(loc.dt[, realtime])) # column name specified in data4trajPlot
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

    
    if(input$chBfoldCh) {
      out.dt = loc.dt[realtime %in% input$inSelTpts]
      loc.dt.aux = loc.dt[realtime %in% c(as.numeric(input$inSelTpts) - input$slFoldChTp)]
      loc.y.prev = loc.dt.aux[, y]
      print(nrow(loc.dt.aux))
      print(nrow(out.dt))
      
      out.dt[, y.prev := loc.y.prev]
      print(out.dt)
      out.dt[, y := abs(y / y.prev)]
      print(out.dt)
      out.dt[, y.prev := NULL]
      print(out.dt)
      
    } else
      out.dt = loc.dt[realtime %in% input$inSelTpts]
    
    
    return(out.dt)
  })
  
  output$varSelTpts = renderUI({
    cat(file = stderr(), 'UI varSelTpts\n')
    
    ns <- session$ns
    
    loc.v = getDataTpts()
    if (!is.null(loc.v)) {
      selectInput(
        ns('inSelTpts'),
        'Select one or more timepoints:',
        loc.v,
        width = '100%',
        selected = 0,
        multiple = TRUE
      )
    }
  })

 
}
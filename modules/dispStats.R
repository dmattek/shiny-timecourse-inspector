#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for displaying stats in an interactive table using DT package
#

# UI ----
modStatsUI =  function(id, label = "Comparing t-points") {
  ns <- NS(id)
  
  tagList(
    checkboxInput(ns('chbTabStats'), 'Show stats', FALSE),
    uiOutput(ns('uiTabStats')),
    uiOutput(ns('uiDownSingleCellData'))
  )
}

# SERVER ----
modStats = function(input, output, session, 
                   in.data, 
                   in.meascol = 'meas.y', 
                   in.bycols = c('meas.x', 'group'),
                   in.fname = 'data4boxplot.csv') {
  
  ns <- session$ns
  
  output$uiTabStats = renderUI({
    cat(file = stderr(), 'UI uiTabStats\n')
    ns <- session$ns
    
    if(input$chbTabStats) {
      DT::dataTableOutput(ns('outTabStats'))
    }
  })
  
  output$uiDownSingleCellData = renderUI({
    cat(file = stderr(), 'UI uiDownSingleCellData\n')
    ns <- session$ns
    
    if(input$chbTabStats) {
      downloadButton(ns('downloadData4BoxPlot'), 'Download single-cell data')
    }
  })
  
  
  
  calcStats = reactive({
    cat(file = stderr(), 'tabBoxPlot: calsStats\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)

    loc.dt.aggr = loc.dt[, sapply(.SD, function(x) list('N' = .N, 
                                                        'Mean' = mean(x), 
                                                        'CV' = sd(x)/mean(x), 
                                                        'Median' = median(x), 
                                                        'rCV (IQR)' = IQR(x)/median(x), 
                                                        'rCV (MAD)'= mad(x)/median(x))), .SDcols = in.meascol, by = in.bycols]
    
    setnames(loc.dt.aggr, c(in.bycols, 'N', 'Mean', 'CV', 'Median', 'rCV IQR', 'rCV MAD'))
    
    return(loc.dt.aggr)
  })
  
  output$downloadData4BoxPlot <- downloadHandler(
    filename = in.fname,
    content = function(file) {
      loc.dt = in.data()
      
      if (is.null(loc.dt))
        return(NULL)
      else
        write.csv(loc.dt, file, row.names = FALSE)
    }
  )
  
  output$outTabStats = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'tabBoxPlot: outTabStats\n')
    loc.dt = calcStats()
    
    if (is.null(loc.dt))
      return(NULL)
    
    loc.n.bycols = length(in.bycols)
    
    datatable(loc.dt, 
              rownames = FALSE,
              extensions = 'Buttons', 
              options = list(
                dom = 'Bfrtip',
                buttons = list('copy', 
                               'print', 
                               list(extend = 'collection',
                                    buttons = list(list(extend='csv',
                                                        filename = 'hitStats'),
                                                   list(extend='excel',
                                                        filename = 'hitStats'),
                                                   list(extend='pdf',
                                                        filename= 'hitStats')),
                                    text = 'Download')))) %>% formatRound(seq(loc.n.bycols + 2, loc.n.bycols + 1 + 5), 3)
  })
  
}
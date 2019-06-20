#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for displaying time series statistics
#

require(DT)
require(data.table)


# UI ----
modTrackStatsUI =  function(id, label = "Comparing t-points") {
  ns <- NS(id)
  
  tagList(
    checkboxInput(ns('chbTabStats'), 'Show stats', FALSE),
    uiOutput(ns('uiTabStats'))
  )
}

# SERVER ----
modTrackStats = function(input, output, session, 
                         in.data) {
  
  ns <- session$ns
  
  output$uiTabStats = renderUI({
    cat(file = stderr(), 'modTrackStats: uiTabStats\n')
    ns <- session$ns
    
    if(input$chbTabStats) {
      tagList(
        htmlOutput(ns('txtNtracks')),
        #br(),
        #p("Track IDs with duplicated objects in a frame"),
        br(),
        DT::dataTableOutput(ns('outTabStats'))
      )
    }
  })
  
  # unused at the moment
  calcStats = reactive({
    cat(file = stderr(), 'modTrackStats: calsStats\n')
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
  
  # Print number of tracks
  output$txtNtracks = renderText({
    cat(file = stderr(), 'modTrackStats: txtNtracks\n')
    loc.dt = in.data()
    
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    
    sprintf('<b>Number of time-series: %d <br>Average length: %.2f time units</b>', 
            length(unique(loc.dt[['id']])), 
            loc.dt[, .(trackLength = .N), by = 'id'][, mean(trackLength)])
    
  })
  
  # Print a table with Track IDs assigned to multiple objects in a frame
  output$outTabStats = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'modTrackStats: outTabStats\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    
    # Look whether there were more objects with the same track ID in the frame
    # Such track IDs will have TRUE assigned in 'dup' column
    # Keep only s.track column with dup=TRUE
    loc.duptracks = loc.dt[, .(dup = (sum(duplicated(get('realtime'))) > 0)), by = 'id'][dup == TRUE, 'id', with = FALSE]
    
    if (nrow(loc.duptracks))
      datatable(loc.duptracks, 
                caption = 'Track IDs with duplicated objects in a frame',
                rownames = TRUE,
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
                                      text = 'Download'))))
    else
      return(NULL)
  })
  
}
#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for displaying time series statistics
#

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
                         in.data,
                         in.bycols = COLGR) {
  
  ns <- session$ns
  
  # UI for displaying various stats
  output$uiTabStats = renderUI({
    cat(file = stderr(), 'modTrackStats:uiTabStats\n')
    ns <- session$ns
    
    if(input$chbTabStats) {
      tagList(
        htmlOutput(ns('txtNtracks')),
        br(),
        tabsetPanel(
          tabPanel("Tracks stats", 
                   DT::dataTableOutput(ns('outTabStatsTracks'))),
          tabPanel("Measurement stats", 
                   DT::dataTableOutput(ns('outTabStatsMeas'))),
          tabPanel("Duplicated IDs",         
                   DT::dataTableOutput(ns('outTabStatsDup')))
          )
        )
    }
  })
  
  # Print number of tracks
  output$txtNtracks = renderText({
    cat(file = stderr(), 'modTrackStats:txtNtracks\n')
    loc.dt = in.data()
    
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    
    sprintf('<b>Total number of time-series: %d <br>Average length: %.2f time units</b>', 
            length(unique(loc.dt[[COLID]])), 
            loc.dt[, .(trackLength = .N), by = COLID][, mean(trackLength)])
    
  })
  
  
  # caclulate stats of the measurement (column Y) per group
  calcStatsMeas = reactive({
    cat(file = stderr(), 'modTrackStats:calsStats\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    
    loc.dt.aggr = loc.dt[, sapply(.SD, function(x) list('nas' = sum(is.na(x)),
                                                        'min' = min(x, na.rm = T),
                                                        'max' = max(x, na.rm = T),
                                                        'measMean' = mean(x, na.rm = T),
                                                        'measSD' = sd(x, na.rm = T),
                                                        'measCV' = sd(x, na.rm = T)/mean(x, na.rm = T), 
                                                        'measMedian' = median(x, na.rm = T),
                                                        'measIQR' = IQR(x, na.rm = T),
                                                        'meas_rCV_IQR' = IQR(x, na.rm = T)/median(x, na.rm = T))), .SDcols = COLY, by = c(in.bycols)]
    
    setnames(loc.dt.aggr, c(in.bycols, '#NAs', 'Min', 'Max', 'Mean', 'SD', 'CV', 'Median', 'IQR', 'rCV'))
    
    return(loc.dt.aggr)
  })
  
  # caclulate stats of tracks per group
  calcStatsTracks = reactive({
    cat(file = stderr(), 'modTrackStats:calsStats\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    
    loc.dt.aggr = loc.dt[, 
                         .(nTpts = .N), 
                         by = c(in.bycols, COLID)][, .(tracksN = .N,
                                                   tracksLenMean = mean(nTpts),
                                                   tracksLenSD = sd(nTpts),
                                                   tracksLenMedian = median(nTpts),
                                                   tracksLenIQR = IQR(nTpts)), by = c(in.bycols)]
    
    setnames(loc.dt.aggr, c(in.bycols, '#Tracks', 'Mean', 'SD', 'Median', 'IQR'))
    
    return(loc.dt.aggr)
  })
  
  # Render a table with track stats
  output$outTabStatsTracks = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'modTrackStats:outTabStats\n')
    loc.dt = calcStatsTracks()
    
    if (is.null(loc.dt))
      return(NULL)
    
    if (nrow(loc.dt))
      datatable(loc.dt, 
                caption = 'Statistics of time series: number of time series, mean/median and the spread of track lengths, SD - standard deviation, IQR - interquartile range.',
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
  
  
  # Render a table with measurement stats
  output$outTabStatsMeas = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'modTrackStats:outTabMeas\n')
    loc.dt = calcStatsMeas()
    
    if (is.null(loc.dt))
      return(NULL)
    
    if (nrow(loc.dt))
      datatable(loc.dt, 
                caption = 'Statistics of measurements: number of NA time points, min/max/mean/median and the spread of the y-axis value. SD - standard deviation, CV - coefficient of variation, SD/mean; IQR - interquartile range, rCV - robust CV, IQR/median.',
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
                                      text = 'Download')))) %>% formatRound(3:10)
    else
      return(NULL)
  })
  
  # Render a table with Track IDs assigned to multiple objects in a frame
  output$outTabStatsDup = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'modTrackStats:outTabStatsDup\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    
    # Look whether there were more objects with the same track ID in the frame
    # Such track IDs will have TRUE assigned in 'dup' column
    # Keep only s.track column with dup=TRUE
    loc.duptracks = loc.dt[, .(dup = (sum(duplicated(get(COLRT))) > 0)), by = COLID][dup == TRUE, COLID, with = FALSE]
    
    if (nrow(loc.duptracks))
      datatable(loc.duptracks, 
                caption = 'Track IDs with duplicated objects in a group. To avoid, create a data-wide unique track ID in the panel on the left or in your input data.',
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
                                      text = 'Download')))) %>% formatRound(3:6)
    else
      return(NULL)
  })
  
}
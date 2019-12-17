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
    
    sprintf('<b>Total number of time series: %d <br>Average length: %.2f time points</b>', 
            length(unique(loc.dt[[COLID]])), 
            loc.dt[, .(trackLength = .N), by = COLID][, mean(trackLength)])
    
  })
  
  
  # calculate stats of the measurement (column Y) per group
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
                                                        'measMedian' = median(as.double(x), na.rm = T),
                                                        'measIQR' = IQR(x, na.rm = T),
                                                        'meas_rCV_IQR' = IQR(x, na.rm = T)/median(x, na.rm = T))), .SDcols = COLY, by = c(in.bycols)]
    
    setnames(loc.dt.aggr, c(in.bycols, '#NAs', 'Min Y', 'Max Y', 'Mean Y', 'SD', 'CV', 'Median Y', 'IQR', 'rCV'))
    
    return(loc.dt.aggr)
  })
  
  # calculate stats of tracks per group
  calcStatsTracks = reactive({
    cat(file = stderr(), 'modTrackStats:calsStats\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    
    loc.dt.aggr = loc.dt[, 
                         .(nTpts = .N), 
                         by = c(in.bycols, COLID)][, .(tracksN = .N,
                                                       tracksMin = min(nTpts),
                                                       tracksMax = max(nTpts),
                                                       tracksLenMean = mean(nTpts),
                                                       tracksLenSD = sd(nTpts),
                                                       tracksLenMedian = median(as.double(nTpts)),
                                                       tracksLenIQR = IQR(as.double(nTpts))), by = c(in.bycols)]

    setnames(loc.dt.aggr, c(in.bycols, 'nTracks', 'Min Length', 'Max Length', 'Mean Length', 'SD', 'Median Length', 'IQR'))
    
    return(loc.dt.aggr)
  })
  
  # Render a table with track stats
  output$outTabStatsTracks = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'modTrackStats:outTabStats\n')
    loc.dt = calcStatsTracks()
    
    shiny::validate(
      shiny::need(!is.null(loc.dt), "Cannot calculate statistics. Load data first!")
    )
    
    if (nrow(loc.dt))
      datatable(loc.dt, 
                caption = paste0("Statistics of time series: number of time series, ",
                                 "min/max/mean/median track length, ",
                                 "SD - standard deviation, IQR - interquartile range."),
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
                                      text = 'Download')))) %>% formatSignif(5:6, digits = SIGNIFDIGITSINTAB)
    else
      return(NULL)
  })
  
  
  # Render a table with measurement stats
  output$outTabStatsMeas = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'modTrackStats:outTabMeas\n')
    loc.dt = calcStatsMeas()
    
    shiny::validate(
      shiny::need(!is.null(loc.dt), "Cannot calculate statistics. Load data first!")
    )
    
    if (nrow(loc.dt))
      datatable(loc.dt, 
                caption = paste0("Statistics of measurements: number of NA time points, ",
                                 "min/max/mean/median of the measurmeent selected for the Y-axis. ",
                                 "SD - standard deviation; CV - coefficient of variation; ",
                                 "SD/mean; IQR - interquartile range; ",
                                 "rCV - robust CV, IQR/median."),
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
                                      text = 'Download')))) %>% formatSignif(3:10, digits = SIGNIFDIGITSINTAB)
    else
      return(NULL)
  })
  
  # Render a table with Track IDs assigned to multiple objects in a frame
  output$outTabStatsDup = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'modTrackStats:outTabStatsDup\n')
    loc.dt = in.data()
    
    shiny::validate(
      shiny::need(!is.null(loc.dt), "Cannot calculate statistics. Load data first!")
    )
    
    # Look whether there were more objects with the same track ID in the frame
    # Such track IDs will have TRUE assigned in 'dup' column
    # Keep only s.track column with dup=TRUE
    loc.duptracks = loc.dt[, 
                           .(dup = (sum(duplicated(get(COLRT))) > 0)), 
                           by = COLID][dup == TRUE, COLID, with = FALSE]
    
    DT::datatable(loc.duptracks, 
                  caption = paste0("Time series with duplicated track IDs. ",
                                   "To avoid, create a data-wide unique track ID in ",
                                   "the panel on the left or in your input data."),
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
                                        text = 'Download')),
                    language = list(
                      zeroRecords = "No records to display")
                  )
    )
  })
  
}
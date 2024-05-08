#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This is a module of a Shiny web application.
# Selection of tracks by length
#
# Input:
# - time-series data in long format
#
# Output
# - time-series data in long format with tracks with selected lengths

helpText.selTrackNoDupl = c(chbSelTrackNoDupl = "Remove tracks with duplicated IDs.")

# UI-select-tracks-lengths ----
modSelTrackNoDuplUI = function(id, label = "Remove duplicate track IDs") {
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  
  tagList(
    checkboxInput(ns('chbSelTrackNoDupl'), 'Remove duplicate track IDs', value = F),
    bsTooltip(ns('chbSelTrackNoDupl'), helpText.selTrackNoDupl [["chbSelTrackNoDupl"]], placement = "top", trigger = "hover", options = NULL),
    
    conditionalPanel(
      condition = "input.chbSelTrackNoDupl",
      ns = ns,
      
      DT::dataTableOutput(ns('outTabStatsDup'))
    )
  )
}

# Server-tracks-lengths ----

#' Select tracks by length
#' 
#' Server part of track selection
#'
#' @param input 
#' @param output 
#' @param session 
#' @param in.data a data.table with time series in long format.
#'
#' @return
#' @export
#'
#' @examples
modSelTrackNoDupl = function(input, output, session, in.data) {
  
  ns = session$ns
  
  dtDupTracks <- reactive({
    loc.out = in.data()
    
    if (is.null(loc.out)) {
      return(NULL)
    }
    
    # Check whether there were more objects with the same track ID in the frame
    # Such track IDs will have TRUE assigned in the 'dup' column
    # Keep only s.track column with dup=TRUE
    loc.duptracks = loc.out[, 
                            .(dup = (sum(duplicated(get(COLRT))) > 0)), 
                            by = COLID][dup == TRUE, COLID, with = FALSE]
    
    return(loc.duptracks)
  })
  
  dtReturn <- reactive({ 
    cat(file = stdout(), 'modSelTrackNoDupl:dtReturn\n')
    
    loc.out = in.data()
    
    if (is.null(loc.out)) {
      return(NULL)
    }
    
    if (input$chbSelTrackNoDupl) {
      loc.duptracks = dtDupTracks()
      loc.out = loc.out[!(get(COLID) %in% loc.duptracks[[COLID]])]
    }
    
    # return cleaned dt
    if (nrow(loc.out) < 1)
      return(NULL) else
        return(loc.out)
  })
  
  # Render a table with Track IDs assigned to multiple objects in a frame
  output$outTabStatsDup = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'modSelTrackNoDupl:outTabStatsDup\n')
    loc.dt = in.data()
    
    shiny::validate(
      shiny::need(!is.null(loc.dt), "Cannot calculate statistics. Load data first!")
    )
    
    loc.duptracks = dtDupTracks()
    
    DT::datatable(loc.duptracks, 
                  caption = paste0("Time series with duplicated track IDs."),
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
  
  return(dtReturn)
}
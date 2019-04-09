#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This is the module of a Shiny web application.
# Download CSV with data
#
# Usage:
# in ui.R
# downCSV('uniqueID', "your_label")
#
# in server.R
# callModule(downCSV, "uniqueID", 'fname.csv', input_data_to_save)

# UI-download-csv ----
downCsvUI <- function(id, label = "Download Data") {
  ns <- NS(id)
  
  tagList(
    # Label to display as h4 header
    h4(label),
    downloadButton(ns('downCellCl'), 'CSV')
  )
}

# SERVER-download-csv ----
downCsv <- function(input, output, session, in.fname, in.data) {
  
  output$downCellCl <- downloadHandler(
    filename = function() {
      in.fname
    },
    
    content = function(file) {
      write.csv(x = in.data, file = file, row.names = FALSE)
    }
  )
  
}
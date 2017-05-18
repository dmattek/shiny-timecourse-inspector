# RShiny module for downloading cellIDs with cluster numbers
# Use:
# in ui.R
# downPlotUI('uniqueID', "your_label")
#
# in server.R
# callModule(downPlot, "uniqueID", 'fname.pdf', input_plot_to_save)


downCellClUI <- function(id, label = "Download Data") {
  ns <- NS(id)
  
  tagList(
    # Label to display as h4 header
    h4(label),
    downloadButton(ns('downCellCl'), 'CSV')
  )
}

downCellCl <- function(input, output, session, in.fname, in.data) {
  
  output$downCellCl <- downloadHandler(
    filename = function() {
      in.fname
    },
    
    content = function(file) {
      write.csv(x = in.data, file = file, row.names = FALSE)
    }
  )
  
}
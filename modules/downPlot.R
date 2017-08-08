# RShiny module for downloading pdf of the plot
# Use:
# in ui.R
# downPlotUI('uniqueID', "your_label")
#
# in server.R
# callModule(downPlot, "uniqueID", 'fname.pdf', input_plot_to_save)


downPlotUI <- function(id, label = "Download Plot") {
  ns <- NS(id)
  
  tagList(
    # Label to display as h4 header
    h4(label),
    
    fluidRow(
      column(
        3,
        numericInput(
          ns('inPlotWidth'),
          "Width",
          17,
          min = 1,
          width = 100
        )
      ),
      column(
        3,
        numericInput(
          ns('inPlotHeight'),
          "Height",
          10,
          min = 1,
          width = 100
        )
      ),
      column(6,
             downloadButton(ns('downPlot'), 'PDF'))
    )
  )
}

downPlot <- function(input, output, session, in.fname, in.plot, in.gg = FALSE) {

  output$downPlot <- downloadHandler(
    filename = function() {
      in.fname
    },
    
    content = function(file) {
      if (in.gg) {
        ggsave(
          file,
          limitsize = FALSE,
          in.plot(),
          width  = input$inPlotWidth,
          height = input$inPlotHeight
        )
      } else {
        if (in.fname %like% 'pdf') {
          pdf(file,
              width  = input$inPlotWidth,
              height = input$inPlotHeight)
        } else {
          png(file,
              width  = input$inPlotWidth,
              height = input$inPlotHeight, units = 'in', res = 300)
        }
        
        
        in.plot()
        dev.off()
      }
    }
  )
  
}
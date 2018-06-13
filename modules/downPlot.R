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
          "Width (in)",
          8.5,
          min = 1,
          width = 100
        )
      ),
      column(
        3,
        numericInput(
          ns('inPlotHeight'),
          "Height (in)",
          11,
          min = 1,
          width = 100
        )
      ),
      column(6,
             uiOutput(ns('uiDownButton')))
    )
  )
}

downPlot <- function(input, output, session, in.fname, in.plot, in.gg = FALSE) {

  output$uiDownButton = renderUI({
    ns <- session$ns
    
    if (in.fname() %like% 'pdf') {
      downloadButton(ns('downPlot'), 'PDF')
    } else {
      downloadButton(ns('downPlot'), 'PNG')
    }
    
  })
  
  output$downPlot <- downloadHandler(
    filename = function() {
      cat(in.fname(), "\n")
      in.fname()
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
        if (in.fname() %like% 'pdf') {
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
#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for downloading pdf of the plot
# Use:
# in ui.R
# downPlotUI('uniqueID', "your_label")
#
# in server.R
# callModule(downPlot, "uniqueID", 'fname.pdf', input_plot_to_save)

# UI ----
downPlotUI <- function(id, label = "Download Plot") {
  ns <- NS(id)
  
  tagList(
    # Label to display as h4 header
    h4(label),
    
    fluidRow(
      # CSS to make label next to text input
      # From: https://stackoverflow.com/a/45299050/1898713
      tags$head(
        tags$style(type="text/css", 
        "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } #inline .form-group { display: table-row;}")
      ),
      

      column(3,
             uiOutput(ns('uiDownButton'))
             ),
      column(
        3,
        tags$div(id = "inline", 
                 numericInput(
                   ns('inPlotWidth'),
                   "Width [in]",
                   11,
                   min = 1,
                   width = 100
                   )
        )
      ),
      column(
        3,
        tags$div(id = "inline", 
                 numericInput(
                   ns('inPlotHeight'),
                   "Height [in]",
                   8.5,
                   min = 1,
                   width = 100
                 )
        )
      )
    )
  )
}

# SERVER ----

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
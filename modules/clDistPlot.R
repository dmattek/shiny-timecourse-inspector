#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for plotting stacked bar plot with percentages of time series in clusters
#

# UI ----
modClDistPlotUI =  function(id, label = "Plot distribution of clusters per groupd") {
  ns <- NS(id)
  
  tagList(
    radioButtons(ns("rBAxisLabelsRotate"), "X-axis labels:",
                 c("horizontal" = 0,
                   "45 deg" = 45,
                   "90 deg" = 90), inline = T),
    actionButton(ns('butPlotClDist'), 'Plot!'),
    plotOutput(ns('outPlotClDist'), height = PLOTBOXHEIGHT, width = 'auto'),
    downPlotUI(ns('downPlotClDist'), "Download Plot")
  )
}



# SERVER ----

# Params:
# in.data - data prepared with data4clDistPlot f-n
# in.colors - table with two columns:
#             - gr.no - with group/cluster number, 
#             - gr.col - with colour assignments
#             prepared with getClColHier
# in.fname - file name for plot download
modClDistPlot = function(input, output, session, 
                         in.data, 
                         in.colors = NULL, 
                         in.fname = 'clDist.pdf') {
  
  ns <- session$ns
 
  # Barplot with distribution of clusters across conditions
  plotClDist = function() {
    cat(file = stderr(), 'plotClDist: in\n')
    
    loc.dt = in.data()
    validate(
      need(!is.null(loc.dt), "Nothing to plot. Load data first!")
    )
    
    # Two statements: "position_fill(reverse = TRUE)" and "guide_legend(reverse = T)"
    # result in a stacked bar plot with categories ordered from the bottom to top of the stacked bar
    p.out = ggplot(loc.dt, aes_string(x = COLGR, y = COLNTRAJ)) +
      geom_bar(aes_string(fill = paste0("as.factor(", COLCL, ")")), 
               stat = 'identity', 
               position = position_fill(reverse = TRUE)) +
      guides(fill = guide_legend(reverse = T))
    
    if(is.null(in.colors))
      p.out = p.out + scale_fill_discrete(name = "Cluster no.")
    else
      p.out = p.out + scale_fill_manual(name = "Cluster no.", 
                                        values = in.colors()[["gr.col"]])
    
    loc.rads = as.numeric(input$rBAxisLabelsRotate) * pi / 180
    loc.hjust = 0.5*(1-sin(loc.rads))
    loc.vjust = 0.5*(1-cos(loc.rads))
    
    p.out = p.out + 
      scale_y_continuous(labels = percent) +
      ylab("Percentage of time series\n") +  
      xlab("Groups") +  
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND) + 
      theme(
        axis.text.x = LOCrotatedAxisElementText(as.numeric(input$rBAxisLabelsRotate), 
                                                size = PLOTFONTAXISTEXT)
    )

    return(p.out)
    
  }
  
  #  display bar plot
  output$outPlotClDist <- renderPlot({

    plotClDist()
  })
  
  # bar Plot - download pdf
  callModule(downPlot, "downPlotClDist", in.fname, plotClDist, TRUE)
  
}
modClDistPlotUI =  function(id, label = "Plot Fractions WIthin Clusters") {
  ns <- NS(id)
  
  tagList(
    radioButtons(ns("rBAxisLabelsRotate"), "X-axis labels:",
                 c("horizontal" = 0,
                   "45 deg" = 45,
                   "90 deg" = 90)),
    actionButton(ns('butPlotClDist'), 'Plot!'),
    plotOutput(ns('outPlotClDist'), height = '800px', width = 'auto'),
    downPlotUI(ns('downPlotClDist'), "Download PDF")
  )
}



# Params:
# in.data - data prepared with data4clDistPlot f-n
# in.cols - table with 1st column as cluster number, 2nd column colour assignments
#           prepared with getClColHier
# in.fname - file name for plot download
modClDistPlot = function(input, output, session, in.data, in.cols = NULL, in.fname = 'clDist.pdf') {
  
  ns <- session$ns
 
  # Barplot with distribution of clusters across conditions
  plotClDist = function() {
    cat(file = stderr(), 'plotClDist: in\n')
    
    loc.dt = in.data()
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'plotClDist: dt is NULL\n')
      return(NULL)
    }
    
    # Two statements: "position_fill(reverse = TRUE)" and "guide_legend(reverse = T)"
    # result in  stacked bar plot with categories ordered from the bottom to top of the stacked bar
    p.out = ggplot(loc.dt[], aes(x = group, y = nCells)) +
      geom_bar(aes(fill = as.factor(cl)), stat = 'identity', position = position_fill(reverse = TRUE)) +
      guides(fill = guide_legend(reverse = T))
    
    if(is.null(in.cols))
      p.out = p.out + scale_fill_discrete(name = "Cluster no.")
    else
      p.out = p.out + scale_fill_manual(name = "Cluster no.", 
                                        values = in.cols()$cl.col) #,
                                        #breaks = in.cols()$cl.no,
                                        #labels = in.cols()$cl.no,
                                        #limits = in.cols()$cl.no)
    
    
    loc.rads = as.numeric(input$rBAxisLabelsRotate) * pi / 180
    loc.hjust = 0.5*(1-sin(loc.rads))
    loc.vjust = 0.5*(1-cos(loc.rads))
    
    p.out = p.out + 
      scale_y_continuous(labels = percent) +
      ylab("percentage of cells\n") +  
      xlab("") +  
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND) + 
      theme(
        axis.text.x = LOCrotatedAxisElementText(as.numeric(input$rBAxisLabelsRotate))
    )

    return(p.out)
    
  }
  
  #  display bar plot
  output$outPlotClDist <- renderPlot({
    locBut = input$butPlotClDist
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotClDist: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotClDist()
  })
  
  # bar Plot - download pdf
  callModule(downPlot, "downPlotClDist", in.fname, plotClDist, TRUE)
  
}
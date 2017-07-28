modClDistPlotUI =  function(id, label = "Plot Fractions WIthin Clusters") {
  ns <- NS(id)
  
  tagList(
    actionButton(ns('butPlotClDist'), 'Plot!'),
    plotOutput(ns('outPlotClDist'), height = '800px', width = 'auto'),
    downPlotUI(ns('downPlotClDist'), "Download PDF")
  )
}


modClDistPlot = function(input, output, session, in.data, in.fname = 'clDist.pdf') {
  
  ns <- session$ns
 
  # Barplot with distribution of clusters across conditions
  plotClDist = function() {
    cat(file = stderr(), 'plotClDist: in\n')
    
    loc.dt = in.data()
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'plotClDist: dt is NULL\n')
      return(NULL)
    }
    
    p.out = ggplot(loc.dt, aes(x = group, y = nCells)) +
      geom_bar(aes(fill = as.factor(cl)), stat = 'identity', position = 'fill') +
      scale_y_continuous(labels = percent) +
      ylab("percentage of cells\n") +  
      xlab("") +  
      scale_fill_discrete(name = "Cluster no.") +
      myGgplotTheme
    
    return(p.out)
    
  }
  
  #  Hierarchical - display bar plot
  output$outPlotClDist <- renderPlot({
    locBut = input$butPlotClDist
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotClDist: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotClDist()
  })
  
  # Hierarchical - Bar Plot - download pdf
  callModule(downPlot, "downPlotClDist", in.fname, plotClDist, TRUE)
  
}
#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Marc-Antoine Jacques
#
# This module is for plotting Power Spectral Analysis
#

require(DT)
require(scales)

 # UI ----
modPSDPlotUI =  function(id, label = "Plot PSD of average trajectory.") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        2,
        radioButtons(ns('rBPSDmethod'), 'Method for PSD estimation:', list('Smoothed Fourier' = 'pgram', 'AR Fit' = 'ar')),
        checkboxInput(ns('chBplotTrajInt'), 'Interactive Plot'),
        actionButton(ns('butPlotTraj'), 'Plot!')
      ),
      column(
        2,
        selectInput(ns('inPSDxchoice'), 'X-axis:', list('Period'= TRUE, 'Frequency'= FALSE)),
        numericInput(ns('ninPSDsamplFreq'), '# time units between 2 points:', value = 1, min = 0, step = 1)
      ),
      column(
        2,
        selectInput(ns('inPSDtransXtype'), 'Transform X-axis:', list('none' = 'none',
                                                                   '1/x'='inverse_trans', 
                                                                   'log2'= 'log2', 
                                                                   'log10'= 'log10', 
                                                                   'ln'= 'log')),
        selectInput(ns('inPSDtransYtype'), 'Transform Y-axis:', list('none' = 'none',
                                                                   '1/y'='inverse_trans', 
                                                                   'log2'= 'log2', 
                                                                   'log10'= 'log10', 
                                                                   'ln'= 'log'))
      ),
      column(
        2,
        numericInput(
          ns('inPlotTrajWidth'),
          'Width [%]:',
          value = 100,
          min = 10,
          width = '100px',
          step = 10
        ),
        numericInput(
          ns('inPlotTrajHeight'),
          'Height [px]:',
          value = PLOTPSDHEIGHT,
          min = 100,
          width = '100px',
          step = 50
        )
      )
    ),
    uiOutput(ns('uiPlotTraj')),
    downPlotUI(ns('downPlotTraj'), "Download PDF")
  )
}

# Server ----

modPSDPlot = function(input, output, session, 
                             in.data,
                             in.fname,
                             in.facet = 'group', 
                             in.facet.color = NULL
                             ) {
  
  ns <- session$ns
  
  output$uiPlotTraj = renderUI({
    if (input$chBplotTrajInt)
      plotlyOutput(
        ns("outPlotTrajInt"),
        width = paste0(input$inPlotTrajWidth, '%'),
        height = paste0(input$inPlotTrajHeight, 'px')
      ) else
        plotOutput(
          ns("outPlotTraj"),
          width = paste0(input$inPlotTrajWidth, '%'),
          height = paste0(input$inPlotTrajHeight, 'px')
        )
  })
  
  
  output$outPlotTraj <- renderPlot({
    
    loc.p = plotTraj()
    if(is.null(loc.p))
      return(NULL)
    
    return(loc.p)
  })
  
  
  output$outPlotTrajInt <- renderPlotly({
    # This is required to avoid
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    if (names(dev.cur()) != "null device")
      dev.off()
    pdf(NULL)
    
    loc.p = plotTraj()
    if(is.null(loc.p))
      return(NULL)
    
    return(plotly_build(loc.p))
  })
  
  
  
  # PSD plot - download pdf
  callModule(downPlot, "downPlotTraj", 
             in.fname = in.fname,
             plotTraj, TRUE)
  
  plotTraj <- function() {
    cat(file = stderr(), 'plotPSD: in\n')
    locBut = input$butPlotTraj
    
    if (locBut == 0) {
      cat(file = stderr(), 'plotPSD: Go button not pressed\n')
      
      return(NULL)
    }
    
    # check if main data exists
    loc.dt = isolate(in.data())
    
    cat("plotPSD: on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'plotPSD: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'plotPSD: dt not NULL\n')
    
    
    
    # Future: change such that a column with colouring status is chosen by the user
    # colour trajectories, if dataset contains mid.in column
    # with filtering status of trajectory
    if (sum(names(loc.dt) %in% 'mid.in') > 0)
      loc.line.col.arg = 'mid.in'
    else
      loc.line.col.arg = NULL
    
    # select every other point for plotting (fixed for PSD because lead to false interpretation of PSD)
    loc.dt = loc.dt[, .SD[seq(1, .N, 1)], by = id]

    # check if columns with XY positions are present
    if (sum(names(loc.dt) %like% 'pos') == 2)
      locPos = TRUE
    else
      locPos = FALSE
    
    # check if column with ObjectNumber is present
    if (sum(names(loc.dt) %like% 'obj.num') == 1)
      locObjNum = TRUE
    else
      locObjNum = FALSE
    
    
    # If in.facet.color present,
    # make sure to include the same number of colours in the palette,
    # as the number of groups in dt.
    # in.facet.color is typically used when plotting time series within clusters.
    # Then, the number of colours in the palette has to be equal to the number of clusters (facetted according to in.facet variable).
    # This might differ if the user selects manually clusters to display.
    if (is.null(in.facet.color)) 
      loc.facet.col = NULL 
    else {
      # get group numbers in dt; 
      # loc.dt[, c(in.facet), with = FALSE] returns a data table with a single column
      # [[1]] at the end extracts the first column and returns as a vector
      loc.groups = unique(loc.dt[, c(in.facet), with = FALSE][[1]])
      
      # get colour palette
      # the length is equal to the number of groups in the original dt.
      # When plotting time series within clusters, the length equals the number of clusters.
      loc.facet.col = in.facet.color()$cl.col
      loc.facet.col = loc.facet.col[loc.groups]
    }
    

    loc.dt.aggr <- LOCcalcPSD(in.dt = loc.dt,
                              in.col.meas = 'y',
                              in.col.id = 'id',
                              in.col.by = in.facet,
                              in.method = input$rBPSDmethod,
                              in.return.period = input$inPSDxchoice,
                              in.time.btwPoints = input$ninPSDsamplFreq
                              )
    loc.dt.aggr[, (in.facet) := as.factor(get(in.facet))]
    
    x_arg <- ifelse('period' %in% colnames(loc.dt.aggr), 'period', 'frequency')
    x_arg_str <- paste0(toupper(substr(x_arg, 1, 1)), tolower(substring(x_arg, 2)))  # capitalized
    p.out <- LOCplotPSD(dt.arg = loc.dt.aggr,
                        x.arg = x_arg,
                        y.arg = 'spec',
                        group.arg = in.facet,
                        facet.color.arg = loc.facet.col,
                        xlab.arg = x_arg_str,
                        ylab.arg = '') +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND)

    # TODO: Restore tick labels when using inverse transformation
    # See: https://stackoverflow.com/questions/56130614/ggplot2-missing-labels-after-custom-scaling-of-axis
    inverse_trans <- scales::trans_new("myinverse", transform = function(x) 1/x,
                                       inverse = function(x) 1/(1/x))
    
    if(input$inPSDtransXtype == "inverse_trans"){
      p.out <- p.out + scale_x_continuous(trans = inverse_trans)
    } else if (!(input$inPSDtransXtype == "none")) {
      p.out <- p.out + scale_x_continuous(trans = input$inPSDtransXtype)
    }
    
    if(input$inPSDtransYtype == "inverse_trans"){
      p.out <- p.out + scale_y_continuous(trans = inverse_trans)
    } else if (!(input$inPSDtransYtype == "none")) {
      p.out <- p.out + scale_y_continuous(trans = input$inPSDtransYtype)
    }
    
    return(p.out)
  }
}
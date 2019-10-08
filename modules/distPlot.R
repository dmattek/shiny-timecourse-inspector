#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for plotting distrubutions at selected time points as a choice of box/violin/dot-plots
# Assumes in.data contains columns:
# realtime
# y
# group
# id

# UI ----
modDistPlotUI =  function(id, label = "Plot distributions") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        4,
        checkboxInput(ns("chBplotTypeBox"),  "Box-plot", value = T),
        checkboxInput(ns("chBplotTypeDot"),  "Dot-plot", value = F),
        checkboxInput(ns("chBplotTypeViol"), "Violin-plot", value = F),
        checkboxInput(ns("chBplotTypeLine"), "Line-plot", value = F),
        checkboxInput(ns('chBplotInt'), 'Interactive Plot'),
        actionButton(ns('butPlot'), 'Plot!')
      ),
      column(
        4,
        uiOutput(ns('uiPlotBoxNotches')),
        uiOutput(ns('uiPlotBoxOutliers')),
        uiOutput(ns('uiPlotBoxDodge')),
        uiOutput(ns('uiPlotBoxAlpha')),
        uiOutput(ns('uiPlotDotNbins')),
        uiOutput(ns('uiPlotDotAlpha')),
        uiOutput(ns('uiPlotViolAlpha')),
        uiOutput(ns('uiPlotLineAlpha'))
      ),
      column(
        4,
        selectInput(
          ns('selPlotBoxLegendPos'),
          label = 'Legend position',
          choices = list(
            "Top" = 'top',
            "Right" = 'right',
            "Bottom" = 'bottom'
          ), 
          width = "120px",
          selected = 'top'
        ),
        radioButtons(ns("rBAxisLabelsRotate"), "X-axis labels",
                     c("horizontal" = 0,
                       "45 deg" = 45,
                       "90 deg" = 90)),
        numericInput(
          ns('inPlotBoxWidth'),
          'Width [%]',
          value = PLOTWIDTH,
          min = 10,
          width = '100px',
          step = 10
        ),
        numericInput(
          ns('inPlotBoxHeight'),
          'Height [px]',
          value = PLOTBOXHEIGHT,
          min = 100,
          width = '100px',
          step = 50
        )
      )
    ),
    
    uiOutput(ns('uiPlotBox')),
    downPlotUI(ns('downPlotBox'), "Download Plot")
  )
}

# SERVER ----
modDistPlot = function(input, output, session, 
                      in.data,                       # input data table in long format
                      in.cols = list(meas.x = COLRT, # column names
                                     meas.y = COLY,
                                     group = COLGR,
                                     id = COLID), 
                      in.labels = list(x = "",       # plot labels
                                       y = "", 
                                       legend = ""),
                      in.fname) {                      # file name for saving the plot

  ns <- session$ns
  
  output$uiPlotBoxNotches = renderUI({
    cat(file = stderr(), 'boxPlot:uiPlotBoxNotches\n')
    
    ns <- session$ns
    
    if(input$chBplotTypeBox)
      checkboxInput(ns('chBplotBoxNotches'), 'Notches in box-plot ', FALSE)
  })
  
  output$uiPlotBoxOutliers = renderUI({
    cat(file = stderr(), 'boxPlot:uiPlotBoxOutliers\n')
    
    ns <- session$ns
    
    if(input$chBplotTypeBox)
      checkboxInput(ns('chBplotBoxOutliers'), 'Outliers in box-plot', FALSE)
  })
  
  output$uiPlotBoxDodge = renderUI({
    cat(file = stderr(), 'boxPlot:uiPlotBoxDodge\n')
    
    ns <- session$ns
    
    # Adjust spacing between box-, violin-, dot-plots.
    # Valid only when plotting multiple groups at a time point.
    # For line plot, each group is drawn separately per facet, thus no need for dodging..
    
    if(!input$chBplotTypeLine)
      sliderInput(ns('slPlotBoxDodge'), 'Space between groups', min = 0, max = 1, value = .4, step = 0.05)
  })
  
  output$uiPlotBoxAlpha = renderUI({
    cat(file = stderr(), 'boxPlot:uiPlotBoxAlpha\n')
    
    ns <- session$ns
    
    if(input$chBplotTypeBox)
      sliderInput(ns('slPlotBoxAlpha'), 'Box-plot transparency', min = 0, max = 1, value = 1, step = 0.1)
  })
  
  output$uiPlotViolAlpha = renderUI({
    cat(file = stderr(), 'boxPlot:uiPlotViolAlpha\n')
    
    ns <- session$ns
    
    if(input$chBplotTypeViol)
      sliderInput(ns('slPlotViolAlpha'), 'Violin-plot transparency', min = 0, max = 1, value = 1, step = 0.1)
  })
  
  output$uiPlotDotAlpha = renderUI({
    cat(file = stderr(), 'boxPlot:uiPlotDotAlpha\n')
    
    ns <- session$ns
    
    if(input$chBplotTypeDot)
      sliderInput(ns('slPlotDotAlpha'), 'Dot-plot transparency', min = 0, max = 1, value = 1, step = 0.1)
  })
  
  output$uiPlotLineAlpha = renderUI({
    cat(file = stderr(), 'boxPlot:uiPlotLineAlpha\n')
    
    ns <- session$ns
    
    if(input$chBplotTypeLine)
      sliderInput(ns('slPlotLineAlpha'), 'Line-plot transparency', min = 0, max = 1, value = 1, step = 0.1)
  })
  
  output$uiPlotDotNbins = renderUI({
    cat(file = stderr(), 'boxPlot:uiPlotDotNbins\n')
    
    ns <- session$ns
    
    if(input$chBplotTypeDot)
      sliderInput(ns('slPlotDotNbins'), 'Number of bins in dot-plot', min = 2, max = 50, value = 30, step = 1)
  })
  
  # Boxplot - display
  output$outPlotBox = renderPlot({
    
    plotBox()
    
  })
  
  
  output$outPlotBoxInt = renderPlotly({
    
    # This is required to avoid 
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    if (names(dev.cur()) != "null device") dev.off()
    pdf(NULL)
    
    return( ggplotly(plotBox())  %>% layout(boxmode = 'group', width = '100%', height = '100%'))
    
  })
  
  
  output$uiPlotBox <- renderUI({
    ns <- session$ns
    
    if (input$chBplotInt)
      plotlyOutput(ns("outPlotBoxInt"), 
                   width = paste0(input$inPlotBoxWidth, '%'),
                   height = paste0(input$inPlotBoxHeight, 'px'))
    else
      plotOutput(ns('outPlotBox'),
                 width = paste0(input$inPlotBoxWidth, '%'),
                 height = paste0(input$inPlotBoxHeight, 'px'))
  })
  
  # Boxplot - download pdf
  callModule(downPlot, "downPlotBox", in.fname, plotBox, TRUE)
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  
  plotBox <- function() {
    cat(file = stderr(), 'plotBox\n')
    locBut = input$butPlot
    
    # Check if main data exists
    # Thanks to solate all mods in the left panel are delayed 
    # until clicking the Plot button
    loc.dt = isolate(in.data())
    validate(
      need(!is.null(loc.dt), "Nothing to plot. Load data first!")
    )    
    
    cat(file = stderr(), 'plotBox:dt not NULL\n')
    
    if(!input$chBplotTypeLine) {
      # Dodging series only for box-, dot-, and violin-plots 
      loc.par.dodge <- position_dodge(width = input$slPlotBoxDodge)
      
      # Color fill for all oplots except line, in which groups are plotted per facet
      p.out = ggplot(loc.dt, aes_string(x = sprintf("factor(%s)", in.cols$meas.x), 
                                        y = in.cols$meas.y,
                                        fill = in.cols$group)) 
      
    }
    else {
      loc.par.dodge = position_dodge(width = 1)
      p.out = ggplot(loc.dt, aes_string(x = sprintf("factor(%s)", in.cols$meas.x), 
                                        y = in.cols$meas.y)) 
      
    }
    
    
    if(input$chBplotTypeDot)
      p.out = p.out + geom_dotplot(color = NA,
                                   binaxis = "y", 
                                   stackdir = "center", 
                                   position = loc.par.dodge, 
                                   binwidth = abs(max(loc.dt[[ in.cols$meas.y ]], 
                                                      na.rm = T) - 
                                                    min(loc.dt[[ in.cols$meas.y ]], 
                                                        na.rm = T)) / (input$slPlotDotNbins - 1), 
                                   method = 'histodot',
                                   alpha = input$slPlotDotAlpha)
    
    if(input$chBplotTypeViol)
      p.out = p.out + 
      geom_violin(position = loc.par.dodge,
                  width = 0.2,
                  alpha = input$slPlotViolAlpha)
    
    if(input$chBplotTypeLine)
      p.out = p.out + 
        geom_path(aes_string(group = in.cols$id),
                  alpha = input$slPlotLineAlpha) +
        facet_wrap(as.formula(paste("~", in.cols$group)))
    
    if (input$chBplotTypeBox)
      p.out = p.out + geom_boxplot(
        position = loc.par.dodge,
        notch = input$chBplotBoxNotches, 
        alpha = input$slPlotBoxAlpha,
        outlier.colour = if (input$chBplotBoxOutliers)
          'red'
        else
          NA
      ) 
    
    p.out = p.out +
      scale_fill_discrete(name = in.labels$legend) +
      xlab(in.labels$x) +
      ylab(in.labels$y) +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND) + 
      theme(legend.position = input$selPlotBoxLegendPos,
            axis.text.x = LOCrotatedAxisElementText(as.numeric(input$rBAxisLabelsRotate), 
                                                    size = PLOTFONTAXISTEXT))
    
    
    return(p.out)
  }
  
}
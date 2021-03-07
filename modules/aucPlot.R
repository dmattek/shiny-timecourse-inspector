#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for plotting AUC as a choice of box/violin/dot-plots
# Assumes in.data contains columns:
# realtime
# y
# group
# id

# UI ----
modAUCplotUI =  function(id, label = "Plot AUC distributions") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        4,
        checkboxInput(ns("chBplotTypeBox"),  "Box-plot", value = T),
        checkboxInput(ns("chBplotTypeDot"),  "Dot-plot", value = F),
        checkboxInput(ns("chBplotTypeViol"), "Violin-plot", value = F)
      ),
      
      column(4,
             conditionalPanel(
               condition = "input.chBplotTypeBox",
               ns = ns,
               
               checkboxInput(ns('chBplotBoxNotches'), 
                             'Notches in box-plot ', 
                             FALSE),
               checkboxInput(ns('chBplotBoxOutliers'), 
                             'Outliers in box-plot', 
                             FALSE)
             ),
             
             conditionalPanel(
               condition = "input.chBplotTypeDot",
               ns = ns,
               
               sliderInput(ns('slPlotDotNbins'), 
                           'Number of bins in dot-plot', 
                           min = 2, 
                           max = 50, 
                           value = 30, 
                           step = 1),
               sliderInput(ns('slPlotDotShade'), 
                           "Shade of grey in dot-plot", 
                           min = 0, 
                           max = 1, 
                           value = 0.5, 
                           step = 0.1)
             )
      )
    ),
    
    checkboxInput(ns('chBplotStyle'),
                  'Appearance',
                  FALSE),
    
    conditionalPanel(
      condition = "input.chBplotStyle",
      ns = ns,
      
      fluidRow(
        column(4,
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
               )),
        column(4,
               radioButtons(ns("rBAxisLabelsRotate"), "X-axis labels",
                            c("horizontal" = 0,
                              "45 deg" = 45,
                              "90 deg" = 90))),
        column(4,
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
      )
    ),
    
    
    checkboxInput(ns('chBdownload'),
                  'Download',
                  FALSE),
    conditionalPanel(
      condition = "input.chBdownload",
      ns = ns,
      
      downPlotUI(ns('downPlotBox'), "Download")
    ),
    
    fluidRow(
      column(2,
             actionButton(ns('butPlot'), 'Plot!')),
      column(2,
             checkboxInput(ns('chBplotInt'), 'Interactive'))),
    uiOutput(ns('uiPlotBox'))
  )
}

# SERVER ----
modAUCplot = function(input, output, session, 
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
    cat(file = stderr(), 'aucPlot:plotBox\n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlot
    
    # Check if main data exists
    # Thanks to solate all mods in the left panel are delayed 
    # until clicking the Plot button
    loc.dt = shiny::isolate(in.data())
    shiny::validate(
      shiny::need(!is.null(loc.dt), "Nothing to plot. Load data first!")
    )    
    
    cat(file = stderr(), 'plotBox:dt not NULL\n')
    
    p.out = ggplot(loc.dt, aes_string(x = sprintf("factor(%s)", in.cols$meas.x), 
                                      y = in.cols$meas.y)) 
    
    
    if(input$chBplotTypeDot) {
      # calculate bin width for dot-plot based on nBins provided in the UI
      loc.binwidth = abs(max(loc.dt[[ in.cols$meas.y ]], 
                             na.rm = T) - 
                           min(loc.dt[[ in.cols$meas.y ]], 
                               na.rm = T)) / (input$slPlotDotNbins - 1)
      
      p.out = p.out + geom_dotplot(fill = grey(input$slPlotDotShade),
                                   color = NA,
                                   binaxis = "y", 
                                   stackdir = "center", 
                                   binwidth = loc.binwidth, 
                                   method = 'histodot')
      
    }
    
    if(input$chBplotTypeViol)
      p.out = p.out + 
      geom_violin(fill = NA,
                  color = "black",
                  width = 0.2)
    
    if (input$chBplotTypeBox)
      p.out = p.out + geom_boxplot(
        fill = NA,
        color = "black",
        notch = input$chBplotBoxNotches, 
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
      theme(axis.text.x = LOCrotatedAxisElementText(as.numeric(input$rBAxisLabelsRotate), 
                                                    size = PLOTFONTAXISTEXT))
    
    
    return(p.out)
  }
  
}
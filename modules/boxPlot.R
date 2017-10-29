# Module for plotting a choice of box/violin/dot-plots
# Assumes in.data contains columns:
# realtime
# y
# group
# id

modBoxPlotUI =  function(id, label = "Plot Box-plots") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        4,
        checkboxGroupInput(ns('inPlotType'), 'Plot type:', list('Dot-plot' = 'dot', 
                                                                'Violin-plot' = 'viol',
                                                                'Box-plot' = 'box', 
                                                                'Line-plot' = 'line'), selected = 'box'),
        checkboxInput(ns('chBPlotBoxInt'), 'Interactive Plot?'),
        actionButton(ns('butPlotBox'), 'Plot!')
      ),
      column(
        4,
        selectInput(
          ns('selPlotBoxLegendPos'),
          label = 'Select legend position',
          choices = list(
            "Top" = 'top',
            "Right" = 'right',
            "Bottom" = 'bottom'
          ),
          selected = 'top'
        ),
        checkboxInput(ns('chBxAxisLabelsRotate'), 'Rotate x-axis labels?'),
        uiOutput(ns('uiPlotBoxNotches')),
        uiOutput(ns('uiPlotBoxOutliers')),
        uiOutput(ns('uiPlotBoxDodge')),
        #uiOutput(ns('uiPlotBoxWidth')),
        uiOutput(ns('uiPlotBoxAlpha')),
        uiOutput(ns('uiPlotDotNbins'))
      ),
      column(
        4,
        numericInput(
          ns('inPlotBoxWidth'),
          'Width [%]:',
          value = 100,
          min = 10,
          width = '100px',
          step = 10
        ),
        numericInput(
          ns('inPlotBoxHeight'),
          'Height [px]:',
          value = 800,
          min = 100,
          width = '100px',
          step = 50
        )
      )
    ),
    
    uiOutput(ns('uiPlotBox')),
    downPlotUI(ns('downPlotBox'), "Download PDF")
  )
}


modBoxPlot = function(input, output, session, 
                      in.data, 
                      in.cols = list(meas.x = 'realtime',
                                     meas.y = 'y',
                                     group = 'group',
                                     id = 'id'), 
                      in.fname = 'boxplot.pdf') {
  
  ns <- session$ns
 
  output$uiPlotBoxNotches = renderUI({
    cat(file = stderr(), 'UI uiPlotBoxNotches\n')
    
    ns <- session$ns
    
    if('box' %in% input$inPlotType)
      checkboxInput(ns('inPlotBoxNotches'), 'Box plot notches?', FALSE)
  })
  
  output$uiPlotBoxOutliers = renderUI({
    cat(file = stderr(), 'UI uiPlotBoxNotches\n')
    
    ns <- session$ns
    
    if('box' %in% input$inPlotType)
      checkboxInput(ns('inPlotBoxOutliers'), 'Box plot outliers?', FALSE)
  })
  
  output$uiPlotBoxDodge = renderUI({
    cat(file = stderr(), 'UI uiPlotBoxDodge\n')
    
    ns <- session$ns
    
    if(!( 'line' %in% input$inPlotType  ))
      sliderInput(ns('inPlotBoxDodge'), 'Dodge series:', min = 0, max = 1, value = .4, step = 0.05)
  })
  
  output$uiPlotBoxWidth = renderUI({
    cat(file = stderr(), 'UI uiPlotBoxWidth\n')
    
    ns <- session$ns
    
    if('box' %in% input$inPlotType)
      sliderInput(ns('inPlotBoxWidth'), 'Box plot width:', min = 0, max = 1, value = .2, step = 0.1)
  })
  
  output$uiPlotBoxAlpha = renderUI({
    cat(file = stderr(), 'UI uiPlotBoxAlpha\n')
    
    ns <- session$ns
    
    if('box' %in% input$inPlotType)
      sliderInput(ns('inPlotBoxAlpha'), 'Box plot transparency:', min = 0, max = 1, value = 1, step = 0.05)
  })
  
  output$uiPlotDotNbins = renderUI({
    cat(file = stderr(), 'UI uiPlotDotNbins\n')
    
    ns <- session$ns
    
    if('dot' %in% input$inPlotType)
      sliderInput(ns('inPlotDotNbins'), 'Dot-plot bin size (10^x):', min = -4, max = 4, value = -1.5, step = 0.1)
  })
  
  # Boxplot - display
  output$outPlotBox = renderPlot({
    locBut = input$butPlotBox
    
    if (locBut == 0) {
      cat(file = stderr(), 'plotBox: Go button not pressed\n')
      return(NULL)
    }
    
    plotBox()
    
  })
  
  
  output$outPlotBoxInt = renderPlotly({
    locBut = input$butPlotBox
    
    if (locBut == 0) {
      cat(file = stderr(), 'plotBox: Go button not pressed\n')
      return(NULL)
    }
    
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
    
    if (input$chBPlotBoxInt)
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
    
    loc.dt = in.data()
    
    cat(file = stderr(), "plotBox: on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'plotBox: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'plotBox:dt not NULL\n')
    
    loc.par.dodge <- position_dodge(width = input$inPlotBoxDodge)
    p.out = ggplot(loc.dt, aes_string(x = sprintf("factor(%s)", in.cols[['meas.x']]), y = in.cols[['meas.y']])) 
    
    if('dot' %in% input$inPlotType)
      p.out = p.out + geom_dotplot(aes_string(fill = in.cols[['group']]), 
                                   binaxis = "y", 
                                   stackdir = "center", 
                                   position = loc.par.dodge, 
                                   binwidth = 10^(input$inPlotDotNbins), 
                                   method = 'histodot')
    
    if('viol' %in% input$inPlotType)
      p.out = p.out + geom_violin(aes_string(fill = in.cols[['group']]),
                                  position = loc.par.dodge,
                                  width = 0.2)
    
    if('line' %in% input$inPlotType)
      p.out = p.out + 
      geom_path(aes_string(color = in.cols[['group']], group = in.cols[['id']])) +
      facet_wrap(as.formula(paste("~", in.cols[['group']])))
    
    if ('box' %in% input$inPlotType)
      p.out = p.out + geom_boxplot(
        aes_string(fill = in.cols[['group']]), 
        position = loc.par.dodge,
        #width = 0.2, #input$inPlotBoxWidth,
        notch = input$inPlotBoxNotches, 
        alpha = input$inPlotBoxAlpha,
        outlier.colour = if (input$inPlotBoxOutliers)
          'red'
        else
          NA
      ) 
    
    p.out = p.out +
      scale_fill_discrete(name = '') +
      xlab('') +
      ylab('') +
      theme_bw(base_size = 18, base_family = "Helvetica") +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.25),
        axis.line.y = element_line(color = "black", size = 0.25),
        axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        strip.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(1, "lines"),
        legend.key.width = unit(2, "lines"),
        legend.position = input$selPlotBoxLegendPos
      )
    
    if (input$chBxAxisLabelsRotate)
      p.out = p.out +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
    else
      p.out = p.out +
      theme(axis.text.x = element_text(size = 12))
    
    
    return(p.out)
  }
  
}
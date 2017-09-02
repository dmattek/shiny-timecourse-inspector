require(DT)

tabBoxPlotUI =  function(id, label = "Comparing t-points") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Box-/dot-/violin plot at selected time points"
    ),
    br(),
    
    uiOutput(ns('varSelTpts')),
    
    checkboxInput(ns('chbTabStats'), 'Show stats', FALSE),
    uiOutput(ns('uiTabStats')),
    uiOutput(ns('uiDownSingleCellData')),
    
    br(),
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

####
## server box-plot
tabBoxPlot = function(input, output, session, in.data) {
  # return all unique time points (real time)
  # This will be used to display in UI for box-plot
  # These timepoints are from the original dt and aren't affected by trimming of x-axis
  getDataTpts <- reactive({
    cat(file = stderr(), 'getDataTpts\n')
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[, realtime])) # column name specified in data4trajPlot
  })
  
  # prepare data for plotting boxplots
  # uses the same dt as for trajectory plotting
  # returns dt with these columns:
  data4boxPlot <- reactive({
    cat(file = stderr(), 'data4boxPlot\n')
    
    loc.dt = in.data()
    if (is.null(loc.dt))
      return(NULL)
    
    loc.out = loc.dt[realtime %in% input$inSelTpts]
  })
  
  output$varSelTpts = renderUI({
    cat(file = stderr(), 'UI varSelTpts\n')
    
    ns <- session$ns
    
    loc.v = getDataTpts()
    if (!is.null(loc.v)) {
      selectInput(
        ns('inSelTpts'),
        'Select one or more timepoints:',
        loc.v,
        width = '100%',
        selected = 0,
        multiple = TRUE
      )
    }
  })

  output$uiTabStats = renderUI({
    cat(file = stderr(), 'UI uiTabStats\n')
    ns <- session$ns
    
    if(input$chbTabStats) {
      DT::dataTableOutput(ns('outTabStats'))
    }
  })
  
  output$uiDownSingleCellData = renderUI({
    cat(file = stderr(), 'UI uiDownSingleCellData\n')
    ns <- session$ns
    
    if(input$chbTabStats) {
      downloadButton(ns('downloadData4BoxPlot'), 'Download single-cell data')
    }
  })
  
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
    
    if(!( input$inPlotType %in% 'line' ))
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
  
  
  calcStats = reactive({
    cat(file = stderr(), 'tabBoxPlot: calsStats\n')
    loc.dt = data4boxPlot()
    
    if (is.null(loc.dt))
      return(NULL)
    
    loc.dt.aggr = loc.dt[, sapply(.SD, function(x) list('N' = .N, 
                                                        'Mean' = mean(x), 
                                                        'CV' = sd(x)/mean(x), 
                                                        'Median' = median(x), 
                                                        'rCV (IQR)' = IQR(x)/median(x), 
                                                        'rCV (MAD)'= mad(x)/median(x))), .SDcols = c('y'), by = .(realtime, group)]
    
    setnames(loc.dt.aggr, c('Time point', 'Group','N', 'Mean', 'CV', 'Median', 'rCV IQR', 'rCV MAD'))
    #print(loc.dt.aggr)
    return(loc.dt.aggr)
  })
  
  output$downloadData4BoxPlot <- downloadHandler(
    filename = 'data4boxplot.csv',
    content = function(file) {
      write.csv(data4boxPlot(), file, row.names = FALSE)
    }
  )
  
  
  # output$outTabStats = DT::renderDataTable(calcStats(), 
  #                                          server = FALSE, 
  #                                          rownames = FALSE,
  #                                          extensions = 'Buttons', 
  #                                          options = list(
  #                                            dom = 'Bfrtip',
  #                                            buttons = list('copy', 
  #                                                           'print', 
  #                                                           list(extend = 'collection',
  #                                                                buttons = list(list(extend='csv',
  #                                                                                    filename = 'hitStats'),
  #                                                                               list(extend='excel',
  #                                                                                    filename = 'hitStats'),
  #                                                                               list(extend='pdf',
  #                                                                                    filename= 'hitStats')),
  #                                                                text = 'Download'))))
  # 
  output$outTabStats = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'tabBoxPlot: outTabStats\n')
    loc.dt = calcStats()

    if (is.null(loc.dt))
      return(NULL)

    datatable(loc.dt, 
              rownames = FALSE,
              extensions = 'Buttons', 
              options = list(
                dom = 'Bfrtip',
                buttons = list('copy', 
                               'print', 
                               list(extend = 'collection',
                                    buttons = list(list(extend='csv',
                                                        filename = 'hitStats'),
                                                   list(extend='excel',
                                                        filename = 'hitStats'),
                                                   list(extend='pdf',
                                                        filename= 'hitStats')),
                                    text = 'Download')))) %>% formatRound(4:8, 3)
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
  callModule(downPlot, "downPlotBox", 'boxplot.pdf', plotBox, TRUE)
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  
  plotBox <- function() {
    cat(file = stderr(), 'plotBox\n')
    
    loc.dt = data4boxPlot()
    
    cat(file = stderr(), "plotBox: on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'plotBox: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'plotBox:dt not NULL\n')
    
    
    loc.par.dodge <- position_dodge(width = input$inPlotBoxDodge)
    p.out = ggplot(loc.dt, aes(x = as.factor(realtime), y = y)) 
    
    if('dot' %in% input$inPlotType)
      p.out = p.out + geom_dotplot(aes(fill = group), 
                                   binaxis = "y", 
                                   stackdir = "center", 
                                   position = loc.par.dodge, 
                                   binwidth = 10^(input$inPlotDotNbins), 
                                   method = 'histodot')
    
    if('viol' %in% input$inPlotType)
      p.out = p.out + geom_violin(aes(fill = group),
                                  position = loc.par.dodge,
                                  width = 0.2)

    if('line' %in% input$inPlotType)
      p.out = p.out + 
      geom_path(aes(color = group, group = id)) +
      facet_wrap(~ group)
    
    if ('box' %in% input$inPlotType)
      p.out = p.out + geom_boxplot(
        aes(fill = group), 
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
      xlab('\nTime (min)') +
      ylab('') +
      theme_bw(base_size = 18, base_family = "Helvetica") +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.25),
        axis.line.y = element_line(color = "black", size = 0.25),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        strip.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(1, "lines"),
        legend.key.width = unit(2, "lines"),
        legend.position = input$selPlotBoxLegendPos
      )
    
    return(p.out)
  }
  
}
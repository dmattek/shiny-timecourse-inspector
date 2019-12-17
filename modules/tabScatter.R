#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for plotting scatter plots between two time points
#
# Use:
# in ui.R
# tabPanel(
#  'Hierarchical',
#  clustHierUI('TabClustHier'))
#
# in server.R
# callModule(clustHier, 'TabClustHier', dataMod)
# where dataMod is the output from a reactive function that returns dataset ready for clustering

helpText.tabScatter = c(
  alScatter = paste0(
    "Display a relationship between measurements at two different time points as a scatter plot. ",
    "Instead of using the exact measurements at selected time points, you have an option to smooth and use local average of measurements around chosen time points."
  ),
  rBfoldChange = paste0(
    "Y-axis can display a value at a selected time point (the magnitude), ",
    "or a difference between the value at time point selected for Y-axis and the value at time point displayed on the X-axis ",
    "(i.e. the amplitude at the second time point with respect to the value at the first time point)."
  ),
  chBregression = 'Add a line with linear regression and regions of 95% confidence interval.',
  inAvgWin = paste0(
    "Length of the averaging window to smooth data before plotting. ",
    "Useful to avoid artefacts due to spurious variations at specific time points."
  ),
  inPlotHeight = 'Height in pixels of the displayed plot',
  inPlotNcolFacet = 'Number of facets in a row. Each facet displayes a scatter plot for a single group.',
  alert2differentTpts = "Select two different time points.",
  alertSmoothWrong = "Smoothing window smaller than the interval between existing time points."
)

# UI ----
tabScatterPlotUI <-
  function(id, label = "Scatter plot between two time points") {
    ns <- NS(id)
    
    tagList(
      h4("Scatter plot between two time points"),
      actionLink(ns("alScatter"), "Learn more"),
      br(),
      br(),
      fluidRow(
        column(
          4,
          uiOutput(ns('uiSelTptX')),
          uiOutput(ns('uiSelTptY')),
          bsAlert("alertAnchor2differentTpts")
        ),
        column(
          4,
          numericInput(
            ns('inAvgWin'),
            'Smoothing',
            value = 0,
            step = 1,
            min = 0,
            width = "120px"
          ),
          bsTooltip(
            ns('inAvgWin'),
            helpText.tabScatter[["inAvgWin"]],
            placement = "top",
            trigger = "hover",
            options = NULL
          ),
          checkboxInput(ns('chBregression'), 'Linear regression with 95% CI'),
          bsTooltip(
            ns('chBregression'),
            helpText.tabScatter[["chBregression"]],
            placement = "top",
            trigger = "hover",
            options = NULL
          ),
          radioButtons(
            ns('rBfoldChange'),
            'Y-axis',
            choices = c("Y" = "y", "Y - X" = "diff"),
            width = "100px",
            inline = T
          ),
          bsTooltip(
            ns("rBfoldChange"),
            helpText.tabScatter[["rBfoldChange"]],
            placement = "top",
            trigger = "hover",
            options = NULL
          )
        ),
        column(
          4,
          numericInput(
            ns('inPlotHeight'),
            'Height [px]',
            value = PLOTSCATTERHEIGHT,
            min = 100,
            step = 100,
            width = "100px"
          ),
          bsTooltip(
            ns('inPlotHeight'),
            helpText.tabScatter[["inPlotHeight"]],
            placement = "top",
            trigger = "hover",
            options = NULL
          ),
          
          numericInput(
            ns('inPlotNcolFacet'),
            '#columns',
            value = PLOTNFACETDEFAULT,
            min = 1,
            step = 1,
            width = "100px"
          ),
          bsTooltip(
            ns('inPlotNcolFacet'),
            helpText.tabScatter[["inPlotNcolFacet"]],
            placement = "top",
            trigger = "hover",
            options = NULL
          )
        )
      ),
      
      br(),
      checkboxInput(ns('plotInt'),
                    'Interactive Plot',
                    value = FALSE),
      actionButton(ns('butPlot'), 'Plot!'),
      uiOutput(ns("plotInt_ui")),
      downPlotUI(ns('downPlotScatter'), "Download Plot")
    )
  }

# SERVER ----
tabScatterPlot <-
  function(input, output, session, in.data, in.fname) {
    ns <- session$ns
    
    # UI rendering ----
    # return all unique time points (real time)
    # This will be used to display in UI for box-plot
    # These timepoints are from the original dt and aren't affected by trimming of x-axis
    getDataTpts <- reactive({
      cat(file = stderr(), 'getDataTpts\n')
      loc.dt = in.data()
      
      if (is.null(loc.dt))
        return(NULL)
      else
        return(unique(loc.dt[[COLRT]]))
    })
    
    output$uiSelTptX = renderUI({
      cat(file = stderr(), 'tabScatter:uiSelTptX\n')
      
      ns <- session$ns
      
      loc.v = getDataTpts()
      
      if (!is.null(loc.v)) {
        selectInput(
          ns('inSelTptX'),
          'Time point for X-axis',
          loc.v,
          width = '180px',
          selected = loc.v[[1]],
          multiple = FALSE
        )
      }
    })
    
    output$uiSelTptY = renderUI({
      cat(file = stderr(), 'tabScatter:uiSelTptY\n')
      
      ns <- session$ns
      
      loc.v = getDataTpts()
      
      if (!is.null(loc.v)) {
        selectInput(
          ns('inSelTptY'),
          'Time point for Y-axis',
          loc.v,
          width = '180px',
          selected = ifelse(length(loc.v) > 1, loc.v[[2]], loc.v[[1]]), 
          multiple = FALSE
        )
      }
    })
    
    # prepare a dataset for scatter plot from long format
    # picks y values from two time points as selected in the UI
    # returns a dt with columns x, y, id, and grouping
    # columns x and y containsmeasurments from two points
    data4scatterPlot <- reactive({
      cat(file = stderr(), 'data4scatterPlot\n')
      
      loc.dt.in = in.data()
      if (is.null(loc.dt.in))
        return(NULL)
      
      # obtain selected time points from UI
      loc.tpt.x = as.numeric(input$inSelTptX)
      loc.tpt.y = as.numeric(input$inSelTptY)
      
      # throw an error if both time points for the scatter plot are identical
      if (loc.tpt.x == loc.tpt.y) {
        createAlert(
          session,
          "alertAnchor2differentTpts",
          "alert2differentTpts",
          title = "Error",
          content = helpText.tabScatter[["alert2differentTpts"]],
          append = FALSE,
          style = "danger"
        )
        return(NULL)
        
      } else {
        closeAlert(session, "alert2differentTpts")
      }
      
      if (input$inAvgWin > 0) {
        # aggregate time points within smoothing window
        loc.winLen = input$inAvgWin
      } else {
        # get data from selected time points
        loc.winLen = .Machine$double.eps
      }
      
      # x and y separate
      # otherwise, a dcast w.r.t. time column would be required, 
      # which is risky if time is a float
      loc.dt.x = loc.dt.in[(get(COLRT) >= loc.tpt.x - 0.5*loc.winLen) & 
                             (get(COLRT) <= loc.tpt.x + 0.5*loc.winLen), 
                           .(y = mean(get(COLY), na.rm = T)), 
                           by = c(COLGR, COLID)]
      
      loc.dt.y = loc.dt.in[(get(COLRT) >= loc.tpt.y - 0.5*loc.winLen) & 
                             (get(COLRT) <= loc.tpt.y + 0.5*loc.winLen), 
                           .(y = mean(get(COLY), na.rm = T)), 
                           by = c(COLGR, COLID)]        
      
      
      # merge measurements from two time points
      loc.dt = merge(loc.dt.x, loc.dt.y, by = COLID)
      loc.dt[, (paste0(COLGR, ".y")) := NULL]
      
      # setting new names; columns with measurements from two time points 
      # are assigned x & y names, respectively (only for internal usage in this module)
      setnames(loc.dt,
               c(paste0(COLGR, '.x'), paste0(COLY, '.x'), paste0(COLY, '.y')),
               c(COLGR, 'x', 'y'))
      
      
      # calculating the fold change between two time points to display on the y axis
      if (input$rBfoldChange == "diff") {
        loc.dt[, y := y - x]
      }
      return(loc.dt)
      
    })
    
    
    # Plotting ----
    # Function instead of reactive as per:
    # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
    # This function is used to plot and to downoad a pdf
    
    plotScatter <- function() {
      cat(file = stderr(), "plotScatter\n")
      locBut = input$butPlot
      
      # Check if main data exists
      # Thanks to solate all mods in the left panel are delayed 
      # until clicking the Plot button
      loc.dt = shiny::isolate(data4scatterPlot())
      shiny::validate(
        shiny::need(!is.null(loc.dt), "Nothing to plot. Load data first!")
      )    
      
      cat(file = stderr(), 'plotScatter:dt not NULL\n')
      
      p.out = LOCggplotScat(
        dt.arg = loc.dt,
        plotlab.arg = NULL,
        facet.arg = COLGR,
        facet.ncol.arg = input$inPlotNcolFacet,
        alpha.arg = 0.5,
        trend.arg = input$chBregression,
        ci.arg = 0.95
      )
      return(p.out)
    }
    
    # Plot rendering ----
    output$outPlotScatter <- renderPlot({
      plotScatter()
    })
    
    output$outPlotScatterInt <- renderPlotly({
      # This is required to avoid
      # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
      # When running on a server. Based on:
      # https://github.com/ropensci/plotly/issues/494
      
      if (names(dev.cur()) != "null device")
        dev.off()
      pdf(NULL)
      
      return(plotly_build(plotScatter()))
      
    })
    
    # download pdf
    callModule(downPlot, "downPlotScatter", in.fname, plotScatter, TRUE)
    
    # Scatter plot - choose to display regular or interactive plot
    output$plotInt_ui <- renderUI({
      ns <- session$ns
      if (input$plotInt)
        tagList(withSpinner(plotlyOutput(
          ns("outPlotScatterInt"),
          height = paste0(input$inPlotHeight, "px")
        )))
      else
        tagList(withSpinner(plotOutput(
          ns('outPlotScatter'), height = paste0(input$inPlotHeight, "px")
        )))
    })
    
    # Pop-overs ----
    addPopover(
      session,
      id = ns("alScatter"),
      title = "Scatter plot",
      content = helpText.tabScatter[["alScatter"]],
      trigger = "click"
    )
  }
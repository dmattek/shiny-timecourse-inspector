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
  inNeighTpts = paste0(
    "Window length in time points for smoothing with the average used before plotting the scatterplot. ",
    "Useful to avoid artefacts in the scatterplot due to spurious variations at specific time points."
  ),
  inPlotHeight = 'Height in pixels of the displayed plot',
  inPlotNcolFacet = 'Number of facets in a row. Each facet displayes a scatter plot for a single group.'
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
            ns('inNeighTpts'),
            'Smoothing',
            value = 0,
            step = 1,
            min = 0,
            width = "120px"
          ),
          bsTooltip(
            ns('inNeighTpts'),
            helpText.tabScatter[["inNeighTpts"]],
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
          selected = 0,
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
          selected = 1,
          multiple = FALSE
        )
      }
    })
    
    
    data4scatterPlot <- reactive({
      cat(file = stderr(), 'data4scatterPlot\n')
      
      loc.dt.in = in.data()
      if (is.null(loc.dt.in))
        return(NULL)
      
      # obtain selected time points from UI
      loc.tpts.x = as.integer(input$inSelTptX)
      loc.tpts.y = as.integer(input$inSelTptY)
      
      if (loc.tpts.x == loc.tpts.y) {
        createAlert(
          session,
          "alertAnchor2differentTpts",
          "alert2differentTpts",
          title = "Error",
          content = "Select two different time points.",
          append = FALSE,
          style = "danger"
        )
        return(NULL)
        
      } else {
        closeAlert(session, "alert2differentTpts")
      }
      
      # if neigbbouring points selected, obtain time points for which the aggregation will be calculated
      if (input$inNeighTpts > 0) {
        # get all time points in the dataset
        loc.dt.in.tpts = unique(loc.dt.in[[COLRT]])
        
        # get indices of time points around selected time points
        loc.tpts.x.id = seq(
          which(loc.dt.in.tpts == loc.tpts.x) - input$inNeighTpts,
          which(loc.dt.in.tpts == loc.tpts.x) + input$inNeighTpts,
          1
        )
        loc.tpts.y.id = seq(
          which(loc.dt.in.tpts == loc.tpts.y) - input$inNeighTpts,
          which(loc.dt.in.tpts == loc.tpts.y) + input$inNeighTpts,
          1
        )
        
        # get only indices of time points that are greater than 0
        loc.tpts.x.id = loc.tpts.x.id[loc.tpts.x.id > 0]
        loc.tpts.y.id = loc.tpts.y.id[loc.tpts.y.id > 0]
        
        # update time points used for aggregation
        loc.tpts.x = loc.dt.in.tpts[loc.tpts.x.id]
        loc.tpts.y = loc.dt.in.tpts[loc.tpts.y.id]
        
        # aggregate separately each time point sets
        loc.dt.x = loc.dt.in[get(COLRT) %in% loc.tpts.x, .(y.aggr = mean(get(COLY))), by = c(COLGR, COLID)]
        loc.dt.y = loc.dt.in[get(COLRT) %in% loc.tpts.y, .(y.aggr = mean(get(COLY))), by = c(COLGR, COLID)]
        
        loc.dt = merge(loc.dt.x, loc.dt.y, by = COLID)
        loc.dt[, group.y := NULL]
        
        setnames(loc.dt,
                 c('group.x', 'y.aggr.x', 'y.aggr.y'),
                 c(COLGR, 'x', 'y'))
        
        #cat(loc.tpts.x.id, '\n')
        #cat(loc.tpts.y.id, '\n')
      } else {
        # get data from selected time points
        loc.dt = loc.dt.in[get(COLRT) %in% c(loc.tpts.x, loc.tpts.y)]
        
        # convert to wide, such that two selected time points are in two columns
        loc.dt = dcast(loc.dt[, c(COLGR, COLID, COLY, COLRT), with = F],
                       as.formula(paste0(COLGR, "+", COLID, "~", COLRT)),
                       value.var = COLY)
        
        setnames(loc.dt, c(COLGR, COLID, "x", "y"))
      }
      
      
      
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
      loc.dt = isolate(in.data())
      validate(
        need(!is.null(loc.dt), "Nothing to plot. Load data first!")
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
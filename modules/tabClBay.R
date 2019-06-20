#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for Bayesian clustering using bclust package
# NOT USED AT THE MOMENT!!!
#
# Use:
# in ui.R
# tabPanel(
#  'Hierarchical',
#  clustBayUI('TabClustBay'))
#
# in server.R
# callModule(clustBay, 'TabClustBay', dataMod)
# where dataMod is the output from a reactive function that returns dataset ready for clustering


require(gplots) # heatmap.2
require(dendextend) # color_branches
require(RColorBrewer) # brewer.pal
require(d3heatmap) # interactive heatmap
require(bclust) # Bayesian clustering


l.col.pal = list(
  "Spectral" = 'Spectral',
  "White-Orange-Red" = 'OrRd',
  "Yellow-Orange-Red" = 'YlOrRd',
  "Reds" = "Reds",
  "Oranges" = "Oranges",
  "Greens" = "Greens",
  "Blues" = "Blues"
)

# UI ----
clustBayUI <- function(id, label = "Sparse Hierarchical CLustering") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Bayesian clustering using ",
      a("bclust", href = "https://cran.r-project.org/web/packages/bclust/index.html")
    ),
    p('The algorithm does not deal with missing values. Use conversion to zeroes in the right panel.'),
    p(
      'Column labels in the heat-map are additionally labeled according to their Bayes weight (\"importance\"):'
    ),
    tags$ol(
      tags$li("Blue with \"-\" - variable not likely to participate in optimal clustering (negative weight)"),
      tags$li("Black - low importance (weight factor in 1st quartile)"),
      tags$li("Green with \"*\" - medium importance (weight factor in 2nd quartile)"),
      tags$li("Orange with \"**\" - high importance (weight factor in 3rd quartile)"),
      tags$li("Red with \"***\" - very high importance (weight factor in 4th quartile)")
    ),
    
    br(),
    fluidRow(
      column(6,
             selectInput(
               ns("selectPlotBayHmPalette"),
               label = "Select colour palette:",
               choices = l.col.pal,
               selected = 'Spctral'
             ),
             checkboxInput(ns('inPlotBayHmRevPalette'), 'Reverse colour palette', TRUE),
             checkboxInput(ns('selectPlotBayDend'),
                           'Plot dendrogram and re-order samples', TRUE),
             checkboxInput(ns('selectPlotBayKey'), 'Plot colour key', TRUE)
             
      ),
      column(6,
             uiOutput(ns('inPlotBayHmNclustSlider')),
             sliderInput(
               ns('inPlotBayHmGridColor'),
               'Shade of grey for grid lines (0 - black, 1 - white)',
               min = 0,
               max = 1,
               value = 0.6,
               step = .1,
               ticks = TRUE
             )
      )
    ),
    
    fluidRow(
      column(
        2,
        numericInput(
          ns('inPlotBayHmMarginX'),
          'Margin below x-axis',
          10,
          min = 1,
          width = 100
        )
      ),
      column(
        2,
        numericInput(
          ns('inPlotBayHmMarginY'),
          'Margin right of y-axis',
          10,
          min = 1,
          width = 100
        )
      ),
      column(
        2,
        numericInput(
          ns('inPlotBayHmFontX'),
          'Font size row labels',
          1,
          min = 0,
          width = 100,
          step = 0.1
        )
      ),
      column(
        2,
        numericInput(
          ns('inPlotBayHmFontY'),
          'Font size column labels',
          1,
          min = 0,
          width = 100,
          step = 0.1
        )
      ),
      column(2,
             numericInput(
               ns('inPlotHeight'),
               'Display plot height',
               value = 1000,
               min = 100,
               step = 100
             )
      )
    ),
    br(),
    
    downPlotUI(ns('downPlotBayHM')),
    
    
    br(),
    checkboxInput(ns('inPlotBayInteractive'), 'Interactive Plot?',  value = FALSE),
    uiOutput(ns("plotBayInt_ui"))
    
  )
}

# SERVER ----
clustBay <- function(input, output, session, dataMod) {
  userFitBclus <- reactive({
    cat(file = stderr(), 'userFitBclus \n')
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    

    bclust(loc.dm, transformed.par = c(0, -50, log(16), 0, 0, 0))
  })
  
  userDendBclus <- reactive({
    cat(file = stderr(), 'userDendBclus \n')
    
    d.bclus = userFitBclus()
    if (is.null(d.bclus))
      return(NULL)
    
    dend <- as.dendrogram(d.bclus)
    #    dend <- color_branches(dend, k = d.bclus$optim.clustno)
    dend <- color_branches(dend, k = input$inPlotBayHmNclust)
    #    browser()
  })
  
  userVarImpBclus <- reactive({
    cat(file = stderr(), 'userVarImpBclus \n')
    
    d.bclus = userFitBclus()
    if (is.null(d.bclus))
      return(NULL)
    
    return(imp(d.bclus)$var)
  })
  
  
  output$inPlotBayHmNclustSlider = renderUI({
    ns <- session$ns
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.d.bclus = userFitBclus()
    if (is.null(loc.d.bclus))
      return(NULL)
    
    sliderInput(
      ns('inPlotBayHmNclust'),
      '#clusters to colour (default: optimal # from bclust)',
      min = 1,
      max = nrow(loc.dm),
      value = loc.d.bclus$optim.clustno,
      step = 1,
      ticks = TRUE,
      round = TRUE
    )
  })
  
  
  plotBayHm <- function() {
    cat(file = stderr(), 'plotBayHm \n')
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.dend = userDendBclus()
    if (is.null(loc.dend))
      return(NULL)
    
    loc.var.imp = imp(userFitBclus())$var
    if (is.null(loc.var.imp))
      return(NULL)
    
    col_labels <- get_leaves_branches_col(loc.dend)
    col_labels <- col_labels[order(order.dendrogram(loc.dend))]
    
    if (input$inPlotBayHmRevPalette)
      my_palette <-
      rev(colorRampPalette(brewer.pal(9, input$selectPlotBayHmPalette))(n = 99))
    else
      my_palette <-
      colorRampPalette(brewer.pal(9, input$selectPlotBayHmPalette))(n = 99)
    
    if (input$selectPlotBayDend) {
      assign("var.tmp.1", loc.dend)
      var.tmp.2 = "row"
    } else {
      assign("var.tmp.1", FALSE)
      var.tmp.2 = "none"
    }
    
    loc.colnames = paste0(ifelse(loc.var.imp < 0, "- ",
                                 ifelse(
                                   loc.var.imp < quantile(loc.var.imp, 0.25), "",
                                   ifelse(loc.var.imp < quantile(loc.var.imp, 0.5), "* ", 
                                          ifelse(loc.var.imp < quantile(loc.var.imp, 0.75), "** ", "*** "))
                                 )), colnames(loc.dm))
    
    loc.colcol   = ifelse(loc.var.imp < 0, "blue",
                          ifelse(
                            loc.var.imp < quantile(loc.var.imp, 0.25), "black",
                            ifelse(loc.var.imp < quantile(loc.var.imp, 0.5), "green", 
                                   ifelse(loc.var.imp < quantile(loc.var.imp, 0.75), "orange", "red"))
                          ))
    
    
    heatmap.2(
      loc.dm,
      Colv = "NA",
      Rowv = var.tmp.1,
      srtCol = 90,
      dendrogram = var.tmp.2,
      trace = "none",
      key = input$selectPlotBayKey,
      margins = c(input$inPlotBayHmMarginX, input$inPlotBayHmMarginY),
      col = my_palette,
      na.col = grey(input$inPlotBayHmNAcolor),
      denscol = "black",
      density.info = "density",
      RowSideColors = col_labels,
      colRow = col_labels,
      colCol = loc.colcol,
      labCol = loc.colnames,
      sepcolor = grey(input$inPlotBayHmGridColor),
      colsep = 1:ncol(loc.dm),
      rowsep = 1:nrow(loc.dm),
      cexRow = input$inPlotBayHmFontX,
      cexCol = input$inPlotBayHmFontY,
      main = "Bayesian Clustering (bclust)"
    )
  }
  
  plotBayImp <- function() {
    cat(file = stderr(), 'plotBayImp \n')
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.d.bclus = userFitBclus()
    if (is.null(loc.d.bclus))
      return(NULL)
    
    #cat(imp(loc.d.bclus)$var)
    
    viplot(
      imp(loc.d.bclus)$var,
      xlab = colnames(loc.dm),
      xlab.srt = 90,
      xlab.mar = input$inPlotBayHmMarginX,
      xlab.cex = input$inPlotBayHmFontY,
      main = '\nVariable importance\n'
    )
  }
  
  
  
  output$outPlotBayHm <- renderPlot({
    plotBayHm()
  })
  
  output$plotBayInt <- renderD3heatmap({
    cat(file = stderr(), 'plotBayInt \n')
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.dend = userDendBclus()
    if (is.null(loc.dend))
      return(NULL)
    
    loc.var.imp = imp(userFitBclus())$var
    if (is.null(loc.var.imp))
      return(NULL)
    
    col_labels <- get_leaves_branches_col(loc.dend)
    col_labels <- col_labels[order(order.dendrogram(loc.dend))]
    
    if (input$inPlotBayHmRevPalette)
      my_palette <-
      rev(colorRampPalette(brewer.pal(9, input$selectPlotBayHmPalette))(n = 99))
    else
      my_palette <-
      colorRampPalette(brewer.pal(9, input$selectPlotBayHmPalette))(n = 99)
    
    if (input$selectPlotBayDend) {
      assign("var.tmp.1", loc.dend)
      var.tmp.2 = "row"
    } else {
      assign("var.tmp.1", FALSE)
      var.tmp.2 = "none"
    }
    
    loc.colnames = paste0(ifelse(loc.var.imp < 0, "- ",
                                 ifelse(
                                   loc.var.imp < quantile(loc.var.imp, 0.25), "",
                                   ifelse(loc.var.imp < quantile(loc.var.imp, 0.5), "* ", 
                                          ifelse(loc.var.imp < quantile(loc.var.imp, 0.75), "** ", "*** "))
                                 )), colnames(loc.dm))
    
    d3heatmap(
      loc.dm,
      Rowv = var.tmp.1,
      dendrogram = var.tmp.2,
      trace = "none",
      revC = FALSE,
      margins = c(input$inPlotBayHmMarginX, input$inPlotBayHmMarginY),
      colors = my_palette,
      na.col = grey(input$inPlotBayNAcolor),
      cexRow = input$inPlotBayHmFontY,
      cexCol = input$inPlotBayHmFontX,
      xaxis_height = input$inPlotBayHmMarginX,
      yaxis_width = input$inPlotBayHmMarginY,
      show_grid = TRUE,
      labRow = rownames(loc.dm),
      labCol = loc.colnames
    )
  })
  
  output$plotBayInt_ui <- renderUI({
    ns <- session$ns
    
    if (input$inPlotBayInteractive)
      d3heatmapOutput(ns("plotBayInt"), height = paste0(input$inPlotHeight, "px"))
    else {
      plotOutput(ns('outPlotBayHm'), height = paste0(input$inPlotHeight, "px"))
    }
  })
  
  callModule(downPlot, "downPlotBayHM", 'clust_bayesian_dend.pdf', plotBayHm)
  
}

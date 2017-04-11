

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(data.table)
library(ggplot2)
library(plotly)

# increase file upload limit
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
source('auxfunc.R')

shinyServer(function(input, output, session) {
  useShinyjs()
  
  # This is only set at session start
  # we use this as a way to determine which input was
  # clicked in the dataInBoth reactive
  counter <- reactiveValues(
    # The value of inDataGen1,2 actionButton is the number of times they were pressed
    dataGen1     = isolate(input$inDataGen1),
    dataLoadNuc  = isolate(input$inButLoadNuc)
    #dataLoadStim = isolate(input$inButLoadStim)
  )
  
  ####
  ## UI for side panel
  
  # This button will reset the inFileLoad
  observeEvent(input$inButReset, {
    reset("inFileLoadNuc")  # reset is a shinyjs function
    #reset("inButLoadStim")  # reset is a shinyjs function
  })
  
  # generate random dataset 1
  dataGen1 <- eventReactive(input$inDataGen1, {
    cat("dataGen1\n")
    
    return(userDataGen())
  })
  
  # load main data file
  dataLoadNuc <- eventReactive(input$inButLoadNuc, {
    cat("dataLoadNuc\n")
    locFilePath = input$inFileLoadNuc$datapath
    
    counter$dataLoadNuc <- input$inButLoadNuc - 1
    
    if (is.null(locFilePath) || locFilePath == '')
      return(NULL)
    else {
      return(fread(locFilePath))
    }
  })
  
  # This button will reset the inFileLoad
  observeEvent(input$butReset, {
    reset("inFileLoadNuc")  # reset is a shinyjs function
    #    reset("inFileStimLoad")  # reset is a shinyjs function
    
  })
  

  output$varSelTrackLabel = renderUI({
    cat(file = stderr(), 'UI varSelTrackLabel\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'rack'][1] # index 1 at the end in case more matches; select 1st
    
    cat(locColSel, '\n')
    selectInput(
      'inSelTrackLabel',
      'Select Track Label (e.g. objNuc_Track_ObjectsLabel):',
      locCols,
      width = '100%',
      selected = locColSel
    )
  })
  
  output$varSelTime = renderUI({
    cat(file = stderr(), 'UI varSelTime\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'RealTime'][1] # index 1 at the end in case more matches; select 1st
    
    cat(locColSel, '\n')
    selectInput(
      'inSelTime',
      'Select time column (e.g. RealTime):',
      locCols,
      width = '100%',
      selected = locColSel
    )
  })
  
  # This is main field to select plot facet grouping
  # It's typically a column with the entire experimental description,
  # e.g. in Yannick's case it's Stim_All_Ch or Stim_All_S.
  # In Coralie's case it's a combination of 3 columns called Stimulation_...
  output$varSelGroup = renderUI({
    cat(file = stderr(), 'UI varSelGroup\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'ite']
      if (length(locColSel) == 0)
        locColSel = locCols[locCols %like% 'eries'][1] # index 1 at the end in case more matches; select 1st
      else if (length(locColSel) > 1) {
        locColSel = locColSel[1]
      }
      #    cat('UI varSelGroup::locColSel ', locColSel, '\n')
      selectInput(
        'inSelGroup',
        'Select one or more facet groupings (e.g. Site, Well, Channel):',
        locCols,
        width = '100%',
        selected = locColSel,
        multiple = TRUE
      )
    }
    
  })
  
  output$varSelSite = renderUI({
    cat(file = stderr(), 'UI varSelSite\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'ite'][1] # index 1 at the end in case more matches; select 1st
    
    cat(locColSel, '\n')
    selectInput(
      'inSelSite',
      'Select FOV (e.g. Metadata_Site or Metadata_Series):',
      locCols,
      width = '100%',
      selected = locColSel
    )
  })
  
  
  
  
  output$varSelMeas1 = renderUI({
    cat(file = stderr(), 'UI varSelMeas1\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'objCyto_Intensity_MeanIntensity_imErkCor.*' | locCols %like% 'Ratio'][1] # index 1 at the end in case more matches; select 1st
      #    cat(locColSel, '\n')
      selectInput(
        'inSelMeas1',
        'Select 1st measurement:',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  
  output$varSelMeas2 = renderUI({
    cat(file = stderr(), 'UI varSelMeas2\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols) &&
        !(input$inSelMath %in% c('', '1 / '))) {
      locColSel = locCols[locCols %like% 'objNuc_Intensity_MeanIntensity_imErkCor.*'][1] # index 1 at the end in case more matches; select 1st
      #    cat(locColSel, '\n')
      selectInput(
        'inSelMeas2',
        'Select 2nd measurement',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  
  ####
  ## data processing
  
  # generate random dataset 1
  dataGen1 <- eventReactive(input$inDataGen1, {
    cat("dataGen1\n")
    
    return(userDataGen())
  })
  
  dataInBoth <- reactive({
    # Without direct references to inDataGen1,2 and inFileLoad, inDataGen2
    #    does not trigger running this reactive once inDataGen1 is used.
    # This is one of the more nuanced areas of reactive programming in shiny
    #    due to the if else logic, it isn't fetched once inDataGen1 is available
    # The morale is use direct retrieval of inputs to guarantee they are available
    #    for if else logic checks!
    
    locInGen1 = input$inDataGen1
    locInLoadNuc = input$inButLoadNuc
    #locInLoadStim = input$inButLoadStim
    
    cat(
      "dataInBoth\ninGen1: ",
      locInGen1,
      "   prev=",
      isolate(counter$dataGen1),
      "\ninDataNuc: ",
      locInLoadNuc,
      "   prev=",
      isolate(counter$dataLoadNuc),
      # "\ninDataStim: ",
      # locInLoadStim,
      # "   prev=",
      # isolate(counter$dataLoadStim),
      "\n"
    )
    
    # isolate the checks of counter reactiveValues
    # as we set the values in this same reactive
    if (locInGen1 != isolate(counter$dataGen1)) {
      cat("dataInBoth if inDataGen1\n")
      dm = dataGen1()
      # no need to isolate updating the counter reactive values!
      counter$dataGen1 <- locInGen1
    } else if (locInLoadNuc != isolate(counter$dataLoadNuc)) {
      cat("dataInBoth if inDataLoadNuc\n")
      dm = dataLoadNuc()
      # no need to isolate updating the counter reactive values!
      counter$dataLoadNuc <- locInLoadNuc
    } else {
      cat("dataInBoth else\n")
      dm = NULL
    }
    return(dm)
  })
  
  # return column names of the main dt
  getDataNucCols <- reactive({
    cat(file = stderr(), 'getDataNucCols: in\n')
    loc.dt = dataInBoth()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(colnames(loc.dt))
  })
  
  # return dt with an added column with unique track object label
  dataMod <- reactive({
    cat(file=stderr(), 'dataMod\n')
    loc.dt = dataInBoth()
    
    if(is.null(loc.dt))
      return(NULL)
    
    loc.dt[, trackObjectsLabelUni := paste(sprintf("%03d", get(input$inSelSite)),
                                           sprintf("%04d", get(input$inSelTrackLabel)),
                                           sep = "_")]
    
    return(loc.dt)
  })
  
  # return all unique track object labels (created in dataMod)
  # This will be used to display in UI for trajectory highlighting
  getDataTrackObjLabUni <- reactive({
    cat(file = stderr(), 'getDataTrackObjLabUni\n')
    loc.dt = dataMod()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt$trackObjectsLabelUni))
  })
  
  # return all unique time points (real time)
  # This will be used to display in UI for trajectory highlighting
  getDataTpts <- reactive({
    cat(file = stderr(), 'getDataTpts\n')
    loc.dt = dataMod()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[[input$inSelTime]]))
  })
  
  
  # prepare data for plotting time courses
  # returns dt with these columns:
  #    realtime - selected from input
  #    y        - measurement selected from input 
  #               (can be a single column or result of an operation on two cols)
  #    id       - trackObjectsLabelUni (created in dataMod)
  #    group    - grouping variable for facetting from input
  #    mid.in   - column with trajectory selection status from the input file or
  #               highlight status from UI
  data4trajPlot <- reactive({
    cat(file=stderr(), 'data4trajPlot\n')
    
    loc.dt = dataMod()
    if(is.null(loc.dt))
      return(NULL)
    
    
    if(input$inSelMath == '')
      loc.s.y = input$inSelMeas1
    else if (input$inSelMath == '1 / ')
      loc.s.y = paste0(input$inSelMath, input$inSelMeas1)
    else
      loc.s.y = paste0(input$inSelMeas1, input$inSelMath, input$inSelMeas2)
    
    # create expression for parsing
    # creates a merged column based on other columns from input
    # used for grouping of plot facets
    loc.s.gr = sprintf("paste(%s, sep=';')", paste(input$inSelGroup, sep = '', collapse = ','))
    
    loc.s.rt = input$inSelTime
    
    # Assign tracks selected for highlighting in UI
    loc.tracks.highlight = input$inSelHighlight
    locBut = input$chBhighlightTraj
    
    # if dataset contains column mid.in with trajectory filtering status,
    # then, include it in plotting
    if (sum(names(loc.dt) %in% 'mid.in') > 0) {
      loc.out = loc.dt[, .(
        y = eval(parse(text = loc.s.y)),
        id = trackObjectsLabelUni,
        group = eval(parse(text = loc.s.gr)),
        realtime = eval(parse(text = loc.s.rt)),
        mid.in = mid.in
      )]
      
      # add 3rd level with status of track selection
      # to a column with trajectory filtering status
      if (locBut) {
        loc.out[, mid.in := ifelse(id %in% loc.tracks.highlight, 'SELECTED', mid.in)]
      }
      
    } else {
      loc.out = loc.dt[, .(
        y = eval(parse(text = loc.s.y)),
        id = trackObjectsLabelUni,
        group = eval(parse(text = loc.s.gr)),
        realtime = eval(parse(text = loc.s.rt))
      )]
      
      # add a column with status of track selection
      if (locBut) {
        loc.out[, mid.in := ifelse(id %in% loc.tracks.highlight, 'SELECTED', 'NOT SEL')]
      }
    }
    
    # remove rows with NA
    return(loc.out[complete.cases(loc.out)])
  })
  
  # prepare data for plotting boxplots
  # uses the same dt as for trajectory plotting
  # returns dt with these columns:
  data4boxPlot <- reactive({
    cat(file=stderr(), 'data4trajPlot\n')
    
    loc.dt = data4trajPlot()
    if(is.null(loc.dt))
      return(NULL)
    
    loc.out = loc.dt[realtime %in% input$inSelTpts]
  })
  
  ####
  ## UI for trajectory plot
  
  output$varSelHighlight = renderUI({
    cat(file = stderr(), 'UI varSelHighlight\n')
    
    locBut = input$chBhighlightTraj
    if (!locBut)
      return(NULL)
    
    loc.v = getDataTrackObjLabUni()
    if(!is.null(loc.v)) {
      selectInput(
        'inSelHighlight',
        'Select one or more rajectories:',
        loc.v,
        width = '100%',
        multiple = TRUE
      )
    }
  })
  
  
  output$uiPlotTraj = renderUI({
    
    plotlyOutput("plotTraj", width = paste0(input$inPlotTrajWidth, '%'), height = paste0(input$inPlotTrajHeight, 'px'))
  })
  
  output$plotTraj <- renderPlotly({
    cat(file=stderr(), 'plotTraj: in\n')
    locBut = input$butPlotTraj
    
    if (locBut == 0) {
      cat(file=stderr(), 'plotTraj: Go button not pressed\n')
      
      return(NULL)
    }
    
    loc.dt = isolate(data4trajPlot())
    
    cat("plotScatter on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file=stderr(), 'plotTraj: dt is NULL\n')
      return(NULL)
    }
    
    cat(file=stderr(), 'plotTraj:dt not NULL\n')
    
    # colour trajectories, if dataset contains mi.din column
    # with filtering status of trajectory
    if(sum(names(loc.dt) %in% 'mid.in') > 0)
      loc.line.col.arg = 'mid.in'
    else
      loc.line.col.arg = NULL
    
    p.out = myGgplotTraj(
      dt.arg = loc.dt,
      x.arg = 'realtime',
      y.arg = 'y',
      group.arg = "id",
      facet.arg = 'group',
      facet.ncol.arg = input$inPlotTrajFacetNcol,
      xlab.arg = 'Time (min)',
      line.col.arg = loc.line.col.arg
    )
    
    
    # This is required to avoid 
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    if (names(dev.cur()) != "null device") dev.off()
    pdf(NULL)
    
    p.out.ly = plotly_build(p.out)
    return(p.out.ly)
  })
  

  ####
  ## UI for box-plot
  
  output$varSelTpts = renderUI({
    cat(file = stderr(), 'UI varSelTpts\n')
    
    loc.v = getDataTpts()
    if(!is.null(loc.v)) {
      selectInput(
        'inSelTpts',
        'Select one or more timepoints:',
        loc.v,
        width = '100%', 
        selected = 0,
        multiple = TRUE
      )
    }
  })
  
  # Boxplot - display
  output$outPlotBox = renderPlot({
    
    locBut = input$butPlotBox
    
    if (locBut == 0) {
      cat(file=stderr(), 'plotBox: Go button not pressed\n')
      return(NULL)
    }
    
    plotBox()

  }, height = 800)
  
  # Boxplot - download pdf
  output$downPlotBox <- downloadHandler(
    filename = 'boxplot.pdf',
    
    content = function(file) {
      cat(file = stderr(), input$inPlotBoxWidth, input$inPlotBoxHeight, "\n")
      ggsave(file, limitsize = FALSE,
             plotBox(), 
             width  = input$inPlotBoxWidth,
             height = input$inPlotBoxHeight)
    }
  )
  
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf

  plotBox <- function(){
    cat(file = stderr(), 'plotBox\n')
    
    loc.dt = data4boxPlot()
    
    cat(file=stderr(), "plotBox: on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file=stderr(), 'plotBox: dt is NULL\n')
      return(NULL)
    }
    
    cat(file=stderr(), 'plotBox:dt not NULL\n')
    
    ggplot(loc.dt, aes(x = as.factor(realtime), y = y)) +
      geom_boxplot(aes(fill = group), 
                   #position = position_dodge(width = 1), 
                   notch = input$inPlotBoxNotches, 
                   outlier.colour = ifelse(input$inPlotBoxOutliers, 'red', NA)) + 
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
  }
})

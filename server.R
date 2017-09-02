


# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(data.table)
library(ggplot2)
library(gplots) # for heatmap.2
library(plotly)
library(d3heatmap) # for interactive heatmap
library(dendextend) # for color_branches
library(colorspace) # for palettes (ised to colour dendrogram)
library(RColorBrewer)
library(sparcl) # sparse hierarchical and k-means
library(scales) # for percentages on y scale

# increase file upload limit
options(shiny.maxRequestSize = 80 * 1024 ^ 2)

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
  
  # FILE LOAD
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
  
  
  # COLUMN SELECTION
  output$varSelTrackLabel = renderUI({
    cat(file = stderr(), 'UI varSelTrackLabel\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'rack'][1] # index 1 at the end in case more matches; select 1st
    
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
    
    if (!input$chBtrackUni) {
      locCols = getDataNucCols()
      locColSel = locCols[locCols %like% 'ite'][1] # index 1 at the end in case more matches; select 1st
      
      selectInput(
        'inSelSite',
        'Select FOV (e.g. Metadata_Site or Metadata_Series):',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  
  
  
  output$varSelMeas1 = renderUI({
    cat(file = stderr(), 'UI varSelMeas1\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'objCyto_Intensity_MeanIntensity_imErkCor.*' |
                            locCols %like% 'Ratio'][1] # index 1 at the end in case more matches; select 1st

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

      selectInput(
        'inSelMeas2',
        'Select 2nd measurement',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  # UI for trimming x-axis (time)
  output$uiSlTimeTrim = renderUI({
    cat(file = stderr(), 'UI uiSlTimeTrim\n')
    
    if (input$chBtimeTrim) {
      locTpts  = getDataTpts()
      
      if(is.null(locTpts))
        return(NULL)
      
      locRTmin = min(locTpts)
      locRTmax = max(locTpts)
      
      sliderInput(
        'slTimeTrim',
        label = 'Time range to include',
        min = locRTmin,
        max = locRTmax,
        value = c(locRTmin, locRTmax),
        step = 1
      )
      
    }
  })
  
  # UI for normalization
  
  output$uiChBnorm = renderUI({
    cat(file = stderr(), 'UI uiChBnorm\n')
    
    if (input$chBnorm) {
      radioButtons(
        'rBnormMeth',
        label = 'Select method',
        choices = list('fold-change' = 'mean', 'z-score' = 'z.score')
      )
    }
  })
  
  output$uiSlNorm = renderUI({
    cat(file = stderr(), 'UI uiSlNorm\n')
    
    if (input$chBnorm) {
      locTpts  = getDataTpts()
      
      if(is.null(locTpts))
        return(NULL)
      
      locRTmin = min(locTpts)
      locRTmax = max(locTpts)
      
      sliderInput(
        'slNormRtMinMax',
        label = 'Time range for norm.',
        min = locRTmin,
        max = locRTmax,
        value = c(locRTmin, 0.1 * locRTmax), 
        step = 1
      )
    }
  })
  
  output$uiChBnormRobust = renderUI({
    cat(file = stderr(), 'UI uiChBnormRobust\n')
    
    if (input$chBnorm) {
      checkboxInput('chBnormRobust',
                    label = 'Robust stats',
                    FALSE)
    }
  })
  
  output$uiChBnormGroup = renderUI({
    cat(file = stderr(), 'UI uiChBnormGroup\n')
    
    if (input$chBnorm) {
      radioButtons('chBnormGroup',
                   label = 'Normalisation grouping',
                   choices = list('Entire dataset' = 'none', 'Per facet' = 'group', 'Per trajectory (Korean way)' = 'id'))
    }
  })
  
  
  # UI for removing outliers
  
  output$uiSlOutliers = renderUI({
    cat(file = stderr(), 'UI uiSlOutliers\n')
    
    if (input$chBoutliers) {
      
      sliderInput(
        'slOutliersPerc',
        label = 'Percentage of middle data',
        min = 90,
        max = 100,
        value = 99.5, 
        step = 0.1
      )
      
      
    }
  })
  
  output$uiTxtOutliers = renderUI({
    if (input$chBoutliers) {
      
      p("Total tracks")
      
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
    cat(file = stderr(), 'dataMod\n')
    loc.dt = dataInBoth()
    
    if (is.null(loc.dt))
      return(NULL)
    
    if (!input$chBtrackUni) {
      loc.types = lapply(loc.dt, class)
      if(loc.types[[input$inSelTrackLabel]] %in% c('numeric', 'integer') & loc.types[[input$inSelSite]] %in% c('numeric', 'integer'))
      {
        loc.dt[, trackObjectsLabelUni := paste(sprintf("%03d", get(input$inSelSite)),
                                               sprintf("%04d", get(input$inSelTrackLabel)),
                                               sep = "_")]
      } else if(loc.types[[input$inSelTrackLabel]] %in% c('numeric', 'integer')) {
        loc.dt[, trackObjectsLabelUni := paste(sprintf("%s", get(input$inSelSite)),
                                               sprintf("%04d", get(input$inSelTrackLabel)),
                                               sep = "_")]
      } else if(loc.types[[input$inSelSite]] %in% c('numeric', 'integer')) {
        loc.dt[, trackObjectsLabelUni := paste(sprintf("%03d", get(input$inSelSite)),
                                               sprintf("%s", get(input$inSelTrackLabel)),
                                               sep = "_")]
      } else {
        loc.dt[, trackObjectsLabelUni := paste(sprintf("%s", get(input$inSelSite)),
                                               sprintf("%s", get(input$inSelTrackLabel)),
                                               sep = "_")]
      }
    } else {
      loc.dt[, trackObjectsLabelUni := get(input$inSelTrackLabel)]
    }
    
    
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
  
  # return all unique track object labels (created in dataMod)
  # This will be used to display in UI for trajectory highlighting
  getDataTrackObjLabUni_afterTrim <- reactive({
    cat(file = stderr(), 'getDataTrackObjLabUni_afterTrim\n')
    loc.dt = data4trajPlot()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt$id))
  })
  
  # return all unique time points (real time)
  # This will be used to display in UI for box-plot
  # These timepoints are from the original dt and aren't affected by trimming of x-axis
  getDataTpts <- reactive({
    cat(file = stderr(), 'getDataTpts\n')
    loc.dt = dataMod()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[[input$inSelTime]]))
  })
  
  # return dt with cell IDs and their corresponding condition name
  # The condition is the column defined by facet groupings
  getDataCond <- reactive({
    cat(file = stderr(), 'getDataCond\n')
    loc.dt = data4trajPlot()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[, .(id, group)]))
    
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
    cat(file = stderr(), 'data4trajPlot\n')
    
    loc.dt = dataMod()
    if (is.null(loc.dt))
      return(NULL)
    
    
    if (input$inSelMath == '')
      loc.s.y = input$inSelMeas1
    else if (input$inSelMath == '1 / ')
      loc.s.y = paste0(input$inSelMath, input$inSelMeas1)
    else
      loc.s.y = paste0(input$inSelMeas1, input$inSelMath, input$inSelMeas2)
    
    # create expression for parsing
    # creates a merged column based on other columns from input
    # used for grouping of plot facets
    if(length(input$inSelGroup) == 0)
      return(NULL)
    loc.s.gr = sprintf("paste(%s, sep=';')",
                       paste(input$inSelGroup, sep = '', collapse = ','))
    
    loc.s.rt = input$inSelTime
    
    # Assign tracks selected for highlighting in UI
    loc.tracks.highlight = input$inSelHighlight
    locBut = input$chBhighlightTraj
    
    
    # Find column names with position
    loc.s.pos.x = names(loc.dt)[names(loc.dt) %like% c('.*ocation.*X') | names(loc.dt) %like% c('.*os.x')]
    loc.s.pos.y = names(loc.dt)[names(loc.dt) %like% c('.*ocation.*Y') | names(loc.dt) %like% c('.*os.y')]
    
    if (length(loc.s.pos.x) == 1 & length(loc.s.pos.y) == 1)
      locPos = TRUE
    else
      locPos = FALSE
    
    # if dataset contains column mid.in with trajectory filtering status,
    # then, include it in plotting
    if (sum(names(loc.dt) %in% 'mid.in') > 0) {
      if (locPos) # position columns present
        loc.out = loc.dt[, .(
          y = eval(parse(text = loc.s.y)),
          id = trackObjectsLabelUni,
          group = eval(parse(text = loc.s.gr)),
          realtime = eval(parse(text = loc.s.rt)),
          pos.x = get(loc.s.pos.x),
          pos.y = get(loc.s.pos.y),
          mid.in = mid.in
        )] else
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
      if (locPos) # position columns present
        loc.out = loc.dt[, .(
          y = eval(parse(text = loc.s.y)),
          id = trackObjectsLabelUni,
          group = eval(parse(text = loc.s.gr)),
          realtime = eval(parse(text = loc.s.rt)),
          pos.x = get(loc.s.pos.x),
          pos.y = get(loc.s.pos.y)
        )] else
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
    
    # add XY location if present in the dataset
    
    # remove NAs
    loc.out = loc.out[complete.cases(loc.out)]
    
    # Trim x-axis (time)
    if(input$chBtimeTrim) {
      loc.out = loc.out[realtime >= input$slTimeTrim[[1]] & realtime <= input$slTimeTrim[[2]] ]
    }
    
    # Normalization
    # F-n myNorm adds additional column with .norm suffix
    if (input$chBnorm) {
      loc.out = myNorm(
        in.dt = loc.out,
        in.meas.col = 'y',
        in.rt.col = 'realtime',
        in.rt.min = input$slNormRtMinMax[1],
        in.rt.max = input$slNormRtMinMax[2],
        in.type = input$rBnormMeth,
        in.robust = input$chBnormRobust,
        in.by.cols = if(input$chBnormGroup %in% 'none') NULL else input$chBnormGroup
      )
      
      # Column with normalized data is renamed to the original name
      # Further code assumes column name y produced by data4trajPlot
      loc.out[, y := NULL]
      setnames(loc.out, 'y.norm', 'y')
    }
    
    ##### MOD HERE
    ## display number of filtered tracks in textUI: uiTxtOutliers
    ## How? 
    ## 1. through reactive values?
    ## 2. through additional comumn to tag outliers?
    
    # Remove outliers
    # 1. Scale all points (independently per track)
    # 2. Pick time points that exceed the bounds
    # 3. Identify IDs of outliers
    # 4. Select cells that don't have these IDs
    
    cat('Ncells orig = ', length(unique(loc.out$id)), '\n')
    
    if (input$chBoutliers) {
      loc.out[, y.sc := scale(y)]  
      loc.tmp = loc.out[ y.sc < quantile(y.sc, (1 - input$slOutliersPerc * 0.01)*0.5) | 
                           y.sc > quantile(y.sc, 1 - (1 - input$slOutliersPerc * 0.01)*0.5)]
      loc.out = loc.out[!(id %in% unique(loc.tmp$id))]
      loc.out[, y.sc := NULL]
    }
    
    cat('Ncells trim = ', length(unique(loc.out$id)), '\n')
    
    return(loc.out)
  })
  
  
  
  # prepare data for clustering
  # return a matrix with:
  # cells as columns
  # time points as rows
  data4clust <- reactive({
    cat(file = stderr(), 'data4clust\n')
    
    loc.dt = data4trajPlot()
    if (is.null(loc.dt))
      return(NULL)
    
    loc.out = dcast(loc.dt, id ~ realtime, value.var = 'y')
    loc.rownames = loc.out$id
    
    
    loc.out = as.matrix(loc.out[, -1])
    rownames(loc.out) = loc.rownames
    return(loc.out)
  }) 
  
  
  # get cell IDs with cluster assignments depending on dendrogram cut
  getDataCl = function(in.dend, in.k, in.ids) {
    cat(file = stderr(), 'getDataCl \n')
    
    loc.dt.cl = data.table(id = in.ids,
                           cl = cutree(as.dendrogram(in.dend), k = in.k))
  }
  
  ####
  ## UI for trajectory plot
  
  output$varSelHighlight = renderUI({
    cat(file = stderr(), 'UI varSelHighlight\n')
    
    locBut = input$chBhighlightTraj
    if (!locBut)
      return(NULL)
    
    loc.v = getDataTrackObjLabUni()
    if (!is.null(loc.v)) {
      selectInput(
        'inSelHighlight',
        'Select one or more rajectories:',
        loc.v,
        width = '100%',
        multiple = TRUE
      )
    }
  })
  
  callModule(modTrajPlot, 'modTrajPlot', data4trajPlot)
  
  ###### Box-plot
  callModule(tabBoxPlot, 'tabBoxPlot', data4trajPlot)
  
  
  
  ###### Scatter plot
  callModule(tabScatterPlot, 'tabScatter', data4trajPlot)
  
  ##### Hierarchical clustering
  
  output$uiPlotHierClSel = renderUI({
    if(input$chBPlotHierClSel) {
      selectInput('inPlotHierClSel', 'Select clusters to display', 
                  choices = seq(1, input$inPlotHierNclust, 1),
                  multiple = TRUE, 
                  selected = 1)
    }
  })
  
  # perform hierarchical clustering and return dendrogram coloured according to cutree
  # branch coloring performed using dendextend package
  userFitDendHier <- reactive({
    cat(file = stderr(), 'userFitDendHier \n')

    dm.t = data4clust()
    if (is.null(dm.t)) {
      return(NULL)
    }
    
    cl.dist = dist(dm.t, method = s.cl.diss[as.numeric(input$selectPlotHierDiss)])
    cl.hc = hclust(cl.dist, method = s.cl.linkage[as.numeric(input$selectPlotHierLinkage)])
    cl.lev = rev(row.names(dm.t))
    
    dend <- as.dendrogram(cl.hc)
    dend <- color_branches(dend, 
                           col = rainbow_hcl, # make sure that n here equals max in the input$inPlotHierNclust slider
                           k = input$inPlotHierNclust)
    
    return(dend)
  })
  
  # prepares a table with cluster numbers in 1st column and colour assignments in 2nd column
  # the number of rows is determined by dendrogram cut
  getClCol <- function(in.dend, in.k) {
    
    loc.col_labels <- get_leaves_branches_col(in.dend)
    loc.col_labels <- loc.col_labels[order(order.dendrogram(in.dend))]
    
    return(unique(
      data.table(cl.no = dendextend::cutree(in.dend, k = in.k, order_clusters_as_data = TRUE),
                 cl.col = loc.col_labels)))
  }
  
  # returns table prepared with f-n getClCol
  # for hierarchical clustering
  getClColHier <- reactive({
    cat(file = stderr(), 'getClColHier \n')
    
    loc.dend = userFitDendHier()
    if (is.null(loc.dend))
      return(NULL)
    
    return(getClCol(loc.dend, input$inPlotHierNclust))
  })
  

  
    # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  plotHier <- function() {
    
    loc.dm = data4clust()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.dend <- userFitDendHier()
    if (is.null(loc.dend))
      return(NULL)
    
    loc.p = myPlotHeatmap(loc.dm,
                  loc.dend, 
                  palette.arg = input$selectPlotHierPalette, 
                  palette.rev.arg = input$inPlotHierRevPalette, 
                  dend.show.arg = input$selectPlotHierDend, 
                  key.show.arg = input$selectPlotHierKey, 
                  margin.x.arg = input$inPlotHierMarginX, 
                  margin.y.arg = input$inPlotHierMarginY, 
                  nacol.arg = input$inPlotHierNAcolor, 
                  font.row.arg = input$inPlotHierFontX, 
                  font.col.arg = input$inPlotHierFontY, 
                  title.arg = paste(
                    "Distance measure: ",
                    s.cl.diss[as.numeric(input$selectPlotHierDiss)],
                    "\nLinkage method: ",
                    s.cl.linkage[as.numeric(input$selectPlotHierLinkage)]
                  ))
    
    return(loc.p)
  }
  
  
  # prepare data for plotting trajectories per cluster
  # outputs dt as data4trajPlot but with an additional column 'cl' that holds cluster numbers
  # additionally some clusters are omitted according to manual selection
  data4trajPlotCl <- reactive({
    cat(file = stderr(), 'data4trajPlotCl: in\n')
    
    loc.dt = data4trajPlot()
    
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'data4trajPlotCl: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'data4trajPlotCl: dt not NULL\n')
    
    # get cellIDs with cluster assignments based on dendrogram cut
    loc.dt.cl = getDataCl(userFitDendHier(), input$inPlotHierNclust, getDataTrackObjLabUni_afterTrim())
    loc.dt = merge(loc.dt, loc.dt.cl, by = 'id')
    
    # display only selected clusters
    if(input$chBPlotHierClSel)
      loc.dt = loc.dt[cl %in% input$inPlotHierClSel]
    
    return(loc.dt)    
  })
  
  callModule(modTrajPlot, 'modPlotHierTraj', 
             in.data = data4trajPlotCl, 
             in.facet = 'cl',  
             in.facet.color = getClColHier,
             in.fname = paste0('clust_hierch_tCourses_',
                                                                            s.cl.diss[as.numeric(input$selectPlotHierDiss)],
                                                                            '_',
                                                                            s.cl.linkage[as.numeric(input$selectPlotHierLinkage)], '.pdf'))
  
  # download a list of cellIDs with cluster assignments
  output$downCellCl <- downloadHandler(
    filename = function() {
      paste0('clust_hierch_data_',
             s.cl.diss[as.numeric(input$selectPlotHierDiss)],
             '_',
             s.cl.linkage[as.numeric(input$selectPlotHierLinkage)], '.csv')
    },
    
    content = function(file) {
      write.csv(x = getDataCl(userFitDendHier(), input$inPlotHierNclust, getDataTrackObjLabUni_afterTrim()), file = file, row.names = FALSE)
    }
  )
  
  output$downCellClSpar <- downloadHandler(
    filename = function() {
      paste0('clust_hierchSpar_data_',
             s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
             '_',
             s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.csv')
    },
    
    content = function(file) {
      write.csv(x = getDataCl(userFitDendHierSpar(), input$inPlotHierSparNclust, getDataTrackObjLabUni_afterTrim()), file = file, row.names = FALSE)
    }
  )
  
  
  # callModule(downCellCl, 'downDataHier', paste0('clust_hierch_data_',
  #                                               s.cl.diss[as.numeric(input$selectPlotHierDiss)],
  #                                               '_',
  #                                               s.cl.linkage[as.numeric(input$selectPlotHierLinkage)], '.csv'),
  #            getDataCl(userFitDendHier, input$inPlotHierNclust, getDataTrackObjLabUni_afterTrim))
  # 
  
  output$downloadDataClean <- downloadHandler(
    filename = 'tCoursesSelected_clean.csv',
    content = function(file) {
      write.csv(data4trajPlot(), file, row.names = FALSE)
    }
  )
  
  
  # prepare data for barplot with distribution of items per condition  
  data4clDistPlot <- reactive({
    cat(file = stderr(), 'data4clDistPlot: in\n')
    
    # get cell IDs with cluster assignments depending on dendrogram cut
    loc.dend <- userFitDendHier()
    if (is.null(loc.dend)) {
      cat(file = stderr(), 'plotClDist: loc.dend is NULL\n')
      return(NULL)
    }
    
    loc.dt.cl = data.table(id = getDataTrackObjLabUni_afterTrim(),
                           cl = cutree(as.dendrogram(loc.dend), k = input$inPlotHierNclust))
    
    
    # get cellIDs with condition name
    loc.dt.gr = getDataCond()
    if (is.null(loc.dt.gr)) {
      cat(file = stderr(), 'plotClDist: loc.dt.gr is NULL\n')
      return(NULL)
    }
    
    loc.dt = merge(loc.dt.cl, loc.dt.gr, by = 'id')
    
    # display only selected clusters
    if(input$chBPlotHierClSel)
      loc.dt = loc.dt[cl %in% input$inPlotHierClSel]
    
    loc.dt.aggr = loc.dt[, .(nCells = .N), by = .(group, cl)]
    
    return(loc.dt.aggr)
    
  })
  

  #  Hierarchical - display heatmap
  getPlotHierHeatMapHeight <- function() {
    return (input$inPlotHierHeatMapHeight)
  }
  
  output$outPlotHier <- renderPlot({
    locBut = input$butPlotHierHeatMap
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotHier: Go button not pressed\n')
      
      return(NULL)
    }

    plotHier()
  }, height = getPlotHierHeatMapHeight)
  
  
  #  Hierarchical - Heat Map - download pdf
  callModule(downPlot, "downPlotHier",       paste0('clust_hierch_heatMap_',
                                                    s.cl.diss[as.numeric(input$selectPlotHierDiss)],
                                                    '_',
                                                    s.cl.linkage[as.numeric(input$selectPlotHierLinkage)], '.png'), plotHier)

  callModule(modClDistPlot, 'hierClDistPlot', 
             in.data = data4clDistPlot,
             in.cols = getClColHier,
             in.fname = paste0('clust_hierch_clDist_',
                    s.cl.diss[as.numeric(input$selectPlotHierDiss)],
                    '_',
                    s.cl.linkage[as.numeric(input$selectPlotHierLinkage)], '.pdf'))
  
  
  ##### Sparse hierarchical clustering using sparcl
  
  # UI for advanced options
  output$uiPlotHierSparNperms = renderUI({
    if (input$inHierSparAdv)
      sliderInput(
        'inPlotHierSparNperms',
        'Number of permutations',
        min = 1,
        max = 20,
        value = 1,
        step = 1,
        ticks = TRUE
      )
  })
  
  # UI for advanced options
  output$uiPlotHierSparNiter = renderUI({
    if (input$inHierSparAdv)
      sliderInput(
        'inPlotHierSparNiter',
        'Number of iterations',
        min = 1,
        max = 50,
        value = 1,
        step = 1,
        ticks = TRUE
      )
  })
  
  output$uiPlotHierSparClSel = renderUI({
    if(input$chBPlotHierSparClSel) {
      selectInput('inPlotHierSparClSel', 'Select clusters to display', 
                  choices = seq(1, input$inPlotHierSparNclust, 1),
                  multiple = TRUE, 
                  selected = 1)
    }
  })
  
  
  getPlotHierSparHeatMapHeight <- function() {
    return (input$inPlotHierSparHeatMapHeight)
  }
  
  userFitHierSpar <- reactive({
    dm.t = data4clust()
    if (is.null(dm.t)) {
      return()
    }
    
    #cat('rownames: ', rownames(dm.t), '\n')
    
    perm.out <- HierarchicalSparseCluster.permute(
      dm.t,
      wbounds = NULL,
      nperms = ifelse(input$inHierSparAdv, input$inPlotHierSparNperms, 1),
      dissimilarity = s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)]
    )
    
    sparsehc <- HierarchicalSparseCluster(
      dists = perm.out$dists,
      wbound = perm.out$bestw,
      niter = ifelse(input$inHierSparAdv, input$inPlotHierSparNiter, 1),
      method = s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)],
      dissimilarity = s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)]
    )
    return(sparsehc)
  })
  
  
  userFitDendHierSpar <- reactive({
    sparsehc = userFitHierSpar()
    if (is.null(sparsehc)) {
      return()
    }
    
    dend <- as.dendrogram(sparsehc$hc)
    dend <- color_branches(dend, 
                           col = rainbow_hcl,
                           k = input$inPlotHierSparNclust)
    
    return(dend)
  })

  # returns table prepared with f-n getClCol
  # for sparse hierarchical clustering
  getClColHierSpar <- reactive({
    cat(file = stderr(), 'getClColHierSpar \n')
    
    loc.dend = userFitDendHierSpar()
    if (is.null(loc.dend))
      return(NULL)
    
    return(getClCol(loc.dend, input$inPlotHierNclust))
  })
  
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  plotHierSpar <- function() {
    
    loc.dm = data4clust()
    if (is.null(loc.dm)) {
      return()
    }
    
    sparsehc <- userFitHierSpar()
    loc.dend <- userFitDendHierSpar()

    loc.colnames = paste0(ifelse(sparsehc$ws == 0, "",
                                 ifelse(
                                   sparsehc$ws <= 0.1,
                                   "* ",
                                   ifelse(sparsehc$ws <= 0.5, "** ", "*** ")
                                 )),  colnames(loc.dm))
    
    loc.colcol   = ifelse(sparsehc$ws == 0,
                          "black",
                          ifelse(
                            sparsehc$ws <= 0.1,
                            "blue",
                            ifelse(sparsehc$ws <= 0.5, "green", "red")
                          ))
    
    loc.p = myPlotHeatmap(loc.dm,
                  loc.dend, 
                  palette.arg = input$selectPlotHierSparPalette, 
                  palette.rev.arg = input$inPlotHierSparRevPalette, 
                  dend.show.arg = input$selectPlotHierSparDend, 
                  key.show.arg = input$selectPlotHierSparKey, 
                  margin.x.arg = input$inPlotHierSparMarginX, 
                  margin.y.arg = input$inPlotHierSparMarginY, 
                  nacol.arg = input$inPlotHierSparNAcolor, 
                  colCol.arg = loc.colcol,
                  labCol.arg = loc.colnames,
                  font.row.arg = input$inPlotHierSparFontX, 
                  font.col.arg = input$inPlotHierSparFontY, 
                  title.arg = paste(
                    "Distance measure: ",
                    s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
                    "\nLinkage method: ",
                    s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)]
                  ))
    
    return(loc.p)
  }
  
  # prepare data for plotting trajectories per cluster
  # outputs dt as data4trajPlot but with an additional column 'cl' that holds cluster numbers
  # additionally some clusters are omitted according to manual selection
  data4trajPlotClSpar <- reactive({
    cat(file = stderr(), 'data4trajPlotClSpar: in\n')
    
    loc.dt = data4trajPlot()
    
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'data4trajPlotClSpar: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'data4trajPlotClSpar: dt not NULL\n')
    
    # get cellIDs with cluster assignments based on dendrogram cut
    loc.dt.cl = getDataCl(userFitDendHierSpar(), input$inPlotHierSparNclust, getDataTrackObjLabUni_afterTrim())
    loc.dt = merge(loc.dt, loc.dt.cl, by = 'id')
    
    # display only selected clusters
    if(input$chBPlotHierSparClSel)
      loc.dt = loc.dt[cl %in% input$inPlotHierSparClSel]
    
    return(loc.dt)    
  })
  
  callModule(modTrajPlot, 'modPlotHierSparTraj', 
             in.data = data4trajPlotClSpar, 
             in.facet = 'cl', 
             in.facet.color = getClColHierSpar,
             paste0('clust_hierchSparse_tCourses_',
                                                                                   s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
                                                                                   '_',
                                                                                   s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.pdf'))

  
  
  # prepare data for barplot with distribution of items per condition  
  data4clSparDistPlot <- reactive({
    cat(file = stderr(), 'data4clSparDistPlot: in\n')
    
    # get cell IDs with cluster assignments depending on dendrogram cut
    loc.dend <- userFitHierSpar()
    if (is.null(loc.dend)) {
      cat(file = stderr(), 'plotClSparDist: loc.dend is NULL\n')
      return(NULL)
    }
    
    loc.dt.cl = data.table(id = getDataTrackObjLabUni_afterTrim(),
                           cl = cutree(as.dendrogram(loc.dend$hc), k = input$inPlotHierSparNclust))
    
    
    # get cellIDs with condition name
    loc.dt.gr = getDataCond()
    if (is.null(loc.dt.gr)) {
      cat(file = stderr(), 'plotClSparDist: loc.dt.gr is NULL\n')
      return(NULL)
    }
    
    loc.dt = merge(loc.dt.cl, loc.dt.gr, by = 'id')
    
    # display only selected clusters
    if(input$chBPlotHierSparClSel)
      loc.dt = loc.dt[cl %in% input$inPlotHierSparClSel]
    
    loc.dt.aggr = loc.dt[, .(nCells = .N), by = .(group, cl)]
    
    return(loc.dt.aggr)
    
  })
  
  callModule(modClDistPlot, 'hierClSparDistPlot', 
             in.data = data4clSparDistPlot,
             in.cols = getClColHierSpar,
             in.fname = paste0('clust_hierchSparse_clDist_',
                    s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
                    '_',
                    s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.pdf'))
  

  
  # Sparse Hierarchical - display heatmap
  output$outPlotHierSpar <- renderPlot({
    locBut = input$butPlotHierSparHeatMap
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotHierSpar: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotHierSpar()
  }, height = getPlotHierSparHeatMapHeight)
  
  # Sparse Hierarchical - Heat Map - download pdf
  callModule(downPlot, "downPlotHierSparHM",       paste0('clust_hierchSparse_heatMap_',
                                                          s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
                                                          '_',
                                                          s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.png'), plotHierSpar)
  

  # Sparse Hierarchical clustering (sparcl) interactive version
  output$plotHierSparInt <- renderD3heatmap({
    dm.t = data4clust()
    if (is.null(dm.t)) {
      return()
    }
    
    sparsehc <- userFitHierSpar()
    
    dend <- as.dendrogram(sparsehc$hc)
    dend <- color_branches(dend, k = input$inPlotHierSparNclust)
    
    if (input$inPlotHierSparRevPalette)
      my_palette <-
      rev(colorRampPalette(brewer.pal(9, input$selectPlotHierSparPalette))(n = 99))
    else
      my_palette <-
      colorRampPalette(brewer.pal(9, input$selectPlotHierSparPalette))(n = 99)
    
    
    col_labels <- get_leaves_branches_col(dend)
    col_labels <- col_labels[order(order.dendrogram(dend))]
    
    if (input$selectPlotHierSparDend == 1)
      assign("var.tmp", dend)
    else
      assign("var.tmp", FALSE)
    
    
    loc.colnames = paste0(colnames(dm.t), ifelse(sparsehc$ws == 0, "",
                                                 ifelse(
                                                   sparsehc$ws <= 0.1,
                                                   " *",
                                                   ifelse(sparsehc$ws <= 0.5, " **", " ***")
                                                 )))
    
    d3heatmap(
      dm.t,
      Rowv = var.tmp,
      dendrogram = ifelse(input$selectPlotHierSparDend == 1, "row", 'none'),
      trace = "none",
      revC = FALSE,
      na.rm = FALSE,
      margins = c(
        input$inPlotHierSparMarginX * 10,
        input$inPlotHierSparMarginY * 10
      ),
      colors = my_palette,
      na.col = grey(input$inPlotHierSparNAcolor),
      cexRow = input$inPlotHierSparFontY,
      cexCol = input$inPlotHierSparFontX,
      xaxis_height = input$inPlotHierSparMarginX * 10,
      yaxis_width = input$inPlotHierSparMarginY * 10,
      show_grid = TRUE,
      #labRow = rownames(dm.t),
      labCol = loc.colnames
    )
  })
  
  #callModule(clustBay, 'TabClustBay', data4clust)
  
})

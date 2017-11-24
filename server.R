


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
library(dtw) # for dynamic time warping
library(imputeTS) # for interpolating NAs

# increase file upload limit
options(shiny.maxRequestSize = 200 * 1024 ^ 2)

shinyServer(function(input, output, session) {
  useShinyjs()
  
  # This is only set at session start
  # we use this as a way to determine which input was
  # clicked in the dataInBoth reactive
  counter <- reactiveValues(
    # The value of inDataGen1,2 actionButton is the number of times they were pressed
    dataGen1     = isolate(input$inDataGen1),
    dataLoadNuc  = isolate(input$inButLoadNuc),
    dataLoadTrajRem = isolate(input$inButLoadTrajRem)
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
  
  # UI for loading csv with cell IDs for trajectory removal
  output$uiFileLoadTrajRem = renderUI({
    cat(file = stderr(), 'UI uiFileLoadTrajRem\n')
    
    if(input$chBtrajRem) 
      fileInput(
        'inFileLoadTrajRem',
        'Select data file (e.g. badTraj.csv) and press "Load Data"',
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      )
  })
  
  output$uiButLoadTrajRem = renderUI({
    cat(file = stderr(), 'UI uiButLoadTrajRem\n')
    
    if(input$chBtrajRem)
      actionButton("inButLoadTrajRem", "Load Data")
  })

  # load main data file
  dataLoadTrajRem <- eventReactive(input$inButLoadTrajRem, {
    cat(file = stderr(), "dataLoadTrajRem\n")
    locFilePath = input$inFileLoadTrajRem$datapath
    
    counter$dataLoadTrajRem <- input$inButLoadTrajRem - 1
    
    if (is.null(locFilePath) || locFilePath == '')
      return(NULL)
    else {
      return(fread(locFilePath))
    }
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
    
    if (input$chBgroup) {
      
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
    
    
    # remove trajectories based on uploaded csv

    if (input$chBtrajRem) {
      cat(file = stderr(), 'dataMod: trajRem not NULL\n')
      
      loc.dt.rem = dataLoadTrajRem()
      
      
      loc.dt = loc.dt[!(trackObjectsLabelUni %in% loc.dt.rem[[1]])]
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
  
  
  
  # prepare data for plotting time courses
  # returns dt with these columns:
  #    realtime - selected from input
  #    y        - measurement selected from input
  #               (can be a single column or result of an operation on two cols)
  #    id       - trackObjectsLabelUni; created in dataMod based on TrackObjects_Label
  #               and FOV column such as Series or Site (if TrackObjects_Label not unique across entire dataset)
  #    group    - grouping variable for facetting from input
  #    mid.in   - column with trajectory selection status from the input file or
  #               highlight status from UI 
  #               (column created if mid.in present in uploaded data or tracks are selected in the UI)
  #    obj.num  - created if ObjectNumber column present in the input data 
  #    pos.x,y  - created if columns with x and y positions present in the input data
  data4trajPlot <- reactive({
    cat(file = stderr(), 'data4trajPlot\n')
    
    loc.dt = dataMod()
    if (is.null(loc.dt))
      return(NULL)
    
    # create expression for 'y' column based on measurements and math operations selected in UI
    if (input$inSelMath == '')
      loc.s.y = input$inSelMeas1
    else if (input$inSelMath == '1 / ')
      loc.s.y = paste0(input$inSelMath, input$inSelMeas1)
    else
      loc.s.y = paste0(input$inSelMeas1, input$inSelMath, input$inSelMeas2)
    
    # create expression for 'group' column
    # creates a merged column based on other columns from input
    # used for grouping of plot facets
    if (input$chBgroup) {
      if(length(input$inSelGroup) == 0)
        return(NULL)
      
      loc.s.gr = sprintf("paste(%s, sep=';')",
                         paste(input$inSelGroup, sep = '', collapse = ','))
    } else {
      # if no grouping required, fill 'group' column with 0
      # because all the plotting relies on the presence of the group column
      loc.s.gr = "paste('0')"
    }
    

    # column name with time
    loc.s.rt = input$inSelTime
    
    # Assign tracks selected for highlighting in UI
    loc.tracks.highlight = input$inSelHighlight
    locButHighlight = input$chBhighlightTraj
    
    
    # Find column names with position
    loc.s.pos.x = names(loc.dt)[names(loc.dt) %like% c('.*ocation.*X') | names(loc.dt) %like% c('.*os.x')]
    loc.s.pos.y = names(loc.dt)[names(loc.dt) %like% c('.*ocation.*Y') | names(loc.dt) %like% c('.*os.y')]
    
    if (length(loc.s.pos.x) == 1 & length(loc.s.pos.y) == 1)
      locPos = TRUE
    else
      locPos = FALSE
    
    
    # Find column names with ObjectNumber
    # This is different from TrackObject_Label and is handy to keep
    # because labels on segmented images are typically ObjectNumber
    loc.s.objnum = names(loc.dt)[names(loc.dt) %like% c('ObjectNumber')]

    if (length(loc.s.objnum) == 1)
      locObjNum = TRUE
    else
      locObjNum = FALSE
    
    
    # if dataset contains column mid.in with trajectory filtering status,
    # then, include it in plotting
    if (sum(names(loc.dt) %in% 'mid.in') > 0)
      locMidIn = TRUE
    else
      locMidIn = FALSE
    
    ## Build expression for selecting columns from loc.dt
    # Core columns
    s.colexpr = paste0('.(y = ', loc.s.y,
                       ', id = trackObjectsLabelUni', 
                       ', group = ', loc.s.gr,
                       ', realtime = ', loc.s.rt)
    
    # account for the presence of 'mid.in' column in uploaded data
    if(locMidIn)
      s.colexpr = paste0(s.colexpr, 
                         ', mid.in = mid.in')
    
    # include position x,y columns in uploaded data
    if(locPos)
      s.colexpr = paste0(s.colexpr, 
                         ', pos.x = ', loc.s.pos.x,
                         ', pos.y = ', loc.s.pos.y)
    
    # include ObjectNumber column
    if(locObjNum)
      s.colexpr = paste0(s.colexpr, 
                         ', obj.num = ', loc.s.objnum)
    
    # close bracket, finish the expression
    s.colexpr = paste0(s.colexpr, ')')
    
    # create final dt for output based on columns selected above
    loc.out = loc.dt[, eval(parse(text = s.colexpr))]
    
    
    # if track selection ON
    if (locButHighlight){
      # add a 3rd level with status of track selection
      # to a column with trajectory filtering status in the uploaded file
      if(locMidIn)
        loc.out[, mid.in := ifelse(id %in% loc.tracks.highlight, 'SELECTED', mid.in)]
      else
        # add a column with status of track selection
        loc.out[, mid.in := ifelse(id %in% loc.tracks.highlight, 'SELECTED', 'NOT SEL')]
    }
      

    ## Interpolate NA's and data points not include
    # From: https://stackoverflow.com/questions/28073752/r-how-to-add-rows-for-missing-values-for-unique-group-sequences
    # Tracks are interpolated only within min and max realtime of every cell id
    setkey(loc.out, group, id, realtime)
    loc.out = loc.out[setkey(loc.out[, .(min(realtime):max(realtime)), by = .(group, id)], group, id, V1)]

    # x-check: print all rows with NA's
    print('Rows with NAs:')
    print(loc.out[rowSums(is.na(loc.out)) > 0, ])
    
    # Merge will create NA's where a realtime is missing.
    # Also, NA's may be already present in the dataset'.
    # Interpolate (linear) them with na.interpolate
    if(locPos)
      s.cols = c('y', 'pos.x', 'pos.y')
    else
      s.cols = c('y')
    
    loc.out[, (s.cols) := lapply(.SD, na.interpolation), by = id, .SDcols = s.cols]
    

    # !!! Current issue with interpolation:
    # The column mid.in is not taken into account.
    # If a trajectory is selected in the UI,
    # the mid.in column is added (if it doesn't already exist in the dataset),
    # and for the interpolated point, it will still be NA. Not really an issue.
    #
    # Also, think about the current option of having mid.in column in the uploaded dataset.
    # Keep it? Expand it?
    # Create a UI filed for selecting the column with mid.in data.
    # What to do with that column during interpolation (see above)
    
    ## Trim x-axis (time)
    if(input$chBtimeTrim) {
      loc.out = loc.out[realtime >= input$slTimeTrim[[1]] & realtime <= input$slTimeTrim[[2]] ]
    }
    
    ## Normalization
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
    
    #print(loc.dt)
    loc.out = dcast(loc.dt, id ~ realtime, value.var = 'y')
    #print(loc.out)
    loc.rownames = loc.out$id
    
    
    loc.out = as.matrix(loc.out[, -1])
    rownames(loc.out) = loc.rownames
    
    # Remove NA's
    # na.interpolation from package imputeTS works with multidimensional data
    # but imputation is performed for each column independently
    # The matrix for clustering contains time series in rows, hence transposing it twice
    loc.out = t(na.interpolation(t(loc.out)))
    
    return(loc.out)
  }) 
  
  
  # download data as prepared for plotting
  # after all modification
  output$downloadDataClean <- downloadHandler(
    filename = 'tCoursesSelected_clean.csv',
    content = function(file) {
      write.csv(data4trajPlot(), file, row.names = FALSE)
    }
  )
  
  
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
  
  ###### Trajectory plotting
  callModule(modTrajPlot, 'modTrajPlot', data4trajPlot)
  
  ###### AUC caluclation and plotting
  callModule(modAUCplot, 'tabAUC', data4trajPlot)
  
  ###### Box-plot
  callModule(tabBoxPlot, 'tabBoxPlot', data4trajPlot)
  
  
  
  ###### Scatter plot
  callModule(tabScatterPlot, 'tabScatter', data4trajPlot)
  
  ##### Hierarchical clustering
  callModule(clustHier, 'tabClHier', data4clust, data4trajPlot)
  
  ##### Sparse hierarchical clustering using sparcl
  callModule(clustHierSpar, 'tabClHierSpar', data4clust, data4trajPlot)

  
})

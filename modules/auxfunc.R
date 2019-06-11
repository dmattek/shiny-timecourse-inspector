#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# These are auxilary functions
#


require(ggplot2)
require(RColorBrewer)
require(gplots) # for heatmap.2
require(grid) # for modifying grob
require(Hmisc) # for CI calculation


# Global parameters ----

# if true, additional output printed to R console
DEB = T

# font sizes in pts for plots
# PLOTFONTBASE = 8
# PLOTFONTAXISTEXT = 8
# PLOTFONTAXISTITLE = 8
# PLOTFONTFACETSTRIP = 10
# PLOTFONTLEGEND = 8

PLOTFONTBASE = 12
PLOTFONTAXISTEXT = 12
PLOTFONTAXISTITLE = 12
PLOTFONTFACETSTRIP = 16
PLOTFONTLEGEND = 12


# default number of facets in plots
PLOTNFACETDEFAULT = 2

# internal column names
COLRT   = 'realtime'
COLY    = 'y'
COLID   = 'id'
COLIDUNI = 'trackObjectsLabelUni'
COLGR   = 'group'
COLIN   = 'mid.in'
COLOBJN = 'obj.num'
COLPOSX = 'pos.x'
COLPOSY = 'pos.y'
COLIDX = 'IDX'
COLIDXDIFF = 'IDXdiff'

# file names
FCSVOUTLIERS = 'outliers.csv'
FCSVTCCLEAN  = 'tCoursesSelected_clean.csv'
FPDFTCMEAN   = "tCoursesMeans.pdf"
FPDFTCSINGLE = "tCourses.pdf"
FPDFTCPSD    = 'tCoursesPsd.pdf'
FPDFBOXAUC   = 'boxplotAUC.pdf'
FPDFBOXTP    = 'boxplotTP.pdf'
FPDFSCATTER  = 'scatter.pdf'

# Colour definitions ----
rhg_cols <- c(
  "#771C19",
  "#AA3929",
  "#E25033",
  "#F27314",
  "#F8A31B",
  "#E2C59F",
  "#B6C5CC",
  "#8E9CA3",
  "#556670",
  "#000000"
)

md_cols <- c(
  "#FFFFFF",
  "#F8A31B",
  "#F27314",
  "#E25033",
  "#AA3929",
  "#FFFFCC",
  "#C2E699",
  "#78C679",
  "#238443"
)

# list of palettes for the heatmap
l.col.pal = list(
  "White-Orange-Red" = 'OrRd',
  "Yellow-Orange-Red" = 'YlOrRd',
  "Reds" = "Reds",
  "Oranges" = "Oranges",
  "Greens" = "Greens",
  "Blues" = "Blues",
  "Spectral" = 'Spectral'
)

# list of palettes for the dendrogram
l.col.pal.dend = list(
  "Rainbow" = 'rainbow_hcl',
  "Sequential" = 'sequential_hcl',
  "Heat" = 'heat_hcl',
  "Terrain" = 'terrain_hcl',
  "Diverge HCL" = 'diverge_hcl',
  "Diverge HSV" = 'diverge_hsv'
)

# Clustering algorithms ----

s.cl.linkage = c("ward.D",
                 "ward.D2",
                 "single",
                 "complete",
                 "average",
                 "mcquitty",
                 "centroid")

s.cl.spar.linkage = c("average",
                      "complete", 
                      "single",
                      "centroid")

s.cl.diss = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "DTW")
s.cl.spar.diss = c("squared.distance","absolute.value")


# Help text ----
# Creates a popup with help text
# From: https://gist.github.com/jcheng5/5913297
helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      #tags$i(class="icon-question-sign")
      # changed based on http://stackoverflow.com/questions/30436013/info-bubble-text-in-a-shiny-interface
      icon("question")
    )
  )
}

help.text = c(
  'Accepts CSV file with a column of cell IDs for removal. 
                   IDs should correspond to those used for plotting. 
  Say, the main data file contains columns Metadata_Site and TrackLabel. 
  These two columns should be then selected in UI to form a unique cell ID, e.g. 001_0001 where former part corresponds to Metadata_Site and the latter to TrackLabel.',
  'Plotting and data processing requires a unique cell ID across entire dataset. A typical dataset from CellProfiler assigns unique cell ID (TrackLabel) within each field of view (Metadata_Site).
                   Therefore, a unique ID is created by concatenating these two columns. If the dataset already contains a unique ID, UNcheck this box and select a single column only.',
  'This option allows to interpolate NAs or missing data. Some rows in the input file might be missing because a particular time point might not had been acquired. 
  This option, interpolates such missing points as well as points with NAs in the measurement column. When this option is checked, the interval of time column must be provided!',
  'Accepts CSV file with 5 columns: grouping (e.g. condition), start and end time points of stimulation, start and end points of y-position, dummy column with id.'
)

help.text.short = c(
  'Load CSV file with a column of cell IDs for removal. IDs should correspond to those used for plotting.',
  'If the track ID is unique only within a group, make it unique globally by combining with the grouping column.',
  'Interpolate missing tpts and pre-existing NAs. When checked, the interval of time column must be provided!',
  'Load CSV file with 5 columns: grouping, start and end tpts of stimulation, start and end of y-position, dummy column with id.',
  'Select columns to group data according to treatment, condition, etc.',
  'Select math operation to perform on a single or two columns,',
  'Select range of time for further processing.',
  'Normalise data to a selected region.',
  'Download data after modification in this section.'
)

# Functions for data processing ----
#' Calculate the mean and CI around time series
#'
#' @param in.dt Data table in long format
#' @param in.col.meas Name of the column with the measurement
#' @param in.col.by Column names for grouping (default NULL - no grouping). Typically, you want to use at least a column with time.
#' @param in.type Choice of normal approximation or boot-strapping
#' @param ... Other params passed to smean.cl.normal and smean.cl.boot; these include \code{conf.int} for the confidence level, \code{B} for the number of boot-strapping iterations.
#'
#' @return Datatable with columns: Mean, lower and upper CI, and grouping columns if provided.
#' @export
#' @import data.table
#' @import Hmisc
#'
#' @examples
#'
#'
#' # generate synthetic time series; 100 time points long, with 10 randomly placed NAs
#' dt.tmp = genTraj(100, 10, 6, 3, in.addna = 10)
#'
#' # calculate single stats from all time points
#' calcTrajCI(dt.tmp, 'objCyto_Intensity_MeanIntensity_imErkCor')
#'
#' # calculate the mean and CI along the time course
#' calcTrajCI(dt.tmp, 'objCyto_Intensity_MeanIntensity_imErkCor', 'Metadata_RealTime')
LOCcalcTrajCI = function(in.dt, in.col.meas, in.col.by = NULL, in.type = c('normal', 'boot'), ...) {
  in.type = match.arg(in.type)
  
  if (in.type %like% 'normal')
    loc.dt = in.dt[, as.list(smean.cl.normal(get(in.col.meas), ...)), by = in.col.by] else
      loc.dt = in.dt[, as.list(smean.cl.boot(get(in.col.meas), ...)), by = in.col.by]
    
    return(loc.dt)
}


#' Calculate the power spectrum density for time-series
#'
#' @param in.dt Data table in long format
#' @param in.col.meas Name of the column with the measurement
#' @param in.col.id Name of the column with the unique series identifier
#' @param in.col.by Column names for grouping (default NULL - no grouping). PSD of individual trajectories will be averaged within a group.
#' @param in.method Name of the method for PSD estimation, must be one of c("pgram", "ar"). Default to "pgram*.
#' @param in.return.period Wheter to return densities though periods (1/frequencies) instead of frequencies.
#' @param ... Other paramters to pass to stats::spectrum()
#'
#' @return Datatable with columns: (frequency or period), spec (the density) and grouping column
#' @export
#' @import data.table
#'
#' @examples
LOCcalcPSD <- function(in.dt,
                    in.col.meas,
                    in.col.id,
                    in.col.by,
                    in.method = "pgram",
                    in.return.period = TRUE,
                    ...){
  require(data.table)
  if(!in.method %in% c("pgram", "ar")){
    stop('Method should be one of: c("pgram", "ar"')
  }
  dt_spec <- copy(in.dt)
  dt_spec[, c("frequency", "spec") := (spectrum(get(in.col.meas), plot = FALSE, method = in.method, ...)[c("freq", "spec")]), by = in.col.id]
  dt_agg <- dt_spec[, .(spec = mean(spec)), by = c(in.col.by, "frequency")]
  if(in.return.period){
    dt_agg[, period := 1/frequency]
    dt_agg[, frequency := NULL]
  }
  return(dt_agg)
}


#' Generate synthetic CellProfiler output with single cell time series
#'
#'
#'
#' @param in.ntpts Number of time points (default 60)
#' @param in.ntracks Number of tracks per FOV (default 10)
#' @param in.nfov Number of FOV (default 6)
#' @param in.nwells Number of wells (default 1)
#' @param in.addna Number of NAs to add randomly in the data (default NULL)
#'
#' @return Data table with the follwoing columns: Metadata_Site, Metadata_Well, Metadata_RealTime, objCyto_Intensity_MeanIntensity_imErkCor (normal distributed),
#' objNuc_Intensity_MeanIntensity_imErkCor (normal distributed), objNuc_Location_X and objNuc_Location_Y (uniform ditributed), TrackLabel
#' @export
#' @import data.table

LOCgenTraj <- function(in.ntpts = 60, in.ntracks = 10, in.nfov = 6, in.nwells = 1, in.addna = NULL, in.addout = NULL) {
  
  x.rand.1 = c(rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 0.5, 0.1), rnorm(in.ntpts * in.ntracks * in.nfov * 1/3,   1, 0.2), rnorm(in.ntpts * in.ntracks * in.nfov * 1/3,  2, 0.5))
  x.rand.2 = c(rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 0.25, 0.1), rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 0.5, 0.2),  rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 1, 0.2))
  
  # add NA's for testing
  if (!is.null(in.addna)) {
    locTabLen = length(x.rand.1)
    x.rand.1[round(runif(in.addna) * locTabLen)] = NA
    x.rand.2[round(runif(in.addna) * locTabLen)] = NA
  }
  
  # add outliers for testing
  if (!is.null(in.addout)) {
    locTabLen = length(x.rand.1)
    x.rand.1[round(runif(in.addout) * locTabLen)] = 5
    x.rand.2[round(runif(in.addout) * locTabLen)] = 5
  }
  
  x.arg = rep(seq(1, in.ntpts), in.ntracks * in.nfov)
  
  dt.nuc = data.table(well = rep(LETTERS[1:in.nwells], each = in.ntpts * in.nfov * in.ntracks / in.nwells),
                      group = rep(1:in.nfov, each = in.ntpts * in.ntracks),
                      time = x.arg,
                      y1 = x.rand.1,
                      y2  = x.rand.2,
                      posx = runif(in.ntpts * in.ntracks * in.nfov, min = 0, max = 1),
                      posy = runif(in.ntpts * in.ntracks * in.nfov, min = 0, max = 1),
                      id = rep(1:(in.ntracks*in.nfov), each = in.ntpts))
  
  return(dt.nuc)
}

#' Normalize Trajectory
#'
#' Returns original dt with an additional column with normalized quantity.
#' The column to be normalised is given by 'in.meas.col'.
#' The name of additional column is the same as in.meas.col but with ".norm" suffix added.
#' Normalisation is based on part of the trajectory;
#' this is defined by in.rt.min and max, and the column with time in.rt.col.#'
#'
#' @param in.dt Data table in long format
#' @param in.meas.col String with the column name to normalize
#' @param in.rt.col String with the colum name holding time
#' @param in.rt.min Lower bound for time period used for normalization
#' @param in.rt.max Upper bound for time period used for normalization
#' @param in.by.cols String vector with 'by' columns to calculate normalization per group; if NULL, no grouping is done
#' @param in.robust Whether robust measures should be used (median instead of mean, mad instead of sd); default TRUE
#' @param in.type Type of normalization: z.score or mean (i.e. fold change w.r.t. mean); default 'z-score'
#'
#' @return Returns original dt with an additional column with normalized quantity.
#' @export
#' @import data.table

LOCnormTraj = function(in.dt,
                    in.meas.col,
                    in.rt.col = COLRT,
                    in.rt.min = 10,
                    in.rt.max = 20,
                    in.by.cols = NULL,
                    in.robust = TRUE,
                    in.type = 'z.score') {
  loc.dt <-
    copy(in.dt) # copy so as not to alter original dt object w intermediate assignments
  
  if (is.null(in.by.cols)) {
    if (in.robust)
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) >= in.rt.min &
                                 get(in.rt.col) <= in.rt.max, .(meas.md = median(get(in.meas.col), na.rm = TRUE),
                                                                meas.mad = mad(get(in.meas.col), na.rm = TRUE))]
    else
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) >= in.rt.min &
                                 get(in.rt.col) <= in.rt.max, .(meas.md = mean(get(in.meas.col), na.rm = TRUE),
                                                                meas.mad = sd(get(in.meas.col), na.rm = TRUE))]
    
    loc.dt = cbind(loc.dt, loc.dt.pre.aggr)
  }  else {
    if (in.robust)
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) >= in.rt.min &
                                 get(in.rt.col) <= in.rt.max, .(meas.md = median(get(in.meas.col), na.rm = TRUE),
                                                                meas.mad = mad(get(in.meas.col), na.rm = TRUE)), by = in.by.cols]
    else
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) >= in.rt.min &
                                 get(in.rt.col) <= in.rt.max, .(meas.md = mean(get(in.meas.col), na.rm = TRUE),
                                                                meas.mad = sd(get(in.meas.col), na.rm = TRUE)), by = in.by.cols]
    
    loc.dt = merge(loc.dt, loc.dt.pre.aggr, by = in.by.cols)
  }
  
  
  if (in.type == 'z.score') {
    loc.dt[, meas.norm := (get(in.meas.col) - meas.md) / meas.mad]
  } else {
    loc.dt[, meas.norm := (get(in.meas.col) / meas.md)]
  }
  
  setnames(loc.dt, 'meas.norm', paste0(in.meas.col, '.norm'))
  
  loc.dt[, c('meas.md', 'meas.mad') := NULL]
  return(loc.dt)
}



# Functions for clustering ----

# Return a dt with cell IDs and corresponding cluster assignments depending on dendrogram cut (in.k)
# This one works wth dist & hclust pair
# For sparse hierarchical clustering use getDataClSpar
# Arguments:
# in.dend  - dendrogram; usually output from as.dendrogram(hclust(distance_matrix))
# in.k - level at which dendrogram should be cut

getDataCl = function(in.dend, in.k) {
  cat(file = stderr(), 'getDataCl \n')
  
  loc.m = dendextend::cutree(in.dend, in.k, order_clusters_as_data = TRUE)
  #print(loc.m)
  
  # The result of cutree containes named vector with names being cell id's
  # THIS WON'T WORK with sparse hierarchical clustering because there, the dendrogram doesn't have original id's
  loc.dt.cl = data.table(id = names(loc.m),
                         cl = loc.m)
  
  #cat('===============\ndataCl:\n')
  #print(loc.dt.cl)
  return(loc.dt.cl)
}


# Return a dt with cell IDs and corresponding cluster assignments depending on dendrogram cut (in.k)
# This one works with sparse hierarchical clustering!
# Arguments:
# in.dend  - dendrogram; usually output from as.dendrogram(hclust(distance_matrix))
# in.k - level at which dendrogram should be cut
# in.id - vector of cell id's

getDataClSpar = function(in.dend, in.k, in.id) {
  cat(file = stderr(), 'getDataClSpar \n')
  
  loc.m = dendextend::cutree(in.dend, in.k, order_clusters_as_data = TRUE)
  #print(loc.m)
  
  # The result of cutree containes named vector with names being cell id's
  # THIS WON'T WORK with sparse hierarchical clustering because there, the dendrogram doesn't have original id's
  loc.dt.cl = data.table(id = in.id,
                         cl = loc.m)
  
  #cat('===============\ndataCl:\n')
  #print(loc.dt.cl)
  return(loc.dt.cl)
}



# prepares a table with cluster numbers in 1st column and colour assignments in 2nd column
# the number of rows is determined by dendrogram cut
getClCol <- function(in.dend, in.k) {
  
  loc.col_labels <- get_leaves_branches_col(in.dend)
  loc.col_labels <- loc.col_labels[order(order.dendrogram(in.dend))]
  
  return(unique(
    data.table(cl.no = dendextend::cutree(in.dend, k = in.k, order_clusters_as_data = TRUE),
               cl.col = loc.col_labels)))
}


# Custom plotting functions ----


#' Custom ggPlot theme based on theme_bw
#'
#' @param in.font.base
#' @param in.font.axis.text
#' @param in.font.axis.title
#' @param in.font.strip
#' @param in.font.legend
#'
#' @return
#' @export
#'
#' @examples
#'
LOCggplotTheme = function(in.font.base = 12,
                       in.font.axis.text = 12,
                       in.font.axis.title = 12,
                       in.font.strip = 14,
                       in.font.legend = 12) {
  loc.theme =
    theme_bw(base_size = in.font.base, base_family = "Helvetica") +
    theme(
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 0.25),
      axis.text = element_text(size = in.font.axis.text),
      axis.title = element_text(size = in.font.axis.title),
      strip.text = element_text(size = in.font.strip, face = "bold"),
      strip.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = in.font.legend),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(2, "lines"))
  
  return(loc.theme)
}

# Build Function to Return Element Text Object
# From: https://stackoverflow.com/a/36979201/1898713
LOCrotatedAxisElementText = function(angle, position='x', size = 12){
  angle     = angle[1]; 
  position  = position[1]
  positions = list(x=0, y=90, top=180, right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")), call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads = (-angle - positions[[ position ]])*pi/180
  hjust = round((1 - sin(rads)))/2
  vjust = round((1 + cos(rads)))/2
  element_text(size = 12, angle = angle, vjust = vjust, hjust = hjust)
}

# Plot individual time series
LOCplotTraj = function(dt.arg, # input data table
                        x.arg,  # string with column name for x-axis
                        y.arg, # string with column name for y-axis
                        group.arg, # string with column name for grouping time series (typicaly cell ID)
                        facet.arg, # string with column name for facetting
                        facet.ncol.arg = 2, # default number of facet columns
                        facet.color.arg = NULL, # vector with list of colours for adding colours to facet names (currently a horizontal line on top of the facet is drawn)
                        line.col.arg = NULL, # string with column name for colouring time series (typically when individual time series are selected in UI)
                        xlab.arg = NULL, # string with x-axis label
                        ylab.arg = NULL, # string with y-axis label
                        plotlab.arg = NULL, # string with plot label
                        dt.stim.arg = NULL, # plotting additional dataset; typically to indicate stimulations (not fully implemented yet, not tested!)
                        x.stim.arg = c('tstart', 'tend'), # column names in stimulation dt with x and xend parameters
                        y.stim.arg = c('ystart', 'yend'), # column names in stimulation dt with y and yend parameters
                        tfreq.arg = 1,
                        ylim.arg = NULL,
                        stim.bar.width.arg = 0.5,
                        aux.label1 = NULL, # 1st point label; used for interactive plotting; displayed in the tooltip; typically used to display values of column holding x & y coordinates
                        aux.label2 = NULL,
                        aux.label3 = NULL,
                        stat.arg = c('', 'mean', 'CI', 'SE')) {
  
  # match arguments for stat plotting
  loc.stat = match.arg(stat.arg, several.ok = TRUE)

  
  # aux.label12 are required for plotting XY positions in the tooltip of the interactive (plotly) graph
  p.tmp = ggplot(dt.arg,
                 aes_string(x = x.arg,
                            y = y.arg,
                            group = group.arg,
                            label = group.arg))
  #,
  #                          label  = aux.label1,
  #                          label2 = aux.label2,
  #                          label3 = aux.label3))
  
  if (is.null(line.col.arg)) {
    p.tmp = p.tmp +
      geom_line(alpha = 0.25, 
                              size = 0.25)
  }
  else {
    p.tmp = p.tmp + 
      geom_line(aes_string(colour = line.col.arg), 
                              alpha = 0.5, 
                              size = 0.5) +
      scale_color_manual(name = '', 
                         values =c("FALSE" = rhg_cols[7], "TRUE" = rhg_cols[3], "SELECTED" = 'green', "NOT SEL" = rhg_cols[7]))
  }

  # this is temporary solution for adding colour according to cluster number
  # use only when plotting traj from clustering!
  # a horizontal line is added at the top of data
  if (!is.null(facet.color.arg)) {

    loc.y.max = max(dt.arg[, c(y.arg), with = FALSE])
    loc.dt.cl = data.table(xx = 1:length(facet.color.arg), yy = loc.y.max)
    setnames(loc.dt.cl, 'xx', facet.arg)
    
    # adjust facet.color.arg to plot
    
    p.tmp = p.tmp +
      geom_hline(data = loc.dt.cl, colour = facet.color.arg, yintercept = loc.y.max, size = 4) +
      scale_colour_manual(values = facet.color.arg,
                          name = '')
  }
  
  if ('mean' %in% loc.stat)
    p.tmp = p.tmp + 
    stat_summary(
      aes_string(y = y.arg, group = 1),
      fun.y = mean,
      colour = 'red',
      linetype = 'solid',
      size = 1,
      geom = "line",
      group = 1
    )

  if ('CI' %in% loc.stat)
    p.tmp = p.tmp + 
    stat_summary(
      aes_string(y = y.arg, group = 1),
      fun.data = mean_cl_normal,
      colour = 'red',
      alpha = 0.25,
      geom = "ribbon",
      group = 1
    )
  
  if ('SE' %in% loc.stat)
    p.tmp = p.tmp + 
    stat_summary(
      aes_string(y = y.arg, group = 1),
      fun.data = mean_se,
      colour = 'red',
      alpha = 0.25,
      geom = "ribbon",
      group = 1
    )
  
  
  
  p.tmp = p.tmp + 
    facet_wrap(as.formula(paste("~", facet.arg)),
               ncol = facet.ncol.arg,
               scales = "free_x")

  # plot stimulation bars underneath time series
  # dt.stim.arg is read separately and should contain 4 columns with
  # xy positions of beginning and end of the bar
  if(!is.null(dt.stim.arg)) {
    p.tmp = p.tmp + geom_segment(data = dt.stim.arg,
                                 aes_string(x = x.stim.arg[1],
                                            xend = x.stim.arg[2],
                                            y = y.stim.arg[1],
                                            yend = y.stim.arg[2],
                                            group = 'group'),
                                 colour = rhg_cols[[3]],
                                 size = stim.bar.width.arg) 
  }
  
  if (!is.null(ylim.arg)) 
    p.tmp = p.tmp + coord_cartesian(ylim = ylim.arg)
  
  p.tmp = p.tmp + 
    xlab(paste0(xlab.arg, "\n")) +
    ylab(paste0("\n", ylab.arg)) +
    ggtitle(plotlab.arg) +
    LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                   in.font.axis.text = PLOTFONTAXISTEXT, 
                   in.font.axis.title = PLOTFONTAXISTITLE, 
                   in.font.strip = PLOTFONTFACETSTRIP, 
                   in.font.legend = PLOTFONTLEGEND) + 
    theme(legend.position = "top")
  
  return(p.tmp)
}

# Plot average time series with CI together in one facet
LOCplotTrajRibbon = function(dt.arg, # input data table
                          x.arg, # string with column name for x-axis
                          y.arg, # string with column name for y-axis
                          group.arg = NULL, # string with column name for grouping time series (here, it's a column corresponding to grouping by condition)
                          col.arg = NULL, # colour pallette for individual time series
                          dt.stim.arg = NULL, # data table with stimulation pattern
                          x.stim.arg = c('tstart', 'tend'), # column names in stimulation dt with x and xend parameters
                          y.stim.arg = c('ystart', 'yend'), # column names in stimulation dt with y and yend parameters
                          stim.bar.width.arg = 0.5,
                          ribbon.lohi.arg = c('Lower', 'Upper'),
                          ribbon.fill.arg = 'grey50',
                          ribbon.alpha.arg = 0.5,
                          xlab.arg = NULL,
                          ylab.arg = NULL,
                          plotlab.arg = NULL) {
  
  p.tmp = ggplot(dt.arg, aes_string(x = x.arg, group = group.arg)) +
    geom_ribbon(aes_string(ymin = ribbon.lohi.arg[1], ymax = ribbon.lohi.arg[2]),
                fill = ribbon.fill.arg,
                alpha = ribbon.alpha.arg) +
    geom_line(aes_string(y = y.arg, colour = group.arg))
  

  # plot stimulation bars underneath time series
  # dt.stim.arg is read separately and should contain 4 columns with
  # xy positions of beginning and end of the bar
  if(!is.null(dt.stim.arg)) {
    p.tmp = p.tmp + geom_segment(data = dt.stim.arg,
                                 aes_string(x = x.stim.arg[1],
                                     xend = x.stim.arg[2],
                                     y = y.stim.arg[1],
                                     yend = y.stim.arg[2]),
                                 colour = rhg_cols[[3]],
                                 size = stim.bar.width.arg,
                                 group = 1) 
  }

  
  if (is.null(col.arg)) {
    p.tmp = p.tmp +
      scale_color_discrete(name = '')
  } else {
    p.tmp = p.tmp +
      scale_colour_manual(values = col.arg, name = '')
  }
  
  if (!is.null(plotlab.arg))
    p.tmp = p.tmp + ggtitle(plotlab.arg)
  
  p.tmp = p.tmp +
    xlab(xlab.arg) +
    ylab(ylab.arg)
  
  return(p.tmp)
}

# Plot average power spectrum density per facet
LOCplotPSD <- function(dt.arg, # input data table
                    x.arg, # string with column name for x-axis
                    y.arg, # string with column name for y-axis
                    group.arg=NULL, # string with column name for grouping time series (here, it's a column corresponding to grouping by condition)
                    xlab.arg = x.arg,
                    ylab.arg = y.arg){
  require(ggplot2)
  if(length(setdiff(c(x.arg, y.arg, group.arg), colnames(dt.arg))) > 0){
    stop(paste("Missing columns in dt.arg: ", setdiff(c(x.arg, y.arg, group.arg), colnames(dt.arg))))
  }
  p.tmp <- ggplot(dt.arg, aes_string(x=x.arg, y=y.arg)) +
    geom_line() +
    facet_wrap(group.arg) +
    labs(x = xlab.arg, y = ylab.arg)
  return(p.tmp)
}

# Plots a scatter plot with marginal histograms
# Points are connected by a line (grouping by cellID)
#
# Assumes an input of data.table with
# x, y - columns with x and y coordinates
# id - a unique point identifier (here corresponds to cellID)
# mid - a (0,1) column by which points are coloured (here corresponds to whether cells are within bounds)

LOCggplotScat = function(dt.arg,
                        band.arg = NULL,
                        facet.arg = NULL,
                        facet.ncol.arg = 2,
                        xlab.arg = NULL,
                        ylab.arg = NULL,
                        plotlab.arg = NULL,
                        alpha.arg = 1,
                        group.col.arg = NULL) {
  p.tmp = ggplot(dt.arg, aes(x = x, y = y))
  
  if (is.null(group.col.arg)) {
    p.tmp = p.tmp +
      geom_point(alpha = alpha.arg, aes(group = id))
  } else {
    p.tmp = p.tmp +
      geom_point(aes(colour = as.factor(get(group.col.arg)), group = id), alpha = alpha.arg) +
      geom_path(aes(colour = as.factor(get(group.col.arg)), group = id), alpha = alpha.arg) +
      scale_color_manual(name = group.col.arg, values =c("FALSE" = rhg_cols[7], "TRUE" = rhg_cols[3], "SELECTED" = 'green'))
  }
  
  if (is.null(band.arg))
    p.tmp = p.tmp +
      stat_smooth(
        # method = function(formula, data, weights = weight)
        #   rlm(formula, data, weights = weight, method = 'MM'),
        method = "lm",
        fullrange = FALSE,
        level = 0.95,
        colour = 'blue'
      )
  else {
    p.tmp = p.tmp +
      geom_abline(slope = band.arg$a, intercept = band.arg$b) +
      geom_abline(
        slope = band.arg$a,
        intercept =  band.arg$b + abs(band.arg$b)*band.arg$width,
        linetype = 'dashed'
      ) +
      geom_abline(
        slope = band.arg$a,
        intercept = band.arg$b - abs(band.arg$b)*band.arg$width,
        linetype = 'dashed'
      )
  }
  
  if (!is.null(facet.arg)) {
    p.tmp = p.tmp +
      facet_wrap(as.formula(paste("~", facet.arg)),
                 ncol = facet.ncol.arg)
    
  }
  
  
  if (!is.null(xlab.arg))
    p.tmp = p.tmp +
      xlab(paste0(xlab.arg, "\n"))
  
  if (!is.null(ylab.arg))
    p.tmp = p.tmp +
      ylab(paste0("\n", ylab.arg))
  
  if (!is.null(plotlab.arg))
    p.tmp = p.tmp +
      ggtitle(paste0(plotlab.arg, "\n"))
  
  
  
  p.tmp = p.tmp +
    LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                   in.font.axis.text = PLOTFONTAXISTEXT, 
                   in.font.axis.title = PLOTFONTAXISTITLE, 
                   in.font.strip = PLOTFONTFACETSTRIP, 
                   in.font.legend = PLOTFONTLEGEND) + 
    theme(legend.position = "none")

  # Marginal distributions don;t work with plotly...
  # if (is.null(facet.arg))
  #   ggExtra::ggMarginal(p.scat, type = "histogram",  bins = 100)
  # else
  return(p.tmp)
}


LOCplotHeatmap <- function(data.arg,
                          dend.arg,
                          palette.arg,
                          palette.rev.arg = TRUE,
                          dend.show.arg = TRUE,
                          key.show.arg = TRUE,
                          margin.x.arg = 5,
                          margin.y.arg = 20,
                          nacol.arg = 0.5,
                          colCol.arg = NULL,
                          labCol.arg = NULL,
                          font.row.arg = 1,
                          font.col.arg = 1,
                          breaks.arg = NULL,
                          title.arg = 'Clustering') {
  
  loc.n.colbreaks = 99
  
  if (palette.rev.arg)
    my_palette <-
    rev(colorRampPalette(brewer.pal(9, palette.arg))(n = loc.n.colbreaks))
  else
    my_palette <-
    colorRampPalette(brewer.pal(9, palette.arg))(n = loc.n.colbreaks)
  
  
  col_labels <- get_leaves_branches_col(dend.arg)
  col_labels <- col_labels[order(order.dendrogram(dend.arg))]
  
  if (dend.show.arg) {
    assign("var.tmp.1", dend.arg)
    var.tmp.2 = "row"
  } else {
    assign("var.tmp.1", FALSE)
    var.tmp.2 = "none"
  }
  
  loc.p = heatmap.2(
    data.arg,
    Colv = "NA",
    Rowv = var.tmp.1,
    srtCol = 90,
    dendrogram = var.tmp.2,
    trace = "none",
    key = key.show.arg,
    margins = c(margin.x.arg, margin.y.arg),
    col = my_palette,
    na.col = grey(nacol.arg),
    denscol = "black",
    density.info = "density",
    RowSideColors = col_labels,
    colRow = col_labels,
    colCol = colCol.arg,
    labCol = labCol.arg,
    #      sepcolor = grey(input$inPlotHierGridColor),
    #      colsep = 1:ncol(loc.dm),
    #      rowsep = 1:nrow(loc.dm),
    cexRow = font.row.arg,
    cexCol = font.col.arg,
    main = title.arg,
    symbreaks = FALSE,
    symkey = FALSE,
    breaks = if (is.null(breaks.arg)) NULL else seq(breaks.arg[1], breaks.arg[2], length.out = loc.n.colbreaks+1)
  )
  
  return(loc.p)
}

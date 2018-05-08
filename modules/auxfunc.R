## Custom plotting
require(ggplot2)
require(RColorBrewer)
require(gplots) # for heatmap.2
require(grid) # for modifying grob

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
  This option, interpolates such missing points as well as points with NAs in the measurement column. When this option is checked, the interval of time column must be provided!'
)


#####
## Functions for clustering 


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


#####
## Common plotting functions

myGgplotTraj = function(dt.arg, # data table
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
                        tfreq.arg = 1,
                        ylim.arg = NULL,
                        stim.bar.height.arg = 0.1,
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
  
  if(!is.null(dt.stim.arg)) {
    p.tmp = p.tmp + geom_segment(data = dt.stim.arg,
                                 aes(x = Stimulation_time - tfreq.arg,
                                     xend = Stimulation_time - tfreq.arg,
                                     y = ylim.arg[1],
                                     yend = ylim.arg[1] + abs(ylim.arg[2] - ylim.arg[1]) * stim.bar.height.arg),
                                 colour = rhg_cols[[3]],
                                 size = stim.bar.width.arg,
                                 group = 1) 
  }
  
  if (!is.null(ylim.arg)) 
    p.tmp = p.tmp + coord_cartesian(ylim = ylim.arg)
  
  p.tmp = p.tmp + 
    xlab(paste0(xlab.arg, "\n")) +
    ylab(paste0("\n", ylab.arg)) +
    ggtitle(plotlab.arg) +
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
      legend.position = "top"
    )
  
  return(p.tmp)
}


# Fast DTW computation
fastDTW <-function (x)
{
  return(dtw(x, window.type = 'sakoechiba', distance.only = T)$normalizedDistance)
}


# Plots a scatter plot with marginal histograms
# Points are connected by a line (grouping by cellID)
#
# Assumes an input of data.table with
# x, y - columns with x and y coordinates
# id - a unique point identifier (here corresponds to cellID)
# mid - a (0,1) column by which points are coloured (here corresponds to whether cells are within bounds)

myGgplotScat = function(dt.arg,
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
        method = function(formula, data, weights = weight)
          rlm(formula, data, weights = weight, method = 'MM'),
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
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
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
      legend.position = "none"
    )
  
  # Marginal distributions don;t work with plotly...
  # if (is.null(facet.arg))
  #   ggExtra::ggMarginal(p.scat, type = "histogram",  bins = 100)
  # else
  return(p.tmp)
}

myGgplotTheme = theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.25),
    axis.line.y = element_line(color = "black", size = 0.25),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 14, face = "bold"),
    strip.text.y = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    legend.key = element_blank(),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(2, "lines"),
    legend.position = "right"
  )


myPlotHeatmap <- function(data.arg,
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

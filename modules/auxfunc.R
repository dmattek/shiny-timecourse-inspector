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

s.cl.diss = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
s.cl.spar.diss = c("squared.distance","absolute.value")

l.col.pal = list(
  "White-Orange-Red" = 'OrRd',
  "Yellow-Orange-Red" = 'YlOrRd',
  "Reds" = "Reds",
  "Oranges" = "Oranges",
  "Greens" = "Greens",
  "Blues" = "Blues",
  "Spectral" = 'Spectral'
)


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
                        stat.arg = c('', 'mean', 'CI', 'SE')) {
  
  # match arguments for stat plotting
  loc.stat = match.arg(stat.arg, several.ok = TRUE)

  
  # aux.label12 are required for plotting XY positions in the tooltip of the interactive (plotly) graph
  p.tmp = ggplot(dt.arg,
                 aes_string(x = x.arg,
                            y = y.arg,
                            group = group.arg,
                            label  = aux.label1,
                            label2 = aux.label2))
  
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


userDataGen <- function() {  
  cat(file=stderr(), 'userDataGen: in\n')
  
  locNtp = 40
  locNtracks = 100
  locNsites = 4
  locNwells = 1
  
  x.rand.1 = c(rnorm(locNtp * locNtracks * locNsites * 0.5, .5, 0.1), rnorm(locNtp * locNtracks * locNsites * 0.5, 1, 0.2))
  x.rand.2 = c(rnorm(locNtp * locNtracks * locNsites * 0.5, 0.25, 0.1), rnorm(locNtp * locNtracks * locNsites * 0.5, 0.5, 0.2))
#  x.rand.3 = rep(rnorm(locNtracks, 2, 0.5), 1, each = locNtp)
#  x.rand.4 = rep(rnorm(locNtracks, 1, 0.1), 1, each = locNtp)
  
#  x.arg = rep(seq(0, locNtp-1) / locNtp * 4 * pi, locNtracks * locNsites)
  x.arg = rep(seq(1, locNtp), locNtracks * locNsites)
  
  dt.nuc = data.table(Metadata_Site = rep(1:locNsites, each = locNtp * locNtracks),
                      Metadata_Well = rep(1:locNwells, each = locNtp * locNsites * locNtracks / locNwells),
                      Metadata_RealTime = x.arg,
                      objCyto_Intensity_MeanIntensity_imErkCor = x.rand.1,
                      objNuc_Intensity_MeanIntensity_imErkCor  = x.rand.2,
                      objNuc_Location_X = runif(locNtp * locNtracks * locNsites, min = 0, max = 1),
                      objNuc_Location_Y = runif(locNtp * locNtracks * locNsites, min = 0, max = 1),
#                      objCyto_Intensity_MeanIntensity_imErkCor = x.rand.3 + ifelse(x.arg < 4, 0, 1) / x.rand.3,
#                      objNuc_Intensity_MeanIntensity_imErkCor  = c(rnorm(locNtp * locNtracks * locNsites * 0.5, .25, 0.1), rnorm(locNtp * locNtracks * locNsites * 0.5, .5, 0.2)),
                      TrackLabel = rep(1:(locNtracks*locNsites), each = locNtp))
  
  return(dt.nuc)
}


# Returns original dt with an additional column with normalized quantity.
# The column to be normalised is given by 'in.meas.col'.
# The name of additional column is the same as in.meas.col but with ".norm" suffix added.
# Normalisation is based on part of the trajectory;
# this is defined by in.rt.min and max, and the column with time in.rt.col.
# Additional parameters:
# in.by.cols - character vector with 'by' columns to calculate normalisation per group
#              if NULL, no grouping is done
# in.robust - whether robust measures should be used (median instead of mean, mad instead of sd)
# in.type - type of normalization: z.score or mean (fi.e. old change w.r.t. mean)

myNorm = function(in.dt,
                  in.meas.col,
                  in.rt.col = 'RealTime',
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
                          title.arg = 'Clustering') {
  
  if (palette.rev.arg)
    my_palette <-
    rev(colorRampPalette(brewer.pal(9, palette.arg))(n = 99))
  else
    my_palette <-
    colorRampPalette(brewer.pal(9, palette.arg))(n = 99)
  
  
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
    main = title.arg
  )
  
  return(loc.p)
}

## Custom plotting
require(ggplot2)

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


myGgplotTraj = function(dt.arg,
                        x.arg,
                        y.arg,
                        group.arg,
                        facet.arg,
                        facet.ncol.arg = 2,
                        line.col.arg = NULL,
                        xlab.arg = NULL,
                        ylab.arg = NULL,
                        plotlab.arg = NULL,
                        dt.stim.arg = NULL,
                        tfreq.arg = 1,
                        ylim.arg = NULL,
                        stim.bar.height.arg = 0.1,
                        stim.bar.width.arg = 0.5) {
  p.tmp = ggplot(dt.arg,
                 aes_string(x = x.arg,
                            y = y.arg,
                            group = group.arg))
  
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
  
  p.tmp = p.tmp + 
    stat_summary(
      aes_string(y = y.arg, group = 1),
      fun.y = mean,
      colour = 'blue',
      linetype = 'solid',
      size = 1,
      geom = "line",
      group = 1
    ) +
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
  
  p.tmp
}


userDataGen <- function() {  
  cat(file=stderr(), 'userDataGen: in\n')
  
  locNtp = 40
  locNtracks = 5
  locNsites = 4
  locNwells = 2
  
  dt.nuc = data.table(Metadata_Site = rep(1:locNsites, each = locNtp * locNtracks),
                      Metadata_Well = rep(1:locNwells, each = locNtp * locNsites * locNtracks / locNwells),
                      Metadata_RealTime = rep(1:locNtp, locNsites* locNtracks),
                      objCyto_Intensity_MeanIntensity_imErkCor = c(rnorm(locNtp * locNtracks * locNsites * 0.5, .5, 0.1), rnorm(locNtp * locNtracks * locNsites * 0.5, 1, 0.2)),
                      objNuc_Intensity_MeanIntensity_imErkCor  = c(rnorm(locNtp * locNtracks * locNsites * 0.5, .25, 0.1), rnorm(locNtp * locNtracks * locNsites * 0.5, .5, 0.2)),
                      TrackLabel = rep(1:(locNtracks*locNsites), each = locNtp))
  
  cat(colnames(dt.nuc))
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
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) > in.rt.min &
                                 get(in.rt.col) < in.rt.max, .(meas.md = median(get(in.meas.col), na.rm = TRUE),
                                                               meas.mad = mad(get(in.meas.col), na.rm = TRUE))]
    else
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) > in.rt.min &
                                 get(in.rt.col) < in.rt.max, .(meas.md = mean(get(in.meas.col), na.rm = TRUE),
                                                               meas.mad = sd(get(in.meas.col), na.rm = TRUE))]
    
    loc.dt = cbind(loc.dt, loc.dt.pre.aggr)
  }  else {
    if (in.robust)
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) > in.rt.min &
                                 get(in.rt.col) < in.rt.max, .(meas.md = median(get(in.meas.col), na.rm = TRUE),
                                                               meas.mad = mad(get(in.meas.col), na.rm = TRUE)), by = in.by.cols]
    else
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) > in.rt.min &
                                 get(in.rt.col) < in.rt.max, .(meas.md = mean(get(in.meas.col), na.rm = TRUE),
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
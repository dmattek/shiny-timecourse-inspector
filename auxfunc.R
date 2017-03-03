## Custom plotting
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
                        maxrt.arg = 60,
                        xaxisbreaks.arg = 10,
                        ylim.arg = c(0,1),
                        stim.bar.height.arg = 0.1,
                        stim.bar.width.arg = 0.5) {
  p.tmp = ggplot(dt.arg,
                 aes_string(x = x.arg,
                            y = y.arg))
  
  if (is.null(line.col.arg))
    p.tmp = p.tmp + geom_line(aes_string(group = group.arg), alpha = 0.25, size = 0.25)
  else
    p.tmp = p.tmp + geom_line(aes_string(group = group.arg, colour = line.col.arg), alpha = 0.5, size = 0.5)
  
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
  
  p.tmp = p.tmp + 
    scale_x_continuous(breaks = seq(0, maxrt.arg, xaxisbreaks.arg)) +
    coord_cartesian(ylim = ylim.arg) +
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
  
  locNtp = 13
  locNtracks = 5
  locNsites = 4
  locNwells = 2
  
  dt.nuc = data.table(Metadata_Site = rep(1:locNsites, each = locNtp * locNtracks),
                      Metadata_Well = rep(1:locNwells, each = locNtp * locNsites * locNtracks / locNwells),
                      Metadata_Time = rep(1:locNtp, locNsites* locNtracks),
                      meas_MeanIntensity_cyto = rnorm(locNtp * locNtracks * locNsites, .5, 0.1),
                      meas_MeanIntensity_nuc  = rnorm(locNtp * locNtracks * locNsites, .5, 0.1),
                      TrackLabel = rep(1:(locNtracks*locNsites), each = locNtp))
  
  cat(colnames(dt.nuc))
  return(dt.nuc)
}

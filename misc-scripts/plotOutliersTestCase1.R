require(ggplot2)
require(data.table)

dt = fread('~/Dropbox/Projects/shiny-server/tcourse-inspector/example-data/test-case-1/mp3-20_FGF_ekar.csv.gz')
dtout = fread('~/Dropbox/Projects/shiny-server/tcourse-inspector/example-data/test-case-1/mp3-20_FGF_badTraj.csv')

dt[, uniid := paste0(fov, "_", id)]

dtouttraj = dt[uniid %in% unique(dtout$id)]
dtmaintraj = dt[!(uniid %in% unique(dtout$id))]

ggplot(dtouttraj, aes(x = realtime, y = intensity_ekar, group = uniid)) + 
  geom_line(alpha = 0.5, size = 0.25) + 
  facet_wrap(~group) +
  stat_summary(data=dtmaintraj, 
               aes(y = intensity_ekar, group = 1),
               fun.y = mean,
               na.rm = T,
               colour = 'red',
               linetype = 'solid',
               size = 1,
               geom = "line",
               group = 1) +
  xlab("Time (min)") +
  ylab("ERK activity") +
  theme_bw(base_size = 8, base_family = "Helvetica") +
  theme(
    panel.spacing = unit(1, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.25),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(2, "lines")
  )

ggsave(filename = "~/Dropbox/Projects/notes/paperTcourseInspector/data/PC12-mp3x20-FGF2/plots/tCourses_outliers.pdf", width = 5, height = 2)

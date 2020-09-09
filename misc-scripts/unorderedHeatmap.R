require(data.table)
require(gplots)
require(RColorBrewer)

dt.tmp = fread('~/Projects/Olivier/PC12.Yannick/Figure1_sustained/sust_EGF_NGF_FGF/sust_E_F_N_clean.csv')

m.tmp = dcast(dt.tmp, id ~ realtime, value.var = 'y')
loc.rownames = m.tmp$condUni
m.tmp = as.matrix(m.tmp[, -1])
rownames(m.tmp) = loc.rownames

m.tmp.shuffle = m.tmp[sample(nrow(m.tmp)), ]

heatmap(m.tmp.shuffle)

heatmap.2(
  m.tmp.shuffle,
  Colv = "NA",
  Rowv = "NA",
  srtCol = 90,
  dendrogram = 'none',
  trace = "none",
  key = FALSE,
  #margins = c(margin.x.arg, margin.y.arg),
  col = rev(colorRampPalette(brewer.pal(9, 'Spectral'))(n = 99)),
  #na.col = grey(nacol.arg),
  denscol = "black",
  density.info = "density",
  #RowSideColors = col_labels,
  #colRow = col_labels,
  #colCol = colCol.arg,
  labRow = NA
  #labCol = labCol.arg,
  #      sepcolor = grey(input$inPlotHierGridColor),
  #      colsep = 1:ncol(loc.dm),
  #      rowsep = 1:nrow(loc.dm),
  #cexRow = font.row.arg,
  #cexCol = font.col.arg,
  #main = title.arg
)

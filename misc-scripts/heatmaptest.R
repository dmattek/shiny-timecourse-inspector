
require(dendextend)
library(RColorBrewer)

dm.tmp = t(matrix(c(rnorm(300, 0, 1), rnorm(300, 2, .5), rnorm(400, 1, .5)), nrow = 100, ncol = 10))


cl.dist = dist(dm.tmp)
cl.hc = hclust(cl.dist)
cl.dend = as.dendrogram(cl.hc)

cutree(cl.dend, k = 3)

cl.dend <- color_branches(cl.dend, k = 3)


cl.tree = cutree_1k.dendrogram(cl.dend, k=3, order_clusters_as_data = TRUE)


col_labels <- get_leaves_branches_col(cl.dend)
col_labels <- col_labels[order(order.dendrogram(cl.dend))]

unique(
  data.frame(cl.no = cl.tree,
           cl.col = col_labels))


p1 = myPlotHeatmap(dm.tmp,
              cl.dend, 
              palette.arg = "Spectral")

heatmap.2(
  dm.tmp,
  Colv = "NA",
  Rowv = cl.dend,
  srtCol = 90,
  dendrogram = 'row',
  trace = "none")

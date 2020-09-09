require(data.table)
require(gplots)
require(dendextend)

dt = fread("~/Dropbox/Projects/olivier/masterStudents/MauroGwerder-shinyapp-outliers/data/20191018_sys_optoSOS_siPOOLs_50nM_singlePulse5x_20per100ms/tCoursesSelected_cleaned_CNerkWithMeta.csv.gz")

dt = dt[, c("track_id_uni", "RealTime", "ratioERK")]
setnames(dt, c("id", "t", "y"))

summary(dt[, .N, by = id][["N"]])

#dm = as.matrix(fread("tabClHier-userFitDistHier-locdm.csv", header = T, drop = c(1)))
dm = dcast(dt, 
           id ~ t,
           value.var = "y")

myRowNames = dm[["id"]]

# omit first column that contains row names
dm = as.matrix(dm[, -1])

# assign row names to the matrix
rownames(dm) = myRowNames

vNnas = rowSums(is.na(dm))

min(vNnas[ vNnas > 0 ])
max(vNnas[ vNnas > 0 ])


cl.dist = proxy::dist(dm, method = "euclidean")

sum(is.na(cl.dist))
sum(is.nan(cl.dist))
sum(is.infinite(cl.dist))


cl.hc = hclust(cl.dist, method = "average")

loc.col = ggthemes::tableau_color_pal(input$selectPlotHierPaletteDend)(n = 4)

col_labels <- get_leaves_branches_col(as.dendrogram(cl.hc))
col_labels <- col_labels[order(order.dendrogram(as.dendrogram(cl.hc)))]



heatmap.2(dm,
          dendrogram = "row",
          trace = "none",
          Colv = FALSE)

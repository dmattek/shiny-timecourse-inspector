# Input: a data matrix, a distance matrix, UI elemts
# Output: a list o fplots

library(data.table)
library(factoextra)

#####################################
in_dt <- fread("~/Dropbox/Projects/shiny-server/tcourse-inspector/example-data/test-case-1/mp3-20_FGF_ekar.csv.gz")
in_dt <- dcast(in_dt, id + fov~ realtime, value.var = "intensity_ekar")
in_dt[, id := NULL]
###################################

in_distance <- "euclidean"
in_linkage <- "complete"
in_kmax <- 20
in_nclust <- 2

# --------------------------------------------------------------------------
# Get distance matrix, MIGHT NEED TO ADD SOME CODE IN THE APP FOR DTW

in_mat <- as.matrix(in_dt)
mat_dist <- dist(in_mat, method = in_distance)

# --------------------------------------------------------------------------
# Customize factoextra functions to accept dissimilarity matrix from start. Otherwise can't use distance functions that are not in base R, like DTW.

# Inherit and adapt hcut function to take input from UI, used for fviz_clust 
my_hcut <- function(x, k = 2, isdiss = inherits(x, "dist"), hc_func = "hclust", hc_method = in_linkage, hc_metric = in_distance){
  if(!inherits(mat_dist, "dist")){stop("x must be a distance matrix")}
  return(factoextra::hcut(x = x, k = k, isdiss = TRUE, hc_func = hc_func, hc_method = hc_method, hc_metric = hc_metric))
}

# Modified from factoextra::fviz_nbclust
# Allow (actually enforce) x to be a distance matrix; no GAP statistics for compatibility
my_nbclust <- function (x, FUNcluster = my_hcut, method = c("silhouette", "wss"), k.max = 10, verbose = FALSE, 
                        barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", 
                        print.summary = TRUE, ...) 
{
  set.seed(123)
  if (k.max < 2) 
    stop("k.max must bet > = 2")
  method = match.arg(method)
  if (!inherits(x, c("dist")))
    stop("x should be an object of class dist")
  else if (is.null(FUNcluster)) 
    stop("The argument FUNcluster is required. ", "Possible values are kmeans, pam, hcut, clara, ...")
  else if (method %in% c("silhouette", "wss")) {
    diss <- x  # x IS ENFORCED TO BE A DISSIMILARITY MATRIX
    v <- rep(0, k.max)
    if (method == "silhouette") {
      for (i in 2:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- factoextra:::.get_ave_sil_width(diss, clust$cluster)
      }
    }
    else if (method == "wss") {
      for (i in 1:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- factoextra:::.get_withinSS(diss, clust$cluster)
      }
    }
    df <- data.frame(clusters = as.factor(1:k.max), y = v)
    ylab <- "Total Within Sum of Square"
    if (method == "silhouette") 
      ylab <- "Average silhouette width"
    p <- ggpubr::ggline(df, x = "clusters", y = "y", group = 1, 
                        color = linecolor, ylab = ylab, xlab = "Number of clusters k", 
                        main = "Optimal number of clusters")
    if (method == "silhouette") 
      p <- p + geom_vline(xintercept = which.max(v), linetype = 2, 
                          color = linecolor)
    return(p)
  }
}

# --------------------------------------------------------------------------
# Choose number of clusters
plot_silhouette <- my_nbclust(x = mat_dist, FUNcluster = my_hcut,  method = "silhouette", verbose = TRUE, k.max = in_kmax) +
  labs(subtitle = "Average silhouette width, to maximize")
plot_wss <- my_nbclust(x = mat_dist, FUNcluster = my_hcut,  method = "wss", verbose = TRUE, k.max = in_kmax) +
  labs(subtitle = "Total within clusters sum of square, look for elbow")

# ---------------------------------------------------------------------------
# Inspection of a cut
current_cut <- my_hcut(x = mat_dist, k = in_nclust, hc_func = "hclust", hc_method = in_linkage, hc_metric = in_distance)
plot_tree <- fviz_dend(x = current_cut, k = in_nclust)
plot_silhouette2 <- fviz_silhouette(current_cut, print.summary = FALSE) +
  labs(subtitle = "Most values should be positive")

# ---------------------------------------------------------------------------
# List of figures to output
out <- list(silhouette_all = plot_silhouette, wss_all = plot_wss, tree_cut = plot_tree, silhouette_cut = plot_silhouette2)

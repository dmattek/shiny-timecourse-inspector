require(data.table)
require(factoextra)
dt = fread('~/Dropbox/Projects/shiny-server/tcourse-inspector/example-data/test-case-1/mp3-20_FGF_ekar.csv.gz')
dt[, iduni := sprintf("%02d_%03d", fov, id)]

dt = fread('~/Downloads/tCoursesSelected_clean.csv')
setnames(dt, c("intensity_ekar", "iduni", "group", "realtime"))


# check for explicit NAs in th emeasurement column
sum(is.na(dt$intensity_ekar))

# convert to wide; ready for diatnce, clustering, etc
dm = dcast(dt, iduni ~ realtime, value.var = "intensity_ekar")
myrownames = dm[["iduni"]]
dm = as.matrix(dm[, -1])
rownames(dm) <- myrownames

# check for NA's; if 1st check gave 0, these NA's result from "stretching"
sum(is.na(dm))

# calc distance
mydistF = factoextra::get_dist(dm, method = "manhattan")
mydistP = proxy::dist(dm, method = "manhattan")
mydistS = stats::dist(dm, method = "manhattan")

# distance always calculated
sum(is.na(mydistP))

hc.cut <-factoextra::hcut(mydistP, k = 3, hc_method = "complete", graph = F)

# dend
p.out = fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
p.out


p.out = fviz_cluster(hc.cut, data = dm, ellipse.type = "convex")
p.out


library('microbenchmark')
library('dtw')

userDataGen <- function() {  
  cat(file=stderr(), 'userDataGen: in\n')
  
  locNtp = 100
  locNtracks = 2
  locNsites = 6
  locNwells = 1
  
  x.rand.1 = c(rnorm(locNtp * locNtracks * locNsites * 1/3, 0.5, 0.1), rnorm(locNtp * locNtracks * locNsites * 1/3,   1, 0.2), rnorm(locNtp * locNtracks * locNsites * 1/3,  2, 0.5))
  x.rand.2 = c(rnorm(locNtp * locNtracks * locNsites * 1/3, 0.25, 0.1), rnorm(locNtp * locNtracks * locNsites * 1/3, 0.5, 0.2),  rnorm(locNtp * locNtracks * locNsites * 1/3, 1, 0.2))
  
  # add NA's for testing
  x.rand.1[c(10,20,30)] = NA
  
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


dt.long = userDataGen()

dt.red = dt.long[, .(TrackLabel, Metadata_RealTime, Metadata_Site, objNuc_Intensity_MeanIntensity_imErkCor)]
setnames(dt.red, c('id', 'realtime', 'group', 'y'))
dt.wide = dcast(dt.red, id ~ realtime, value.var = 'y')

v.rownames = dt.wide$id

dm.t = as.matrix(dt.wide[, -1])
rownames(dm.t) = v.rownames

s.cl.diss = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "fastDTW")


mbm = microbenchmark('old' = {cl.dist.none = dist(dm.t, method ="DTW")},
                     'none' = {cl.dist.none = dtwDist(dm.t, window.type = 'none')},
                     'itakura' = {cl.dist.itak = dtwDist(dm.t, windowmb.type = 'itakura')}, 
                     'sakoechiba' = {cl.dist.itak = dtwDist(dm.t, windowmb.type = 'sakoechiba')}, 
                     'slantedband' = {cl.dist.itak = dtwDist(dm.t, windowmb.type = 'slantedband')}, times = 10)

autoplot(mbm)

# Fast DTW computation
fastDTW <-function (x)
{
  return(dtw(x, window.type = 'sakoechiba', distance.only = T)$normalizedDistance)
}


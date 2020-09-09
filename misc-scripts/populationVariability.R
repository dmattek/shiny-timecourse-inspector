Ns = 1E6
x1 = runif(Ns, min = 0, max = 1)
x2 = runif(Ns, min = 0, max = 1000)

x1.z = scale(x1, center = T, scale = T)
x2.z = scale(x2, center = T, scale = T)

plot(density(x1.z))
lines(density(x2.z))


# Data to obtain these differences (TS of length 2)

# 2 columns of differences
x1 = c(rep(1,5), rep(10,5))
x2 = 1:10
x3 = 101:110

sd(x1)/mean(x1)
sd(x2)/mean(x2)

marcAlg = function(x) {
  x.diff = abs(diff(x))
  #x.ma = filter(x, c(0.5, 0.5), sides = 2, method = 'conv')
  x.ma = x
  return(x.diff / x.ma[1:(length(x.ma)-1)])
  
  #return(x.diff)
}

myCV = function(x) {
  return(sd(x) / mean(x))
}

mean(marcAlg(x1))
mean(marcAlg(x2))
mean(marcAlg(x3))

myCV(x1)
myCV(x2)
myCV(x3)



mean(marcAlg(scale(x1)))
mean(marcAlg(scale(x2)))

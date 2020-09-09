m = matrix(c(1:10), ncol = 2)

m.ind = which(is.na(m), arr.ind = TRUE)

m.ind.low = matrix(c(m.ind[, 'row'] - 1, m.ind[, 'col']), ncol = 2, dimnames = dimnames(m.ind))
m.ind.high = matrix(c(m.ind[, 'row'] + 1, m.ind[, 'col']), ncol = 2, dimnames = dimnames(m.ind))

# NEXT: 
# 1. read elements of m matrix based on the entire m.ind matrix
# 2. same but from m.ind.low/high
# 3. linear interpolation with approx function

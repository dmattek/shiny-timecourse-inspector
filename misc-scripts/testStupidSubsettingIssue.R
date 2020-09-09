require(data.table)



# Works
setNumericRounding(2)
dt = data.table(id = rep(0, 10),
                    t = seq(0.1, 1, 0.1),
                    y = 1:10)
dt = dt[!(t == .5)]

setkey(dt, id, t)
dtAux = dt[, 
              .(seq(min(t), max(t), .1)),
              by = id]
setkey(dtAux, id, V1)

dt[dtAux]


# Doesn't work

fwrite(dt, "~/Desktop/test.csv", row.names = F)
dtFromFile = fread("~/Desktop/test.csv")

setkey(dtFromFile, id, t)

dtFromFile[dtAux]

dt$t - dtFromFile$t

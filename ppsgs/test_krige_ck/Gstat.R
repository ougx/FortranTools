library(gstat)

setwd("c:\\Cloud\\Dropbox\\FortranCode\\FortranTools\\ppsgs\\test_krige_ck")

obs1 = read.csv("../test_data/pc2d.csv")
obs2 = read.csv("../test_data/aem2d.csv")
grid = read.csv("../test_data/grid2d.csv")
#coordinates(obs) = ~x+y
#coordinates(grid) = ~x+y

m1 <- vgm(0.12 , "Sph", 5000, .0)
mc <- vgm(0.04 , "Sph", 5000, .05)
m2 <- vgm(0.068, "Sph", 5000, .0)

g <- gstat(NULL, "wellog", pc    ~1, locations=~x+y, data=obs1, model=m1, nmax=300, set = list(nocheck = 1))
g <- gstat(g   , "aem"   , logRho~1, locations=~x+y, data=obs2, model=m2, nmax=300, set = list(nocheck = 1))
g <- gstat(g, id = c("wellog", "aem"), model=mc, set = list(nocheck = 1))

x <- predict(g, grid)

write.csv(x, "py_ck.csv")

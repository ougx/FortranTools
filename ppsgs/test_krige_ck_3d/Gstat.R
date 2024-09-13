library(gstat)

setwd("c:\\Cloud\\Dropbox\\FortranCode\\FortranTools\\ppsgs\\test_krige_ck_3d")

obs1 = read.csv("../test_data/obs3d_clay.csv")
obs2 = read.csv("../test_data/aem3d_clay.csv")
grid = read.csv("../test_data/gridxyz.csv")
#coordinates(obs) = ~x+y
#coordinates(grid) = ~x+y

m1 <- vgm(0.12 , "Sph", 5000, .0 , anis = c(0,0,0,1,0.5))
mc <- vgm(0.04 , "Sph", 5000, .05, anis = c(0,0,0,1,0.5))
m2 <- vgm(0.068, "Sph", 5000, .0 , anis = c(0,0,0,1,0.5))

g <- gstat(NULL, "wellog", lith~1, locations=~x+y+z, data=obs1, model=m1, nmax=30, set = list(nocheck = 1))
g <- gstat(g   , "aem"   , pdf ~1, locations=~x+y+z, data=obs2, model=m2, nmax=30, set = list(nocheck = 1))
g <- gstat(g, id = c("wellog", "aem"), model=mc, set = list(nocheck = 1))

x <- predict(g, grid)

write.csv(x, "py_ck.csv")


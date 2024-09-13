library(gstat)

setwd("c:\\Cloud\\Dropbox\\FortranCode\\FortranTools\\ppsgs\\test_krige_ok_3d")

obs1 = read.csv("../test_data/obs3d_clay.csv")
grid = read.csv("../test_data/gridxyz.csv")
#coordinates(obs) = ~x+y
#coordinates(grid) = ~x+y

m1 <- vgm(0.12 , "Sph", 5000, .0)

g <- gstat(NULL, "wellog", lith~1, locations=~x+y+z, data=obs1, model=m1, nmax=30, set = list(nocheck = 1))
x <- predict(g, grid)

write.csv(x, "py_ok.csv")


m1 <- vgm(0.12 , "Sph", 5000, .0 , anis = c(0,0,0,1,0.5))

g <- gstat(NULL, "wellog", lith~1, locations=~x+y+z, data=obs1, model=m1, nmax=30, set = list(nocheck = 1))
x <- predict(g, grid)

write.csv(x, "py_ok_ani.csv")
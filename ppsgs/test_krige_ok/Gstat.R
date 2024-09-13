library(gstat)

setwd("c:\\Cloud\\Dropbox\\FortranCode\\FortranTools\\ppsgs\\test_krige_ok")

obs1 = read.csv("../test_data/pc2d.csv")
grid = read.csv("../test_data/grid2d.csv")
#coordinates(obs) = ~x+y
#coordinates(grid) = ~x+y


m1 <- vgm(0.12 , "Sph", 5000, .0)
g <- gstat(NULL, "wellog", pc    ~1, locations=~x+y, data=obs1, model=m1, nmax=300, set = list(nocheck = 1))
x <- predict(g, grid)
write.csv(x, "py_ok.csv")



m1 <- vgm(0.12 , "Sph", 5000, .0 , anis = c(45,0.5))

g <- gstat(NULL, "wellog", pc    ~1, locations=~x+y, data=obs1, model=m1, nmax=300, set = list(nocheck = 1))

x <- predict(g, grid)

write.csv(x, "py_ok_rotated.csv")

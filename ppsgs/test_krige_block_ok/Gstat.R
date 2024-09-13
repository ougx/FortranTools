library(gstat)
library(sp)
setwd("c:\\Cloud\\Dropbox\\FortranCode\\FortranTools\\ppsgs\\test_krige_block_ok")

obs = read.csv("../test_data/pc2d.csv")
grid = read.csv("../test_data/gridblock2d.csv")
#coordinates(obs) = ~x+y
#coordinates(grid) = ~x+y

m <- vgm(0.12, "Sph", 5000, .0)
x <- krige(pc~1, locations=~x+y, obs, grid, mode = m)

write.csv(x, "py_ok.csv")


##### block kriging
x <- krige(pc~1, locations=~x+y, obs, grid, mode = m, block = c(2000,2000), debug.level = 544)
write.csv(x, "py_ok_block.csv")

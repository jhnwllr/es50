# general polygon plottin function 


# mergeToGrid

library(dplyr)
library(dggridR)
library(roperators)
library(purrr)

taxonKey = "1"
spacing = 300 # area of hexagons to generate 

# grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
# D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

# grid = gbifrasters::mergeToGrid(grid,D) # get all grid data
# saveRDS(grid,file="C:/Users/ftw712/Desktop/gridMerge.rda")
grid = readRDS(file="C:/Users/ftw712/Desktop/gridMerge.rda")

# variable = "es50"



pdf("C:/Users/ftw712/Desktop/polymapTest.pdf",width=9,height=5)

gbifrasters::plotPolyMap(grid,variable="es50") + 
theme(plot.margin=margin(0.7, 0.7, 0, 0, "cm")) + 
coord_map("ortho",orientation=c(35, 13, 5),xlim=NULL, ylim=NULL) 


dev.off()

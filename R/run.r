if(FALSE) { # run to get data. 

library(dplyr)
library(dggridR)
library(roperators)
library(purrr)

taxonKey = "216"
spacing = 300 # area of hexagons to generate 

# grid = gbifrasters::getAndSaveGrid(spacing)
grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
D = gbifrasters::getAndSavePolyRaster(grid,spacing,taxonKey,facet="genusKey") 

glimpse(D)

# D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+% ".rda")

gbifrasters::mergeToGrid(grid,D) %>% 
saveRDS(file="C:/Users/ftw712/Desktop/es50/data/grid_" %+% spacing %+% "_" %+% taxonKey %+% ".rda")
}

library(roperators)
library(dplyr)
library(dggridR)
library(magrittr) # for %T>% pipe

taxonKey = "789"
spacing = 300 # space between hexagon grid
facet = "speciesKey"

grid = dggridR::dgconstruct(spacing=spacing, metric=FALSE, resround='down') %>%
gbifrasters::getPolygonGrid(spacing,landOnly=FALSE) %>% # get from entire globe
gbifrasters::getData(taxonKey,facet=facet) %T>%  # get data from GBIF using facet api
saveRDS(file="C:/Users/ftw712/Desktop/grid.rda") # save data because it takes a while usually...

# grid = readRDS(file="C:/Users/ftw712/Desktop/gridTest.rda")

p = gbifrasters::plotPolyMap(grid,
variable="es50",
legend.position = c(.50,-0.06),
breaks=NULL,
labels=NULL,
polygon_text_size=3,
polygon_alpha = 1,
labelType="",
keywidth=0.01,
keyheight=0.3,
legend_text_size=15
)


ggsave("C:/Users/ftw712/Desktop/plot.pdf",plot=p,device="pdf",scale=1,width=9,height=5)

# glimpse(grid)
	
# gridData = grid %>% select(cell,geometry) %>% unique()
# polyraster = gbifrasters::getData(taxonKey,gridData,facet)
  	

getAndSavePolyRaster = function(grid,spacing,taxonKey,facet="speciesKey") {

  gridData = grid %>% select(cell,geometry) %>% unique()

  polyraster = gbifrasters::getData(taxonKey,gridData,facet)

  polyraster %>% saveRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

  return(polyraster)
}



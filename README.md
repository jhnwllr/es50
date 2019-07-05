# GBIF es50

Work on es50 maps for GBIF. 

# Scripts below for making simple hexagon es50 maps for any taxonKey using the api

installation 

```

```

# making an es50 map 

```
library(roperators)
library(dplyr)
library(dggridR)
library(magrittr) # for %T>% pipe

taxonKey = "789"
spacing = 300 # space between hexagon grid
facet = "speciesKey" # which rank to use... # can also be genusKey

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


```

# Making es50 plot

getting data 



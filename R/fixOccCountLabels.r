
# fix occ count labels 

library(dggridR)
library(roperators)
library(purrr)
library(dplyr)

taxonKey = "1"
spacing = 300 # area of hexagons to generate 

grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

cellOccCounts = gbifrasters::getOccCountsByCell(D)

grid = merge(grid,cellOccCounts,id="cell",all.x=TRUE) %>%
	arrange(cell, order) %>% 
	na.omit() %>%
	glimpse() %>%
	mutate(plotLabel = case_when(
	occCounts >= 1e6 ~ round(occCounts/1e6) %+% "M",
	occCounts >= 1e3 ~ round(occCounts/1e3) %+% "K",
	TRUE ~ as.character(occCounts)
	))	 

grid %>% glimpse()
	
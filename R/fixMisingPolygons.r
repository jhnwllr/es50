library(dplyr)
library(dggridR)
library(roperators)
library(purrr)


x = 100000000

D = tibble(x)

tibble(format(D$x[1],scientific = FALSE) %+% "%20" %+% "dog")

# spacing=300

# dggs = dggridR::dgconstruct(spacing=spacing, metric=FALSE, resround='down')

# D = gbifrasters::getGridData(1) %>% # no really small polygons
# CoordinateCleaner::cc_sea(lon="longitudeMid",lat="latitudeMid",value = "clean") %>%
# mutate(cell = dggridR::dgGEO_to_SEQNUM(dggs,longitudeMid,latitudeMid)$seqnum)

# grid = dggridR::dgcellstogrid(dggs,D$cell,frame=TRUE,wrapcells=TRUE)

# saveRDS(grid,file="C:/Users/ftw712/Desktop/grid.rda")

# grid = readRDS(file="C:/Users/ftw712/Desktop/grid.rda")

# grid %>%
# group_split(.$cell) %>% 
# map_dbl(~ .x %>% 
# select(long,lat) %>% 
# unique() %>% 
# nrow() 
# ) %>% unique()




# map_dbl(~ nrow(unique(.x)))
	
# cellList	
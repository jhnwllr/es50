# experiment with exactly polygon from ddggr grid object 

library(dggridR)
library(purrr)
library(roperators)

#Construct a global grid with cells approximately 1000 miles across

getPolygonGrid = function(dggs,spacing=300) {

  D = gbifrasters::getGridData(1) %>% # no really small polygons
    CoordinateCleaner::cc_sea(lon="longitudeMid",lat="latitudeMid",value = "clean") %>%
	mutate(cell = dggridR::dgGEO_to_SEQNUM(dggs,longitudeMid,latitudeMid)$seqnum)
	
  grid = dggridR::dgcellstogrid(dggs,D$cell,frame=TRUE,wrapcells=TRUE)

  cellList = grid %>%
    split(.$cell)

  # print(cellList)
  cell = cellList %>% map_chr(~ .x$cell %>% unique() %>% unname()) %>% as.numeric()

  # this could probably be better 
  geometry = cellList %>%
    modify_if(~ nrow(.x) == 6, ~
                "geometry=POLYGON((" %+%
                .x$long[1] %+% "%20" %+% .x$lat[1] %+% "," %+%
                .x$long[2] %+% "%20" %+% .x$lat[2] %+% "," %+%
                .x$long[3] %+% "%20" %+% .x$lat[3] %+% "," %+%
                .x$long[4] %+% "%20" %+% .x$lat[4] %+% "," %+%
                .x$long[5] %+% "%20" %+% .x$lat[5] %+% "," %+%
                .x$long[6] %+% "%20" %+% .x$lat[6] %+% "," %+%
                .x$long[1] %+% "%20" %+% .x$lat[1] %+% "))&"
    ) %>%
    modify_if(~ class(.x) == "data.frame", ~
                "geometry=POLYGON((" %+%
                .x$long[1] %+% "%20" %+% .x$lat[1] %+% "," %+%
                .x$long[2] %+% "%20" %+% .x$lat[2] %+% "," %+%
                .x$long[3] %+% "%20" %+% .x$lat[3] %+% "," %+%
                .x$long[4] %+% "%20" %+% .x$lat[4] %+% "," %+%
                .x$long[5] %+% "%20" %+% .x$lat[5] %+% "," %+%
                .x$long[6] %+% "%20" %+% .x$lat[6] %+% "," %+%
                .x$long[7] %+% "%20" %+% .x$lat[7] %+% "))&"
    ) %>%
    flatten_chr()

  D = tibble(cell,geometry)
  D = merge(grid,D,id="cell",all.x=TRUE) # merge with grid
	
  return(D)
}

spacing = 300

dggs = dggridR::dgconstruct(spacing=spacing, metric=FALSE, resround='down')


getPolygonGrid(dggs,spacing=300)


# D = gbifrasters::getGridData(1)

# D %>% glimpse()

# CoordinateCleaner::cc_sea(D,lon="longitudeMid",lat="latitudeMid",value = "clean") %>% glimpse()




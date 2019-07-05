
if(FALSE) { # different polygon experiemnts 
# 'POLYGON((0 0, %s %s, %s %s, %s %s, %s %s, %s %s, 0 0))',
# (_width *  0.5), 
# (_height * 0.25),
# (_width *  0.5), 
# (_height * 0.75),
# 0 ,  
# _height,
# (_width * -0.5), 
# (_height * 0.75),
# (_width * -0.5), 
# (_height * 0.25)
# )


library(hexbin)

x <- rnorm(10000)
y <- rnorm(10000)
bin <- hexbin::hexbin(x, y)

polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell,x + unitcell/2), c(y + unitcell * 0.125,
                               y + unitcell * 0.875,
                               y + unitcell * 1.125,
                               y + unitcell * 0.875,
                               y + unitcell * 0.125,
                               y - unitcell * 0.125),col = col, border=NA) 
# hcell2xyInt(bin)

# hgridcent(xbins = 10, xbnds=c(0,10), shape=10, ybnds=c(0,10), edge.add = 0)


# unclass(bin)

# POLYGON((10.89844%2034.88593,15.82031%2019.31114,31.99219%2011.1784,41.13281%2023.88584,40.42969%2035.46067,25.66406%2047.5172,10.89844%2034.88593))
}


if(FALSE) {
#Include libraries
library(dggridR)
library(rgdal)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(roperators)

#Construct a global grid with cells approximately 1000 miles across
dggs = dgconstruct(spacing=1000, metric=FALSE, resround='down')



getGridData = function(stepSize) { 

latitudeBreaks = seq(-90,(90-stepSize),stepSize)
latitudeBreaksOffset = seq(-1*(90-stepSize),90,stepSize)

longitudeBreaks = seq(-180,(180-stepSize),stepSize)
longitudeBreaksOffset = seq(-1*(180-stepSize),180,stepSize)

gridGeometries = longitudeBreaks %>% 
map2(
longitudeBreaksOffset, ~ 
"geometry=POLYGON((" %+% 
.x %+% "%20" %+% latitudeBreaks %+%
"," %+%
.y %+% "%20" %+% latitudeBreaks %+%
"," %+%
.y %+% "%20" %+% latitudeBreaksOffset %+%
"," %+%
.x %+% "%20" %+% latitudeBreaksOffset %+%
"," %+%
.x %+% "%20" %+% latitudeBreaks%+%
"))&"
) 

longitudeMid = rep(longitudeBreaks, each = length(latitudeBreaks)) + (stepSize/2)
latitudeMid = rep(latitudeBreaks,length(longitudeBreaks)) + (stepSize/2)

# print(latitudeMid)

gridData = gridGeometries %>% 
map(~ 
enframe(.x) %>%
select(geometry = value)
) %>%
plyr::rbind.fill() %>% 
add_column(latitudeMid) %>% 
add_column(longitudeMid) 

return(gridData)
}

stepSize = 5
gridData = getGridData(stepSize)

#Load included test data set
data(dgquakes)

# str(dgquakes)

#Get the corresponding grid cells for each earthquake epicenter (lat-long pair)
dgquakes$cell = dgGEO_to_SEQNUM(dggs,dgquakes$lon,dgquakes$lat)$seqnum

#Converting SEQNUM to GEO gives the center coordinates of the cells
cellcenters = dgSEQNUM_to_GEO(dggs,dgquakes$cell)

#Get the number of earthquakes in each cell
quakecounts = dgquakes %>% group_by(cell) %>% summarise(count=n())

# quakecounts

#Get the grid cell boundaries for cells which had quakes
grid = dgcellstogrid(dggs,quakecounts$cell,frame=TRUE,wrapcells=TRUE)

# dgGEO_to_PLANE(grid)

#Update the grid cells' properties to include the number of earthquakes
#in each cell
# grid          <- merge(grid,quakecounts,by.x="cell",by.y="cell")

#Make adjustments so the output is more visually interesting
# grid$count <- log(grid$count)
# cutoff <- quantile(grid$count,0.9)
# grid  <- grid %>% mutate(count=ifelse(count>cutoff,cutoff,count))


# str(cellcenters)

#Get polygons for each country of the world
countries = map_data("world")

pdf("C:/Users/ftw712/Desktop/plot2.pdf")

#Plot everything on a flat map
p = ggplot() + 
    geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
    scale_fill_gradient(low="blue", high="red") + 
    geom_point(aes(x=cellcenters$lon_deg, y=cellcenters$lat_deg)) 
	# +
    # geom_point(data=gridData,aes(x=longitudeMid, y=latitudeMid)) 
p
    # geom_polygon(data=grid,      aes(x=long, y=lat, group=group, fill=count), alpha=0.4)    +
    # geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black")   +

dev.off()
}


if(FALSE) { # 1st attempt with dggridR with graphs
#Include libraries
library(dggridR)
library(rgdal)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(roperators)

#Construct a global grid with cells approximately 1000 miles across
dggs = dgconstruct(spacing=300, metric=FALSE, resround='down')

# start dggridR from scratch 

getGridData = function(stepSize) { 

latitudeBreaks = seq(-90,(90-stepSize),stepSize)
latitudeBreaksOffset = seq(-1*(90-stepSize),90,stepSize)

longitudeBreaks = seq(-180,(180-stepSize),stepSize)
longitudeBreaksOffset = seq(-1*(180-stepSize),180,stepSize)

gridGeometries = longitudeBreaks %>% 
map2(
longitudeBreaksOffset, ~ 
"geometry=POLYGON((" %+% 
.x %+% "%20" %+% latitudeBreaks %+%
"," %+%
.y %+% "%20" %+% latitudeBreaks %+%
"," %+%
.y %+% "%20" %+% latitudeBreaksOffset %+%
"," %+%
.x %+% "%20" %+% latitudeBreaksOffset %+%
"," %+%
.x %+% "%20" %+% latitudeBreaks%+%
"))&"
) 

longitudeMid = rep(longitudeBreaks, each = length(latitudeBreaks)) + (stepSize/2)
latitudeMid = rep(latitudeBreaks,length(longitudeBreaks)) + (stepSize/2)

# print(latitudeMid)

gridData = gridGeometries %>% 
map(~ 
enframe(.x) %>%
select(geometry = value)
) %>%
plyr::rbind.fill() %>% 
add_column(latitudeMid) %>% 
add_column(longitudeMid) 

return(gridData)
}

getSpeciesOccCounts = function(url,Step=1000,maxPages=200) {

L = gbifapi::page_api_facet(url,pluck="facets",Step=Step,maxPages=maxPages,verbose=TRUE) %>%
map(~ .x$counts) %>%
flatten() 

taxonkey = L %>%
map(~ .x$name) %>%
flatten_chr()

count = L %>%
map(~ .x$count) %>%
flatten_dbl()

spOcc = tibble(taxonkey,count)

return(spOcc)
}

getData = function(stepSize,taxonKey,gridData) {

geometries = gridData %>% pull(geometry)

# taxonKey = "212"

api = "http://api.gbif.org/v1/occurrence/search?limit=0&"
queryParams = "has_coordinate=true" %+% "&" %+% "has_geospatial_issue=false&"
# geometry = "geometry=POLYGON((-180%20-90,180%20-90,180%20-80,-180%20-80,-180%20-90))&"
facet = "facet=speciesKey&taxonKey=" %+% taxonKey

apiCalls = geometries %>% map(~ api %+% queryParams %+% .x %+% facet)

# print(apiCalls)
DL = apiCalls %>% map(~ getSpeciesOccCounts(.x,Step=100,maxPages=200))

return(DL)
}

taxonKey = "212"
stepSize = 2
# gridData = getGridData(stepSize)
# getData(stepSize,taxonKey,gridData) %>% saveRDS(file="C:/Users/ftw712/Desktop/es50/data/grid_" %+% stepSize %+% "_" %+% taxonKey %+%".rda")

DL = readRDS(file="C:/Users/ftw712/Desktop/es50/data/grid_" %+% stepSize %+% "_" %+% taxonKey %+%".rda")

latitudeMid = gridData %>% pull(latitudeMid) # 
longitudeMid = gridData %>% pull(longitudeMid) # 

D = DL %>% 
map2(latitudeMid,~ .x %>% mutate(latitudeMid = .y)) %>%
map2(longitudeMid,~ .x %>% mutate(longitudeMid = .y)) %>% 
plyr::rbind.fill() %>%
as_tibble() %>% 
mutate(cell = dgGEO_to_SEQNUM(dggs,longitudeMid,latitudeMid)$seqnum)

grid = dgcellstogrid(dggs,D$cell,frame=TRUE,wrapcells=TRUE)

centerList = dgSEQNUM_to_GEO(dggs,as.numeric(grid$cell))
cellCenters = tibble(cell=grid$cell,lonCenter = pluck(centerList,"lon_deg"), latCenter =  pluck(centerList,"lat_deg"))

# compute es50 for each cell 
esNum = 50

es50 = D %>%
group_split(cell) %>%
map(~ .x %>%
group_by(taxonkey) %>% 
summarize(occCount = sum(count))
) %>%
map(~ deframe(.x)) %>%
modify_if(~ length(.x) <= esNum, ~ NA) %>% # run only if more than 50 species
modify_if(~ !anyNA(.x), ~ entropart::Hurlbert(.x, esNum)) %>%
map(~ unname(.x)) %>%
flatten_dbl() 

cell = D %>%
group_split(cell) %>% 
map(~ .x %>% mutate(cell = as.character(cell))) %>% 
map_chr(~ unique(.x$cell))

es50Table = tibble(cell,es50)

cellSpCounts = D %>% 
group_by(cell,taxonkey) %>% 
summarise(count=n()) %>%
group_by(cell) %>% 
summarise(spCount=n()) %>%
mutate(cell = as.character(cell))

cellOccCounts = D %>% 
group_by(cell) %>% 
summarise(occCounts = sum(count))

grid = merge(grid,cellSpCounts,id="cell",all.x=TRUE)
grid = merge(grid,es50Table,id="cell",all.x=TRUE)
grid = merge(grid,cellOccCounts,id="cell",all.x=TRUE)
grid = merge(grid,cellCenters,id="cell",all.x=TRUE)

grid %>% glimpse()

# grid = grid %>% na.omit()

biasTable = grid %>% 
select(latCenter,occCounts) %>%
mutate(latCenter = plyr::round_any(latCenter,5)) %>%
group_by(latCenter) %>% 
summarise(occCounts = sum(occCounts))

# %>% filter(occCounts > 

# grid %>% str()
# grid %>% group_by(cell) 
# grid = grid %>% 
# case_when(grid$spCount %% 35 == 0 ~ NA_character_)
# mutate(spCount = if_else(is.na(es50),NA,spCount))

countries = map_data("world")

library(ggplot2)
library(ggthemes)

pdf("C:/Users/ftw712/Desktop/plot3.pdf",width=10,height=5)
# svg("C:/Users/ftw712/Desktop/plot3.svg",width=10,height=5)

p = ggplot(biasTable, aes(latCenter,occCounts)) + 
	geom_bar(width=4,stat="identity") + 
	theme_hc()
	# +
	# geom_smooth(span = 0.1)
	# geom_density(aes(y = ..count..), alpha=0.25)
	# stat_density(aes(group = ind, color = ind),position="identity",geom="line")
	
	# geom_density()
	 

p

# p = ggplot() + 
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.9), fill="gray", color="gray",alpha=0.8) + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(occCounts,c(0,5000,1e6,50e6)),alpha=0.9)) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# guides(fill=guide_legend(title="occCounts")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	# p 

# p = ggplot() + 
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(es50,breaks=c(0,30,40,45,50)),alpha=0.8)) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# guides(fill=guide_legend(title="es50")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	# p 

# p = ggplot() + 
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(spCount,breaks=c(0,400,900,1300,1600)),alpha=0.8)) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# guides(fill=guide_legend(title="spCount")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	# p 

# p = ggplot() + 
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(spCount,5),alpha=0.8)) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# guides(fill=guide_legend(title="spCount")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	# p 
	

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, 
	# fill=cut(spCount, 5))) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") + 
	# guides(fill=guide_legend(title="sp count"))

	# p 


# # try different bins

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(es50, scales::pretty_breaks(4)(0:50)))) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") + 
	# guides(fill=guide_legend(title="es50"))

	# p 

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, 
	# fill=cut(spCount, scales::pretty_breaks(4)(1:max(spCount,na.rm=TRUE))))) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") + 
	# guides(fill=guide_legend(title="sp count"))

	# p 
		

# #Plot everything on a flat map
# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=es50), alpha=1) +
    # geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") +
    # scale_fill_gradient(low="blue", high="red")
    # # aes(fill=cut(stat(count), scales::pretty_breaks(4)(0:maxCountHurlbert))))
	
	# # geom_point(data=gridData,aes(x=longitudeMid, y=latitudeMid)) +
	# # +
	# # + 
    # # geom_point(aes(x=cellcenters$lon_deg, y=cellcenters$lat_deg)) 
	# p
    # # geom_polygon(data=grid,      aes(x=long, y=lat, group=group, fill=count), alpha=0.4)    +

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=spCount), alpha=1) +
    # geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") +
    # scale_fill_gradient(low="blue", high="red")
    # # geom_point(data=gridData,aes(x=longitudeMid, y=latitudeMid)) +
	# # +
	# # + 
    # # geom_point(aes(x=cellcenters$lon_deg, y=cellcenters$lat_deg)) 
	# p
    # # geom_polygon(data=grid,      aes(x=long, y=lat, group=group, fill=count), alpha=0.4)    +
	
	
dev.off()

}

# 2nd attempt with dggridR query polygon directly 

library(dplyr)
library(dggridR)
library(roperators)
library(purrr)

taxonKey = "952"
spacing = 300 # area of hexagons to generate 

# getAndSaveGrid = function(spacing) {

dggs = dggridR::dgconstruct(spacing=spacing, metric=FALSE, resround='down') 

gbifrasters::getPolygonGrid(dggs,spacing) %>% 
saveRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")

# }

gridData = grid %>% select(cell,geometry) %>% unique()
gbifrasters::getData(taxonKey,gridData) %>% 
saveRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")



grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

es50LatTable = gbifrasters::getEs50LatTable(D,grid)


library(ggplot2)
library(ggthemes)

pdf("C:/Users/ftw712/Desktop/plot3.pdf",width=10,height=5)

gbifrasters::plotWireFrame(grid)
gbifrasters::plotSpCounts(D,grid) 
gbifrasters::plotOccCounts(D,grid)
gbifrasters::plotEs50(D,grid)

dev.off()


es50LatTable

# grid %>% glimpse()

# biasTable = grid %>% 
# select(latCenter,occCounts) %>%
# mutate(latCenter = plyr::round_any(latCenter,5)) %>%
# group_by(latCenter) %>% 
# summarise(occCounts = sum(occCounts))

# speciesCountTable = grid %>% 
# select(latCenter,spCount) %>% 
# mutate(latCenter = plyr::round_any(latCenter,5)) %>%
# group_by(latCenter) %>%
# summarise(spCount = max(spCount))

# es50LatTable = grid %>% 
# select(latCenter,es50) %>% 
# group_by(latCenter) %>%
# summarise(es50Mean = mean(es50,na.rm=TRUE)) %>% 
# na.omit()




# dev.off()

# countries = map_data("world")

# library(ggplot2)
# library(ggthemes)

# pdf("C:/Users/ftw712/Desktop/wireFrame.pdf",width=10,height=5)

# p = ggplot() + 
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
	# geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	# guides(fill=guide_legend(title="spCount"))
	
	# p 

# dev.off()	



if(FALSE) {
# gridData %>% glimpse()
# gridData = grid %>% select(cell,geometry) %>% unique()
# getData(taxonKey,gridData) %>% saveRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

es50Table = gbifrasters::getEs50Table(D) 

es50Table

cellSpCounts = D %>% 
group_by(cell) %>% 
summarise(spCount=n()) %>%
mutate(cell = as.character(cell))

cellOccCounts = D %>% 
group_by(cell) %>% 
summarise(occCounts = sum(count))

# grid = merge(grid,cellSpCounts,id="cell",all.x=TRUE)
# grid = merge(grid,es50Table,id="cell",all.x=TRUE)
# grid = merge(grid,cellOccCounts,id="cell",all.x=TRUE)

grid %>% glimpse()

biasTable = grid %>% 
select(latCenter,occCounts) %>%
mutate(latCenter = plyr::round_any(latCenter,5)) %>%
group_by(latCenter) %>% 
summarise(occCounts = sum(occCounts))

speciesCountTable = grid %>% 
select(latCenter,spCount) %>% 
mutate(latCenter = plyr::round_any(latCenter,5)) %>%
group_by(latCenter) %>%
summarise(spCount = max(spCount))

es50LatTable = grid %>% 
select(latCenter,es50) %>% 
group_by(latCenter) %>%
summarise(es50Mean = mean(es50,na.rm=TRUE)) %>% 
na.omit()

countries = map_data("world")

library(ggplot2)
library(ggthemes)

pdf("C:/Users/ftw712/Desktop/plot3.pdf",width=10,height=5)
# svg("C:/Users/ftw712/Desktop/plot3.svg",width=10,height=5)

p = ggplot(speciesCountTable, aes(latCenter,spCount)) + 
	geom_bar(width=4,stat="identity") + 
	theme_hc()
p

p = ggplot(es50LatTable, aes(latCenter,es50Mean)) + 
	geom_point() + 
	geom_smooth(span = 1) + 
	theme_hc()
p

p = ggplot(biasTable, aes(latCenter,occCounts)) + 
	geom_bar(width=4,stat="identity") + 
	theme_hc()
p

p = ggplot() + 
	coord_cartesian(c(-150,170),c(-55,80)) +
	theme_void() +
	geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.9), fill="gray", color="gray",alpha=0.8) + 
    geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(occCounts, scales::pretty_breaks(6)(0:max(occCounts,na.rm=TRUE))),alpha=0.9)) +
    scale_fill_brewer(palette = "Spectral",direction=-1) +
	guides(fill=guide_legend(title="occCounts")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	p 

p = ggplot() + 
	coord_cartesian(c(-150,170),c(-55,80)) +
	theme_void() +
	geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(es50,breaks=c(0,10,20,30,40,50)),alpha=0.8)) +
    scale_fill_brewer(palette = "Spectral",direction=-1) +
	guides(fill=guide_legend(title="es50")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	p 

p = ggplot() + 
	coord_cartesian(c(-150,170),c(-55,80)) +
	theme_void() +
	geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(log(spCount), scales::pretty_breaks(4)(1:max(log(spCount),na.rm=TRUE))),alpha=0.8)) +
    scale_fill_brewer(palette = "Spectral",direction=-1) +
	guides(fill=guide_legend(title="spCount")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	
	p = ggplot() + 
	coord_cartesian(c(-150,170),c(-55,80)) +
	theme_void() +
	geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    geom_path(data=raw_grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	guides(fill=guide_legend(title="spCount"))
	
	p 

	
	# 
	
# p = ggplot() + 
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(spCount,5),alpha=0.8)) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# guides(fill=guide_legend(title="spCount")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	# p 
	

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, 
	# fill=cut(spCount, 5))) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") + 
	# guides(fill=guide_legend(title="sp count"))

	# p 


# # try different bins

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(es50, scales::pretty_breaks(4)(0:50)))) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") + 
	# guides(fill=guide_legend(title="es50"))

	# p 

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, 
	# fill=cut(spCount, scales::pretty_breaks(4)(1:max(spCount,na.rm=TRUE))))) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") + 
	# guides(fill=guide_legend(title="sp count"))

	# p 
		

# #Plot everything on a flat map
# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=es50), alpha=1) +
    # geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") +
    # scale_fill_gradient(low="blue", high="red")
    # # aes(fill=cut(stat(count), scales::pretty_breaks(4)(0:maxCountHurlbert))))
	
	# # geom_point(data=gridData,aes(x=longitudeMid, y=latitudeMid)) +
	# # +
	# # + 
    # # geom_point(aes(x=cellcenters$lon_deg, y=cellcenters$lat_deg)) 
	# p
    # # geom_polygon(data=grid,      aes(x=long, y=lat, group=group, fill=count), alpha=0.4)    +

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=spCount), alpha=1) +
    # geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") +
    # scale_fill_gradient(low="blue", high="red")
    # # geom_point(data=gridData,aes(x=longitudeMid, y=latitudeMid)) +
	# # +
	# # + 
    # # geom_point(aes(x=cellcenters$lon_deg, y=cellcenters$lat_deg)) 
	# p
    # # geom_polygon(data=grid,      aes(x=long, y=lat, group=group, fill=count), alpha=0.4)    +
	
	
dev.off()


if(FALSE) {
DL = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

cell = gridData %>% pull(cell)

# cell
D = DL %>% 
map2(cell,~ .x %>% mutate(cell = .y)) %>% # add the cell bank into to the downloaded data 
plyr::rbind.fill() %>%
as_tibble() 

# the grid table for plotting
grid = dggridR::dgcellstogrid(dggs,D$cell,frame=TRUE,wrapcells=TRUE)

raw_grid = grid # save for inspection

centerList = dgSEQNUM_to_GEO(dggs,as.numeric(grid$cell))
cellCenters = tibble(cell=grid$cell,lonCenter = pluck(centerList,"lon_deg"), latCenter =  pluck(centerList,"lat_deg"))

# get the es50 statistic 
es50Table = gbifrasters::getEs50Table(D) 

cellSpCounts = D %>% 
group_by(cell) %>% 
summarise(spCount=n()) %>%
mutate(cell = as.character(cell))

cellOccCounts = D %>% 
group_by(cell) %>% 
summarise(occCounts = sum(count))

grid = merge(grid,cellSpCounts,id="cell",all.x=TRUE)
grid = merge(grid,es50Table,id="cell",all.x=TRUE)
grid = merge(grid,cellOccCounts,id="cell",all.x=TRUE)
grid = merge(grid,cellCenters,id="cell",all.x=TRUE)

grid %>% glimpse()

# Lat Gradients 

biasTable = grid %>% 
select(latCenter,occCounts) %>%
mutate(latCenter = plyr::round_any(latCenter,5)) %>%
group_by(latCenter) %>% 
summarise(occCounts = sum(occCounts))

speciesCountTable = grid %>% 
select(latCenter,spCount) %>% 
mutate(latCenter = plyr::round_any(latCenter,5)) %>%
group_by(latCenter) %>%
summarise(spCount = max(spCount))

es50LatTable = grid %>% 
select(latCenter,es50) %>% 
group_by(latCenter) %>%
summarise(es50Mean = mean(es50,na.rm=TRUE)) %>% 
na.omit()

countries = map_data("world")

library(ggplot2)
library(ggthemes)

pdf("C:/Users/ftw712/Desktop/plot3.pdf",width=10,height=5)
# svg("C:/Users/ftw712/Desktop/plot3.svg",width=10,height=5)

p = ggplot(speciesCountTable, aes(latCenter,spCount)) + 
	geom_bar(width=4,stat="identity") + 
	theme_hc()
p

p = ggplot(es50LatTable, aes(latCenter,es50Mean)) + 
	geom_point() + 
	geom_smooth(span = 1) + 
	theme_hc()
p

p = ggplot(biasTable, aes(latCenter,occCounts)) + 
	geom_bar(width=4,stat="identity") + 
	theme_hc()
p

p = ggplot() + 
	coord_cartesian(c(-150,170),c(-55,80)) +
	theme_void() +
	geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.9), fill="gray", color="gray",alpha=0.8) + 
    geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(occCounts, scales::pretty_breaks(6)(0:max(occCounts,na.rm=TRUE))),alpha=0.9)) +
    scale_fill_brewer(palette = "Spectral",direction=-1) +
	guides(fill=guide_legend(title="occCounts")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	p 

p = ggplot() + 
	coord_cartesian(c(-150,170),c(-55,80)) +
	theme_void() +
	geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(es50,breaks=c(0,10,20,30,40,50)),alpha=0.8)) +
    scale_fill_brewer(palette = "Spectral",direction=-1) +
	guides(fill=guide_legend(title="es50")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	p 

p = ggplot() + 
	coord_cartesian(c(-150,170),c(-55,80)) +
	theme_void() +
	geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(log(spCount), scales::pretty_breaks(4)(1:max(log(spCount),na.rm=TRUE))),alpha=0.8)) +
    scale_fill_brewer(palette = "Spectral",direction=-1) +
	guides(fill=guide_legend(title="spCount")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	
	p = ggplot() + 
	coord_cartesian(c(-150,170),c(-55,80)) +
	theme_void() +
	geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    geom_path(data=raw_grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	guides(fill=guide_legend(title="spCount"))
	
	p 

	
	# 
	
# p = ggplot() + 
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group,alpha=0.8), fill="gray", color="gray",alpha=0.8) + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=0.8, color="black") +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(spCount,5),alpha=0.8)) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# guides(fill=guide_legend(title="spCount")) 
	# # + 
	# # geom_point(data=D,aes(longitudeMid,latitudeMid)) 

	# p 
	

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, 
	# fill=cut(spCount, 5))) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") + 
	# guides(fill=guide_legend(title="sp count"))

	# p 


# # try different bins

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=cut(es50, scales::pretty_breaks(4)(0:50)))) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") + 
	# guides(fill=guide_legend(title="es50"))

	# p 

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, 
	# fill=cut(spCount, scales::pretty_breaks(4)(1:max(spCount,na.rm=TRUE))))) +
    # scale_fill_brewer(palette = "Spectral",direction=-1) +
	# geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") + 
	# guides(fill=guide_legend(title="sp count"))

	# p 
		

# #Plot everything on a flat map
# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=es50), alpha=1) +
    # geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") +
    # scale_fill_gradient(low="blue", high="red")
    # # aes(fill=cut(stat(count), scales::pretty_breaks(4)(0:maxCountHurlbert))))
	
	# # geom_point(data=gridData,aes(x=longitudeMid, y=latitudeMid)) +
	# # +
	# # + 
    # # geom_point(aes(x=cellcenters$lon_deg, y=cellcenters$lat_deg)) 
	# p
    # # geom_polygon(data=grid,      aes(x=long, y=lat, group=group, fill=count), alpha=0.4)    +

# p = ggplot() + 
    # geom_path(data=grid,aes(x=long, y=lat, group=group), alpha=1, color="black") +
	# coord_cartesian(c(-150,170),c(-55,80)) +
	# theme_void() +
	# geom_polygon(data=grid, aes(x=long, y=lat, group=group, fill=spCount), alpha=1) +
    # geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black") +
    # scale_fill_gradient(low="blue", high="red")
    # # geom_point(data=gridData,aes(x=longitudeMid, y=latitudeMid)) +
	# # +
	# # + 
    # # geom_point(aes(x=cellcenters$lon_deg, y=cellcenters$lat_deg)) 
	# p
    # # geom_polygon(data=grid,      aes(x=long, y=lat, group=group, fill=count), alpha=0.4)    +
	
	
dev.off()

}
}


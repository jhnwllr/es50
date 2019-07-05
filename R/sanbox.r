
if(FALSE) { # es50 sandbox

library(dplyr)
library(purrr)
library(roperators)
library(httr)
library(tibble)

getDataFromBand = function(taxonKey = 212,stepSize = 10,Step=100,maxPages=30) { # Get occurrences from each band 

getSpeciesOccurrences = function(x,Step,maxPages) {

L = x %>% 
gbifapi::page_api(pluck="results",Step=Step,maxPages=maxPages,verbose=TRUE) 

taxonKey = L %>%
map_chr(~ .x$taxonKey) 

taxonRank = L %>%
map_chr(~ .x$taxonRank) 

D = tibble(taxonKey,taxonRank) %>%
filter(taxonRank == "SPECIES") 

return(D)
}

latitudeBreaks = seq(-90,(90-stepSize),stepSize)
latitudeBreaksOffset = seq(-1*(90-stepSize),90,stepSize)

api = "http://api.gbif.org/v1/occurrence/search?"
# https://www.gbif.org/occurrence/search?

queryParams = "has_coordinate=true&has_geospatial_issue=false&taxon_key=" %+% taxonKey 

apiCalls = latitudeBreaks %>% 
map2(latitudeBreaksOffset,~ 
"&geometry=POLYGON((-180%20" %+% .x %+% ",180%20" %+% .x %+% ",180%20" %+% .y %+% ",-180%20" %+% .y %+% ",-180%20" %+% .x %+%"))"
) %>% 
map(~ api %+% queryParams %+% .x)

occData = apiCalls %>%
map2(latitudeBreaks,~ getSpeciesOccurrences(.x,Step=Step,maxPages=maxPages) %>% mutate(latitude = .y)) 

return(occData)
}

es50 = function(D) {
# randomly select 50 species 

es50 = D %>%
sample_n(50,replace = TRUE) %>% # es50
pull(taxonKey) %>% 
unique() %>%
length()

return(es50)
}

getEs50 = function(x) {

x %>% 
map_dbl(~ es50(.x)) %>% 
enframe()

}


taxonKey = 212
getDataFromBand(taxonKey,stepSize = 1,Step=100,maxPages=100) %>% # 10k records from each band
saveRDS(file="C:/Users/ftw712/Desktop/es50/data/"%+% taxonKey %+%".rda")

stepSize = 10

latitudeBreaks = seq(-90,(90-stepSize),stepSize)

DL = readRDS(file="C:/Users/ftw712/Desktop/es50/data/212.rda") 

D = 1:10 %>% 
map(~
getEs50(DL) %>%
mutate(run = .x)
) %>% 
plyr::rbind.fill() %>% 
group_by(name) %>% 
summarise(es50 = mean(value)) %>%
add_column(latitudeBreaks)

D

library(ggplot2)

pdf("C:/Users/ftw712/Desktop/plot.pdf")

ggplot(D, aes(latitudeBreaks, es50)) + 
geom_point() + 
geom_line() + 
scale_x_continuous(limits = c(-90, 90))

dev.off()


}

if(FALSE) { # sampling experiments

library(entropart)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(roperators)

# Load Paracou data (number of trees per species in two 1-ha plot of a tropical forest)
# data(Paracou618)

# Ns is the total number of trees per species
# Ns <- as.AbdVector(Paracou618.MC$Ns)
# Ns

# table(Paracou618.MC$Ns)

N = 10
sp = "sp_" %+% as.character(1:N) 

distr = ppois((1:N)*0.5, lambda=12, lower=FALSE)

distr

spNames = sample(sp,500,replace=TRUE,prob=distr)

x_table = table(spNames) %>% as.vector()


Hurlbert(x_table, 50)
sample(spNames,50) %>% unique() %>% length()
}

if(FALSE) { # make species counts with latitude bands 

library(purrr)
library(dplyr)
library(tibble)
library(roperators)


getLatBands = function(stepSize=10) {

latitudeBreaks = seq(-90,(90-stepSize),stepSize)
latitudeBreaksOffset = seq(-1*(90-stepSize),90,stepSize)

latBandsPolygons = latitudeBreaks %>% 
map2(latitudeBreaksOffset,~ 
"geometry=POLYGON((-180%20" %+% .x %+% ",180%20" %+% .x %+% ",180%20" %+% .y %+% ",-180%20" %+% .y %+% ",-180%20" %+% .x %+%"))&") 

return(latBandsPolygons)
}

getSpeciesOccCounts = function(url,Step=100,maxPages=200) {

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

getData = function(stepSize,taxonKey) { 
latBands = getLatBands(stepSize) # get latitude bands 

api = "http://api.gbif.org/v1/occurrence/search?limit=0&"
queryParams = "has_coordinate=true" %+% "&" %+% "has_geospatial_issue=false&"
geometry = "geometry=POLYGON((-180%20-90,180%20-90,180%20-80,-180%20-80,-180%20-90))&"
facet = "facet=speciesKey&taxonKey=" %+% taxonKey

apiCalls = latBands %>% map(~ api %+% queryParams %+% .x %+% facet)

DL = apiCalls %>% map(~ getSpeciesOccCounts(.x,Step=100,maxPages=200))

return(DL)
}


stepSize = 5
taxonKey = "789"

latitudeBreaks = seq(-90,(90-stepSize),stepSize)

# DL = getData(stepSize,taxonKey) %>% saveRDS(file="C:/Users/ftw712/Desktop/es50/data/"%+% taxonKey %+%".rda")

DL = readRDS(file="C:/Users/ftw712/Desktop/es50/data/"%+% taxonKey %+%".rda") 

hurlbert = DL %>%
map(~ deframe(.x)) %>% 
modify_if(~ length(.x) <= 50, ~ NA) %>% # run only if more than 50 species 
modify_if(~ !anyNA(.x), ~ entropart::Hurlbert(.x, 50)) %>%
map(~ unname(.x)) %>%
flatten_dbl() 

numSp = DL %>% 
map(~ deframe(.x)) %>% 
map_dbl(~ length(.x)) 


D = tibble(numSp, hurlbert, latitude = latitudeBreaks + 5)

library(ggplot2)

pdf("C:/Users/ftw712/Desktop/plot.pdf")

ggplot(D, aes(numSp, hurlbert)) + 
geom_point() + 
geom_abline(intercept = 0, slope = 0.5) 
# + 
# geom_line() 
# + 

# ggplot(D, aes(latitude, numSp)) + 
# geom_point() + 
# geom_line() + 
# scale_x_continuous(limits = c(-90, 90))
dev.off()

}

if(FALSE) { # generate world grid 

library(roperators)
library(purrr)
library(tibble)
library(dplyr)
library(tidyr)

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

getSpeciesOccCounts = function(url,Step=100,maxPages=200) {

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


stepSize = 1
taxonKey = "212"

gridData = getGridData(stepSize)
# getData(stepSize,taxonKey,gridData) %>% saveRDS(file="C:/Users/ftw712/Desktop/es50/data/grid_" %+% stepSize %+% "_" %+% taxonKey %+%".rda")

gridData %>% glimpse()

DL = readRDS(file="C:/Users/ftw712/Desktop/es50/data/grid_"%+% taxonKey %+%".rda") 

esNum = 50

hurlbert = DL %>%
map(~ deframe(.x)) %>% 
modify_if(~ length(.x) <= esNum, ~ NA) %>% # run only if more than 50 species 
modify_if(~ !anyNA(.x), ~ entropart::Hurlbert(.x, esNum)) %>%
map(~ unname(.x)) %>%
flatten_dbl() 

spCount = DL %>% 
map(~ deframe(.x)) %>% 
map_dbl(~ length(.x)) 

maxCountHurlbert = max(hurlbert,na.rm=TRUE)
maxCountSpCount = max(spCount,na.rm=TRUE)

latitudeMid = gridData %>% pull(latitudeMid) # + rnorm(nrow(gridData),sd=0.1)
longitudeMid = gridData %>% pull(longitudeMid) # + rnorm(nrow(gridData),sd=0.1)

D_hurlbert = tibble(hurlbert, latitudeMid, longitudeMid) %>%
na.omit() %>%
mutate(hurlbert = round(hurlbert)) %>%
uncount(hurlbert) 
# na.omit() %>% 

D_SpCount = tibble(spCount, latitudeMid, longitudeMid) %>% 
filter(!spCount == 0) %>% 
uncount(spCount)

glimpse(D_hurlbert)
glimpse(D_SpCount)


D %>% glimpse()


library(ggplot2)
library(ggthemes)
library(maps)

pdf("C:/Users/ftw712/Desktop/plot.pdf",width=10,height=5)

world_map = map_data("world")

ggplot(D_hurlbert, aes(longitudeMid, latitudeMid)) +
	geom_polygon(data=world_map,aes(x = long, y = lat, group = group),fill="#E8E8E8", colour = NA,alpha = 0.5) +
    geom_hex(binwidth = c(2, 2),aes(fill=cut(stat(count), scales::pretty_breaks(4)(0:maxCountHurlbert)))) +
    scale_fill_brewer(palette = "YlGn",direction=1) +
	theme(legend.title=element_blank()) + 
	theme_void() + 
	xlab("") + 
	ylab("") + 
	coord_cartesian(c(-150,170),c(-55,80)) + 
	theme(legend.position = "none")
	# geom_point(data=gridData,aes(longitudeMid,latitudeMid)) + 
    # coord_equal() + 
	# theme_hc() + 
	# scale_y_continuous(limits = c(-55, 82)) + 
	# scale_x_continuous(limits = c(-170, 190))  
	
ggplot(D_SpCount, aes(longitudeMid, latitudeMid)) +
	geom_polygon(data=world_map,aes(x = long, y = lat, group = group),fill="#E8E8E8", colour = NA,alpha = 0.5) +
    geom_hex(binwidth = c(2, 2),aes(fill=cut(..count.., scales::pretty_breaks(4)(0:maxCountSpCount)))) +
    scale_fill_brewer(palette = "YlGn",direction=1) +
    coord_equal() + 
	theme(legend.title=element_blank()) + 
	theme_void() + 
	xlab("") + 
	ylab("") + 
	coord_cartesian(c(-150,170),c(-55,80)) + 
	theme(legend.position = "none") 
	# +
	# geom_point(data=gridData,aes(longitudeMid,latitudeMid)) 
	
    # stat_bin_hex(binwidth = c(10, 20),aes(fill=cut(spCount, c(0, 5, 10, 50, 100,500, 1000, Inf)), size=1)) +
	# + 
	# labs(fill = NULL) +

dev.off()



  # theme(legend.position = "none",
        # panel.grid = element_blank(),
        # axis.title = element_blank(),
        # axis.text = element_blank(),
        # axis.ticks = element_blank(),
        # panel.background = element_blank())

}

if(FALSE) { # 2nd attempt with dggridR query polygon directly 
	
library(dplyr)
library(dggridR)
library(roperators)
library(purrr)

taxonKey = "789"
spacing = 300 # area of hexagons to generate 

# grid = gbifrasters::getAndSaveGrid(spacing)
# D = gbifrasters::getAndSavePolyRaster(grid,spacing,taxonKey) 
grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

occCountLatTable = gbifrasters::getOccCountLatTable(D,grid)
spCountLatTable = gbifrasters::getSpCountLatTable(D,grid)
es50LatTable = gbifrasters::getEs50LatTable(D,grid)

# occCountLatTable

spCounts = spCountLatTable %>% 
filter(spCountMean > 0) %>% 
select(latCenter,value = spCountMean) %>%
na.omit() %>% 
arrange(latCenter) %>% 
mutate(group = "spCount") %>% 
mutate(valueScale = scale(value)) 

spCountsLog = spCountLatTable %>% 
filter(spCountMean > 0) %>% 
select(latCenter,value = spCountMean) %>%
na.omit() %>% 
arrange(latCenter) %>% 
mutate(group = "spCountLog") %>% 
mutate(valueScale = scale(log10(value))) 

es50 = es50LatTable %>%
glimpse() %>% 
select(latCenter,value = es50Mean) %>% 
na.omit() %>% 
arrange(latCenter) %>%
mutate(group = "es50") %>% 
mutate(valueScale = scale(value))

# occCountLatTable

pd = rbind(spCounts,spCountsLog,es50)

library(ggplot2)
library(ggthemes)

pdf("C:/Users/ftw712/Desktop/plot4.pdf",width=10,height=5)

p = ggplot(pd, aes(latCenter,valueScale,group=group,colour=group)) + 
	geom_point(aes(colour=group,fill=group,alpha=0.1)) + 
	geom_smooth(span = 0.5,aes(fill=group)) + 
	theme_hc() 
p

p = ggplot(occCountLatTable, aes(latCenter,occCounts)) + 
	geom_bar(width=2,stat="identity") + 
	theme_hc()
p

gbifrasters::plotSpCounts(D,grid)
gbifrasters::plotSpCountsLog(D,grid)
gbifrasters::plotEs50(D,grid)
gbifrasters::plotWireFrame(grid,textSize=1)

dev.off()
}







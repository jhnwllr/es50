# effort vs species counts 

library(dplyr)
library(dggridR)
library(roperators)
library(purrr)
library(magrittr)

taxonKey = "1"
spacing = 300 # area of hexagons to generate 

if(FALSE) {
grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

cellOccCounts = gbifrasters::getOccCountsByCell(D)
cellSpCounts = gbifrasters::getSpCountsByCell(D)

grid = merge(grid,cellSpCounts,id="cell",all.x=TRUE) %>% 
merge(cellOccCounts,id="cell",all.x=TRUE) 

grid = grid %>%
mutate(occCountLog10 = log10(occCounts)) %>%
mutate(spCountLog10 = log10(spCount)) %>%
select(cell,lonCenter,latCenter,spCountLog10,occCountLog10) %>% 
unique() %>% 
mutate(label = "") %>%
mutate(decimallatitude = latCenter) %>% 
mutate(decimallongitude = lonCenter) %>%
gbifapi::reverseGeocode() %>%
gbifapi::addCountryName(iso2) %>%
gbifapi::addContinentName(iso2) %T>%
saveRDS(file="C:/Users/ftw712/Desktop/grid.rda")
}


grid = readRDS(file="C:/Users/ftw712/Desktop/grid.rda")

grid %>% group_by(country) %>% 
summarise(max = max(spCountLog10), min=min(spCountLog10)) %>% 
arrange(-max) %>%
as.data.frame()
	
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(ggforce)

grid = grid %>% 
mutate(country = recode(country, Netherlands = "Belgium"))

countryList = c('Sweden','United States','Brazil','Sudan','Madagascar','Iceland','Belgium')

grid = grid %>% 
mutate(LabelMe = country %in% countryList) 

# pdf("C:/Users/ftw712/Desktop/effortCount_" %+% taxonKey %+% ".pdf",width=6,height=5)
svg("C:/Users/ftw712/Desktop/effortCount_" %+% taxonKey %+% ".svg",width=6,height=5)

ggplot(grid, aes(occCountLog10,spCountLog10,label=label)) +
geom_point(colour="gray") + 
theme_bw() + 
geom_mark_ellipse(aes(fill = country,label=country,filter = LabelMe),label.buffer = unit(3, "mm"), label.margin = margin(1, 0, 1, 0, "mm")) +  
scale_y_continuous(limits=c(1,5),labels=c("10","100","1K","10K","100K")) + 
scale_x_continuous(limits=c(1,8),breaks=c(2,4,6,8),labels=c("100","1K","1M","100M")) + 
ylab("richness (genus count)") + 
xlab("effort (occurrence count)") + 
theme(legend.position = "none") + 
theme(plot.margin = margin(0.7, 0.7, 0.1, 0.1, "cm")) +
theme(axis.text=element_text(size=12)) + 
theme(axis.title.x = element_text(size = 14,face="bold")) +
theme(axis.title.y = element_text(size = 14,face="bold")) 
dev.off()

# 






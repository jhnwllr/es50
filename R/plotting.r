if(FALSE) { # es50 plotting 

library(dplyr)
library(dggridR)
library(roperators)
library(purrr)

taxonKey = "359"
spacing = 300 # area of hexagons to generate 

grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")


pdf("C:/Users/ftw712/Desktop/es50_" %+% taxonKey %+% ".pdf",width=9,height=5)

gbifrasters::plotEs50(D,grid,breaks=5)

dev.off()

}

if(FALSE) { # maps 

library(dplyr)
library(dggridR)
library(roperators)
library(purrr)

taxonKey = "1"
spacing = 300 # area of hexagons to generate 

grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

# pdf("C:/Users/ftw712/Desktop/maps_" %+% taxonKey %+% ".pdf",width=9,height=5)
svg("C:/Users/ftw712/Desktop/maps_" %+% taxonKey %+% ".svg",width=9,height=5)

# c(0,2e03,4e03,4e03,6e03,8e03,1e04,1.2e04,1.4e04,1.6e04)
breaks_spCounts = c(0,2e03,4e03,6e03,8e03,1e04,1.2e04,1.4e04,1.6e04)
labels_spCounts = c("0,2k","2k,4k","4k,6k","6k,8k","8k,10k","10k,12k","12k,14k","14k,16k")

gbifrasters::plotSpCounts(D,grid,breaks=breaks_spCounts,labels=labels_spCounts,legend_title="genus count")

breaks_es50 = c(1,27,38,41,43,46,48,50)
labels_es50 = c("1,27","27,38","38,41","41,43","43,46","46,48","48,50")
gbifrasters::plotEs50(D,grid,breaks=breaks_es50,labels=labels_es50)

dev.off()

}

if(FALSE) { # latitude gradients 

library(dplyr)
library(dggridR)
library(roperators)
library(purrr)

taxonKey = "1"
spacing = 300 # area of hexagons to generate 

grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

plotSpCountVsEs50 = function(D,grid) {

spCountLatTable = gbifrasters::getSpCountLatTable(D,grid,round=2)
es50LatTable = gbifrasters::getEs50LatTable(D,grid,round=2)
occCountLatTable = gbifrasters::getOccCountLatTable(D,grid,round=2)

occCounts = occCountLatTable %>%
select(latCenter,value = occCounts) %>%
na.omit() %>% 
arrange(latCenter) %>%
mutate(group = "occurrence count") %>% 
mutate(valueScale = value)

spCounts = spCountLatTable %>% 
filter(spCountMean > 0) %>% 
select(latCenter,value = spCountMean) %>%
na.omit() %>% 
arrange(latCenter) %>% 
mutate(group = "species count") %>% 
mutate(valueScale = value)

es50 = es50LatTable %>%
select(latCenter,value = es50Mean) %>% 
na.omit() %>% 
arrange(latCenter) %>%
mutate(group = "es50") %>% 
mutate(valueScale = value)

pd = rbind(spCounts,es50)



library(ggplot2)
library(ggthemes)

p = ggplot(pd, aes(latCenter,valueScale,fill=group)) + 
	geom_point(alpha=0.1) + 
	geom_smooth(span = 0.5,aes(colour=group)) + 
	theme_hc() + 
	facet_wrap(~group,scales="free_y",ncol=1) + 
	ylab("") + 
	xlab("") + 
	scale_colour_manual(values = c("#3487BD", "#D63E51", "#FEE08B")) + 
	scale_fill_manual(values = c("#3487BD", "#D63E51", "#FEE08B")) + 
	theme(legend.position = "none") + 
	scale_y_continuous(labels = scales::comma)

return(p)
}
	
pdf("C:/Users/ftw712/Desktop/graphs_" %+% taxonKey %+% ".pdf",width=5,height=5)

plotSpCountVsEs50(D,grid)

dev.off()


}

if(FALSE) { # plot maps species count map

library(dplyr)
library(dggridR)
library(roperators)
library(purrr)

taxonKey = "1"
spacing = 300 # area of hexagons to generate 

grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

pdf("C:/Users/ftw712/Desktop/maps_" %+% taxonKey %+% ".pdf",width=9,height=5)
# svg("C:/Users/ftw712/Desktop/maps_" %+% taxonKey %+% ".svg",width=9,height=5)

breaks_occCounts = c(0,1e03,1e04,1e05,1e06,1e07,60e6)
labels_occCounts = c("1e03","1e04","1e05","1e06","1e07","60e6")
gbifrasters::plotOccCounts(D,grid,breaks=breaks_occCounts,labels=labels_occCounts,legend_title="")

# c(0,2e03,4e03,4e03,6e03,8e03,1e04,1.2e04,1.4e04,1.6e04)
breaks_spCounts = c(0,2e03,4e03,6e03,8e03,1e04,1.2e04,1.4e04,1.6e04)
labels_spCounts = c("0,2k","2k,4k","4k,6k","6k,8k","8k,10k","10k,12k","12k,14k","14k,16k")

gbifrasters::plotSpCounts(D,grid,breaks=breaks_spCounts,labels=labels_spCounts,legend_title="genus count")

breaks_es50 = c(1,27,38,41,43,46,48,50)
labels_es50 = c("1,27","27,38","38,41","41,43","43,46","46,48","48,50")
gbifrasters::plotEs50(D,grid,breaks=breaks_es50,labels=labels_es50)

gbifrasters::plotWireFrame(grid,textSize=1)

dev.off()


}



if(FALSE) { # effort and occurrences relationship ggforce 

p = gbifrasters::plotEffortOcc()

filename = "C:/Users/ftw712/Desktop/es50/plots/effortAndSpeciesCounts"

library(roperators)

ggsave(filename %+% ".pdf",plot=p,device="pdf",scale=1,width=6,height=5,units="in")
ggsave(filename %+% ".svg",plot=p,device="svg",scale=1,width=6,height=5,units="in")
ggsave(filename %+% ".png",plot=p,device="png",scale=1,width=6,height=5,units="in")
ggsave(filename %+% ".jpg",plot=p,device="jpg",scale=1,width=6,height=5,units="in",dpi = 600)

webFilename="C:/Users/ftw712/Desktop/data-blog/static/post/2019-06-25-exploring-es50-for-gbif_files/effortAndSpeciesCounts"

ggsave(webFilename %+% ".svg",plot=p,device="svg",scale=1,width=6,height=5,units="in")
}


if(FALSE) { # big global globalOccurrenceCounts_ animal genus 
library(ggplot2)
grid = readRDS(file="C:/Users/ftw712/Desktop/gridMerge.rda")

p = gbifrasters::plotPolyMap(grid,
variable="occCounts",
legend_title="occ counts",
legend.position = c(.50,-0.06),
breaks=c(0,1e03,1e04,1e05,1e06,1e07,1e8),
labels=c("0-1K","1K-10K","10K-100K","100K-1M","1M-10M","10M-100M"),
polygon_text_size=3,
labelType="",
legend_text_size=15
) 

library(roperators)
filename = "C:/Users/ftw712/Desktop/es50/plots/globalOccurrenceCounts_" %+% "1"

ggsave(filename %+% ".pdf",plot=p,device="pdf",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".svg",plot=p,device="svg",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".png",plot=p,device="png",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".jpg",plot=p,device="jpg",scale=1,width=9,height=5,units="in",dpi = 600)

webFilename="C:/Users/ftw712/Desktop/data-blog/static/post/2019-06-25-exploring-es50-for-gbif_files/globalOccurrenceCounts_" %+% "1"

ggsave(webFilename %+% ".svg",plot=p,device="svg",scale=1,width=9,height=5,units="in")

}

if(FALSE) { # big global globalSpeciesCounts_ animal genus

library(dplyr)
library(ggplot2)
grid = readRDS(file="C:/Users/ftw712/Desktop/gridMerge.rda")

grid %>% glimpse()

p = gbifrasters::plotPolyMap(grid,
variable="spCount",
legend_title="genus count",
legend.position = c(.50,-0.06),
breaks=c(0,2e3,4e03,6e03,8e03,1e04,1.2e04,1.4e04,1.6e04),
labels=c("0-2K","2-4K","4K-6K","6K-8K","8K-10K","10K-12K","12K-14K","14K-16K"),
polygon_text_size=3,
labelType="",
legend_text_size=15
)

library(roperators)
filename = "C:/Users/ftw712/Desktop/es50/plots/globalSpeciesCounts_" %+% "1"

ggsave(filename %+% ".pdf",plot=p,device="pdf",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".svg",plot=p,device="svg",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".png",plot=p,device="png",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".jpg",plot=p,device="jpg",scale=1,width=9,height=5,units="in",dpi = 600)

webFilename="C:/Users/ftw712/Desktop/data-blog/static/post/2019-06-25-exploring-es50-for-gbif_files/globalSpeciesCounts_" %+% "1"

ggsave(webFilename %+% ".svg",plot=p,device="svg",scale=1,width=9,height=5,units="in")

}

if(FALSE) { # big global globalSpeciesCountsLog10_ animal genus
library(dplyr)

grid = readRDS(file="C:/Users/ftw712/Desktop/gridMerge.rda")

grid %>% glimpse()

p = gbifrasters::plotPolyMap(grid,
variable="spCount",
legend_title="genus count",
legend.position = c(.50,-0.06),
breaks=c(0,1e1,1e2,1e3,1e4,1e5),
labels=NULL,
polygon_text_size=3,
labelType=""
)

library(roperators)
filename = "C:/Users/ftw712/Desktop/es50/plots/globalSpeciesCountsLog10_" %+% "1"

ggsave(filename %+% ".pdf",plot=p,device="pdf",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".svg",plot=p,device="svg",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".png",plot=p,device="png",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".jpg",plot=p,device="jpg",scale=1,width=9,height=5,units="in",dpi = 600)
}

if(FALSE) { # big global globalEs50_ animal genus

library(dplyr)

grid = readRDS(file="C:/Users/ftw712/Desktop/gridMerge.rda")

grid %>% glimpse()

p = gbifrasters::plotPolyMap(grid,
variable="es50",
legend.position = c(.50,-0.06),
breaks=c(1,27,38,42,44,46,48,50),
labels=c("1-27","27-38","38-41","41-43","43-46","46-48","48-50"),
polygon_text_size=3,
polygon_alpha = 1,
labelType=""
)

library(roperators)
filename = "C:/Users/ftw712/Desktop/es50/plots/globalEs50_" %+% "1"

ggsave(filename %+% ".pdf",plot=p,device="pdf",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".svg",plot=p,device="svg",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".png",plot=p,device="png",scale=1,width=9,height=5,units="in")
ggsave(filename %+% ".jpg",plot=p,device="jpg",scale=1,width=9,height=5,units="in",dpi = 600)

webFilename = "C:/Users/ftw712/Desktop/data-blog/static/post/2019-06-25-exploring-es50-for-gbif_files/globalEs50_" %+% "1"

ggsave(webFilename %+% ".svg",plot=p,device="svg",scale=1,width=9,height=5,units="in")


}

if(FALSE) { # plot zoom indonesia with minimap occCounts

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

grid %>% glimpse()

library(viridis)
library(ggplot2)
library(cowplot)

miniMap = gbifrasters::plotMiniMap(grid,
orientation = c(-0.238566,116.2990554,4),
xlim_ortho = c(100,150),
ylim_ortho = c(-18,14))

p1 = gbifrasters::plotZoomMap(grid,
orientation = c(-0.238566,116.2990554,4),
xlim_ortho = c(100,150),
ylim_ortho = c(-18,14),
variable="occCounts",
legend_title="occ counts",
breaks = c(0,1e03,1e04,1e05,1e06,1e07,1e8),
labels = c("0-1K","1K-10K","10K-100K","100K-1M","1M-10M","10M-100M"),
polygon_text_size = 3,
labelType = "fancy")


filename = "C:/Users/ftw712/Desktop/es50/plots/miniMapOccCounts_" %+% "1"

pdf(filename %+% ".pdf",width=5,height=4)

ggdraw() +
draw_plot(p1, 0, 0, 1, 1) +
draw_plot(miniMap, 0.79, 0.64, 0.15, 0.4)

dev.off()

svg(filename %+% ".svg",width=5,height=4)

ggdraw() +
draw_plot(p1, 0, 0, 1, 1) +
draw_plot(miniMap, 0.79, 0.64, 0.15, 0.4)

dev.off()

webFilename = "C:/Users/ftw712/Desktop/data-blog/static/post/2019-06-25-exploring-es50-for-gbif_files/miniMapOccCounts_" %+% "1"

svg(webFilename %+% ".svg",width=5,height=4)

ggdraw() +
draw_plot(p1, 0, 0, 1, 1) +
draw_plot(miniMap, 0.79, 0.64, 0.15, 0.4)

dev.off()

}

if(FALSE) { # plot zoom indonesia with minimap miniMapSpCount_

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

grid %>% glimpse()

library(viridis)
library(ggplot2)
library(cowplot)

miniMap = gbifrasters::plotMiniMap(grid,
orientation = c(-0.238566,116.2990554,4),
xlim_ortho = c(100,150),
ylim_ortho = c(-18,14))

p1 = gbifrasters::plotZoomMap(grid,
orientation = c(-0.238566,116.2990554,4),
xlim_ortho = c(100,150),
ylim_ortho = c(-18,14),
variable="spCount",
legend_title="genus count",
breaks=c(0,2e3,4e03,6e03,8e03,1e04,1.2e04,1.4e04,1.6e04),
labels=c("0-2K","2K-4K","4K-6K","6K-8K","8K-10K","10K-12K","12K-14K","14K-16K"),
polygon_text_size = 3,
labelType = "fancy")

filename = "C:/Users/ftw712/Desktop/es50/plots/miniMapSpCount_" %+% "1"

pdf(filename %+% ".pdf",width=5,height=4)

ggdraw() +
draw_plot(p1, 0, 0, 1, 1) +
draw_plot(miniMap, 0.79, 0.64, 0.15, 0.4)

dev.off()

# svg(filename %+% ".svg",width=5,height=4)

# ggdraw() +
# draw_plot(p1, 0, 0, 1, 1) +
# draw_plot(miniMap, 0.79, 0.64, 0.15, 0.4)

# dev.off()

webFilename = "C:/Users/ftw712/Desktop/data-blog/static/post/2019-06-25-exploring-es50-for-gbif_files/miniMapSpCount_" %+% "1"

svg(webFilename %+% ".svg",width=5,height=4)

ggdraw() +
draw_plot(p1, 0, 0, 1, 1) +
draw_plot(miniMap, 0.79, 0.64, 0.15, 0.4)

dev.off()

}

if(FALSE) { # plot zoom indonesia with minimap miniMapEs50

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

grid %>% glimpse()

library(viridis)
library(ggplot2)
library(cowplot)

miniMap = gbifrasters::plotMiniMap(grid,
orientation = c(-0.238566,116.2990554,4),
xlim_ortho = c(100,150),
ylim_ortho = c(-18,14))

p1 = gbifrasters::plotZoomMap(grid,
orientation = c(-0.238566,116.2990554,4),
xlim_ortho = c(100,150),
ylim_ortho = c(-18,14),
variable="es50",
legend_title="es50",
breaks=c(1,27,38,42,44,46,48,50),
labels=c("1-27","27-38","38-41","41-43","43-46","46-48","48-50"),
polygon_text_size = 3,
labelType="identity")

filename = "C:/Users/ftw712/Desktop/es50/plots/miniMapEs50_" %+% "1"

pdf(filename %+% ".pdf",width=5,height=4)
ggdraw() +
draw_plot(p1, 0, 0, 1, 1) +
draw_plot(miniMap, 0.79, 0.64, 0.15, 0.4)
dev.off()

svg(filename %+% ".svg",width=5,height=4)
ggdraw() +
draw_plot(p1, 0, 0, 1, 1) +
draw_plot(miniMap, 0.79, 0.64, 0.15, 0.4)
dev.off()

webFilename = "C:/Users/ftw712/Desktop/data-blog/static/post/2019-06-25-exploring-es50-for-gbif_files/miniMapEs50" %+% "1"

svg(webFilename %+% ".svg",width=5,height=4)

ggdraw() +
draw_plot(p1, 0, 0, 1, 1) +
draw_plot(miniMap, 0.79, 0.64, 0.15, 0.4)

dev.off()

}

if(FALSE) { # Latitude trends graph

library(roperators)
library(dplyr)
library(ggplot2)

taxonKey = "1"
spacing = 300

# grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/grid_" %+% spacing %+% "_" %+% taxonKey %+% ".rda")
grid = readRDS(file="C:/Users/ftw712/Desktop/gridMerge.rda")
grid %>% glimpse()

spCountLatTable = gbifrasters::getSpCountLatTable(grid,round=2)
es50LatTable = gbifrasters::getEs50LatTable(grid,round=2)

spCounts = spCountLatTable %>% 
filter(spCountMean > 0) %>% 
select(latCenter,value = spCountMean) %>%
na.omit() %>% 
arrange(latCenter) %>% 
mutate(group = "genus count") %>% 
mutate(valueScale = value)

es50 = es50LatTable %>%
select(latCenter,value = es50Mean) %>% 
na.omit() %>% 
arrange(latCenter) %>%
mutate(group = "es50") %>% 
mutate(valueScale = value)

pd = rbind(spCounts,es50)

p = ggplot(pd, aes(latCenter,valueScale,fill=group)) + 
	geom_point(colour="gray") + 
	geom_smooth(span = 0.5,aes(colour=group)) + 
	theme_bw() + 
	facet_wrap(~group,scales="free_y",ncol=2) + 
	ylab("") + 
	xlab("latitude") + 
	scale_colour_manual(values = c("#D63E51","#3487BD", "#FEE08B")) + 
	scale_fill_manual(values = c("#D63E51","#3487BD", "#FEE08B")) + 
	theme(legend.position = "none") + 
	scale_y_continuous(labels = scales::comma)

pdf("C:/Users/ftw712/Desktop/es50/plots/latGraphs_1.pdf",width=5,height=2.5)
p	
dev.off()

svg("C:/Users/ftw712/Desktop/es50/plots/latGraphs_1.svg",width=5,height=2.5)
p
dev.off()

svg("C:/Users/ftw712/Desktop/data-blog/static/post/2019-06-25-exploring-es50-for-gbif_files/latGraphs_1.svg",width=5,height=2.5)
p
dev.off()

}

if(FALSE) { # plot es50 any group 
# grid = readRDS(file="C:/Users/ftw712/Desktop/gridMerge.rda")

library(ggplot2)
library(ggthemes)
library(roperators)
library(dplyr)
library(purrr)

getLabels = function(breaks) {

Start = 2:length(breaks) %>% map_dbl(~breaks[.x-1]) 
Finish = 1:(length(breaks)-1) %>% map_dbl(~breaks[.x+1]) 

return(paste0(Start,"-",Finish))
}

saveAllFormats = function(p,filename,width=9,height=5)  {

ggsave(filename %+% ".pdf",plot=p,device="pdf",scale=1,width=width,height=height,units="in")
ggsave(filename %+% ".svg",plot=p,device="svg",scale=1,width=width,height=height,units="in")
# ggsave(filename %+% ".png",plot=p,device="png",scale=1,width=width,height=height,units="in")
ggsave(filename %+% ".jpg",plot=p,device="jpg",scale=1,width=width,height=height,units="in",dpi = 600)

}

plotDefaultES50map = function(taxonKey = 359,spacing = 300,data_dir = "C:/Users/ftw712/Desktop/es50/data/",useObisBreaks=FALSE) {

grid_name = "grid_" %+% spacing %+% "_" %+% taxonKey 
grid_filename = data_dir %+% grid_name %+% ".rda"

grid = readRDS(file=grid_filename)

sd4 = sd(grid$es50,na.rm=TRUE)*4 # smart breaks skip lower bunch
es50_breaks = grid %>% filter(es50 > sd4) %>% pull(es50) 
breaks = seq(sd4,max(es50_breaks)+1,length.out=6) %>% round(0)
breaks = c(0,breaks)

# breaks = scales::pretty_breaks(6.6)(es50_breaks)

if(useObisBreaks) breaks = c(1,27,38,42,44,46,48,50)
labels = getLabels(breaks)

# c("1-27","27-38","38-41","41-43","43-46","46-48","48-50") # Obis Breaks
# c(1,27,38,42,44,46,48,50)

p = gbifrasters::plotPolyMap(grid,
variable="es50",
legend.position = c(.50,-0.06),
breaks=breaks,
labels=labels,
polygon_text_size=3,
polygon_alpha = 1,
labelType="",
keywidth=0.01,
keyheight=0.3,
legend_text_size=15
)

return(p)
}

otherFormatLinks = function(blog_dir="",image_name="",formats = c("jpg","pdf","svg")) { 

Links = formats %>% 
map_chr(~ "[" %+% .x %+% "](" %+% blog_dir %+% image_name %+% "." %+% .x %+% ")") %>% 
paste(collapse=" | ")

return(Links)
}


taxonKey = 216
spacing = 300
data_dir = "C:/Users/ftw712/Desktop/es50/data/"

p = plotDefaultES50map(taxonKey,spacing,data_dir,useObisBreaks=FALSE)

# save in locale folder
image_name = "map_es50_" %+% spacing %+% "_" %+% taxonKey
filename = "C:/Users/ftw712/Desktop/es50/plots/" %+% image_name
saveAllFormats(p,filename,width=9,height=5)

# save for blog
blog_dir = "post/2019-06-25-exploring-es50-for-gbif_files/"
filename = "C:/Users/ftw712/Desktop/data-blog/static/" %+% blog_dir %+% image_name
saveAllFormats(p,filename,width=9,height=5)
otherFormatLinks(blog_dir,image_name,formats = c("jpg","pdf","svg")) 

}

if(FALSE) { # Plot Wire Frame 

grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/grid_300_1.rda")

library(ggplot2)
p = gbifrasters::plotWireFrame(grid,textSize=1)


filename="C:/Users/ftw712/Desktop/data-blog/static/post/2019-06-25-exploring-es50-for-gbif_files/wireframe.jpg"

ggsave(filename,plot=p,device="jpg",scale=1,width=9,height=5,units="in",dpi = 600)
}




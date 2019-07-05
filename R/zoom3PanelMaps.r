if(FALSE) { # 1 st attempt zoom map cowplot


library(dplyr)
library(dggridR)
library(roperators)
library(purrr)

taxonKey = "1"
spacing = 300 # area of hexagons to generate 

grid = readRDS(file="C:/Users/ftw712/Desktop/es50/data/gridframe_" %+% spacing %+% ".rda")
D = readRDS(file="C:/Users/ftw712/Desktop/es50/data/polygon_" %+% spacing %+% "_" %+% taxonKey %+%".rda")

pdf("C:/Users/ftw712/Desktop/3PanelMaps_" %+% taxonKey %+% ".pdf",width=7,height=3)
# svg("C:/Users/ftw712/Desktop/3PanelMaps_" %+% taxonKey %+% ".svg",width=9,height=5)

breaks_occCounts = c(0,1e03,1e04,1e05,1e06,1e07,60e6)
labels_occCounts = c("1e03","1e04","1e05","1e06","1e07","60e6")

p1 = gbifrasters::plotOccCounts(D,grid,
breaks=breaks_occCounts,
labels=labels_occCounts,
legend_title="",
zoom_x=c(-40,40),
zoom_y=c(35,70),
legend.position="none",
margin = margin(0.7, 0, 0, 0, "cm"),
polygon_text_size = 1.7)

breaks_spCounts = c(0,2e03,4e03,6e03,8e03,1e04,1.2e04,1.4e04,1.6e04)
labels_spCounts = c("0,2k","2k,4k","4k,6k","6k,8k","8k,10k","10k,12k","12k,14k","14k,16k")

p2 = gbifrasters::plotSpCounts(D,grid,
breaks=breaks_spCounts,
labels=labels_spCounts,
legend_title="genus count",
zoom_x=c(-40,40),
zoom_y=c(35,70),
legend.position="none",
margin = margin(0.7, 0, 0, 0, "cm"),
polygon_text_size=1.7) 


p3 = gbifrasters::plotEs50(D,grid,
breaks=c(1,27,38,42,44,46,48,50),
labels=NULL,
legend_title="es50",
zoom_x=c(-40,40),
zoom_y=c(35,70),
legend.position="none",
margin = margin(0.7, 0, 0, 0, "cm"),
polygon_text_size=1.7,
polygon_alpha=1, 
orthoProjection=TRUE,
orientation = c(47.8280325,10.6239369, 6),
coord_map_xlim=c(-40,40),
coord_map_ylim=c(35,70)) 



cowplot::plot_grid(p1,p2,p3,ncol = 3)

dev.off()
}


if(FALSE) { # europe zoom
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


pdf("C:/Users/ftw712/Desktop/3PanelMaps_" %+% taxonKey %+% ".pdf",width=5,height=4)

gbifrasters::plotPolyMap(grid,
variable="occCounts",
legend_title="occ counts",
legend.position = c(.50,-0.1),
breaks=c(0,1e03,1e04,1e05,1e06,1e07,1e8),
labels=c("0-1K","1K-10K","10K-100K","100K-1M","1M-10M","10M-100M"),
polygon_text_size=3,
labelType="fancy"
) + 
theme(plot.margin=margin(0, 0.7, 0, 0, "cm")) + 
coord_map("ortho",orientation=c(35, 13, 5),xlim=c(-40,40), ylim=c(35,70)) +
theme(axis.ticks.x=element_blank())+
theme(axis.ticks.y=element_blank())

gbifrasters::plotPolyMap(grid,
variable="spCount",
legend_title="genus count",
legend.position = c(.50,-0.1),
breaks=c(0,2e03,4e03,6e03,8e03,1e04,1.2e04,1.4e04,1.6e04),
labels=c("0-2K","2K-4K","4K-6K","6K-8K","8K-10K","10K-12K","12K-14K","14K-16K"),
polygon_text_size=3,
labelType="fancy"
) + 
theme(plot.margin=margin(0, 0.7, 0, 0, "cm")) + 
coord_map("ortho",orientation=c(35, 13, 5),xlim=c(-40,40), ylim=c(35,70)) +
theme(axis.ticks.x=element_blank())+
theme(axis.ticks.y=element_blank())

gbifrasters::plotPolyMap(grid,
variable="es50",
legend.position = c(.50,-0.1),
breaks=c(1,27,38,42,44,46,48,50),
labels=c("1-27","27-38","38-41","41-43","43-46","46-48","48-50"),
polygon_text_size=3,
polygon_alpha = 1,
labelType="identity"
) + 
theme(plot.margin=margin(0, 0.7, 0, 0, "cm")) + 
coord_map("ortho",orientation=c(35, 13, 5),xlim=c(-40,40), ylim=c(35,70)) +
theme(axis.ticks.x=element_blank())+
theme(axis.ticks.y=element_blank())


dev.off()

}

if(FALSE) { # indonesia zoom
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

orientation = c(-0.238566,116.2990554,4)
xlim_ortho = c(100,150)
ylim_ortho = c(-18,14)

pdf("C:/Users/ftw712/Desktop/3PanelMap_Indonesia_" %+% taxonKey %+% ".pdf",width=5,height=4)

gbifrasters::plotPolyMap(grid,
variable="occCounts",
legend_title="occ counts",
legend.position = c(.50,-0.1),
breaks=c(0,1e03,1e04,1e05,1e06,1e07,1e8),
labels=c("0-1K","1K-10K","10K-100K","100K-1M","1M-10M","10M-100M"),
polygon_text_size=3,
labelType="fancy"
) + 
theme(plot.margin=margin(0, 0.7, 0, 0, "cm")) + 
coord_map("ortho",orientation=orientation,xlim=xlim_ortho, ylim=ylim_ortho) +
theme(axis.ticks.x=element_blank())+
theme(axis.ticks.y=element_blank())

gbifrasters::plotPolyMap(grid,
variable="spCount",
legend_title="genus count",
legend.position = c(.50,-0.1),
breaks=c(0,4e03,6e03,8e03,1e04,1.2e04,1.4e04,1.6e04),
labels=c("0-2K","4K-6K","6K-8K","8K-10K","10K-12K","12K-14K","14K-16K"),
polygon_text_size=3,
labelType="fancy"
) + 
theme(plot.margin=margin(0, 0.7, 0, 0, "cm")) + 
coord_map("ortho",orientation=orientation,xlim=xlim_ortho, ylim=ylim_ortho) +
theme(axis.ticks.x=element_blank())+
theme(axis.ticks.y=element_blank())

gbifrasters::plotPolyMap(grid,
variable="es50",
legend.position = c(.50,-0.1),
breaks=c(1,27,38,42,44,46,48,50),
labels=c("1-27","27-38","38-41","41-43","43-46","46-48","48-50"),
polygon_text_size=3,
polygon_alpha = 1,
labelType="identity"
) + 
theme(plot.margin=margin(0, 0.7, 0, 0, "cm")) + 
coord_map("ortho",orientation=orientation,xlim=xlim_ortho, ylim=ylim_ortho) +
theme(axis.ticks.x=element_blank())+
theme(axis.ticks.y=element_blank())

gbifrasters::plotPolyMap(grid,
variable="es50",
legend.position = "none",
breaks=c(1,27,38,42,44,46,48,50),
labels=c("1-27","27-38","38-41","41-43","43-46","46-48","48-50"),
polygon_text_size=3,
polygon_alpha = 1,
labelType=""
) + 
theme(plot.margin=margin(0, 0.7, 0, 0, "cm")) + 
coord_map("ortho",orientation=orientation,xlim=NULL, ylim=NULL) +
theme(axis.ticks.x=element_blank())+
theme(axis.ticks.y=element_blank())


dev.off()

}


# plotZoomMap with miniMap

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
polygon_text_size = 3)


pdf("C:/Users/ftw712/Desktop/miniMap.pdf",width=5,height=4)

ggdraw() +
draw_plot(p1, 0, 0, 1, 1) +
draw_plot(miniMap, 0.79, 0.64, 0.15, 0.4)

dev.off()


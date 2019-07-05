# mini map 

pdf("C:/Users/ftw712/Desktop/miniMap.pdf")

# plot.mpg and plot.diamonds were defined earlier
library(viridis)
library(ggplot2)
library(cowplot)

p1 = ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar() +
theme(axis.text.x = element_text(angle=70, vjust=0.5))

p2 = ggplot(mpg, aes(x = cty, y = hwy, colour = factor(cyl))) + 
geom_point(size=2.5)

ggdraw() +
draw_plot(p1, 0, 0, 1, 1) +
draw_plot(p2, 0.5, 0.52, 0.5, 0.4) 

dev.off()




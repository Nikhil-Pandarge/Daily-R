# importing dataset which is named as tree(for sugarcane)

data(trees)

head(trees)

data()

str(trees) ## look at the structure of the variables

library(GGally) ## this library for ggpairs() function 
 
ggpairs(data=trees, columns=1:3, title="SugarCane data")

fit_1 <- lm(Volume ~ Girth, data = trees)

summary(fit_1)

ggplot(data=trees, aes(fit_1$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "orange") +
  theme(panel.background = element_rect(fill = "grey"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals area wise production")
# residual is observed value - predicted value

ggplot(data = trees, aes(x = Girth, y = Volume)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "grey"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Sugarcane production Data")

predict(fit_1, data.frame(Girth = 18.2))

# the above line gives area wise prediction.

fit_2 <- lm(Volume ~ Girth + Height, data = trees)

summary(fit_2)

Girth <- seq(9,21, by=0.5) ## make a girth vector

Height <- seq(60,90, by=0.5) ## make a height vector

pred_grid <- expand.grid(Girth = Girth, Height = Height)
## make a grid using the vectors

pred_grid$Volume2 <-predict(fit_2, new = pred_grid)

#------------------------

install.packages("ggimage")

library(ggimage)

install.packages("ggtree")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

library(ggtree)

url <- paste0("https://raw.githubusercontent.com/TreeViz/",
              "metastyle/master/design/viz_targets_exercise/")

x <- read.tree(paste0(url, "tree_boots.nwk"))
info <- read.csv(paste0(url, "tip_data.csv"))

p <- ggtree(x) %<+% info + xlim(-.1, 4)
p2 <- p + geom_tiplab(offset = .6, hjust = .5) +
  geom_tippoint(aes(shape = trophic_habit, color = trophic_habit, size = mass_in_kg)) + 
  theme(legend.position = "right") + scale_size_continuous(range = c(3, 10))

d2 <- read.csv(paste0(url, "inode_data.csv"))
p2 %<+% d2 + geom_label(aes(label = vernacularName.y, fill = posterior)) + 
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(3, "YlGnBu"))



#-----------------------------------
install.packages("scatterplot3d")

library(scatterplot3d)

fit_2_sp <- scatterplot3d(pred_grid$Girth, pred_grid$Height, pred_grid$Volume2, angle = 60, color = "dodgerblue", pch = 1, ylab = "Hight (ft)", xlab = "Girth (in)", zlab = "Volume (ft3)" )

fit_2_sp$points3d(trees$Girth, trees$Height, trees$Volume, pch=16)

predict(fit_2, data.frame(Girth = 18.2, Height = 72))

# the above line gives area wise prediction.

library(rgeos)
library(rgdal)
library(maptools)     # also loads sp()
library(RColorBrewer) # creates nice color schemes
library(classInt) 
summary(orcounty.shp)

load("geog495.RData")
plotvar <- orcounty.shp@data$POP1990
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

# block 2
plot(orcounty.shp, xlim=c(-124.5, -115), ylim=c(42,47))
plot(orcounty.shp, col=colcode, add=T)
title(main="Area wise Sugarcane production",
      sub="Quantile (Equal-Frequency) Class Intervals")
legend(-117, 44, legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.6, bty="n")

##############################################################################################################################
# HIDDEN RIVERS of the UNITED STATES
# script to plot rivers, waterbodies, and watersheds
# author: Dan MacGuigan
# dmacguig@buffalo.edu

##############################################################################################################################

library(RColorBrewer)
library(maps)
library(rgdal)
library(gridExtra)
library(maptools)
library(scales)
library(raster)
library(rgeos)
library(ggplot2)
library(grid)
library(mapplots)
library(dplyr)    
library(mapproj)
library(ggmap)
library(ggspatial)
library(sf)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(cowplot)
library(shades)

##############################################################################################################################
# specify the following parameters
river_name <- "10Maps_summary" # river name for files
river_name_plot <- "10Maps_summary" # river name for plot title
wd <- "H:/USHiddenRivers/" # top level working directory

setwd(wd)

##############################################################################################################################
# READ DATA and PARSE shapefiles

# rivers
rivers <- readOGR(paste(wd, "/shapefiles/allRivers/streaml010g.shp", sep=""))
rivers <- spTransform(rivers, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
feat <- as.character(rivers@data$Feature)
rivers <- rivers[which(!as.logical(rowSums(sapply(c("Intracoastal Waterway"), grepl, x=feat, fixed=TRUE)))),]
riverList=list()
riverFiles <- list.files(paste(wd, "shapefiles/isolatedRivers/", sep=""), "*.shp", full.names=TRUE)
completeRivers <- readOGR(riverFiles[1])
completeRivers <- spTransform(completeRivers, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
completeRivers@data$RIVER <- factor(rep(riverFiles[1], nrow(completeRivers@data)))
for(i in 2:length(riverFiles)){
  temp <- readOGR(riverFiles[i])
  temp <- spTransform(temp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  temp@data$RIVER <- factor(rep(riverFiles[i], nrow(temp@data)))
  completeRivers <- raster::union(completeRivers, temp)
}
levels(completeRivers@data$RIVER) <- c(1:length(riverFiles))


# lakes
wbs <- readOGR(paste(wd, "/shapefiles/waterbodies/wtrbdyp010g.shp", sep=""))
wbs <- spTransform(wbs, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
feature <- as.character(wbs$Feature)
wbs_lakes <- wbs[which(as.logical(rowSums(sapply(c("Lake"), grepl, x=feature, fixed=TRUE)))),]
feature <- as.character(wbs_lakes$Feature)
wbs_lakes <- wbs_lakes[which(!as.logical(rowSums(sapply(c("Lake Dry"), grepl, x=feature, fixed=TRUE)))),]
wbsList=list()
wbsFiles <- list.files(paste(wd, "shapefiles/isolatedWaterbodies/", sep=""), "*_waterbodies_lakes.shp", full.names=TRUE)
completeWbs <- readOGR(wbsFiles[1])
completeWbs <- spTransform(completeWbs, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
completeWbs@data$RIVER <- factor(rep(wbsFiles[1], nrow(completeWbs@data)))
for(i in 2:length(wbsFiles)){
  temp <- try(readOGR(wbsFiles[i]))
  if(class(temp)[1] != "try-error"){
    temp <- spTransform(temp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    temp@data$RIVER <- factor(rep(wbsFiles[i], nrow(temp@data)))
    completeWbs <- raster::union(completeWbs, temp)
  } else {
    levels(completeWbs@data$RIVER) <- c(levels(completeWbs@data$RIVER), wbsFiles[i])
  }
}
levels(completeWbs@data$RIVER) <- c(1:length(wbsFiles))


# inlets
inletsList=list()
inletsFiles <- list.files(paste(wd, "shapefiles/isolatedInlets/", sep=""), "*.shp", full.names=TRUE)
completeInlets <- readOGR(inletsFiles[1])
completeInlets <- spTransform(completeInlets, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
completeInlets@data$RIVER <- factor(rep(inletsFiles[1], nrow(completeInlets@data)))
for(i in 2:length(inletsFiles)){
  temp <- try(readOGR(wbsFiles[i]))
  if(class(temp)[1] != "try-error"){
    temp <- spTransform(temp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    temp@data$RIVER <- factor(rep(inletsFiles[i], nrow(temp@data)))
    completeInlets <- raster::union(completeInlets, temp)
  } else {
    levels(completeInlets@data$RIVER) <- c(levels(completeInlets@data$RIVER), inletsFiles[i])
  }
}
levels(completeInlets@data$RIVER) <- c(1:length(inletsFiles))


# STATE BORDERS
# read in shape border shapefile
# data from https://github.com/jasperdebie/VisInfo/blob/master/us-state-capitals.csv
state_borders <- readOGR(paste(wd, "/shapefiles/stateBorders/statesp010g.shp", sep=""))
state_borders <- spTransform(state_borders, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# COASTLINE
# data from https://www.sciencebase.gov/catalog/item/581d051ce4b08da350d523ba
coastline <- readOGR(paste(wd, "/shapefiles/coastline/coastll010g.shp", sep=""))
coastline <- spTransform(coastline, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# RASTER TOPO DATA 
# from https://www.naturalearthdata.com/downloads/10m-raster-data/10m-shaded-relief/
topo <- raster(paste(wd, "/shapefiles/shadedRelief/GRAY_HR_SR_OB.tif", sep=""))


###############################################################################################################################
# PLOT MAP

# get extent of target watershed
extents <- extent(coastline)
# get x and y min and max values
# may need to adjust these manually
ymax <- 50
extents@ymax <- ymax
ymin <- 25
extents@ymin <- ymin
xmax <- -65
extents@xmax <-  xmax
xmin <- -130
extents@xmin <- xmin

# colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
col <- gg_color_hue(length(wbsFiles))
col_light<- lightness(col, 80)
names(col_light) <- as.character(1:length(wbsFiles))

# set up data for ggplot
world <- ne_countries(scale='large',returnclass = 'sf')
us_states <- ne_states(country="United States of America", returnclass = 'sf')

completeInlets_sf <- st_as_sf(completeInlets)

completeRivers_sf <- st_as_sf(completeRivers)
completeRivers$Strahler[completeRivers$Strahler<0] <- 1

completeWbs_sf <- st_as_sf(completeWbs)

state_borders_sf <- st_as_sf(state_borders)
#state_borders_sf_crop <- st_crop(state_borders_sf, xmin = xmin, xmax = xmax,
#                                 ymin = ymin, ymax = ymax)

coastline_sf <- st_as_sf(coastline)
coastline_sf_crop <- st_crop(coastline_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)

wbs_lakes_sf <- st_as_sf(wbs_lakes)
#wbs_lakes_sf_crop <- st_crop(wbs_lakes_sf, xmin = xmin, xmax = xmax,
#                                    ymin = ymin, ymax = ymax)

rivers_sf <- st_as_sf(rivers)
rivers_sf_crop <- st_crop(rivers_sf, xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax)
# replace negative stream order values with 1
rivers_sf_crop$Strahler[rivers_sf_crop$Strahler<0] <- 1


topo_crop <- crop(topo, extents)
topo_crop_df <- as.data.frame(topo_crop, xy = TRUE)


# main plot
p <- ggplot() +
  geom_sf(data=state_borders_sf, fill="black", color=NA) +
  geom_raster(topo_crop_df, mapping = aes(x = x, y = y, alpha = GRAY_HR_SR_OB)) +
  scale_alpha(range =  c(0, 1), guide = "none") + 
  geom_sf(data=rivers_sf_crop, color="gray40", lineend = "round", aes(size=factor(Strahler)), show.legend = FALSE,) +
  geom_sf(data=wbs_lakes_sf, fill="gray40", color=NA) +
  geom_sf(data=state_borders_sf, size=0.2, fill=NA, color="white", ) +
  geom_sf(data=completeInlets_sf, aes(fill=RIVER), color=NA, show.legend = FALSE,) +
  geom_sf(data=completeWbs_sf, aes(fill=RIVER), color=NA, show.legend = FALSE,) +
  geom_sf(data=completeRivers_sf, aes(size=factor(Strahler), color=RIVER), 
          show.legend = FALSE, lineend = "round") +
  scale_color_manual(values = col_light) +
  scale_fill_manual(values = col_light) +
  scale_size_manual(values=seq(0.1,2,by=0.03)) +
  coord_equal() +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) + 
  ylab("") +
  xlab("") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "#1c5163"),
        plot.title = element_text(color = "black", size = 16, hjust = 0.5))

# plot dimensions
# mess with the multipliers to get a nice figure
height = (ymax-ymin)*0.3
width = (xmax-xmin)*0.3

png(paste(wd, "maps/", "10Rivers", ".png", sep=""), units = "in", res=600, height=height, width=width)
p
dev.off()



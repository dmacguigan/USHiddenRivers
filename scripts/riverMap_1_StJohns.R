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

##############################################################################################################################
# specify the following parameters
HUCs <- c("030801") # top-level vector of HUCs to match (must be strings, not numeric)
# search for HUCs here: https://water.usgs.gov/wsc/map_index.html
states <- c("FL") # states spanned by the river basin (for parsing waterbodies), use abbreviations
river_name <- "StJohns" # river name for files
river_name_plot <- "St. Johns" # river name for plot title
wd <- "H:/USHiddenRivers/" # top level working directory

setwd(wd)

##############################################################################################################################
# READ DATA and PARSE shapefiles

# RIVERS
# load shapefile for all US streams at 1 million scale
# data from https://nationalmap.gov/small_scale/mld/1strmsl.html
rivers <- readOGR(paste(wd, "./shapefiles/allRivers/streaml010g.shp", sep=""))
rivers <- spTransform(rivers, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
reachCodes <- as.character(rivers$ReachCode)
target_river <- rivers[which(as.logical(rowSums(sapply(HUCs, startsWith, x=reachCodes)))),]
target_river <- as(target_river, "SpatialLinesDataFrame" )
setwd(paste(wd, "./shapefiles/isolatedRivers", sep=""))
writeOGR(target_river, dsn = '.', layer = paste(river_name, "_river", sep=""), driver = "ESRI Shapefile", overwrite_layer=TRUE)

# BASINS
# load shapefile for US watersheds (HUC 10 level)
# data from https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
# will need to use external GIS platform such as QGIS to extract HUC 10 watersheds from database
watersheds <- readOGR(paste(wd, "./shapefiles/watersheds/WBD_HU10_watersheds.shp", sep=""))
watersheds <- spTransform(watersheds, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
target_watershed <- aggregate(target_watershed, dissolve=T)
# crop watershed at coastline using low resolution state borders shapefile
# data from https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_5m.zip
state_borders_lowres <- readOGR("./shapefiles/stateBorders_lowRes/cb_2018_us_division_5m.shp")
target_watershed <- crop(target_watershed, state_borders_lowres)
target_watershed <- as(target_watershed, "SpatialPolygonsDataFrame" )
setwd(paste(wd, "./shapefiles/isolatedWatersheds", sep=""))
writeOGR(target_watershed, dsn = '.', layer = paste(river_name, "_watershed", sep=""), driver = "ESRI Shapefile", overwrite_layer=TRUE)

# WATERBODIES
# read shapefile
# data from https://maps.princeton.edu/catalog/stanford-sv709xw7113
wbs <- readOGR(paste(wd, "./shapefiles/waterbodies", "wtrbdyp010g", sep=""))
wbs <- spTransform(wbs, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
state <- as.character(wbs$State)
target_wbs <- wbs[which(as.logical(rowSums(sapply(states, grepl, x=state, fixed=TRUE)))),]
feature <- as.character(target_wbs$Feature)
# lakes only
target_wbs_lakes <- target_wbs[which(as.logical(rowSums(sapply(c("Lake"), grepl, x=feature, fixed=TRUE)))),]
in_ws_lakes <- over(target_wbs_lakes, target_watershed)
target_wbs_lakes <- target_wbs_lakes[!is.na(in_ws_lakes$dummy),]
target_wbs_lakes <- aggregate(target_wbs_lakes, dissolve=T)
target_wbs_lakes <- as(target_wbs_lakes, "SpatialPolygonsDataFrame" )
setwd(paste(wd, "/shapefiles/isolatedWaterbodies", sep=""))
writeOGR(target_wbs_lakes, dsn = '.', layer = paste(river_name, "_waterbodies_lakes", sep=""), driver = "ESRI Shapefile", overwrite_layer=TRUE)
# swamps only
target_wbs_swamps <- target_wbs[which(as.logical(rowSums(sapply(c("Swamp or Marsh"), grepl, x=feature, fixed=TRUE)))),]
in_ws_swamps <- over(target_wbs_swamps, target_watershed)
target_wbs_swamps <- target_wbs_swamps[!is.na(in_ws_swamps$dummy),]
target_wbs_swamps <- aggregate(target_wbs_swamps, dissolve=T)
target_wbs_swamps <- as(target_wbs_swamps, "SpatialPolygonsDataFrame")
setwd(paste(wd, "/shapefiles/isolatedWaterbodies", sep=""))
writeOGR(target_wbs_swamps, dsn = '.', layer = paste(river_name, "_waterbodies_swamps", sep=""), driver = "ESRI Shapefile", overwrite_layer=TRUE)

# STATE BORDERS
# read in shape border shapefile
# data from https://github.com/jasperdebie/VisInfo/blob/master/us-state-capitals.csv
state_borders <- readOGR(paste(wd, "./shapefiles/stateBorders/statesp010g.shp", sep=""))
state_borders <- spTransform(state_borders, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# INLETS
inlet_outline <- crop(state_borders, target_watershed)
inlet <- gDifference(target_watershed, inlet_outline)
inlet <- as(inlet, "SpatialPolygonsDataFrame" )
setwd(paste(wd, "/shapefiles/isolatedInlets", sep=""))
writeOGR(inlet, dsn = '.', layer = paste(river_name, "_inlet", sep=""), driver = "ESRI Shapefile", overwrite_layer=TRUE)

# COASTLINE
# data from https://www.sciencebase.gov/catalog/item/581d051ce4b08da350d523ba
coastline <- readOGR(paste(wd, "./shapefiles/coastline/coastll010g.shp", sep=""))
coastline <- spTransform(coastline, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# STATE CAPITOLS
# data from https://github.com/jasperdebie/VisInfo/blob/master/us-state-capitals.csv
setwd(wd)
capitols <- read.csv("./data/stateCapitols.csv", header=TRUE)


###############################################################################################################################
# PLOT MAP

# get extent of target watershed
extents <- extent(target_watershed)
# get x and y min and max values
# may need to adjust these manually
ymax <- ceiling(extents@ymax)
ymin <- floor(extents@ymin)
xmax <- ceiling(extents@xmax)
xmin <- floor(extents@xmin) - 2

# set up data for ggplot
world <- ne_countries(scale='large',returnclass = 'sf')
us_states <- ne_states(country="United States of America", returnclass = 'sf')
capitols_subset <- capitols[which(capitols$lat < ymax & capitols$lat > ymin &
                                    capitols$long < xmax & capitols$long > xmin),]
inlet_sf <- st_as_sf(inlet)
inlet_sf_crop <- st_crop(inlet_sf, xmin = xmin, xmax = xmax,
                         ymin = ymin, ymax = ymax)
state_borders_sf <- st_as_sf(state_borders)
state_borders_sf_crop <- st_crop(state_borders_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
coastline_sf <- st_as_sf(coastline)
coastline_sf_crop <- st_crop(coastline_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
state_borders_sf <- st_as_sf(state_borders)
state_borders_sf_crop <- st_crop(state_borders_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
target_wbs_swamps_sf <- st_as_sf(target_wbs_swamps)
target_wbs_swamps_sf_crop <- st_crop(target_wbs_swamps_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
target_wbs_lakes_sf <- st_as_sf(target_wbs_lakes)
target_wbs_lakes_sf_crop <- st_crop(target_wbs_lakes_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
target_watershed_sf <- st_as_sf(target_watershed)
target_watershed_sf_crop <- st_crop(target_watershed_sf, xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax)
rivers_sf <- st_as_sf(rivers)
rivers_sf_crop <- st_crop(rivers_sf, xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax)
target_river_sf <- st_as_sf(target_river)
target_river_sf_crop <- st_crop(target_river_sf, xmin = xmin, xmax = xmax,
                          ymin = ymin, ymax = ymax)

# replace negative stream order values with 1
target_river_sf_crop$Strahler[target_river_sf_crop$Strahler<0] <- 1

# inset plot
gworld <- ggplot(data = world) +
  geom_sf(fill="black", color="white") +
  geom_sf(data=us_states, fill="black", color="white") +
  geom_rect(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
            fill = NA, colour = "red", size = 1.2) +
  coord_sf(xlim = c(-130, -65), ylim = c(25, 55), expand = FALSE, datum=NA) +
  theme(panel.border = element_rect(colour = "white", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(), 
        panel.background = element_rect(fill = "#1c5163"),
        plot.background = element_rect(fill = "transparent", color = NA))
gworld_ratio <- (xmax - xmin) / (ymax - (ymin))

# main plot
p <- ggplot() +
  geom_sf(data=state_borders_sf_crop, fill="black", color="white") +
  geom_sf(data=rivers_sf_crop, color="#565656", lineend = "round") +
  geom_sf(data=state_borders_sf_crop, fill=NA, color="white") +
  #geom_sf(data=coastline_sf_crop, fill=NA, color="black") +
  geom_sf(data=target_watershed_sf_crop, fill="#3e3e3e", color=NA) +
  geom_point(data = capitols_subset, aes(y=lat, x=long), pch="\u2605", size=5, color="white") +
  geom_label_repel(data = capitols_subset, aes(y=lat, x=long, label=paste(city, ", ", state, sep="")),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'white', show.legend = FALSE) + 
  geom_sf(data=target_wbs_swamps_sf_crop, fill=alpha("#82eefd", 0.3), color=NA) +
  geom_sf(data=inlet_sf_crop, fill="#82eefd", color=NA) +
  geom_sf(data=target_wbs_lakes_sf_crop, fill="#82eefd", color=NA) +
  geom_sf(data=target_river_sf_crop, aes(size=factor(Strahler)), color="#82eefd", 
          show.legend = FALSE, lineend = "round") +
  scale_size_manual(values=seq(0.5,2,by=0.1)) +
  annotation_scale(location = "bl", width_hint = 0.2, unit_category="imperial", text_col="white") + # adjust scale location as needed
  annotation_scale(location = "tr", width_hint = 0.2, unit_category="metric", text_col="white") + # adjust scale location as needed
  annotation_north_arrow(location = "tr", which_north = "true", # adjust arrow location as needed
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = paste(river_name_plot, "River Basin")) +
  coord_equal() +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) + 
  ylab("") +
  xlab("") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "#1c5163"),
        plot.title = element_text(color = "black", size = 16, hjust = 0.5))

# adjust x and y position of inset plot
xpos = 0.16
ypos = 0.2
ggdraw(p) +   
  draw_plot(gworld, width = 0.26, height = 0.26 * 10/6 * gworld_ratio, x = xpos, y = ypos) 

# plot dimensions
height = (ymax-ymin)*1.5
width = (xmax-xmin)*1.4 
# save as png
ggsave(paste(wd, "maps/", river_name, ".png", sep=""), units = "in", dpi=900, height=height, width=width)
ggsave(paste(wd, "maps/", river_name, ".pdf", sep=""), height=height, width=width)


# calculate area of watershed square km
raster::area(target_watershed) / 1000000 
# calculate area of watershed in square miles
(raster::area(target_watershed) / 1000000) * 0.386102



###############################################################################################################################
# OLD CODE using base plot

png(file=paste(wd, "maps/", river_name, "_base.png", sep=""), units = "in", res=600, height=height, width=width)

# set base map
maps::map(database = "world", xlim=c(xmin, xmax), ylim=c(ymin, ymax), fill = FALSE)
# background rectangle
rect(xmin, ymin, xmax, ymax, col = "#1c5163")
# states
plot(state_borders, add = TRUE, col="black", border="white")
# all rivers
plot(rivers, add = TRUE, col = "#565656")
# states
plot(state_borders, add = TRUE, col=NA, border="white")
# coastline
plot(coastline, add = TRUE, col="black")
# watershed
plot(target_watershed, add = TRUE, col="#3e3e3e", border = NA)

# state capitols
points(y=capitols$lat, x=capitols$long, pch=8, cex=2, col="white")
points(y=capitols$lat, x=capitols$long, pch=1, cex=2.1, col="white")

# swamps
plot(target_wbs_swamps, add = TRUE, col=alpha("#82eefd", 0.3), border = NA)
# inlet
plot(inlet, add = TRUE, col="#82eefd", border = NA)
# lakes
plot(target_wbs_lakes, add = TRUE, col="#82eefd", border = NA)
# target river
# specify line thicknesses for different order rivers
lwds <- seq(1,5,by=0.3)
# replace negative stream order values with 1
target_river$Strahler[target_river$Strahler<0] <- 1
plot(target_river, add = TRUE, col = "#82eefd", lwd=lwds[target_river$Strahler])


# add scale bar
maps::map.scale(x = xmax - 0.5, y = ymax - 0.5, units="km", ratio = TRUE)
# add north arrow
north.arrow(xmax - 0.5, ymax - 0.7, len = 0.025, lab = "N")

# add axes
box()
axis(side = 1)
axis(side = 2)

dev.off()



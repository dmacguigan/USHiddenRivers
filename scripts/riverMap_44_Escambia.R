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
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(geomtextpath)

##############################################################################################################################
# specify the following parameters
HUCs <- c("031403") # top-level vector of HUCs to match (must be strings, not numeric)
# search for HUCs here: https://water.usgs.gov/wsc/map_index.html
river_name <- "Escambia" # river name for files
river_name_plot <- "Escambia" # river name for plot title
wd <- "H:/USHiddenRivers/" # top level working directory

setwd(wd)

##############################################################################################################################
# READ DATA and PARSE shapefiles

# BASINS
# load shapefile for US watersheds (HUC 10 level)
# data from https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
# will need to use external GIS platform such as QGIS to extract HUC 10 watersheds from database
watersheds <- readOGR(paste(wd, "/shapefiles/watersheds/WBD_HU10_watersheds.shp", sep=""))
watersheds <- spTransform(watersheds, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
#huc10s <- as.character(target_watershed$huc10)
#target_watershed <- target_watershed[-which(as.logical(rowSums(sapply(c("0310010304","0310010303"), startsWith, x=huc10s)))),] # remove outer harbor
target_watershed <- aggregate(target_watershed, dissolve=T)
# crop watershed at coastline using low resolution state borders shapefile
# data from https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_5m.zip
state_borders_lowres <- readOGR(paste(wd, "/shapefiles/stateBorders_lowRes/cb_2018_us_division_5m.shp", sep=""))
state_borders_lowres <- spTransform(state_borders_lowres, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#target_watershed <- crop(target_watershed, state_borders_lowres)
target_watershed <- as(target_watershed, "SpatialPolygonsDataFrame" )
setwd(paste(wd, "./shapefiles/isolatedWatersheds", sep=""))
writeOGR(target_watershed, dsn = '.', layer = paste(river_name, "_watershed", sep=""), driver = "ESRI Shapefile", overwrite_layer=TRUE)

# RIVERS
# load shapefile for all US streams at 1 million scale
# data from https://nationalmap.gov/small_scale/mld/1strmsl.html
rivers <- readOGR(paste(wd, "/shapefiles/allRivers/streaml010g.shp", sep=""))
rivers <- spTransform(rivers, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# remove intercoastal waterway
feat <- as.character(rivers@data$Feature)
rivers <- rivers[which(!as.logical(rowSums(sapply(c("Intracoastal Waterway"), grepl, x=feat, fixed=TRUE)))),]
reachCodes <- as.character(rivers$ReachCode)
target_rivers_ws <- rivers[which(as.logical(rowSums(sapply(HUCs, startsWith, x=reachCodes)))),]
target_rivers_ws <- as(target_rivers_ws, "SpatialLinesDataFrame" )
# crop rivers by watershed boundary
#in_ws_rivers <- over(rivers, target_watershed)
#target_rivers_ws <- rivers[!is.na(in_ws_rivers$dummy),]
setwd(paste(wd, "./shapefiles/isolatedRivers", sep=""))
writeOGR(target_rivers_ws, dsn = '.', layer = paste(river_name, "_river", sep=""), driver = "ESRI Shapefile", overwrite_layer=TRUE)

# WATERBODIES
# read shapefile
# data from https://maps.princeton.edu/catalog/stanford-sv709xw7113
wbs <- readOGR(paste(wd, "/shapefiles/waterbodies/wtrbdyp010g.shp", sep=""))
wbs <- spTransform(wbs, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# lakes only
feature <- as.character(wbs$Feature)
target_wbs_lakes_all <- wbs[which(as.logical(rowSums(sapply(c("Lake", "Reservoir"), grepl, x=feature, fixed=TRUE)))),]
feature <- as.character(target_wbs_lakes_all$Feature)
target_wbs_lakes_all <- target_wbs_lakes_all[which(!as.logical(rowSums(sapply(c("Lake Dry"), grepl, x=feature, fixed=TRUE)))),]
in_ws_lakes <- over(target_wbs_lakes_all, target_watershed)
target_wbs_lakes <- target_wbs_lakes_all[!is.na(in_ws_lakes$dummy),]
target_wbs_lakes <- aggregate(target_wbs_lakes, dissolve=T)
# below code fixes some bad polygons in this shapefile
# simplify the polgons a tad (tweak 0.00001 to your liking)
target_wbs_lakes <- gSimplify(target_wbs_lakes, tol = 0.00001)
target_wbs_lakes <- as(target_wbs_lakes, "SpatialPolygonsDataFrame" )
setwd(paste(wd, "/shapefiles/isolatedWaterbodies", sep=""))
writeOGR(target_wbs_lakes, dsn = '.', layer = paste(river_name, "_waterbodies_lakes", sep=""), driver = "ESRI Shapefile", overwrite_layer=TRUE)
# swamps only
feature <- as.character(wbs$Feature)
all_wbs_swamps <- wbs[which(as.logical(rowSums(sapply(c("Swamp or Marsh"), grepl, x=feature, fixed=TRUE)))),]
# below code fixes some bad polygons in this shapefile
# simplify the polgons a tad (tweak 0.00001 to your liking)
all_wbs_swamps <- gSimplify(all_wbs_swamps, tol = 0.00001)
all_wbs_swamps <- aggregate(all_wbs_swamps, dissolve=T)
all_wbs_swamps <- gDifference(all_wbs_swamps, target_watershed)

target_wbs_swamps <- wbs[which(as.logical(rowSums(sapply(c("Swamp or Marsh"), grepl, x=feature, fixed=TRUE)))),]
in_ws_swamps <- over(target_wbs_swamps, target_watershed)
target_wbs_swamps <- target_wbs_swamps[!is.na(in_ws_swamps$dummy),]
target_wbs_swamps <- aggregate(target_wbs_swamps, dissolve=T)
# below code fixes some bad polygons in this shapefile
# simplify the polgons a tad (tweak 0.00001 to your liking)
target_wbs_swamps <- gSimplify(target_wbs_swamps, tol = 0.00001)
target_wbs_swamps <- as(target_wbs_swamps, "SpatialPolygonsDataFrame")
setwd(paste(wd, "/shapefiles/isolatedWaterbodies", sep=""))
writeOGR(target_wbs_swamps, dsn = '.', layer = paste(river_name, "_waterbodies_swamps", sep=""), driver = "ESRI Shapefile", overwrite_layer=TRUE)
# below code fixes some bad polygons in this shapefile
# simplify the polgons a tad (tweak 0.00001 to your liking)
target_wbs_lakes_all <- gSimplify(target_wbs_lakes_all, tol = 0.00001)

# STATE BORDERS
# read in shape border shapefile
# data from https://github.com/jasperdebie/VisInfo/blob/master/us-state-capitals.csv
state_borders <- readOGR(paste(wd, "/shapefiles/stateBorders/statesp010g.shp", sep=""))
state_borders <- spTransform(state_borders, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# below code fixes some bad polygons in this shapefile
# simplify the polgons a tad (tweak 0.00001 to your liking)
state_borders <- gSimplify(state_borders, tol = 0.00001)
# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
state_borders <- gBuffer(state_borders, byid=TRUE, width=0)
# any bad polys?
sum(gIsValid(state_borders, byid=TRUE)==FALSE)

# INLETS
inlet_outline <- crop(state_borders, target_watershed)
inlet <- gDifference(target_watershed, inlet_outline)
inlet <- as(inlet, "SpatialPolygonsDataFrame" )
inlet <- gSimplify(inlet, tol = 0.00001)
inlet <- gBuffer(inlet, byid=TRUE, width=0)
inlet <- as(inlet, "SpatialPolygonsDataFrame" )
setwd(paste(wd, "/shapefiles/isolatedInlets", sep=""))
writeOGR(inlet, dsn = '.', layer = paste(river_name, "_inlet", sep=""), driver = "ESRI Shapefile", overwrite_layer=TRUE)

# INLAND BORDERS
# data from https://www.sciencebase.gov/catalog/item/4fb555ebe4b04cb937751db9
# modified with QGIS to remove coastal borders
landBorders <- readOGR(paste(wd, "/shapefiles/PoliticalBoundaries_Shapefiles/boundary_l_v2.shp", sep=""))
landBorders <- spTransform(landBorders, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
landBorders <- gSimplify(landBorders, tol = 0.00001)

# CITIES
# data from https://github.com/jasperdebie/VisInfo/blob/master/us-state-capitals.csv
# and https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population
cities <- read.csv(paste(wd, "/data/majorCities.csv", sep=""), header=TRUE)

# RASTER TOPO DATA 
# from https://www.naturalearthdata.com/downloads/10m-raster-data/10m-shaded-relief/
topo <- raster(paste(wd, "/shapefiles/shadedRelief/GRAY_HR_SR_OB.tif", sep=""))


###############################################################################################################################
# PLOT MAP

# get extent of target watershed
extents <- extent(target_watershed)
# get x and y min and max values
# may need to adjust these manually
ymax <- ceiling(extents@ymax) - 0.7
extents@ymax <- ymax
ymin <- floor(extents@ymin) 
extents@ymin <- ymin
xmax <- ceiling(extents@xmax) - 0.5
extents@xmax <-  xmax 
xmin <- floor(extents@xmin) 
extents@xmin <- xmin 

# set up data for ggplot
world <- ne_countries(scale='large',returnclass = 'sf')
us_states <- ne_states(country="United States of America", returnclass = 'sf')
cities_subset <- cities[which(cities$lat < ymax & cities$lat > ymin &
                                  cities$long < xmax & cities$long > xmin),]
cities_subset <- cities_subset[1,]
cities_subset[1,] <- c("FL", "Pensacola", 30.4213, -87.2169)
cities_subset[,3] <- as.numeric(cities_subset[,3])
cities_subset[,4] <- as.numeric(cities_subset[,4])

inlet_sf <- st_as_sf(inlet)
#inlet_sf_crop <- st_crop(inlet_sf, xmin = xmin, xmax = xmax,
#                         ymin = ymin, ymax = ymax) 
landBorders_sf <- st_as_sf(landBorders)
landBorders_sf_crop <- st_crop(landBorders_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
state_borders_sf <- st_as_sf(state_borders)
state_borders_sf_crop <- st_crop(state_borders_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
target_wbs_swamps_crop <- raster::crop(target_wbs_swamps, target_watershed)
target_wbs_swamps_sf <- st_as_sf(target_wbs_swamps_crop)
target_wbs_swamps_sf_crop <- st_crop(target_wbs_swamps_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
target_wbs_lakes_sf <- st_as_sf(target_wbs_lakes)
target_wbs_lakes_sf_crop <- st_crop(target_wbs_lakes_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
target_wbs_lakes_all_sf <- st_as_sf(target_wbs_lakes_all)
target_wbs_lakes_all_sf_crop <- st_crop(target_wbs_lakes_all_sf, xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax)
target_watershed_sf <- st_as_sf(target_watershed)
target_watershed_sf_crop <- st_crop(target_watershed_sf, xmin = xmin,  xmax = xmax,
                                    ymin = ymin, ymax = ymax) 
rivers_sf <- st_as_sf(rivers)
rivers_sf_crop <- st_crop(rivers_sf, xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax)
target_rivers_ws_sf <- st_as_sf(target_rivers_ws)
target_rivers_ws_sf_crop <- st_crop(target_rivers_ws_sf, xmin = xmin, xmax = xmax,
                          ymin = ymin, ymax = ymax)
topo_crop <- crop(topo, extents)
topo_crop_df <- as.data.frame(topo_crop, xy = TRUE)
sf::sf_use_s2(FALSE)
all_wbs_swamps_sf <- st_as_sf(all_wbs_swamps)
all_wbs_swamps_sf_crop <- st_crop(all_wbs_swamps_sf, xmin = xmin, xmax = xmax,
                                  ymin = ymin, ymax = ymax)

# replace negative stream order values with 1
rivers_sf_crop$Strahler[rivers_sf_crop$Strahler<0] <- 1
target_rivers_ws_sf_crop$Strahler[target_rivers_ws_sf_crop$Strahler<0] <- 1

# inset plot
gworld <- ggplot(data = world) +
  geom_sf(fill="gray20", color="white", size=0.2) +
  geom_sf(data=us_states, fill="gray20", color="white", size=0.2) +
  geom_rect(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
            fill = NA, colour = "red", size = 0.6) +
  coord_sf(xlim = c(-130, -65), ylim = c(25, 55), expand = FALSE, datum=NA) +
  theme(panel.border = element_rect(colour = "gray60", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(), 
        panel.background = element_rect(fill = "#1c5163"),
        plot.background = element_rect(fill = "transparent", color = NA))
gworld_ratio <- (xmax - xmin) / (ymax - (ymin))


# main plot
p <- ggplot() +
  geom_sf(data=state_borders_sf_crop, fill="black", color=NA) +
  geom_raster(topo_crop_df, mapping = aes(x = x, y = y, alpha = GRAY_HR_SR_OB)) +
  scale_alpha(range =  c(0, 1), guide = "none") +
  geom_sf(data=target_watershed_sf_crop, fill=alpha("white", 0.15), color=NA) +
  geom_sf(data=all_wbs_swamps_sf_crop , fill="#002b0a", color=NA) +
  geom_sf(data=rivers_sf_crop, color="#565656", lineend = "round", aes(size=factor(Strahler)), show.legend = FALSE) +
  geom_sf(data=target_wbs_lakes_all_sf_crop, fill="#565656", color=NA) +
  geom_sf(data=landBorders_sf_crop, fill=NA, color="white", size=1) +
  geom_sf(data=target_wbs_swamps_sf_crop, fill=alpha("palegreen", 0.3), color=NA) +
  geom_sf(data=inlet_sf, fill="#82eefd", color=NA)  +
  geom_sf(data=target_wbs_lakes_sf_crop, fill="#82eefd", color=NA) +
  geom_sf(data=target_rivers_ws_sf_crop, aes(size=factor(Strahler)), color="#82eefd",
           show.legend = FALSE, lineend = "round") +
  geom_point(data = cities_subset, aes(y=lat, x=long), pch=21, size=1, stroke=2, color="red", fill="black") +
  geom_label_repel(data = cities_subset[1,], aes(y=lat, x=long, label=paste(city,state,sep=",")),
                   box.padding   = 0.35, size = 3, alpha=0.9, segment.alpha=1, point.padding = 0.5,
                   ylim=c(30, 30.4), 
                   xlim=c(-88, -87.5),
                   segment.color = 'red', show.legend = FALSE) +
  scale_size_manual(values=seq(0.25,5,by=0.12)) +
  annotation_scale(location = "tl", width_hint = 0.2, unit_category="imperial", text_col="white", line_col="white") +  # adjust scale location as needed
  annotation_scale(location = "tr", width_hint = 0.2, unit_category="metric",  text_col="white", line_col="white") +  # adjust scale location as needed
  annotation_north_arrow(location = "tl", which_north = "true", # adjust arrow location as needed
                          pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"), 
                          style = north_arrow_fancy_orienteering(text_col = "white", fill=c("darkgray", "white"), line_col="white"),
                          height = unit(1, "cm"), width = unit(1, "cm")) +
  annotate("text", x=-86.6, y=31.03, label="Alabama", color="white", hjust=0.5, vjust=0.5, size=3, angle=0) +
  annotate("text", x=-86.6, y=30.97, label="Florida", color="white", hjust=0.5, vjust=0.5, size=3, angle=0) +
  labs(title = paste(river_name_plot, "River Basin")) +
  coord_sf(expand = FALSE, xlim = c(xmin, xmax), ylim=c(ymin,ymax)) + 
  ylab("") +
  xlab("") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.7),
       panel.background = element_rect(fill = "#1c5163"),
       plot.title = element_text(color = "black", size = 16, hjust = 0.5))

# adjust x and y position and size of inset plot
xpos = 0.68
ypos = -0.07
size = 0.24
p2 <- ggdraw(p) + 
  draw_plot(gworld, width = size, height = size * 10/6 * gworld_ratio, 
            x = xpos, y = ypos, hjust=0, vjust=0) 

# plot dimensions
# mess with the multipliers to get a nice figure
height = (ymax-ymin)*3
width = (xmax-xmin)*3
# save as png
ggsave(plot=p2, paste(wd, "maps/", river_name, ".png", sep=""), units = "in", dpi=600, height=height, width=width)
ggsave(plot=p2, paste(wd, "maps/", river_name, ".pdf", sep=""), height=height, width=width)

# calculate area of watershed square km
raster::area(target_watershed) / 1000000 
# [1] 10994.81

# calculate area of watershed in square miles
(raster::area(target_watershed) / 1000000) * 0.386102
# [1] 4245.12



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
library(stringr)

wd <- "H:/USHiddenRivers/" # top level working directory
setwd(wd)


##############################################################################################################################
# READ data for isolated rivers and watersheds
riverFiles <- list.files(path="./shapefiles/isolatedRivers/", pattern="*.shp")
wsFiles <- list.files(path="./shapefiles/isolatedWatersheds/", pattern="*.shp")
lakesFiles <- list.files(path="./shapefiles/isolatedWaterbodies/", pattern="*_lakes.shp")
inletFiles <- list.files(path="./shapefiles/isolatedInlets/", pattern="*_inlet.shp")

# drop some rivers
riverFiles <- riverFiles[-c(3,5,6,21)]
wsFiles <- wsFiles[-c(3,5,6,21)]
lakeFiles <- lakesFiles[-c(3,5,6,8,11,19,22,23)]
inletFiles <- inletFiles[-c(2,4,5,7,17,20,21,34)]

# read each file into a list
targetRivers <- list()
targetWS <- list()
for(i in 1:length(riverFiles)){
  targetRivers[[i]] <- readOGR(paste(wd, "/shapefiles/isolatedRivers/", riverFiles[[i]], sep=""))
  targetRivers[[i]]@data$target <- rep(str_replace(riverFiles[i], '.shp', ''), length(targetRivers[[i]]@data[,1])) 
  targetWS[[i]] <- readOGR(paste(wd, "/shapefiles/isolatedWatersheds/", wsFiles[[i]], sep=""))
  targetWS[[i]]@data$target <- c(str_replace(wsFiles[i], '.shp', '')) 
}
targetLakes <- list()
for(i in 1:length(lakeFiles)){
  targetLakes[[i]] <- readOGR(paste(wd, "/shapefiles/isolatedWaterbodies/", lakeFiles[[i]], sep=""))
  targetLakes[[i]]@data$target <- c(str_replace(lakeFiles[i], '.shp', '')) 
}
targetInlets <- list()
for(i in 1:length(inletFiles)){
  targetInlets[[i]] <- readOGR(paste(wd, "/shapefiles/isolatedInlets/", inletFiles[[i]], sep=""))
  targetInlets[[i]]@data$target <- c(str_replace(inletFiles[i], '.shp', '')) 
}

# remove stream order values <4 for St. John river
# just makes the plot less dense
targetRivers[[25]] <- targetRivers[[25]][targetRivers[[25]]$strmOrder>2,]
targetRivers[[25]]$strmOrder <- as.numeric(targetRivers[[25]]$strmOrder) - 2
targetRivers[[25]]$Strahler <- targetRivers[[25]]$strmOrder 

# merge shapefiles
targetRivers_merged <- do.call(bind, targetRivers)
targetWS_merged <- do.call(bind, targetWS)
targetLakes_merged <- do.call(bind, targetLakes)
targetInlets_merged <- do.call(bind, targetInlets)

# fix projections
targetInlets_merged <- spTransform(targetInlets_merged, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# below code fixes some bad polygons in this shapefile
# simplify the polgons a tad (tweak 0.00001 to your liking)
targetInlets_merged <- gSimplify(targetInlets_merged, tol = 0.00001)
# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
#targetInlets_merged <- gBuffer(targetInlets_merged, byid=TRUE, width=0)

##############################################################################################################################
# READ DATA and PARSE shapefiles

# US RIVERS
# load shapefile for all US streams at 1 million scale
# data from https://nationalmap.gov/small_scale/mld/1strmsl.html
rivers_US <- readOGR(paste(wd, "/shapefiles/allRivers/streaml010g.shp", sep=""))
rivers_US <- spTransform(rivers_US, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# remove intercoastal waterway
feat <- as.character(rivers_US@data$Feature)
rivers_US <- rivers_US[which(!as.logical(rowSums(sapply(c("Intracoastal Waterway"), grepl, x=feat, fixed=TRUE)))),]

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

# INLAND BORDERS
# data from https://www.sciencebase.gov/catalog/item/4fb555ebe4b04cb937751db9
# modified with QGIS to remove coastal borders
landBorders <- readOGR(paste(wd, "/shapefiles/PoliticalBoundaries_Shapefiles/boundary_l_v2.shp", sep=""))
landBorders <- spTransform(landBorders, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
landBorders <- gSimplify(landBorders, tol = 0.00001)

# basin labels
drainages <- read.csv(paste(wd, "/data/eastCoastRivers.csv", sep=""), header=TRUE)
# split draianges for plotting labels
drainages_1 <- drainages[drainages$lat<34.5,]
drainages_2 <- drainages[drainages$lat>34.5 & drainages$lat<39,]
drainages_3 <- drainages[drainages$lat>39 & drainages$lat<42,]
drainages_4 <- drainages[drainages$lat>42 & drainages$lat<48,]

# RASTER TOPO DATA 
# from https://www.naturalearthdata.com/downloads/10m-raster-data/10m-shaded-relief/
topo <- raster(paste(wd, "/shapefiles/shadedRelief/GRAY_HR_SR_OB.tif", sep=""))

# CANADA BORDERS
# data from https://open.canada.ca/data/en/dataset/306e5004-534b-4110-9feb-58e3a5c3fd97
canada_borders <- readOGR(paste(wd, "/shapefiles/canadaBorders_2016census/lpr_000b16a_e.shp", sep=""))
canada_borders <- spTransform(canada_borders, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# below code fixes some bad polygons in this shapefile
# simplify the polgons a tad (tweak 0.00001 to your liking)
canada_borders <- gSimplify(canada_borders, tol = 0.00001)

# CANADA ISLANDS
# data from https://open.canada.ca/data/en/dataset/80aa8ec6-4947-48de-bc9c-7d09d48b4cad
canada_islands <- readOGR(paste(wd, "/shapefiles/canvec_1M_CA_Land_shp/canvec_1M_CA_Land/island_2.shp", sep=""))
canada_islands <- spTransform(canada_islands, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# below code fixes some bad polygons in this shapefile
# simplify the polgons a tad (tweak 0.00001 to your liking)
canada_islands <- gSimplify(canada_islands, tol = 0.00001)

# CANADA LAKES
canada_lakes <- readOGR(paste(wd, "/shapefiles/canvec_water_1M/waterbody_2.shp", sep=""))
canada_lakes <- spTransform(canada_lakes, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctry <- as.character(canada_lakes@data$ctry_en)
canada_lakes_can <- canada_lakes[which(as.logical(rowSums(sapply(c("Canada"), grepl, x=ctry, fixed=TRUE)))),]
# below code fixes some bad polygons in this shapefile
# simplify the polgons a tad (tweak 0.00001 to your liking)
canada_lakes_can <- gSimplify(canada_lakes_can, tol = 0.00001)
# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
canada_lakes_can <- gBuffer(canada_lakes_can, byid=TRUE, width=0)

# CANADA RIVERS
canada_rivers <- readOGR(paste(wd, "/shapefiles/canvec_water_1M/watercourse_1.shp", sep=""))
canada_rivers <- spTransform(canada_rivers, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctry <- as.character(canada_rivers@data$ctry_en)
canada_rivers_can <- canada_rivers[which(as.logical(rowSums(sapply(c("Canada"), grepl, x=ctry, fixed=TRUE)))),]


###############################################################################################################################
# PLOT MAP

# get extent of target watershed
extents <- extent(targetWS_merged)
# get x and y min and max values
# may need to adjust these manually
ymax <- ceiling(extents@ymax)  
extents@ymax <- ymax
ymin <- floor(extents@ymin) - 0.5
extents@ymin <- ymin
xmax <- ceiling(extents@xmax)
extents@xmax <-  xmax 
xmin <- floor(extents@xmin) - 0.5
extents@xmin <- xmin 

# set up data for ggplot
sf::sf_use_s2(FALSE)

landBorders_sf <- st_as_sf(landBorders)
landBorders_sf_crop <- st_crop(landBorders_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
state_borders_sf <- st_as_sf(state_borders)
state_borders_sf_crop <- st_crop(state_borders_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
targetLakes_merged_sf <- st_as_sf(targetLakes_merged)
targetLakes_merged_sf_crop <- st_crop(targetLakes_merged_sf, xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax)
targetInlets_merged_sf <- st_as_sf(targetInlets_merged)
targetInlets_merged_sf_crop <- st_crop(targetInlets_merged_sf, xmin = xmin, xmax = xmax,
                                      ymin = ymin, ymax = ymax)
targetWS_merged_sf <- st_as_sf(targetWS_merged)
targetWS_merged_sf_crop <- st_crop(targetWS_merged_sf, xmin = xmin,  xmax = xmax,
                                    ymin = ymin, ymax = ymax) 
targetWS_merged_sf_crop$target_fac <- factor(targetWS_merged_sf_crop$target, levels=drainages$ws)
rivers_sf <- st_as_sf(rivers_US)
rivers_sf_crop <- st_crop(rivers_sf, xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax)
targetRivers_merged_sf <- st_as_sf(targetRivers_merged)
targetRivers_merged_sf_crop <- st_crop(targetRivers_merged_sf, xmin = xmin, xmax = xmax,
                          ymin = ymin, ymax = ymax)


sf::sf_use_s2(TRUE)
canada_borders_sf <- st_as_sf(canada_borders)
canada_borders_sf_crop <- st_crop(canada_borders_sf, xmin = xmin, xmax = xmax,
                                  ymin = ymin, ymax = ymax)
canada_islands_sf <- st_as_sf(canada_islands)
canada_islands_sf_crop <- st_crop(canada_islands_sf, xmin = xmin, xmax = xmax,
                                  ymin = ymin, ymax = ymax)
# drop a problematic lake with spherical geometry
canada_lakes_can <- canada_lakes_can[-2312]
canada_lakes_can_sf <- st_as_sf(canada_lakes_can)
canada_lakes_can_sf_crop <- st_crop(canada_lakes_can_sf, xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax)
canada_rivers_can_sf <- st_as_sf(canada_rivers_can)
canada_rivers_can_sf_crop <- st_crop(canada_rivers_can_sf, xmin = xmin, xmax = xmax,
                                     ymin = ymin, ymax = ymax)
topo_crop <- crop(topo, extents)
topo_crop_df <- as.data.frame(topo_crop, xy = TRUE)
target_wbs_lakes_all_sf <- st_as_sf(target_wbs_lakes_all)
target_wbs_lakes_all_sf_crop <- st_crop(target_wbs_lakes_all_sf, xmin = xmin, xmax = xmax,
                                        ymin = ymin, ymax = ymax)

# replace negative stream order values with 1
targetRivers_merged_sf_crop$Strahler[targetRivers_merged_sf_crop$Strahler<0] <- 1

# main plot
p <- ggplot() +
  geom_sf(data=canada_borders_sf_crop, fill="black", color=NA) +
  geom_sf(data=state_borders_sf_crop, fill="black", color=NA) +
  #geom_raster(topo_crop_df, mapping = aes(x = x, y = y, alpha = GRAY_HR_SR_OB)) +
  #scale_alpha(range =  c(0, 1), guide = "none") +
  #geom_sf(data=targetWS_merged_sf_crop, fill=alpha("white", 0.15), color=NA) +
  scale_fill_manual(values=rep(c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d'), 10)) +
  geom_sf(data=canada_rivers_can_sf_crop, color="#565656", lineend = "round", size=0.25, show.legend = FALSE,) + #CHECK STRAHLER
  geom_sf(data=rivers_sf_crop, color="#565656", lineend = "round", aes(size=factor(Strahler)), show.legend = FALSE) +
  geom_sf(data=canada_lakes_can_sf_crop, fill="#565656", color=NA) +
  geom_sf(data=canada_islands_sf_crop, fill="black", color=NA) +
  geom_sf(data=target_wbs_lakes_all_sf_crop, fill="#565656", color=NA) +
  geom_sf(data=landBorders_sf_crop, fill=NA, color="white", size=0.8) +
  geom_sf(data=targetWS_merged_sf_crop, aes(fill=factor(target_fac)), alpha=0.5, color=NA, show.legend = FALSE,) +
  #geom_sf(data=targetInlets_merged_sf_crop, fill="#82eefd", color=NA) +
  geom_sf(data=targetInlets_merged_sf_crop, fill="gray90", color=NA) +
  geom_sf(data=targetLakes_merged_sf_crop, fill="gray90", color=NA) +
  geom_sf(data=targetRivers_merged_sf_crop, aes(size=factor(Strahler)), color="gray90",
           show.legend = FALSE, lineend = "round") +
  geom_label_repel(data = drainages_1, aes(y=lat, x=long, label=River),
                    box.padding   = 0.35, size = 5, alpha=0.9, segment.alpha=1, point.padding = 0.5,
                    xlim=c(-78.5, -65), 
                    ylim=c(20, 33.7),
                    segment.color = 'white', show.legend = FALSE,
                   arrow = arrow(length = unit(0.15, "cm"))) +
  geom_label_repel(data = drainages_2, aes(y=lat, x=long, label=River),
                   box.padding   = 0.35, size = 5, alpha=0.9, segment.alpha=1, point.padding = 0.5,
                   xlim=c(-75, -65), 
                   ylim=c(34, 38.2),
                   segment.color = 'white', show.legend = FALSE,
                   arrow = arrow(length = unit(0.15, "cm"))) +
  geom_label_repel(data = drainages_3, aes(y=lat, x=long, label=River),
                   box.padding   = 0.35, size = 5, alpha=0.9, segment.alpha=1, point.padding = 0.5,
                   xlim=c(-74, -65), 
                   ylim=c(38.2, 40.7),
                   force=15,
                   force_pull = 0,
                   max.time=5,
                   max.iter=1000000,
                   segment.color = 'white', show.legend = FALSE,
                   arrow = arrow(length = unit(0.15, "cm"))) +
  geom_label_repel(data = drainages_4, aes(y=lat, x=long, label=River),
                   box.padding   = 0.35, size = 5, alpha=0.9, segment.alpha=1, point.padding = 0.5,
                   xlim=c(-70, -65), 
                   ylim=c(40.6, 43.7),
                   segment.color = 'white', show.legend = FALSE,
                   force=10,
                   max.time=5,
                   max.iter=1000000,
                   arrow = arrow(length = unit(0.15, "cm"))) +
  scale_size_manual(values=seq(0.1,20,by=0.04)) +
  annotation_north_arrow(location = "br", which_north = "true", # adjust arrow location as needed
                        pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"), 
                        style = north_arrow_fancy_orienteering(text_size=20, text_col = "white", fill=c("darkgray", "white"), line_col="white"),
                        height = unit(2, "cm"), width = unit(2, "cm")) +
  annotate("text", x=-70, y=34, label="Atlantic Ocean", color="gray60", hjust=0.5, vjust=0.5, size=20, angle=50) +
  #labs(title = paste(river_name_plot, "River Basin")) +
  #coord_equal() +
  coord_sf(expand = FALSE, xlim=c(xmin, xmax), ylim=c(ymin, ymax)) + 
  ylab("") +
  xlab("") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.7),
       panel.background = element_rect(fill = "#1c5163"),
       plot.title = element_text(color = "black", size = 16, hjust = 0.5))


# plot dimensions
# mess with the multipliers to get a nice figure
height = (ymax-ymin)*0.7
width = (xmax-xmin)*0.6
# save as png
ggsave(paste(wd, "maps/", "eastCoastRivers", ".png", sep=""), plot=p,  units = "in", dpi=900, height=height, width=width)
#ggsave(paste(wd, "maps/", river_name, ".pdf", sep=""), height=height, width=width)




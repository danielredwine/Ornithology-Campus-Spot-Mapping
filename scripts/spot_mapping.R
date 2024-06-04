# Daniel Redwine
# 4 June 2024

# This script aims to utilize campus spot mapping data to create maps

# Load libraries
library(OpenStreetMap) # To obtain base maps, if you get an error install 64 bit java
library(sf) # Used to encode spatial data
library(tidyverse) # Includes DPLYR, GGplot, TIDYR, etc...
library(raster) # To rasterize, analyze, and model spatial data
library(viridisLite) # Contains resources for colorblind friendly maps

# Load and clean the datasets, dataset should not require much cleaning
spotmap_data23 <- read.csv("data/2023data.csv") # load in dataset
spotmap_data23$lat<-as.numeric(spotmap_data23$lat) # Sets data type to numeric 
spotmap_data23$lon<-as.numeric(spotmap_data23$lon) # Would also remove non-numeric data
spotmap_data23$species<-as.factor(spotmap_data23$species) # Sets species as factor

# Set the extent (max/min lat/lon) and open the base map
# Uses the min and max latitude and longitude in the dataset and adds a small amount
static_base_map <- openmap(c(max(spotmap_data23$lat)+.0005, min(spotmap_data23$lon)-.0005), 
                          + c(min(spotmap_data23$lat)-.0005, 
                              max(spotmap_data23$lon)+.0005),type = "esri-topo")

# Types "osm", "bing", "esri","esri-topo", "esri-imagery", "esri-natgeo" 

# Project latitude and longitude onto the base map so lat/lon data can be plotted
base_map_projection <- openproj(static_base_map, projection = "+proj=longlat +datum=WGS84")

# Create the most basic type of map with data points only
point_plot_23 <- autoplot.OpenStreetMap(base_map_projection, expand = TRUE) + 
  geom_point(data = spotmap_data23, aes(lon, lat), size = 1) + # add points to map
  aes(color = species) + # color by factor species 
  theme(axis.title = element_text(face="bold")) + # bold the axis title
  labs(x="Longitude", y="Latitude") + # Labels
  guides(color=guide_legend("Species")) +  theme_bw(base_size = 8) # Legend

point_plot_23 # Call the object

ggsave("output/point_plot_23.png") # Save the object

## Make kernel density estimates
## Create SpatialPoionts object for the dataset
## CRS = coordinate reference system, +init=epsg:4326 is the correct code
spotmap_data23pts <- sp::SpatialPoints(coords = cbind(spotmap_data23$lon, 
                            spotmap_data23$lat), proj4string = sp::CRS("+init=epsg:4326"))

## put all the data into a single SpatialPointsDataFrame (spdf)
spotmap_data23_spdf <- sp::SpatialPointsDataFrame(spotmap_data23pts, spotmap_data23)
head(spotmap_data23_spdf)

## now we'll split the dataset by species
separate_species <- split(x = spotmap_data23_spdf,
                          f = spotmap_data23_spdf$species, drop = FALSE)

## kernel density estimation
## Step one: do least squares cross-validation to estimate bandwidth
bw <- lapply(separate_species, FUN = function(x){ks::Hlscv(x@coords)})


## Step two: generate kde
Species_kde <-mapply(separate_species,bw,
                     SIMPLIFY = FALSE,
                     FUN = function(x,y){
                       raster(kde(x@coords,h=y,))})

# This code makes a custom function called getContour. 
# Inputs:
#    kde = kernel density estimate
#    prob = probabily - default is 0.95

getContour <- function(kde, prob = 0.95){
  # set all values 0 to NA
  kde[kde == 0]<-NA
  # create a vector of raster values
  kde_values <- raster::getValues(kde)
  # sort values 
  sortedValues <- sort(kde_values[!is.na(kde_values)],decreasing = TRUE)
  # find cumulative sum up to ith location
  sums <- cumsum(as.numeric(sortedValues))
  # binary response is value in the probabily zone or not
  p <- sum(sums <= prob * sums[length(sums)])
  # Set values in raster to 1 or 0
  kdeprob <- raster::setValues(kde, kde_values >= sortedValues[p])
  # return new kde
  return(kdeprob)
}

all_95kde <- lapply(Species_kde,FUN = getContour, prob = 0.95)
## The numbers (1-4) refer to each species, listed alphabetically (from the list)
## These next plots put MCP on top of KDE

# make a raster extent object to set the bounds to the base map
# We can then use setExtent to set the extent of each of the KDE plots individually
# This way the KDE plots are the same size as the base map

extent <- extent(min(spotmap_data23$lon)-0.0025, max(spotmap_data23$lon)+0.0025, min(spotmap_data23$lat)-0.0025,
                 max(spotmap_data23$lat)+0.0025)

# in set extent the species_kde calls to the kde we made and extent calls to the extent object

hosp_kde <- setExtent(Species_kde[[3]], extent)
eust_kde <- setExtent(Species_kde[[2]], extent)
eabl_kde <- setExtent(Species_kde[[1]], extent)
nomo_kde <- setExtent(Species_kde[[4]], extent)

# EABL:

plot(eabl_kde)

# EUST
plot(eust_kde)

# HOSP
plot(hosp_kde)

# NOMO
plot(nomo_kde)

## can we put a KDE on top of a map?
## first a blank basemap:

base_raster <- openmap(c(max(spotmap_data23$lat)+0.0025, min(spotmap_data23$lon)-0.0025), + 
                         c(min(spotmap_data23$lat)-0.0025, max(spotmap_data23$lon)+0.0025), type = "osm")
base_raster_proj <- openproj(base_raster, projection = "+proj=longlat +datum=WGS84")

## B&W version? NOT WORKING
###base_rasterBW <- openmap(c(max(spotmap_data23$lat)+0.001, min(spotmap_data23$lon)-0.001), + #####3
###                         c(min(spotmap_data23$lat)-0.001, max(spotmap_data23$lon)+0.001), type = "waze")
####base_rasterBW_proj <- openproj(base_rasterBW, projection = "+proj=longlat +datum=WGS84")

## plot base map, then add kde of each species

##House Sparrow#
plot(base_raster_proj)
plot(hosp_kde, alpha = 0.5, add=TRUE, title("House Sparrow"), horizontal=TRUE)
##plot(hosp_mcp,add = TRUE)## this works!

#European Starling#
plot(base_raster_proj)
plot(eust_kde, alpha = 0.5, add=TRUE, title("European Starling"), horizontal=TRUE)
##plot(eust_mcp,add = TRUE)

#Eastern Bluebird#
plot(base_raster_proj)
plot(eabl_kde, alpha = 0.5, add=TRUE, title("Eastern Bluebird"), horizontal=TRUE)
##EABL_mcp<-plot(eabl_mcp,add = TRUE)

#Northern Mockingbird#
plot(base_raster_proj)
plot(nomo_kde, alpha = 0.5, add=TRUE, title ("Northern Mockingbird"), horizontal=TRUE)
##plot(nomo_mcp,add = TRUE)
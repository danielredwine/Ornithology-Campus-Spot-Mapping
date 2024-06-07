# Daniel Redwine
# 7 June 2024

# This script aims to utilize campus spot mapping data to create maps

# Clear environment
rm(list = ls())

# Load libraries
library(OpenStreetMap)  # To obtain base maps, install 64-bit Java if you get an error
library(sf)  # Used to encode spatial data
library(tidyverse)  # Includes DPLYR, GGplot, TIDYR, etc.
library(raster)  # To rasterize, analyze, and model spatial data

# Load and clean the datasets, dataset should not require much cleaning
spotmap_data <- read.csv("data/2023data.csv")  # load in dataset
spotmap_data$lat <- as.numeric(spotmap_data$lat)  # Sets data type to numeric 
spotmap_data$lon <- as.numeric(spotmap_data$lon)  # Would also remove non-numeric data
spotmap_data$species <- as.factor(spotmap_data$species)  # Sets species as factor

# Set the extent (max/min lat/lon) and open the base map
# Uses the min and max latitude and longitude in the dataset and adds a small amount
static_base_map <- openmap(c(max(spotmap_data$lat) + .0005, min(spotmap_data$lon) - .0005),
                           c(min(spotmap_data$lat) - .0005, 
                             max(spotmap_data$lon) + .0005), type = "esri-topo")

# Types "osm", "bing", "esri", "esri-topo", "esri-imagery", "esri-natgeo" 

# Project latitude and longitude onto the base map so lat/lon data can be plotted
base_map_projection <- openproj(static_base_map, projection = "+proj=longlat +datum=WGS84")

# Create the most basic type of map with data points only
# This map is messy and not informative
all_species_point <- autoplot.OpenStreetMap(base_map_projection, expand = TRUE) + 
  geom_point(data = spotmap_data, aes(lon, lat, color = species), size = 1) +  # add point
  theme(axis.title = element_text(face = "bold")) +  # bold the axis title
  labs(x = "Longitude", y = "Latitude") +  # Labels
  guides(color = guide_legend("Species")) +  theme_bw(base_size = 8)  # Legend

all_species_point  # Call the object

# Individually map species
# Create separate data for each species
eabl_data <- spotmap_data %>% 
  filter(species == "EABL")

hosp_data <- spotmap_data %>%
  filter(species == "HOSP")

nomo_data <- spotmap_data %>% 
  filter(species == "NOMO")

eust_data <- spotmap_data %>% 
  filter(species == "EUST")

# Create point maps for each species
# Point size can be changed to match the count for the observation
# EABL
eabl_point <- autoplot.OpenStreetMap(base_map_projection, expand = TRUE) + 
  geom_point(data = eabl_data, aes(lon, lat, size = num_ind), # point size mapped to count
             color = "royalblue4", alpha = 0.5) +  # add points to map
  theme(axis.title = element_text(face = "bold")) +  # bold the axis title
  labs(x = "Longitude", y = "Latitude") + # Labels
  guides(size = guide_legend("Count")) +  theme_bw(base_size = 8) + # Legend title
  scale_size_continuous(breaks = c(1,2,3)) # setting values to display in legend


eabl_point  # Call the object

ggsave("output/eabl_point.png")  # Save the object

# NOMO
nomo_point <- autoplot.OpenStreetMap(base_map_projection, expand = TRUE) + 
  geom_count(data = nomo_data, aes(lon, lat, size = num_ind), # size is the number of ind
             color = "honeydew4", alpha = 0.5) +  # add points to map
  theme(axis.title = element_text(face = "bold")) +  # bold the axis title
  labs(x = "Longitude", y = "Latitude") + # Labels
  guides(size = guide_legend("Count")) +  theme_bw(base_size = 8) + # Legend title
  scale_size_continuous(breaks = c(1,4,7)) # setting values to display in legend

nomo_point  # Call the object

ggsave("output/nomo_point.png")  # Save the object

# HOSP
hosp_point <- autoplot.OpenStreetMap(base_map_projection, expand = TRUE) + 
  geom_point(data = hosp_data, aes(lon, lat, size = num_ind), # size is the number of ind
             color = "sienna", alpha = 0.5) +  # add points to map
  theme(axis.title = element_text(face = "bold")) +  # bold the axis title
  labs(x = "Longitude", y = "Latitude") + # Labels
  guides(size = guide_legend("Count")) +  theme_bw(base_size = 8) + # Legend title
  scale_size_continuous(breaks = c(1,5,9)) # setting values to display in legend

hosp_point  # Call the object

ggsave("output/hosp_point.png")  # Save the object

# EUST
eust_point <- autoplot.OpenStreetMap(base_map_projection, expand = TRUE) + 
  geom_point(data = eust_data, aes(lon, lat, size = num_ind), # size is the number of ind
             color = "purple4", alpha = 0.5) +  # add points to map
  theme(axis.title = element_text(face = "bold")) +  # bold the axis title
  labs(x = "Longitude", y = "Latitude") + # Labels
  guides(size = guide_legend("Count")) +  theme_bw(base_size = 8) + # Legend title
  scale_size_continuous(breaks = c(1,15,30)) # setting values to display in legend

eust_point  # Call the object

ggsave("output/eust_point.png")  # Save the object

# the next bunch of stuff comes from 
# https://mhallwor.github.io/_pages/activities_GenerateTerritories
# goal is to create kernel density estimates with an approximation of density

# first make the data into a SpatialPoints object
# CRS = coordinate reference system, +init=epsg:4326 should be the correct code
spotmap_datapts <- sp::SpatialPoints(coords = cbind(spotmap_data$lon, spotmap_data$lat),
                                proj4string = sp::CRS("+init=epsg:4326"))

# put all the data into a single SpatialPointsDataFrame (spdf)
spotmap_data_spdf <- sp::SpatialPointsDataFrame(spotmap_datapts, spotmap_data)
head(spotmap_data_spdf)

# split the dataset by species
separate_species_spdf <- split(x = spotmap_data_spdf, f = spotmap_data_spdf$species, 
                               drop = FALSE)

# now to try kernel density estimation
# Step one: do least squares cross-validation to estimate bandwidth (bw)
# This basically means
bw <- lapply(separate_species_spdf, FUN = function(x){ks::Hlscv(x@coords)})


# Step two: generate kde
Species_kde <-mapply(separate_species_spdf,bw,
                     SIMPLIFY = FALSE,
                     FUN = function(x,y){
                       raster(kde(x@coords,h=y,))})

# This code calculates which areas account for 95% of density
# Inputs:
# kde = kernel density estimate
# prob = probabily - default is 0.95
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

# make a raster extent object to set the bounds to the base map
# We can then use setExtent to set the extent of each of the KDE plots individually
# This way the KDE plots are the same size as the base map
extent <- extent(min(spotmap_data$lon)-0.0025, max(spotmap_data$lon)+0.0025, 
                 min(spotmap_data$lat)-0.0025, max(spotmap_data$lat)+0.0025)

# Set extent of KDE and create an object
hosp_kde <- setExtent(Species_kde[["HOSP"]], extent)
eust_kde <- setExtent(Species_kde[["EUST"]], extent)
eabl_kde <- setExtent(Species_kde[["EABL"]], extent)
nomo_kde <- setExtent(Species_kde[["NOMO"]], extent)

# EABL
plot(eabl_kde)

# EUST
plot(eust_kde)

# HOSP
plot(hosp_kde)

# NOMO
plot(nomo_kde)

## can we put a KDE on top of a map?
## first a blank basemap:

base_raster <- openmap(c(max(spotmap_data$lat)+0.0025, min(spotmap_data$lon)-0.0025), + 
                         c(min(spotmap_data$lat)-0.0025, max(spotmap_data$lon)+0.0025), 
                       type = "esri-topo")
base_raster_proj <- openproj(base_raster, projection = "+proj=longlat +datum=WGS84")

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

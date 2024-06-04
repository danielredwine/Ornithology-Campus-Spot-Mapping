# Daniel Redwine
# 6 May 2024

# This script aims to utilize campus spot mapping data to create maps of species
# distribution and density on campus

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
                           + c(min(spotmap_data23$lat)-.0005, max(spotmap_data23$lon)+.0005),
                           type = "osm") # Map type, some of the map types do not work

# Project latitude and longitude onto the base map so lat/lon data can be plotted
base_map_projection <- openproj(static_base_map, projection = "+proj=longlat +datum=WGS84")

# Create the most basic type of map with data points only
point_plot_23 <- autoplot.OpenStreetMap(base_map_projection, expand = TRUE) + 
  geom_point(data = spotmap_data23, aes(lon, lat), size = 1) + 
  aes(color = species) +
  theme(axis.title = element_text(face="bold")) + 
  labs(x="Longitude", y="Latitude") +
  guides(color=guide_legend("Species")) +  theme_bw(base_size = 8)

point_plot_23 # Call the object

ggsave("output/point_plot_23.png") # Save the object


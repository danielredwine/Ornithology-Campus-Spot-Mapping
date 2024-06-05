# Daniel Redwine
# 4 June 2024

# This script aims to utilize campus spot mapping data to create maps

# Clear environment
rm(list = ls())

# Load libraries
library(OpenStreetMap)  # To obtain base maps, install 64-bit Java if you get an error
library(sf)  # Used to encode spatial data
library(tidyverse)  # Includes DPLYR, GGplot, TIDYR, etc.
library(raster)  # To rasterize, analyze, and model spatial data

# Load and clean the datasets, dataset should not require much cleaning
spotmap_data23 <- read.csv("data/2023data.csv")  # load in dataset
spotmap_data23$lat <- as.numeric(spotmap_data23$lat)  # Sets data type to numeric 
spotmap_data23$lon <- as.numeric(spotmap_data23$lon)  # Would also remove non-numeric data
spotmap_data23$species <- as.factor(spotmap_data23$species)  # Sets species as factor

# Set the extent (max/min lat/lon) and open the base map
# Uses the min and max latitude and longitude in the dataset and adds a small amount
static_base_map <- openmap(c(max(spotmap_data23$lat) + .0005, min(spotmap_data23$lon) - .0005), 
                           c(min(spotmap_data23$lat) - .0005, 
                             max(spotmap_data23$lon) + .0005), type = "esri-topo")

# Types "osm", "bing", "esri", "esri-topo", "esri-imagery", "esri-natgeo" 

# Project latitude and longitude onto the base map so lat/lon data can be plotted
base_map_projection <- openproj(static_base_map, projection = "+proj=longlat +datum=WGS84")

# Create the most basic type of map with data points only
point_plot_23 <- autoplot.OpenStreetMap(base_map_projection, expand = TRUE) + 
  geom_point(data = spotmap_data23, aes(lon, lat, color = species), size = 1) +  # add points to map
  theme(axis.title = element_text(face = "bold")) +  # bold the axis title
  labs(x = "Longitude", y = "Latitude") +  # Labels
  guides(color = guide_legend("Species")) +  theme_bw(base_size = 8)  # Legend

point_plot_23  # Call the object

ggsave("output/point_plot_23.png")  # Save the object

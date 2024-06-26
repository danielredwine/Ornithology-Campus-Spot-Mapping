# Daniel Redwine
# 7 June 2024

# This script aims to utilize campus spot mapping data to create various maps

# Clear environment
rm(list = ls())

# Load libraries
library(OpenStreetMap)  # To obtain base maps, install 64-bit Java if you get an error
library(sf)  # Used to encode spatial data
library(tidyverse)  # Includes DPLYR, GGplot, TIDYR, etc.
library(raster)  # To rasterize, analyze, and model spatial data
library(ks) # For KDE smoothing

# Load and clean the datasets, dataset should not require much cleaning
spotmap_data <- read.csv("data/2023data.csv")  # load in dataset
spotmap_data$lat <- as.numeric(spotmap_data$lat)  # Sets data type to numeric 
spotmap_data$lon <- as.numeric(spotmap_data$lon)  # This also removes non-numeric data
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

# Next map KDE and 95% propbability ranges
# The function Hpi used below cannot weight our observations based on count (num_ind)
# as a workaround to take count into effect we will use the function uncount()
# This removes the column num_ind and duplicates the corresponding row by that value 
spotmap_data <- spotmap_data %>%
  uncount(num_ind)

# first make the data into a SpatialPoints object
# CRS = coordinate reference system, use same throughout (+proj=longlat +datum=WGS84)
spotmap_datapts <- sp::SpatialPoints(coords = cbind(spotmap_data$lon, spotmap_data$lat),
                                proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

# put all the data into a single SpatialPointsDataFrame (spdf)
spotmap_data_spdf <- sp::SpatialPointsDataFrame(spotmap_datapts, spotmap_data)
head(spotmap_data_spdf)

# split the dataset by species
separate_species_spdf <- split(x = spotmap_data_spdf, f = spotmap_data_spdf$species, 
                               drop = FALSE)

# now to try kernel density estimation
# do least squares cross-validation to estimate bandwidth (bw)
# This optimizes smoothing of the raster
bw <- lapply(separate_species_spdf, FUN = function(x){ks::Hpi(x@coords)})


# Generate KDE using the optimal bandwidth
Species_kde <-mapply(separate_species_spdf,bw,
                     SIMPLIFY = FALSE,
                     FUN = function(x,y){
                       raster(kde(x@coords,h=y,))})

# This code calculates which areas account for 95% of observations
# 95% is normally used for home range
# Here we could use it as a pseudo home range for the species, or a range map
# Inputs: kde = kernel density estimate, prob = probability - default is 0.95
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

# Apply the contour to the raster
all_95kde <- lapply(Species_kde,FUN = getContour, prob = 0.95)

# make a raster extent object to set the bounds to the base map
# We can then use setExtent to set the extent of each of the KDE plots individually
# This way the KDE plots are the same size as the base map
extent <- extent(min(spotmap_data$lon)-0.0025, max(spotmap_data$lon)+0.0025, 
                 min(spotmap_data$lat)-0.0025, max(spotmap_data$lat)+0.0025)

# Set extent of KDE and create an object for plotting KDE
hosp_kde <- setExtent(Species_kde[["HOSP"]], extent)
eust_kde <- setExtent(Species_kde[["EUST"]], extent)
eabl_kde <- setExtent(Species_kde[["EABL"]], extent)
nomo_kde <- setExtent(Species_kde[["NOMO"]], extent)

# Set extent of 95% probability and create an object
hosp_95 <- setExtent(all_95kde[["HOSP"]], extent)
eust_95 <- setExtent(all_95kde[["EUST"]], extent)
eabl_95 <- setExtent(all_95kde[["EABL"]], extent)
nomo_95 <- setExtent(all_95kde[["NOMO"]], extent)

# View the KDE and 95% probability without a base map
# EABL
plot(eabl_kde)
plot(eabl_95)

# EUST
plot(eust_kde)
plot(eust_95)

# HOSP
plot(hosp_kde)
plot(hosp_95)

# NOMO
plot(nomo_kde)
plot(nomo_95)

# Generate a base map to plot the KDE onto
base_map <- openmap(c(max(spotmap_data$lat)+0.0025, min(spotmap_data$lon)-0.0025), + 
                         c(min(spotmap_data$lat)-0.0025, max(spotmap_data$lon)+0.0025), 
                       type = "esri-topo")

base_map_proj <- openproj(base_map, projection = "+proj=longlat +datum=WGS84")

# Plot House Sparrow KDE
plot(base_map_proj)
plot(hosp_kde, alpha = 0.5, add=TRUE, title("House Sparrow"))

# Plot European Starling KDE
plot(base_map_proj)
plot(eust_kde, alpha = 0.5, add=TRUE, title("European Starling"))

# Plot Eastern Bluebird KDE
plot(base_map_proj)
plot(eabl_kde, alpha = 0.5, add=TRUE, title("Eastern Bluebird"))

# Plot Northern Mockingbird KDE 
plot(base_map_proj)
plot(nomo_kde, alpha = 0.5, add=TRUE, title ("Northern Mockingbird"))

# Plot House Sparrow 95% probability range 
plot(base_map_proj)
plot(hosp_95, alpha = 0.5, add=TRUE, title("House Sparrow"), legend = FALSE)

# Plot European Starling 95% Probability range
plot(base_map_proj)
plot(eust_95, alpha = 0.5, add=TRUE, title("European Starling"), legend = FALSE)

# Plot Eastern Bluebird 95% Probability range
plot(base_map_proj)
plot(eabl_95, alpha = 0.5, add=TRUE, title("Eastern Bluebird"), legend = FALSE)

# Plot Northern Mockingbird 95% Probability range
plot(base_map_proj)
plot(nomo_95, alpha = 0.5, add=TRUE, title ("Northern Mockingbird"), legend = FALSE)

# To save 95% probability maps export in plots pane with width 1200 and height 850 
# To save KDE maps use export in plots pane with width 1536 and height 890
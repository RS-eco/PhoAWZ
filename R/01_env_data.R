library(terra); library(ggplot2); library(sf)
library(dplyr); library(dtplyr); library(magrittr)

# List global MARSPEC files
# not located in data folder anymore
files <- list.files("data", pattern="30s.tif", full.names=T)[1:3]

#files <- list.files("/home/matt/Documents/Bio-Oracle/allLayers", pattern="Present", full.names=T)
#files

# Get AWZ outline
library(geodat)
data("eez")
awz <- eez %>% filter(Country == "Germany")
ggplot() + geom_sf(data=awz)
st_bbox(awz); rm(eez)

# Load data
dat <- terra::rast(files)

# Crop by awz extent
dat_awz <- terra::crop(dat, awz)
dat_awz
writeCDF(dat_awz, filename="data/marspec_awz.nc", overwrite=T,
            compression=9)
plot(dat_awz)

# Crop by Pho pho extent
dat <- terra::crop(dat,  c(xmin = -10, xmax = 22, ymin = 40, ymax = 70))
writeCDF(dat, filename="data/marspec_pho_pho.nc", overwrite=T,
            compression=9)
plot(dat[[1]])

marspec_awz <- terra::rast("data/marspec_awz.nc")
marspec_awz
plot(marspec_awz)



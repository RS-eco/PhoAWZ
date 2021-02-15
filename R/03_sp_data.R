
# Load packages
library(robis)
library(dplyr); library(dtplyr); library(ggplot2); library(magrittr)
library(geodat); library(sf)

# Get AWZ data
data(eez)
awz <- eez %>% filter(Country == "Germany"); rm(eez)
awz2 <- st_buffer(awz, dist=0.1)
ggplot() + geom_sf(data=awz, fill=NA) + geom_sf(data=awz2, col="red", fill=NA)
st_bbox(awz); st_bbox(awz2)

# Download Schweinswal data
if(!file.exists("data/pho_pho_15022021.rds")){
  pho_pho <- occurrence(scientificname = "Phocoena phocoena")
  saveRDS(pho_pho, "data/pho_pho_15022021.rds", compress="xz")
} else{
  pho_pho <- readRDS("data/pho_pho_15022021.rds")
}

if(!file.exists("data/pho_pho_15022021.rds")){
  pho_pho_awz <- occurrence(scientificname = "Phocoena phocoena", 
                            geometry = "POLYGON ((3 53, 3 56, 15 56, 15 53, 3 53))")
  
  # Plot Schweinswal data
  ggplot() + 
    geom_point(data=pho_pho_awz, aes(x=decimalLongitude, y=decimalLatitude)) + 
    geom_sf(data=awz, fill=NA) + coord_sf()
  
  pho_pho_awz_sf <- st_as_sf(pho_pho_awz, coords = c("decimalLongitude", "decimalLatitude"),
                             crs = 4326)
  
  # Subset Schweinswal data by AWZ outline
  pho_pho_awz_sf <- st_intersection(pho_pho_awz_sf, awz2)
  ggplot() + 
    geom_sf(data=pho_pho_awz_sf) + 
    geom_sf(data=awz, fill=NA) + coord_sf()
  
  saveRDS(pho_pho_awz_sf, "data/pho_pho_awz_15022021.rds", compress="xz")
} else{
  pho_pho <- readRDS("data/pho_pho_awz_15022021.rds")
}

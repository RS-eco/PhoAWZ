library(ggplot2); library(sf)
library(dplyr); library(dtplyr); library(magrittr)
library(dismo); library(sp); library(raster)

# Get AWZ outline
library(geodat)
data("eez")
awz <- eez %>% filter(Country == "Germany")
ggplot() + geom_sf(data=awz)
st_bbox(awz); rm(eez)

# Get Pho pho data
pho_pho <- readRDS("data/pho_pho_05022021.rds")

# Plot Schweinswal data
ggplot() + 
  geom_point(data=pho_pho, aes(x=decimalLongitude, y=decimalLatitude)) + 
  geom_sf(data=awz, fill=NA) + coord_sf()

# Subset data by North Atlantic
pho_pho_sf <- st_as_sf(pho_pho, coords = c("decimalLongitude", "decimalLatitude"),
                       crs = 4326)
pho_pho_sf <- st_crop(pho_pho_sf, c(xmin = -10, xmax = 22, ymin = 40, ymax = 70))

library(ggmap2)
data(outline)
ggplot() + 
  geom_sf(data = outline, fill = "#dddddd", colour="black") +
  geom_sf(data=pho_pho_sf, colour="blue") + theme_bw() + 
  #geom_sf(data=awz, fill=NA) + 
  coord_sf(xlim=c(-10,22), ylim=c(40,70)) + 
  theme(axis.text = element_text(size=10))
ggsave("figures/pho_pho_occ.png", dpi=600, width=6, height=8)

st_bbox(pho_pho_sf)

# Load env data
env_dat <- raster::stack("data/marspec_pho_pho.nc")
names(env_dat) <- c("bathy_30s", "biogeo08_30s", "biogeo13_30s")
raster::plot(env_dat[[1]])
plot(st_geometry(pho_pho_sf), add=T)
# Sampling bias
# create a RasterLayer with correct extent
r <- raster::raster(env_dat[[1]])
r
# set the resolution of the cells to (for example) 1 degree
raster::res(r) <- 0.008333333
# sample:
sampl_bias <- dismo::gridSample(as(pho_pho_sf, "Spatial"), r, n=1)
# to illustrate the method and show the result
#p <- rasterToPolygons(r)
raster::plot(env_dat[[1]])
plot(st_geometry(pho_pho_sf), add=T)
# selected points in red
points(sampl_bias, cex=1, col='red', pch='x')

# Turn species data into raster
# List through species and turn data into raster

# presence/absensce (NA) (is there a point or not?)
r1 <- raster::rasterize(pho_pho_sf, r, field=1)

# how many points?
sp_pho <- as(st_geometry(pho_pho_sf), "Spatial")
r2 <- raster::rasterize(sp_pho, r, fun=function(x,...)length(x))

raster::plot(r1)
raster::plot(r2)

#' ##´ Create absence points

# circles with a radius of 100 km
library(rgeos)
x <- dismo::circles(sampl_bias, d=100000, lonlat=TRUE)
pol <- sp::polygons(x)

# sample randomly from all circles
set.seed(123)
samp1 <- spsample(pol, 20000, type='random', iter=25)
# get unique cells
cells <- raster::cellFromXY(r, samp1)
length(cells)
cells <- unique(cells)
length(cells)
xy <- raster::xyFromCell(r, cells)
xy <- xy[!is.na(xy[,1]),]
xy <- xy[!is.na(xy[,2]),]

plot(pol, axes=TRUE)
points(xy, cex=0.75, pch=20, col='blue')

spxy <- SpatialPoints(xy, proj4string=CRS('+proj=longlat +datum=WGS84'))
o <- over(spxy, geometry(x))
xyInside <- xy[!is.na(o), ]

abs <- SpatialPoints(xyInside, proj4string=CRS('+proj=longlat +datum=WGS84'))
pres <- SpatialPoints(sampl_bias, proj4string=CRS('+proj=longlat +datum=WGS84'))

#' ## Extract values

presvals <- raster::extract(env_dat, pres)
absvals <- raster::extract(env_dat, abs)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals))) %>% tidyr::drop_na()
sdmdata$biogeo08_30s <- sdmdata$biogeo08_30s/100
sdmdata$biogeo13_30s <- sdmdata$biogeo13_30s/100
head(sdmdata)
tail(sdmdata)
summary(sdmdata)
pairs(sdmdata[,2:4], cex=0.1)

saveRDS(sdmdata, "data/sdmdata.rds", compress="xz")

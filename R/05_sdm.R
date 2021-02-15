library(ggplot2); library(sf)
library(RColorBrewer); library(lattice)
library(dplyr)
library(randomForest)
# Install the mecofun package
#remotes::install_git("https://gitup.uni-potsdam.de/macroecology/mecofun.git")
library(mecofun)

# Get AWZ outline
library(geodat)
data("eez")
awz <- eez %>% filter(Country == "Germany")
ggplot() + geom_sf(data=awz)
st_bbox(awz); rm(eez)

# Load SDM data
sdmdata <- readRDS("data/sdmdata.rds")

#' ## Run model
library(gbm)
m_gbm <- gbm(pb ~ bathy_30s + biogeo08_30s + biogeo13_30s, data=sdmdata)
summary(m_gbm)

ggplot() + geom_bar(aes(x=c("Bathymetrie", "SSS", "SST"), 
                        y=relative.influence(m_gbm)/sum(relative.influence(m_gbm))*100),
                    width=.8, stat="identity") + coord_flip() + theme_bw() + 
  labs(x="", y="Relative Gewichtung (%)") + 
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12, face="bold"))
ggsave("figures/rel_inf_gbm.png", dpi=600, width=9, height=5)

# Fit GLM
m_glm <- glm(pb ~ bathy_30s + I(bathy_30s^2) + biogeo08_30s + I(biogeo08_30s^2) + biogeo13_30s + I(biogeo13_30s^2), family='binomial', data=sdmdata)
summary(m_glm)

# Names of our variables:
pred <- c('bathy_30s', 'biogeo08_30s', 'biogeo13_30s')

# We want three panels next to each other:
par(mfrow=c(1,3)) 

# Plot the partial responses
partial_response(m_glm, predictors = sdmdata[,pred],
                 label=c("Bathymetry (m)", "SSS (psu)", "SST (°C)"))

# We prepare the response surface by making a dummy data set where two predictor variables range from their minimum to maximum value, 
# and the remaining predictor is kept constant at its mean:
xyz <- data.frame(expand.grid(seq(min(sdmdata[,pred[2]]),max(sdmdata[,pred[2]]),length=50), 
                              seq(min(sdmdata[,pred[3]]),max(sdmdata[,pred[3]]),length=50)), mean(sdmdata[,pred[1]]))
names(xyz) <- pred[c(2,3,1)]

# Make predictions
xyz$z <- predict(m_glm, xyz, type='response')
summary(xyz)

# Make a colour scale
cls <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))(100)

# plot 3D-surface
wireframe(z ~ biogeo08_30s + biogeo13_30s, data = xyz, zlab = list("Occurrence prob.", rot=90),
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), zlim = c(0, 1), 
          main='GLM', xlab='SSS', ylab='SST', screen=list(z = 120, x = -70, y = 3))

# Fit RF
(m_rf <- randomForest( x=sdmdata[,2:4], y=sdmdata[,1], ntree=1000, nodesize=10, importance =T))

# Variable importance:
importance(m_rf,type=1)
varImpPlot(m_rf)

# Now, we plot response curves in the same way as we did for GLMs above:
par(mfrow=c(1,3)) 
partial_response(m_rf, predictors = sdmdata[,pred], main='Random Forest')

# Plot the response surface:
xyz$z <- predict(m_rf, xyz)   # Note that we created the xyz data.frame in the GLM example above
wireframe(z ~ biogeo08_30s + biogeo13_30s, data = xyz, zlab = list("Occurrence prob.", rot=90),
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), zlim = c(0, 1), 
          main='RF', xlab='SSS', ylab='SST', screen=list(z = 120, x = -70, y = 3))

# Make cross-validated predictions for GLM:
crosspred_glm <- mecofun::crossvalSDM(m_glm, traindat= sdmdata, colname_pred=pred, 
                                      colname_species = "pb")

# Make cross-validated predictions for RF:
crosspred_rf <- mecofun::crossvalSDM(m_rf, traindat= sdmdata, colname_pred=pred, 
                                     colname_species = "pb")

# Look at correlation between GLM and RF predictions:
plot(crosspred_glm, crosspred_rf, pch=19, col='grey35')

(eval_glm <- mecofun::evalSDM(observation = sdmdata$pb, predictions = crosspred_glm))
(eval_rf <- mecofun::evalSDM(observation = sdmdata$pb, predictions = crosspred_rf))

# Derive mean predictions:
crosspred_ens <- apply(data.frame(crosspred_glm, crosspred_rf),1, mean)

# Evaluate ensemble predictions
(eval_ens <- mecofun::evalSDM(observation = sdmdata$pb, predictions = crosspred_ens))
rm(awz, xyz, cls, pred, env_dat, sdmdata)
save.image("data/model_dat.RData", compress="xz")

# Load env data
load("data/model_dat.RData")
env_dat <- raster::stack("data/marspec_pho_pho.nc")
env_dat
names(env_dat) <- c("bathy_30s", "biogeo08_30s", "biogeo13_30s")
raster::plot(env_dat[[1]])

# Make predictions to current climate:
bio_curr_df <- data.frame(raster::rasterToPoints(env_dat)); rm(env_dat); gc()
colnames(bio_curr_df)
bio_curr_df$biogeo08_30s <- bio_curr_df$biogeo08_30s/100
bio_curr_df$biogeo13_30s <- bio_curr_df$biogeo13_30s/100
gc()
bio_curr_df$pred_glm <- mecofun::predictSDM(m_glm, bio_curr_df)
bio_curr_df$pred_rf <- mecofun::predictSDM(m_rf, bio_curr_df)
bio_curr_df$pred_ens <- apply(bio_curr_df[,-c(1:5)],1,median)
gc()

# Make binary predictions:
bio_curr_df$bin_glm <- ifelse(bio_curr_df$pred_glm > eval_glm$thresh, 1, 0)
bio_curr_df$bin_rf <- ifelse(bio_curr_df$pred_rf > eval_rf$thresh, 1, 0)
bio_curr_df$bin_ens <- ifelse(bio_curr_df$pred_ens > eval_ens$thresh, 1, 0)
gc()
head(bio_curr_df)

r_pred_curr <- raster::rasterFromXYZ(bio_curr_df); rm(bio_curr_df); gc()
#raster::writeRaster(r_pred_curr, filename="data/pred_curr.nc", compression=9, overwrite=T)
#raster::plot(r_pred_curr)

# Mask raster by AWZ
r_pred_curr_awz <- raster::mask(raster::crop(r_pred_curr, awz), awz)
raster::writeRaster(r_pred_curr_awz, 
                    filename="data/pred_curr_awz.nc", compression=9, overwrite=T)
raster::plot(r_pred_curr_awz)

r_pred_curr <- raster::stack("data/pred_curr.nc")
r_pred_curr <- raster::aggregate(r_pred_curr, fact=2)
pred_curr <- as.data.frame(raster::rasterToPoints(r_pred_curr)); rm(r_pred_curr); gc()
colnames(pred_curr) <- c("x", "y", "bathy_30s", "biogeo08_30s", "biogeo13_30s", "pred_glm", "pred_rf", "pred_ens", "bin_glm", "bin_rf", "bin_ens")

library(scico)
library(ggmap2)
data(outline)
p <- ggplot() + 
  geom_sf(data = outline, fill = "#dddddd", colour="black", size=0.75/.pt) +
  geom_tile(data=pred_curr, aes(x=x, y=y, fill=pred_ens)) + 
  coord_sf(xlim=c(-10,22), ylim=c(40,70), expand=c(0,0)) + theme_bw() + 
  theme(axis.title = element_blank(), axis.text = element_text(size=10),
        legend.text = element_text(size=10), 
        legend.title = element_text(size=12, face="bold")) + 
  scale_fill_scico(name="Vorkommens- \nwahrscheinlichkeit", palette="roma")
ggsave("figures/pho_pho_prob_all.png", p, dpi=400, width=8, height=9)

r_pred_curr_awz <- raster::stack("data/pred_curr_awz.nc")
pred_curr_awz <- as.data.frame(raster::rasterToPoints(r_pred_curr_awz))
colnames(pred_curr_awz) <- c("x", "y", "bathy_30s", "biogeo08_30s", "biogeo13_30s", "pred_glm", "pred_rf", "pred_ens", "bin_glm", "bin_rf", "bin_ens")

library(scico)
ggplot() + geom_tile(data=pred_curr_awz, aes(x=x, y=y, fill=pred_ens)) + 
  geom_sf(data=awz, fill=NA, col="black", size=0.75/.pt) + coord_sf() + theme_bw() + 
  theme(axis.title = element_blank(), legend.position="bottom",
        axis.text = element_text(size=10), 
        legend.key.width = unit(1.5, "cm"), legend.text = element_text(size=10), 
        legend.title = element_text(size=12, face="bold", vjust=0.8)) + 
  scale_fill_scico(name="Vorkommenswahrscheinlichkeit", palette="roma")
ggsave("figures/pho_pho_prob.png", dpi=1000, width=10, height=5)

pred_curr_awz %>% mutate(bin_ens = factor(bin_ens, levels=c(0,1), labels=c("abwesend", "präsent"))) %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=bin_ens)) + 
  geom_sf(data=awz, fill=NA, col="black", size=0.75/.pt) + coord_sf() + theme_bw() + 
  theme(axis.title = element_blank(), legend.position="bottom",
        axis.text = element_text(size=10),
        legend.key.width = unit(1.5, "cm"), legend.text = element_text(size=10), 
        legend.title = element_text(size=12, face="bold")) + 
  scale_fill_brewer(name="Vorkommen", palette = "Paired") 
ggsave("figures/pho_pho_bin.png", dpi=1000, width=10, height=5)

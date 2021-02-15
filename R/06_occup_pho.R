#install.packages("devtools")
#devtools::install_github("biologicalrecordscentre/sparta")
library(sparta)

molluscs <- read.csv("molluscs.csv")

head(molluscs)

occData <- formatOccData(taxa = molluscs$clean_name, 
                         site = molluscs$site, 
                         time_period = as.Date(molluscs$analysis_date))

str(occData)

taxa <- as.character(c("Limacina retroversa", 
                       "Nucella lapillus", 	
                       "Loligo forbesii",
                       "Calliostoma zizyphinum"))

occ_out<-NULL

for (i in 1:length(taxa)){
  
  occ_out[[i]] <- occDetFunc(taxa_name = taxa[i],
                             occDetdata = occData$occDetdata,
                             spp_vis = occData$spp_vis,
                             n_iterations = 50000,
                             burnin = 5000,
                             n_chains = 2,
                             model.file = occDetModel_mod)
  
}

head(occ_out[[1]]$BUGSoutput$summary)


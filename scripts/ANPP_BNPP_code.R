# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)
ForC_simplified$site_plot <- paste0(ForC_simplified$sites.sitename," ", ForC_simplified$plot.name)
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)
ForC_simplified <- ForC_simplified[which(ForC_simplified$stand.age >= 50), ]
dist.to.keep <- ForC_simplified$managed %in% 0
ForC_simplified <- ForC_simplified[which(dist.to.keep),]

ForC_simplified$biomes <- NA
ForC_simplified$biomes <- ifelse((grepl("Temperate", ForC_simplified$FAO.ecozone, fixed = TRUE)), "temperate", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Boreal", ForC_simplified$FAO.ecozone, fixed = TRUE)), "boreal", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Tropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "tropical", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Subtropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "subtropical", ForC_simplified$biomes)

BNPP_root_fine <- ForC_simplified[ForC_simplified$variable.name %in% c("BNPP_root_fine"),]
ANPP <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_0", "ANPP_1", "ANPP_2"),]
ANPP$variable.name <- gsub("(\\w*)(_1$|_2$|_0$)", "\\1", ANPP$variable.name, perl = T)

ANPP_and_BNPP_root <- merge(ANPP, BNPP_root_fine[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))

#####now working with ANPP_woody_stem_and_canopy

names(ANPP_and_BNPP_root)[13] <- "ANPP"
names(ANPP_and_BNPP_root)[41] <- "BNPP_root_fine"

write.csv(ANPP_and_BNPP_root, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_and_BNPP_root.csv")

##### extract WorldClim and CRU data for ForC_simplified

rm(list = ls())
library(raster)
library(ncdf4)
library(Hmisc)

ANPP_and_BNPP_root <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_and_BNPP_root.csv")

ANPP_and_BNPP_root <- ANPP_and_BNPP_root[!is.na(ANPP_and_BNPP_root$lat),]
coordinates(ANPP_and_BNPP_root)<-c("lon", "lat")
proj<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(ANPP_and_BNPP_root)<-proj
plot(ANPP_and_BNPP_root)
as.data.frame(ANPP_and_BNPP_root)

#### this section takes a long time to run
# cld
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.cld.dat.nc", varname="cld")
cld.1901.2014 <- raster::extract(r, ANPP_and_BNPP_root)

# frs
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.frs.dat.nc", varname="frs")
frs.1901.2014 <- raster::extract(r, ANPP_and_BNPP_root)

# pet
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.pet.dat.nc", varname="pet")
pet.1901.2014 <- raster::extract(r, ANPP_and_BNPP_root)

# wet
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.01.1901.2014.wet.dat.nc", varname="wet")
wet.1901.2014 <- raster::extract(r, ANPP_and_BNPP_root)

## save ####

# cld
cld <- data.frame(ANPP_and_BNPP_root)
cld <- data.frame(measurement.ID = cld[,1], sites.sitename = as.character(cld[,2]), plot.name = as.character(cld[,3]), cld.1901.2014)
write.csv(cld, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/cld.1901.2014.csv", row.names = F)

# frs
frs <- data.frame(ANPP_and_BNPP_root)
frs <- data.frame(measurement.ID = cld[,1], sites.sitename = as.character(cld[,2]), plot.name = as.character(cld[,3]), frs.1901.2014)
write.csv(frs, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/frs.1901.2014.csv", row.names = F)

# pet
pet <- data.frame(ANPP_and_BNPP_root)
pet <- data.frame(measurement.ID = cld[,1], sites.sitename = as.character(cld[,2]), plot.name = as.character(cld[,3]), pet.1901.2014)
write.csv(pet, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/pet.1901.2014.csv", row.names = F)

# wet
wet <- data.frame(ANPP_and_BNPP_root)
wet <- data.frame(measurement.ID = cld[,1], sites.sitename = as.character(cld[,2]), plot.name = as.character(cld[,3]), wet.1901.2014)
write.csv(wet, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/wet.1901.2014.csv", row.names = F)

#cld <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/cld.1901.2014.csv")
#frs <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/frs.1901.2014.csv")
#pet <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/pet.1901.2014.csv")
#wet <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/wet.1901.2014.csv")

cld$mean <- rowMeans(cld[, c(4:1371)], na.rm = TRUE)
cld <- cld[, c(1:3, 1372)]

frs$mean <- rowSums(frs[, c(4:1371)], na.rm = TRUE)
frs$mean <- (frs$mean)/114
frs <- frs[, c(1:3, 1372)]

wet$mean <- rowSums(wet[, c(4:1371)], na.rm = TRUE)
wet$mean <- (wet$mean)/114
wet <- wet[, c(1:3, 1372)]

pet_colnames <- colnames(pet)[(4:1371)]
pet_colnames <- gsub("[a-zA-Z ]", "", pet_colnames)
pet_colnames <- as.Date(pet_colnames, format = "%Y.%m.%d")
pet_colnames <- monthDays(pet_colnames)
pet_month <- pet[, c(4:1371)]*matrix(rep(pet_colnames, nrow(pet)), nrow = nrow(pet), byrow = T)
pet_month <- cbind(pet[,c(1:3)], pet_month)
pet$mean <- rowSums(pet_month[, c(4:1371)], na.rm = TRUE)
pet$mean <- (pet$mean)/114
pet <- pet[, c(1:3, 1372)]


r <- stack("S:/Global Maps Data/WorldClim/tiff/1bioclim_stacked_all.tif")
# plot(r) 
points(ANPP_and_BNPP_root)
WorldClim <- raster::extract(r, ANPP_and_BNPP_root)

WorldClimDF <- data.frame(ANPP_and_BNPP_root)
WorldClimDF <- cbind(WorldClimDF, WorldClim)
#rename variables
names(WorldClimDF)[44:62] <- c("AnnualMeanTemp", "MeanDiurnalRange", "Isothermality","TempSeasonality", "MaxTWarmestMonth", "MinTColdestMonth", "TempRangeAnnual", "MeanTWetQ", "MeanTDryQ","MeanTWarmQ","MeanTColdQ", "AnnualPre","PreWetMonth", "PreDryMonth", "PreSeasonality", "PreWetQ", "PreDryQ", "PreWarmQ", "PreColdQ")
head(WorldClimDF)
WorldClimDF <- cbind(WorldClimDF, cld$mean, frs$mean, pet$mean, wet$mean)
names(WorldClimDF)[63:66] <- c("CloudCover", "AnnualFrostDays","AnnualPET", "AnnualWetDays")


WorldClimDF$AnnualMeanTemp  <- WorldClimDF$AnnualMeanTemp / 10
WorldClimDF$MeanDiurnalRange <- WorldClimDF$MeanDiurnalRange / 10
WorldClimDF$Isothermality <- WorldClimDF$Isothermality / 100
WorldClimDF$TempSeasonality  <- WorldClimDF$TempSeasonality / 100
WorldClimDF$MaxTWarmestMonth  <- WorldClimDF$MaxTWarmestMonth / 10
WorldClimDF$MinTColdestMonth <- WorldClimDF$MinTColdestMonth / 10
WorldClimDF$TempRangeAnnual <- WorldClimDF$TempRangeAnnual / 10
WorldClimDF$MeanTWetQ <- WorldClimDF$MeanTWetQ / 10
WorldClimDF$MeanTDryQ <- WorldClimDF$MeanTDryQ / 10
WorldClimDF$MeanTWarmQ <- WorldClimDF$MeanTWarmQ / 10
WorldClimDF$MeanTColdQ <- WorldClimDF$MeanTColdQ / 10

head(WorldClimDF)

write.csv(WorldClimDF,"C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_and_BNPP_root_WorldClim_CRU.csv", row.names = F)

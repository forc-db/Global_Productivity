##### extract WorldClim and CRU data for ForC_simplified

rm(list = ls())
library(raster)
library(ncdf4)
library(Hmisc)

ForC_simplified <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified.csv")

ForC_simplified <- ForC_simplified[!is.na(ForC_simplified$lat),]
coordinates(ForC_simplified)<-c("lon", "lat")
proj<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(ForC_simplified)<-proj
plot(ForC_simplified)
as.data.frame(ForC_simplified)

#### this section takes a long time to run
# cld
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.cld.dat.nc", varname="cld")
cld.1901.2014 <- raster::extract(r, ForC_simplified)

# frs
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.frs.dat.nc", varname="frs")
frs.1901.2014 <- raster::extract(r, ForC_simplified)

# pet
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.pet.dat.nc", varname="pet")
pet.1901.2014 <- raster::extract(r, ForC_simplified)

# wet
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.01.1901.2014.wet.dat.nc", varname="wet")
wet.1901.2014 <- raster::extract(r, ForC_simplified)

## save ####

# cld
cld <- data.frame(ForC_simplified)
cld <- data.frame(measurement.ID = cld[,1], sites.sitename = as.character(cld[,2]), plot.name = as.character(cld[,3]), cld.1901.2014)
write.csv(cld, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/cld.1901.2014.csv", row.names = F)

# frs
frs <- data.frame(ForC_simplified)
frs <- data.frame(measurement.ID = frs[,1], sites.sitename = as.character(frs[,2]), plot.name = as.character(frs[,3]), frs.1901.2014)
write.csv(frs, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/frs.1901.2014.csv", row.names = F)

# pet
pet <- data.frame(ForC_simplified)
pet <- data.frame(measurement.ID = pet[,1], sites.sitename = as.character(pet[,2]), plot.name = as.character(pet[,3]), pet.1901.2014)
write.csv(pet, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/pet.1901.2014.csv", row.names = F)

# wet
wet <- data.frame(ForC_simplified)
wet <- data.frame(measurement.ID = wet[,1], sites.sitename = as.character(wet[,2]), plot.name = as.character(wet[,3]), wet.1901.2014)
write.csv(wet, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/wet.1901.2014.csv", row.names = F)

cld <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/cld.1901.2014.csv")
frs <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/frs.1901.2014.csv")
pet <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/pet.1901.2014.csv")
wet <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/wet.1901.2014.csv")

cld$mean <- rowMeans(cld[, c(4:1371)], na.rm = TRUE)
cld[,1372]
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
###multiply each value by days in month to get monthly total
pet_month <- pet[, c(4:1371)]*matrix(rep(pet_colnames, nrow(pet)), nrow = nrow(pet), byrow = T)
pet_month <- cbind(pet[,c(1:3)], pet_month)
###sum all months (114 years)
pet$mean <- rowSums(pet_month[, c(4:1371)], na.rm = TRUE)
###divide by 114 to get annual mean
pet$mean <- (pet$mean)/114
pet <- pet[, c(1:3, 1372)]


setwd("S:/Global Maps Data/WorldClim/tiff")
# unzip("wc2.0_30s_vapr.zip")
S_filenames<- paste("wc2.0_30s_srad_", c(paste(0,1:9, sep=""), 10, 11, 12), ".tif",sep="")
srad <- stack(S_filenames) 
month <- c("01 Jan 2010", "01 Feb 2010", "01 Mar 2010", "01 Apr 2010", "01 May 2010", "01 Jun 2010", "01 Jul 2010", "01 Aug 2010", "01 Sep 2010", "01 Oct 2010", "01 Nov 2010", "01 Dec 2010")

names(srad) <- month

ForC_srad <- raster::extract(srad, ForC_simplified)
srad1 <- data.frame(ForC_simplified)
srad1 <- data.frame(measurement.ID = srad1[,1], sites.sitename = as.character(srad1[,2]), plot.name = as.character(srad1[,3]), ForC_srad)

srad_colnames <- colnames(srad1)[(4:15)]
srad_colnames <- gsub("X", "", srad_colnames)
srad_colnames <- as.Date(srad_colnames, format = "%d.%b.%Y")
srad_colnames <- monthDays(srad_colnames)

srad_month <- srad1[, c(4:15)]*matrix(rep(srad_colnames, nrow(srad1)), nrow = nrow(srad1), byrow = T)
srad_month <- cbind(srad1[,c(1:3)], srad_month)
srad1$mean <- rowSums(srad_month[, c(4:15)], na.rm = TRUE)
srad <- srad1[, c(1:3, 16)]

# unzip("wc2.0_30s_vapr.zip")
V_filenames<- paste("wc2.0_30s_vapr_", c(paste(0,1:9, sep=""), 10, 11, 12), ".tif",sep="")
vapr <- stack(V_filenames) 

ForC_vapr <- raster::extract(vapr, ForC_simplified)
vapr1 <- data.frame(ForC_simplified)
vapr1 <- data.frame(measurement.ID = vapr1[,1], sites.sitename = as.character(vapr1[,2]), plot.name = as.character(vapr1[,3]), ForC_vapr)

vapr1$vapr_mean <- rowSums(vapr1[, c(4:15)], na.rm = TRUE)
vapr1$vapr_mean <- (vapr1$vapr_mean)/12
vapr <- vapr1[, c(1:3, 16)]


r <- stack("S:/Global Maps Data/WorldClim/tiff/1bioclim_stacked_all.tif")
# plot(r) 
points(ForC_simplified)
WorldClim <- raster::extract(r, ForC_simplified)

WorldClimDF <- data.frame(ForC_simplified)
WorldClimDF <- cbind(WorldClimDF, WorldClim)
#rename variables
names(WorldClimDF)[36:54] <- c("AnnualMeanTemp", "MeanDiurnalRange", "Isothermality","TempSeasonality", "MaxTWarmestMonth", "MinTColdestMonth", "TempRangeAnnual", "MeanTWetQ", "MeanTDryQ","MeanTWarmQ","MeanTColdQ", "AnnualPre","PreWetMonth", "PreDryMonth", "PreSeasonality", "PreWetQ", "PreDryQ", "PreWarmQ", "PreColdQ")
head(WorldClimDF)
WorldClimDF <- cbind(WorldClimDF, cld$mean, frs$mean, pet$mean, wet$mean, vapr1$vapr_mean, srad1$mean)
names(WorldClimDF)[55:60] <- c("CloudCover", "AnnualFrostDays","AnnualPET", "AnnualWetDays", "VapourPressure", "SolarRadiation")
head(WorldClimDF)


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
WorldClimDF$VapourPressure <- WorldClimDF$VapourPressure / 10
WorldClimDF$SolarRadiation <- WorldClimDF$SolarRadiation / 10

head(WorldClimDF)

write.csv(WorldClimDF,"C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_WorldClim_CRU.csv", row.names = F)

WorldClimDFRefined <- WorldClimDF[,-c(38, 40:41, 43:46, 48:49, 51:54)]
head(WorldClimDFRefined)
write.csv(WorldClimDFRefined,"C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", row.names = F)


#############################################

setwd("S:/Global Maps Data/Global Aridity Index/")
# unzip("7504448.zip")
# unzip("global-et0_annual.tif.zip")
# unzip("global-ai_et0.zip")
# unzip("global-et0_monthly.tif.zip")

ForC_simplified <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)

ForC_simplified <- ForC_simplified[!is.na(ForC_simplified$lat),]
coordinates(ForC_simplified)<-c("lon", "lat")
proj<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(ForC_simplified)<-proj
plot(ForC_simplified)
as.data.frame(ForC_simplified)

r <- stack("S:/Global Maps Data/Global Aridity Index/ai_et0/ai_et0.tif")
ForC_arid <- raster::extract(r, ForC_simplified)

r <- stack("S:/Global Maps Data/Global Aridity Index/et0_yr/et0_yr.tif")
ForC_evap <- raster::extract(r, ForC_simplified)

ForC_simplified <- data.frame(ForC_simplified)

ForC_simplified <- cbind(ForC_simplified, ForC_arid, ForC_evap)
names(ForC_simplified)[49:50] <- c("Aridity", "PotentialEvapotranspiration")

write.csv(ForC_simplified,"C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", row.names = F)

#################
ForC_simplified <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)

setwd("S:/Global Maps Data/TerraClimate")

ForC_simplified <- ForC_simplified[!is.na(ForC_simplified$lat),]
coordinates(ForC_simplified)<-c("lon", "lat")
proj<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(ForC_simplified)<-proj
plot(ForC_simplified)
as.data.frame(ForC_simplified)

vpd_filenames<- paste("TerraClimate_vpd_", c(paste(1958:2017, sep="")), ".nc",sep="")
vpd <- stack(vpd_filenames) 
ForC_vpd <- raster::extract(vpd, ForC_simplified)

vpd1 <- data.frame(ForC_simplified)
vpd1 <- data.frame(measurement.ID = vpd1[,1], sites.sitename = as.character(vpd1[,2]), plot.name = as.character(vpd1[,3]), ForC_vpd)

vpd1$vpd_mean <- rowSums(vpd1[, c(4:723)], na.rm = TRUE)
vpd1$vpd_mean <- (vpd1$vpd_mean)/720
vpd <- vpd1[, c(1:3, 724)]

ForC_simplified <- data.frame(ForC_simplified)

ForC_simplified <- cbind(ForC_simplified, vpd$vpd_mean)
names(ForC_simplified)[52] <- "VapourPressureDeficit"

write.csv(ForC_simplified,"C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", row.names = F)

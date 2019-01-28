##### extract WorldClim data for ForC_simplified

rm(list = ls())
library(raster)
library(ncdf4)

ForC_simplified <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified.csv")

ForC_simplified <- ForC_simplified[!is.na(ForC_simplified$lat),]
coordinates(ForC_simplified)<-c("lon", "lat")
proj<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(ForC_simplified)<-proj
plot(ForC_simplified)
as.data.frame(ForC_simplified)

# cld
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.cld.dat.nc", varname="cld")
cld.1901.2014 <- raster::extract(r, ForC_simplified)

# frs
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.frs.dat.nc", varname="frs")
frs.1901.2014 <- raster::extract(r, ForC_simplified)

# pet
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.pet.dat.nc", varname="pet")
pet.1901.2014 <- raster::extract(r, ForC_simplified)

# tmn
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.tmn.dat.nc", varname="tmn")
tmn.1901.2014 <- raster::extract(r, ForC_simplified)

# tmx
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.tmx.dat.nc", varname="tmx")
tmx.1901.2014 <- raster::extract(r, ForC_simplified)

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
frs <- data.frame(measurement.ID = cld[,1], sites.sitename = as.character(cld[,2]), plot.name = as.character(cld[,3]), frs.1901.2014)
write.csv(frs, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/frs.1901.2014.csv", row.names = F)

# pet
pet <- data.frame(ForC_simplified)
pet <- data.frame(measurement.ID = cld[,1], sites.sitename = as.character(cld[,2]), plot.name = as.character(cld[,3]), pet.1901.2014)
write.csv(pet, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/pet.1901.2014.csv", row.names = F)

# tmn
tmn <- data.frame(ForC_simplified)
tmn <- data.frame(measurement.ID = cld[,1], sites.sitename = as.character(cld[,2]), plot.name = as.character(cld[,3]), tmn.1901.2014)
write.csv(tmn, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/tmn.1901.2014.csv", row.names = F)

# tmx
tmx <- data.frame(ForC_simplified)
tmx <- data.frame(measurement.ID = cld[,1], sites.sitename = as.character(cld[,2]), plot.name = as.character(cld[,3]), tmx.1901.2014)
write.csv(tmx, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/tmx.1901.2014.csv", row.names = F)

# wet
wet <- data.frame(ForC_simplified)
wet <- data.frame(measurement.ID = cld[,1], sites.sitename = as.character(cld[,2]), plot.name = as.character(cld[,3]), wet.1901.2014)
write.csv(wet, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/climate.data/wet.1901.2014.csv", row.names = F)




r <- stack("S:/Global Maps Data/WorldClim/tiff/1bioclim_stacked_all.tif")
# plot(r) 
points(ForC_simplified)
WorldClim <- raster::extract(r, ForC_simplified)

WorldClimDF <- data.frame(ForC_simplified)
WorldClimDF <- cbind(WorldClimDF, WorldClim)
#rename variables
names(WorldClimDF)[36:54] <- c("AnnualMeanTemp", "MeanDiurnalRange", "Isothermality","TempSeasonality", "MaxTWarmestMonth", "MinTColdestMonth", "TempRangeAnnual", "MeanTWetQ", "MeanTDryQ","MeanTWarmQ","MeanTColdQ", "AnnualPre","PreWetMonth", "PreDryMonth", "PreSeasonality", "PreWetQ", "PreDryQ", "PreWarmQ", "PreColdQ")
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

head(WorldClimDF)

write.csv(WorldClimDF,"C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_WorldClim_CRU.csv", row.names = F)

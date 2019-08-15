##### extract WorldClim and CRU data for ForC_simplified

rm(list = ls())
library(raster)
library(ncdf4)
library(Hmisc)
library(progress)
library(dplyr)

ForC_simplified <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified.csv")

ForC_simplified <- ForC_simplified[!is.na(ForC_simplified$lat),]
ForC_simplified <- ForC_simplified[!is.na(ForC_simplified$lon),]
coordinates(ForC_simplified)<-c("lon", "lat")
proj<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(ForC_simplified)<-proj
plot(ForC_simplified)
as.data.frame(ForC_simplified)
ForC <- as.data.frame(ForC_simplified)

# frs
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.frs.dat.nc", varname="frs")
frs.1901.2014 <- raster::extract(r, ForC_simplified)

frs <- data.frame(ForC_simplified)
frs <- data.frame(measurement.ID = frs[,1], sites.sitename = as.character(frs[,2]), plot.name = as.character(frs[,3]), frs.1901.2014)

base <- data.frame(measurement.ID = frs[,1], sites.sitename = as.character(frs[,2]), plot.name = as.character(frs[,3]))
base$sites.sitename <- as.character(base$sites.sitename)
base$plot.name <- as.character(base$plot.name)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
years <- c(1901:2014)

subset.all <- NULL

for(year in years){
  subset <- frs[,grepl(year, colnames(frs))]
  if(year == 1901) subset.all <- subset
  subset.all <- cbind(subset, subset.all)
}

frs_month <- cbind(base, subset.all)

for(month in months){
  month_frame <- frs_month[,grep(paste0("\\.", month, "\\."), colnames(frs_month))]
  month_frame$mean <- rowSums(month_frame)
  month_frame$mean <- month_frame$mean/114
  colnames(month_frame)[colnames(month_frame)=="mean"] <- month
  base <- cbind(base, month_frame[, month])
}

names(base)[4:15] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
frs_by_month <- base

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for(month in months){
  month_frame <- frs_by_month[,paste(month)]
  month_frame <- ifelse(month_frame < 1, TRUE, FALSE)
  base <- cbind(base, month_frame)
}

names(base)[16:27] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

frs_months <- base[, c(1:3, 16:27)]

###### monthly average daily minimum temperature

# tmn
r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.tmn.dat.nc", varname="tmn")
tmn.1901.2014 <- raster::extract(r, ForC_simplified)

tmn <- data.frame(ForC_simplified)
tmn <- data.frame(measurement.ID = tmn[,1], sites.sitename = as.character(tmn[,2]), plot.name = as.character(tmn[,3]), tmn.1901.2014)

base <- data.frame(measurement.ID = tmn[,1], sites.sitename = as.character(tmn[,2]), plot.name = as.character(tmn[,3]))
base$sites.sitename <- as.character(base$sites.sitename)
base$plot.name <- as.character(base$plot.name)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
years <- c(1901:2014)

subset.all <- NULL

for(year in years){
  subset <- tmn[,grepl(year, colnames(tmn))]
  if(year == 1901) subset.all <- subset
  subset.all <- cbind(subset, subset.all)
}

tmn_month <- cbind(base, subset.all)

for(month in months){
  month_frame <- tmn_month[,grep(paste0("\\.", month, "\\."), colnames(tmn_month))]
  month_frame$mean <- rowSums(month_frame)
  month_frame$mean <- month_frame$mean/114
  colnames(month_frame)[colnames(month_frame)=="mean"] <- month
  base <- cbind(base, month_frame[, month])
}

names(base)[4:15] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
tmn_by_month <- base

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for(month in months){
  month_frame <- tmn_by_month[,paste(month)]
  month_frame <- ifelse(month_frame > 0.5, TRUE, FALSE)
  base <- cbind(base, month_frame)
}

names(base)[16:27] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

tmn_months <- base[, c(1:3, 16:27)]

########## mean annual temperature

climate <- c("cld", "pet", "pre", "tmp", "vap")

for(clim in climate){

# tmp
r <- brick(paste0("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.", clim, ".dat.nc"), varname= clim)
var.1901.2014 <- raster::extract(r, ForC_simplified)

var <- data.frame(ForC_simplified)
var <- data.frame(measurement.ID = var[,1], sites.sitename = as.character(var[,2]), plot.name = as.character(var[,3]), var.1901.2014)

base <- data.frame(measurement.ID = var[,1], sites.sitename = as.character(var[,2]), plot.name = as.character(var[,3]))
base$sites.sitename <- as.character(base$sites.sitename)
base$plot.name <- as.character(base$plot.name)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
years <- c(1901:2014)

subset.all <- NULL

for(year in years){
  subset <- var[,grepl(year, colnames(var))]
  if(year == 1901) subset.all <- subset
  subset.all <- cbind(subset, subset.all)
}

var_month <- cbind(base, subset.all)

for(month in months){
  month_frame <- var_month[,grep(paste0("\\.", month, "\\."), colnames(var_month))]
  month_frame$mean <- rowSums(month_frame)
  month_frame$mean <- month_frame$mean/114
  colnames(month_frame)[colnames(month_frame)=="mean"] <- month
  base <- cbind(base, month_frame[, month])
}

names(base)[4:15] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
var_by_month <- base

########## merge datasets

var <- data.frame(tmn_months, var_by_month[, c(4:15)])

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for(month in months){
x <- ifelse(var[, month] == TRUE, var[, paste0(month, ".1")], NA)
var <- cbind(var, x)
}

var <- var[, c(28:39)]
names(var) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
var$mean <- rowMeans(var, na.rm = TRUE)

ForC <- cbind(ForC, var$mean)
}

names(ForC)[35:39] <- c("cld", "pet", "pre", "tmp", "vap")                

write.csv(ForC, "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_growing_season_climate.csv", row.names = F)

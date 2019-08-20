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

########################## monthly PET

r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.pet.dat.nc", varname="pet")
pet.1901.2014 <- raster::extract(r, ForC_simplified)

# # pet
pet <- data.frame(ForC_simplified)
pet <- data.frame(measurement.ID = pet[,1], sites.sitename = as.character(pet[,2]), plot.name = as.character(pet[,3]), pet.1901.2014)

pet_colnames <- colnames(pet)[(4:1371)]
pet_colnames <- gsub("[a-zA-Z ]", "", pet_colnames)
pet_colnames <- as.Date(pet_colnames, format = "%Y.%m.%d")
pet_colnames <- monthDays(pet_colnames)

pet_month <- pet[, c(4:1371)]*matrix(rep(pet_colnames, nrow(pet)), nrow = nrow(pet), byrow = T)
pet_month <- cbind(pet[,c(1:3)], pet_month)

base <- data.frame(measurement.ID = pet[,1], sites.sitename = as.character(pet[,2]), plot.name = as.character(pet[,3]))
base$sites.sitename <- as.character(base$sites.sitename)
base$plot.name <- as.character(base$plot.name)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
years <- c(1901:2014)

subset.all <- NULL

for(year in years){
  subset <- pet_month[,grepl(year, colnames(pet_month))]
  if(year == 1901) subset.all <- subset
  subset.all <- cbind(subset, subset.all)
}

pet_month <- cbind(base, subset.all)

for(month in months){
  month_frame <- pet_month[,grep(paste0("\\.", month, "\\."), colnames(pet_month))]
  month_frame$mean <- rowSums(month_frame)
  month_frame$mean <- month_frame$mean/114
  month_frame$mean <- month_frame$mean*0.5 ################## because we are comparing pre > 0.5*pet
  colnames(month_frame)[colnames(month_frame)=="mean"] <- month
  base <- cbind(base, month_frame[, month])
}

names(base)[4:15] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
pet_by_month <- base

############## monthly pre

r <- brick("S:/Global Maps Data/CRU/v3.23/ncfiles/cru_ts3.23.1901.2014.pre.dat.nc", varname="pre")
pre.1901.2014 <- raster::extract(r, ForC_simplified)

pre <- data.frame(ForC_simplified)
pre <- data.frame(measurement.ID = pre[,1], sites.sitename = as.character(pre[,2]), plot.name = as.character(pre[,3]), pre.1901.2014)

base <- data.frame(measurement.ID = pre[,1], sites.sitename = as.character(pre[,2]), plot.name = as.character(pre[,3]))
base$sites.sitename <- as.character(base$sites.sitename)
base$plot.name <- as.character(base$plot.name)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
years <- c(1901:2014)

subset.all <- NULL

for(year in years){
  subset <- pre[,grepl(year, colnames(pre))]
  if(year == 1901) subset.all <- subset
  subset.all <- cbind(subset, subset.all)
}

pre_month <- cbind(base, subset.all)

for(month in months){
  month_frame <- pre_month[,grep(paste0("\\.", month, "\\."), colnames(pre_month))]
  month_frame$mean <- rowSums(month_frame)
  month_frame$mean <- month_frame$mean/114
  colnames(month_frame)[colnames(month_frame)=="mean"] <- month
  base <- cbind(base, month_frame[, month])
}

names(base)[4:15] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
pre_by_month <- base

############### pet and pre

pet_prec <- cbind(pre_by_month, pet_by_month[,4:15])
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

base <- data.frame(measurement.ID = pet[,1], sites.sitename = as.character(pet[,2]), plot.name = as.character(pet[,3]))
base$sites.sitename <- as.character(base$sites.sitename)
base$plot.name <- as.character(base$plot.name)

for(month in months){
  month_frame <- pet_prec[,grep(paste0(month), colnames(pet_prec))]
  month_frame$larger <- ifelse(month_frame[,1] > month_frame[,2], TRUE, FALSE)
  base <- cbind(base, month_frame$larger)
  water_months <- base
}

names(water_months)[4:15] <- c("Jan.1", "Feb.1", "Mar.1", "Apr.1", "May.1", "Jun.1", "Jul.1", "Aug.1", "Sep.1", "Oct.1", "Nov.1", "Dec.1")

for(month in months){
  month_frame <- pet_prec[,grep(paste0(month), colnames(pet_prec))]
  month_frame$index <- (month_frame[,1] - month_frame[,2])/month_frame[,2]
  base <- cbind(base, month_frame$index)
  moisture_index <- base
}

names(moisture_index)[4:15] <- c("Jan.1", "Feb.1", "Mar.1", "Apr.1", "May.1", "Jun.1", "Jul.1", "Aug.1", "Sep.1", "Oct.1", "Nov.1", "Dec.1")

##################### combine temperature and water criteria

var <- cbind(tmn_months, water_months[,4:15])

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for(month in months){
  x <- ifelse(var[, month] == TRUE & var[, paste0(month, ".1")] == TRUE, TRUE, FALSE)
  var <- cbind(var, x)
}

var <- var[, c(1:3, 28:39)]
names(var)[4:15] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
growing_season <- var

########## climate variables

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

var <- data.frame(growing_season, var_by_month[, c(4:15)])

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

##### count length of growing season

growing_season$count <- rowSums(growing_season == TRUE)

ForC <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_growing_season_climate.csv", stringsAsFactors = FALSE)

ForC <- cbind(ForC, growing_season$count)

ForC$monthly_mean <- ForC$mean/ForC$`growing_season$count`
ForC$length_growing_season <- ForC$`growing_season$count`

ForC <- ForC[, c(1:39, 41, 42)]

write.csv(ForC, "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_growing_season_climate.csv", row.names = F)


setwd("S:/Global Maps Data/WorldClim/tiff")
S_filenames<- paste("wc2.0_30s_srad_", c(paste(0,1:9, sep=""), 10, 11, 12), ".tif",sep="")
srad <- stack(S_filenames) 

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(srad) <- month

ForC_srad <- raster::extract(srad, ForC_simplified)
srad1 <- data.frame(ForC_simplified)
srad1 <- data.frame(measurement.ID = srad1[,1], sites.sitename = as.character(srad1[,2]), plot.name = as.character(srad1[,3]), ForC_srad)

var <- data.frame(growing_season, srad1[, c(4:15)])

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for(month in months){
  x <- ifelse(var[, month] == TRUE, var[, paste0(month, ".1")], NA)
  var <- cbind(var, x)
}

var <- var[, c(28:39)]
names(var) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
var$mean <- rowMeans(var, na.rm = TRUE)
var$sum <- rowSums(var, na.rm = TRUE)

ForC <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_growing_season_climate.csv", stringsAsFactors = FALSE)

ForC <- cbind(ForC, var$mean)
ForC <- cbind(ForC, var$sum)

names(ForC)[42] <- "solarradiationmonthly"
names(ForC)[43] <- "solarradiationtotal"
write.csv(ForC, "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC/ForC_simplified/ForC_simplified_growing_season_climate.csv", row.names = F)



# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(MuMIn)
library(plyr)
library(merTools)
library(visreg)
library(r2glmm)
library(nlme)
library(viridis)
library(AICcmodavg)

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_growing_season_climate.csv", stringsAsFactors = F)

VARIABLES <- read.csv(paste0(dirname(getwd()), "/ForC/data/ForC_variables.csv"), stringsAsFactors = F)


na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


# Prepare data ####

## change to numerics what it needs to
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)
ForC_simplified$min.db <- as.numeric(ForC_simplified$min.db)

## make plot and geographic areas factors
ForC_simplified$plot.name <- addNA(ForC_simplified$plot.name)
ForC_simplified$geographic.area <- addNA(ForC_simplified$geographic.area)


## take absolute value of latitude
ForC_simplified$lat <- abs(ForC_simplified$lat)

ForC_simplified$site_plot <- paste0(ForC_simplified$sites.sitename," ", ForC_simplified$plot.name)


## Keep only age >=100 (or 999)
age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
age.greater.than.200 <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
ages <- c("age.greater.than.100")

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0
ForC_simplified <- ForC_simplified[dist.to.keep, ]


## give leaf type
broadleaf_codes <- c("2TEB", "2TDB", "2TB")
needleleaf_codes <- c("2TEN", "2TDN", "2TN")
mixed <- c("2TD", "2TE", "2TM", "2TREE")

ForC_simplified$leaf.type <- ifelse(ForC_simplified$dominant.veg %in% broadleaf_codes, "broadleaf",
                                    ifelse(ForC_simplified$dominant.veg %in% needleleaf_codes, "needleleaf",
                                           ifelse(ForC_simplified$dominant.veg %in% mixed, "mixed", "Other")))

## give leaf phenology
evergreen_codes <- c("2TE", "2TEB", "2TEN")
deciduous_codes <- c("2TDN", "2TDB", "2TD")


ForC_simplified$leaf.phenology <- ifelse(ForC_simplified$dominant.veg %in% evergreen_codes, "evergreen",
                                         ifelse(ForC_simplified$dominant.veg %in% deciduous_codes, "deciduous", "Other"))

### exclude Tura due to extreme high latitude
ForC_simplified <- ForC_simplified[!(ForC_simplified$sites.sitename %in% "Tura"),]


## prepare results table

all.results <- NULL

###comparing models


all.results <- NULL
all.aictab <- NULL
all.koeppen <- NULL

fixed.variables <- c("cld", "pet", "pre", "tmp", "vap", "solarradiation")



response.variables.groups <- list(c("GPP", "NPP", "BNPP_root", "BNPP_root_fine"),
                                  c("ANPP", "ANPP_foliage", "ANPP_woody_stem"),
                                  c("R_auto", "R_auto_root"))

for(response.variables in response.variables.groups){
  
  if(response.variables[1] == "GPP") n <- 1
  if(response.variables[1] == "ANPP") n <- 2
  if(response.variables[1] == "R_auto") n <- 3
  
  ### mature forests only ####
  for (age in ages){
    
    if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
    if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
    
    for(fixed.v in fixed.variables){
      
      png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/growing_season/Effect_of_", fixed.v, "_MATURE_only_", age, "_", n, ".png"), width = 2255, height = 2000, units = "px", res = 300)
      
      par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,0))
      print(fixed.v)
      
      fixed.v.info <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
      
      xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
      
      ###subset ForC
      response.variables.col <- 1:length(response.variables)
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        col.sym <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
        
        col <- col.sym$col[which(col.sym$variable %in% response.v)]
        sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2")
        if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
        if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
        
        
        response.v.color <- response.variables.col[which(response.variables %in% response.v)]
        
        rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
        
        fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
        
        df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
        
        # df$masl <- df$masl/1000
        
        df$fixed <- df[, fixed.v]
        
        if(fixed.v == "AnnualPET") df <- df[df$fixed != 0,]
        # ylim <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]$mean)
        
        a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
        ylim <- range(tapply(a$monthly_mean, a$variable.name, scale))
        ylim[1] <- ylim[1] - 0.25
        ylim[2] <- ylim[2] + 0.25
        
        mod <-  lmer(scale(monthly_mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim <- lmer(scale(monthly_mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim.poly <- lmer(scale(monthly_mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        
        if (best.model == "mod") mod.full <- lmer(scale(monthly_mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim") mod.full <- lmer(scale(monthly_mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim.poly") mod.full <- lmer(scale(monthly_mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$monthly_mean)
        
        if (best.model == "mod.clim") mod.full <- lmer(scale(monthly_mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.clim.poly") mod.full <- lmer(scale(monthly_mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        
        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        if(first.plot) plot(scale(monthly_mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], pch = sym, yaxt = "n", ylim = ylim)
        if(!first.plot) points(scale(monthly_mean) ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
        
        
        lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
        
        first.plot <- FALSE
        
        mod.linear <- lmer(scale(monthly_mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        
        r <- round(fixef(mod.linear), 5)
        slope <- r[2]
        
        equation <-  paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2])
        legend <- paste(response.v, "sample size =", sample.size)
        mtext(side = 3, line = -which(response.variables %in% response.v), text = legend, adj = 0.1, col = plasma(10)[col], cex = 0.5)
        
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        significance <- signif(significance, digits=4)
        
        Rsq <- as.data.frame(r.squaredGLMM(mod.full))
        Rsq <- signif(Rsq, digits=4)
        legend2 <- paste(response.v, "r-squared = ", Rsq[1], "p-value = ", significance)
        mtext(side = 3, line = -which(response.variables %in% response.v), text = legend2, adj = 0.9, col = plasma(10)[col], cex = 0.5)
        
        number_plots <- length(unique(df$site_plot))
        
        date = Sys.Date()
        altitude = FALSE
        
        results <- data.frame(date.run = date, response = response.v, fixed = fixed.v, altitude_included = altitude, random = "geographic.area/plot.name", Age.filter = age, best.model = best.model, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq, delta.aic = delta.aic, linear.slope = slope, number.plots = number_plots)
        
        all.results <- rbind(all.results, results)
        all.aictab <- rbind(all.aictab, aictab)
        
      }
      
      
      title (paste("Effect of", fixed.v), outer = T, line = 1)
      mtext(side = 1, line = 3, text = eval(parse(text = xaxis)), outer = T)
      mtext(side = 2, line = 3,  text = expression("Productivity (scaled values)"), outer = T) 
      dev.off()
    }
    
  }
  
}


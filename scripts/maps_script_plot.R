rm(list = ls())
library(raster)
library(ncdf4)
library(Hmisc)
library(lme4)
library(MuMIn)
library(ggplot2)
library(viridis)
library(maps)


# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)
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

## change sign of NEE 
ForC_simplified[grepl("NEE", ForC_simplified$variable.name,ignore.case = F),]$mean <- -ForC_simplified[grepl("NEE", ForC_simplified$variable.name,ignore.case = F),]$mean

## take absolute value of latitude
# ForC_simplified$lat <- abs(ForC_simplified$lat)


# Control for some factors ####


## keep all ages except na, 0 and 999
ages.not.999.nor.0.nor.na <- !ForC_simplified$stand.age %in% 999 &  !ForC_simplified$stand.age %in% 0 & !is.na(ForC_simplified$stand.age)

## Keep only age >=100 (or 999)
age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
age.greater.than.200 <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
ages <- c("age.greater.than.100")

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0
ForC_simplified <- ForC_simplified[dist.to.keep, ]

## keep only records with min.dbh <= 10cm
ForC_simplified$min.dbh <- as.numeric(ForC_simplified$min.dbh)
# min.dbh.to.keep <- ForC_simplified$min.dbh <= 10 & is.na(ForC_simplified$min.dbh) # this doesn work great, not enough data, but maybe we can just keep the NAs?
min.dbh.to.remove <- ForC_simplified$min.dbh >= 10 & !is.na(ForC_simplified$min.dbh)
ForC_simplified <- ForC_simplified[-min.dbh.to.remove, ]

###keep only altitude below 500 metres
# 
# alt.to.keep <- ForC_simplified$masl <= 500 & !is.na(ForC_simplified$masl)
# ForC_simplified <- ForC_simplified[alt.to.keep, ]

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

# Prepare some variables ####

## response variable list (fluxes) ####
all.response.variables <- VARIABLES[c(4, 7:18, 25:32, 37:38, 51:52),]$variable.name
all.response.variables <- gsub("(_OM|_C)", "", all.response.variables)
all.response.variables <- gsub("(_0|_1|_2|_3|_4|_5)", "", all.response.variables)
all.response.variables <- all.response.variables[all.response.variables %in% ForC_simplified$variable.name]
all.response.variables <- unique(gsub("_\\d", "", all.response.variables))

response.variables.groups <- list(c("GPP", "NPP", "BNPP_root", "BNPP_root_fine"),
                                  c("ANPP_1", "ANPP_foliage", "ANPP_repro"),
                                  c("ANPP_woody", "ANPP_woody_stem", "ANPP_woody_branch"))

# response.variables.groups <- list(c("ANPP_1"))

all.response.variables[!all.response.variables %in% unlist(response.variables.groups)]

## fixed variables list ####
fixed.variables <- c("AnnualMeanTemp")

## prepare results table

all.results <- NULL

ForC_simplified$variable.name <- gsub("(_1|_2)", "", ForC_simplified$variable.name)

fixed.v <- c("AnnualMeanTemp")
fixed.no.na <- !is.na(ForC_simplified[, fixed.v])
ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)

ForC_simplified <- ForC_simplified[ages.to.keep & fixed.no.na, ]

ForC_simplified <- ForC_simplified[!is.na(ForC_simplified$lat),]
ForC_simplified <- ForC_simplified[!is.na(ForC_simplified$lon),]
coordinates(ForC_simplified)<-c("lon", "lat")
proj<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(ForC_simplified)<-proj

response.variables <- c("GPP", "NPP", "ANPP", "ANPP_woody_stem", "ANPP_foliage", "BNPP_root", "BNPP_root_fine", "R_auto", "R_auto_root")

# png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/maps/distribution_all_samples.png"), width = 2255, height = 2000, units = "px", res = 300)

# par(mfrow = c(1,1), mar = c(0,0,0,0), oa = c(0,0,0,1))

layout(matrix(1:2, nrow = 1), widths = c(1,1,0.25))
par(mar = c(3,3,3,0), oma = c(3,3,0,7))

first.plot = T


for(response.v in response.variables){
  
  col.sym <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
  
  col <- col.sym$col[which(col.sym$variable %in% response.v)]
  sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
  
  df <- ForC_simplified[ForC_simplified$variable.name %in% response.v,]
  if(first.plot) map(database = "world", interior = F, col = "darkgray", mar = c(0,0,0,0))
  points(df, col = plasma(10)[col], pch = sym)
  # legend <- paste(response.v)
  # mtext(side = 3, line = -(which(response.variables %in% response.v)), text = legend, adj = 0.95, col = plasma(10)[col], pch = sym, cex = 0.5, outer = T)
  
  first.plot = F
}

plot(1:1, type="n", axes = F)

legend("topleft", legend = c("GPP", "NPP", "ANPP", "BNPP_root", "R_auto"), col = plasma(10)[c(1, 3, 5, 8, 4)], pch = c(1, 3, 5, 8, 4), xpd = T, text.col = plasma(10)[c(1, 3, 5, 8, 4)], bty = "n", title = "Major fluxes", title.col = "black")
legend("left", legend = c("ANPP_woody_stem", "ANPP_foliage", "BNPP_root_fine", "R_auto_root"), col = plasma(10)[c(7, 9, 6, 2)], pch = c(7, 9, 6, 2), xpd = T, text.col = plasma(10)[c(7, 9, 6, 2)], bty = "n", title = "Subsidiary fluxes", title.col = "black")
# mtext(side = 3, line = -(which(col.sym$variable %in% response.v)), text = legend2, adj = 1, col = plasma(10)[col], cex = 0.5, outer = T)
# if(n==1) mtext(side = 3, line = 0, text = legend1, adj = 1, col = "black", cex = 0.5, outer = T)
# mtext(side = 3, line = -7 - which(response.variables %in% response.v), text = legend3, adj = 0.95, col = plasma(8)[response.v.color], cex = 0.5, outer = T)

dev.off()

# dev.off()


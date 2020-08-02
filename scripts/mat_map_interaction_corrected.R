

# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/becky/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(MuMIn)
library(AICcmodavg)
library(piecewiseSEM)
library(viridis)
library(mgcv)

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
ForC_simplified$lat <- abs(ForC_simplified$lat)

ForC_simplified$SolarRadiation <- ForC_simplified$SolarRadiation/1000


# Control for some factors ####


## keep all ages except na, 0 and 999
ages.not.999.nor.0.nor.na <- !ForC_simplified$stand.age %in% 999 &  !ForC_simplified$stand.age %in% 0 & !is.na(ForC_simplified$stand.age)

## Keep only age >=100 (or 999)
age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
age.greater.than.200 <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
ages <- c("age.greater.than.100")

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0

## keep only records with min.dbh <= 10cm
min.dbh.to.keep <- ForC_simplified$min.dbh <= 10 ##& !is.na(ForC_simplified$min.dbh) # this doesn work great, not enough data, but maybe we can just keep the NAs?
min.dbh.to.keep <- rep(TRUE, nrow(ForC_simplified))

ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)


fixed.no.na <- !is.na(ForC_simplified[, "map"]) & !is.na(ForC_simplified[, "masl"]) & !is.na(ForC_simplified[, "mat"])


ForC_simplified <- ForC_simplified[dist.to.keep & fixed.no.na & ages.to.keep, ]

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


all.response.variables[!all.response.variables %in% unlist(response.variables.groups)]


## prepare results table

all.results <- NULL
best.results <- NULL
outputs <- NULL

effects <- c("mat", "map", "(1|geographic.area/plot.name)")
pannel.nb <- 1
best.models <- NULL


response.variables <- c("GPP", "NPP", "ANPP", "ANPP_woody_stem", "ANPP_foliage", "BNPP_root", "BNPP_root_fine", "R_auto", "R_auto_root")
response.variables <- "NPP"

par(mfrow = c(3,3), mar = c(1,0,0,2), oma = c(5,8,2,0), xpd = T)

for (response.v in response.variables){
  
  if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
  if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
  if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
  if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
  
  
  rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
  
  fixed.no.na <- !is.na(ForC_simplified[, "map"]) & !is.na(ForC_simplified[, "mat"])
  df <- ForC_simplified[rows.with.response & fixed.no.na, ]
  df$masl <- (df$masl/1000)
  
  mod.null <- lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
  mod.single <- lmer(mean ~ mat + (1|geographic.area/plot.name), data = df, REML = F)
  mod.add <- lmer(mean ~ mat + map + (1|geographic.area/plot.name), data = df, REML = F)
  mod.int <- lmer(mean ~ mat * map + (1|geographic.area/plot.name), data = df, REML = F) 
  aictab <- aictab(list(mod.single = mod.single, mod.add = mod.add, mod.int = mod.int), sort = F)
  Rsq.s <- as.data.frame(r.squaredGLMM(mod.single))
  Rsq.s <- signif(Rsq.s, digits=4)[1]
  Rsq.a <- as.data.frame(r.squaredGLMM(mod.add))
  Rsq.a <- signif(Rsq.a, digits=4)[1]
  Rsq.i <- as.data.frame(r.squaredGLMM(mod.int))
  Rsq.i <- signif(Rsq.i, digits=4)[1]
  BIC <- BIC(mod.single, mod.add, mod.int)
  best.model <- as.character(aictab(list(mod.single = mod.single, mod.add = mod.add, mod.int = mod.int), sort = T)$Modname[1])
  
  sig.s <- anova(mod.null, mod.single)$"Pr(>Chisq)"[2] < 0.05
  
  if(!sig.s) best.model <- mod.null
  if(sig.s){
    sig.a <- anova(mod.single, mod.add)$"Pr(>Chisq)"[2] < 0.05
    
    if(!sig.a) best.model <- mod.single
    if(sig.a){
      dAIC <- AIC(mod.single) - AIC(mod.add) > 2
    if(!dAIC) best.model <- mod.single
    if(dAIC){
      sig.i <- anova(mod.add, mod.int)$"Pr(>Chisq)"[2] < 0.05
      if(!sig.i) best.model <- mod.add
      if(sig.i){ dAIC2 <- AIC(mod.add) - AIC(mod.int) > 2
      if(!dAIC2) best.model <- mod.add
      if(dAIC2) best.model <- mod.int
    }}
    }}
  
  best.mod <- as.character(best.model@call)[2]
  table <- data.frame(response.v = response.v, best.mod = best.mod)
  best.models <- rbind(best.models, table)

}

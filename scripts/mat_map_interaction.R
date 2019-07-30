

# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(MuMIn)
library(AICcmodavg)
library(piecewiseSEM)

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

effects <- c("mat", "map", "(1|geographic.area/plot.name)")


response.variables <- c("GPP", "NPP", "BNPP_root", "BNPP_root_fine", "ANPP", "ANPP_foliage", "ANPP_woody_stem","R_auto", "R_auto_root")

png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/interactions/mat_map_interaction.png"), width = 2255, height = 2000, units = "px", res = 300)
par(mfrow = c(3,3), mar = c(0,0,0,0), oma = c(5,5,2,0))

for (response.v in response.variables){
  
  if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1")
  if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1")
  if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
  if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
  
  
  rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
  
  fixed.no.na <- !is.na(ForC_simplified[, "map"]) & !is.na(ForC_simplified[, "mat"])
  df <- ForC_simplified[rows.with.response & fixed.no.na, ]
  df$masl <- (df$masl/1000)
  
  mod.full <- lmer(mean ~ mat * map + (1|geographic.area/plot.name), data = df, REML = F)
  significant.effect.of.interaction <- drop1(mod.full)$AIC[2] > drop1(mod.full)$AIC[1]
  
  if(!significant.effect.of.interaction) {

    mod.full <- lmer(mean ~ mat + map + (1|geographic.area/plot.name), data = df, REML = F)
    mod <- lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
    
    significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05

    significant.effect.of.additive <- drop1(mod.full)$AIC[3] > drop1(mod.full)$AIC[1]

    if(!significant.effect.of.additive) {
      mod.full <- lmer(mean ~ mat + (1|geographic.area/plot.name), data = df)
      significant.effect <- drop1(mod.full)$AIC[2] > drop1(mod.full)$AIC[1]
    }

    if(significant.effect.of.additive) { # just to know if there is a significant effect of the main fixed effect
      significant.effect <-  drop1(mod.full)$AIC[2] > drop1(mod.full)$AIC[1]
    }

  }
  
  if(significant.effect.of.interaction) { # just to know if there is a significant effect of the main fixed effect
    significant.effect.of.additive <- FALSE
    significant.effect <- TRUE
  }
  
  significant.effect.of.additive <- TRUE
  
  
  
  
  
  
  
  # par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,0))
  ylim = range(df$mean)
  
  if (significant.effect.of.interaction | significant.effect.of.additive) {
    # predict 
    
    newDat <- expand.grid(mat = seq(min(df$mat), max(df$mat), length.out = 100), map = c(500, 1000, 2000, 3000))
    
    newDat$fit <- predict(mod.full, newDat, re.form = NA)
    
    first.plot <- TRUE
    # plot
      if(first.plot) plot(mean ~ mat, data = df, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = ylim)
      if(!first.plot) points(mean ~ fixed, data = df, ylab = "", col = response.v.color) 
      
      for(map in unique(newDat$map)){
        i <- which(unique(newDat$map) %in% map)
        lines(fit ~ mat, data = newDat[newDat$map %in% map,], lty = ifelse(significant.effect.of.interaction|significant.effect, 1, 2), lwd = i)
        
      }
    if(response.v == "GPP") legend("topleft", lwd = c(1:4), legend = c(500, 1000, 2000, 3000))
    # title (paste(response.v), outer = T, line = 1)
    mtext(side = 1, line = 3, text = "Mean Annual Temperature", outer = T)
    mtext(side = 2, line = 3,  text = expression("Mg C"~ha^-1~yr^-1), outer = T)
    
    # add equation
    
    # r <- round(fixef(mod.full), 2)
    # 
    # if(significant.effet.of.interaction) equation <- paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2], " + age x", r[3], "+", r[4], " x interaction" )
    # if(significant.effet.of.additive) equation <-  paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2], " + age x", r[3])
    # 
    # 
   mtext(side = 3, line = -1, text = paste(response.v), adj = 0.1, cex = 0.5)
  }
  
  #create all combinations of random / fixed effects
  
  
}

dev.off()


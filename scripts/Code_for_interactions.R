

# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(MuMIn)

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


library(AICcmodavg)
all.results <- NULL
all.aictab <- NULL

fixed.variables.set1 <- c("mat", "map", "TempSeasonality", "AnnualWetDays", "SolarRadiation", "Aridity", "PotentialEvapotranspiration", "VapourPressureDeficit")
fixed.variables.set2 <- c("mat", "map", "TempSeasonality", "AnnualWetDays", "SolarRadiation", "Aridity", "PotentialEvapotranspiration", "VapourPressureDeficit")


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
    
    for(fixed.v1 in fixed.variables.set1){
      for(fixed.v2 in fixed.variables.set2){
        if (fixed.v1 == fixed.v2) {
          next
        }
      
      # tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/test/best_model/Effect_of_", fixed.v, "_MATURE_only_", age, "_", n, ".tiff"), width = 2255, height = 2000, units = "px", res = 300)
      # 
      par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,0))
      print(fixed.v1)
      
      
      
      ###subset ForC
      response.variables.col <- 1:length(response.variables)
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2")
        if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
        if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
        
        
        response.v.color <- response.variables.col[which(response.variables %in% response.v)]
        
        rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
        
        fixed.no.na.1 <- !is.na(ForC_simplified[, fixed.v1])
        fixed.no.na.2 <- !is.na(ForC_simplified[, fixed.v2]) & !is.na(ForC_simplified[, "masl"])
        
        df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na.1 & fixed.no.na.2, ]
        
        df$fixed1 <- df[, fixed.v1]
        df$fixed2 <- df[, fixed.v2]
        df$masl <- (df$masl/1000)
        ylim <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]$mean)
        
        mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.linear <- lmer(mean ~ poly(fixed1, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        mod.poly <- lmer(mean ~ poly(fixed1, 2) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        mod.int <- lmer(mean ~ fixed1*fixed2 + masl + (1|geographic.area/plot.name), data = df, REML = F)
        mod.add <- lmer(mean ~ fixed1 + fixed2 + masl + (1|geographic.area/plot.name), data = df, REML = F)
        
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly, mod.int = mod.int, mod.add = mod.add), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod.linear = mod.linear, mod.poly = mod.poly, mod.int = mod.int, mod.add = mod.add), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        if (best.model == "mod.poly") mod.full <- lmer(mean ~ poly(fixed1, 2) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.linear") mod.full <- lmer(mean ~ poly(fixed1, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod") mod.full <- lmer(mean ~ poly(fixed1, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.int") mod.full <- lmer(mean ~ fixed1*fixed2 + masl + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.add") mod.full <- lmer(mean ~ fixed1 + fixed2 + masl + (1|geographic.area/plot.name), data = df, REML = F)
        
         significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.poly") mod.full <- lmer(mean ~ poly(fixed1, 2) + masl + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.linear") mod.full <- lmer(mean ~ poly(fixed1, 1) + masl + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod") mod.full <- lmer(mean ~ poly(fixed1, 1) + masl + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.int") mod.full <- lmer(mean ~ fixed1*fixed2 + masl + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.add") mod.full <- lmer(mean ~ fixed1 + fixed2 + masl + (1|geographic.area/plot.name), data = df, REML = T)
        
               
        r <- round(fixef(mod.full), 2)
        equation <-  paste(response.v, "=", r[1], "+", fixed.v1,  "x", r[2])
       
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        significance <- signif(significance, digits=4)
        
        Rsq <- as.data.frame(r.squaredGLMM(mod.full))
        Rsq <- signif(Rsq, digits=4)
        
        results <- data.frame(response = response.v, fixed1 = fixed.v1, fixed2 = fixed.v2, random = "geographic.area/plot.name", Age.filter = age, best.model = best.model, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq, delta.aic = delta.aic)
        
        all.results <- rbind(all.results, results)
        
      }
      
    }
    
    }
  }
  
}

write.csv(all.results, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/best_model_outputs/global_trend_models_interaction.csv", row.names = F)

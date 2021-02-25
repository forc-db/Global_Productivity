
# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("")

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
library(rlist)

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)

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

ForC_simplified$site_plot <- paste0(ForC_simplified$sites.sitename," ", ForC_simplified$plot.name)


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

### exclude Tura due to extreme high latitude
ForC_simplified <- ForC_simplified[!(ForC_simplified$sites.sitename %in% "Tura"),]

## prepare results table
all.results <- NULL

###comparing models
all.results <- NULL
all.aictab <- NULL
all.koeppen <- NULL
aictab.top <- NULL

fixed.variables <- c("mat", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "SolarRadiation", "TempSeasonality", "PreSeasonality", "length_growing_season", "TempRangeAnnual", "Aridity", "CloudCover", "AnnualFrostDays", "AnnualWetDays", "MaxVPD", "WaterStressMonths")


response.variables.groups <- list(c("GPP", "NPP", "BNPP_root", "BNPP_root_fine"),
                                  c("ANPP", "ANPP_foliage", "ANPP_woody_stem"),
                                  c("R_auto", "R_auto_root"))

# response.variables.groups <- list(c("NPP"))

for(response.variables in response.variables.groups){
  
  if(response.variables[1] == "GPP") n <- 1
  if(response.variables[1] == "ANPP") n <- 2
  if(response.variables[1] == "R_auto") n <- 3
  
  ### mature forests only ####
  for (age in ages){

      for (response.v in response.variables){
        
        col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
        
        col <- col.sym$col[which(col.sym$variable %in% response.v)]
        sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
        if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
        if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
        
        
        rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
        
        model_list <- NULL
        
        df <- ForC_simplified[rows.with.response, ]
        
        df$mat <- df$mat + 11
        df$AnnualFrostDays <- df$AnnualFrostDays + 0.1
        df$WaterStressMonths <- df$WaterStressMonths + 0.1
        
        for(fixed.v in fixed.variables){
          ages.to.keep <- df$stand.age >= 100 & !is.na(df$stand.age)
          fixed.no.na <- !is.na(df[, fixed.v]) & !is.na(df[, "masl"])
          df <- df[ages.to.keep & fixed.no.na, ]
        
        # df$masl <- df$masl/1000
        
        df$fixed <- df[, fixed.v]
        
        
        # ylim <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]$mean)
        
        a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]

        mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim.poly <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim.log <- lmer(scale(mean) ~ log(fixed) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.poly = mod.clim.poly, mod.clim.log = mod.clim.log), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.poly = mod.clim.poly, mod.clim.log = mod.clim.log), sort = T)$Modname[1])
        
        if (best.model == "mod.clim.poly"){ 
          
          test.delta.aic <- as.numeric(aictab(list(mod.clim = mod.clim, mod.clim.poly = mod.clim.poly))$Delta_AICc[2])
          
          best.model <- ifelse(test.delta.aic > 2, "mod.clim.poly", "mod.clim")
        }
        
        if (best.model == "mod.clim"){ 
          
          test.delta.aic <- as.numeric(aictab(list(mod = mod, mod.clim = mod.clim))$Delta_AICc[2])
          
          best.model <- ifelse(test.delta.aic > 2, "mod.clim", "mod")
        }
        
        if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        if (best.model == "mod.clim.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        if (best.model == "mod.clim.log") mod.full <- lmer(scale(mean) ~ log(fixed) + (1|geographic.area/plot.name), data = df, REML = F)
        
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.full = mod.full))$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.clim.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.clim.log") mod.full <- lmer(scale(mean) ~ log(fixed) + (1|geographic.area/plot.name), data = df, REML = T)

        mod.linear <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        
        r <- round(fixef(mod.linear), 5)
        slope <- r[2]
        
        final.model <- as.character(mod.full@call)[2]
        
        final.model <- gsub("fixed", fixed.v, final.model, fixed = T)
        
        is.sig <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        
        if(is.sig) model_list <- append(model_list, final.model)
        
        
        equation <-  paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2])
        
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        significance <- signif(significance, digits=4)
        
        Rsq <- as.data.frame(r.squaredGLMM(mod.full))
        Rsq <- signif(Rsq, digits=4)

        number_plots <- length(unique(df$site_plot))
        geog_area <- length(unique(df$geographic.area))
        
        date = Sys.Date()
        altitude = FALSE
        
        results <- data.frame(date.run = date, response = response.v, fixed = fixed.v, altitude_included = altitude, random = "geographic.area/plot.name", Age.filter = age, best.model = best.model, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq, delta.aic = delta.aic, linear.slope = slope, number.plots = number_plots, geographic.areas = geog_area)
        
        all.results <- rbind(all.results, results)
        all.aictab <- rbind(all.aictab, aictab)
        }
      
        
        all_models <- lapply(model_list, function(x){
          fit1 <- lmer(x, data = df, REML=FALSE)
          return(fit1)
        })
       aictab.list <- aictab(all_models, sort = T, modnames = model_list)
       aictab.best <- aictab.list[aictab.list$Delta_AICc <= 2,]
       aictab.best <- aictab.best[, c(1,4)]
       aictab.best$flux <- paste(response.v)
       aictab.top <- rbind(aictab.top, aictab.best)

      write.csv(aictab.list, file = "")
      }
      
      
    
    
    
  }
  
}


write.csv(aictab.top, file = "")
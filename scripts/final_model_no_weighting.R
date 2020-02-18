
# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/becky/Dropbox (Smithsonian)/GitHub/ForC")

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

## keep only records with min.dbh <= 10cm ## commented out because no values with min.dbh > 10; comment in with future merges if this changes
# ForC_simplified$min.dbh <- as.numeric(ForC_simplified$min.dbh)
# # min.dbh.to.keep <- ForC_simplified$min.dbh <= 10 & is.na(ForC_simplified$min.dbh) # this doesn work great, not enough data, but maybe we can just keep the NAs?
# 
# min.dbh.to.remove <- ForC_simplified$min.dbh >= 10 & !is.na(ForC_simplified$min.dbh)
# ForC_simplified <- ForC_simplified[-min.dbh.to.remove, ]

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


## prepare results table

all.results <- NULL

###comparing models


all.results <- NULL
all.aictab <- NULL
all.koeppen <- NULL

fixed.variables <- c("mat", "map", "lat", "AnnualMeanTemp", "TempSeasonality", "TempRangeAnnual", "AnnualPre", "AnnualFrostDays", "AnnualWetDays", "Aridity", "PotentialEvapotranspiration", "VapourPressureDeficit", "SolarRadiation", "PreSeasonality", "MaxVPD", "WaterStressMonths", "length_growing_season")



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
      
      png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/Effect_of_", fixed.v, "_MATURE_only_", age, "_", n, ".png"), width = 2255, height = 2000, units = "px", res = 300)
      
      par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,0))
      print(fixed.v)
      
      fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
      
      xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
      
      ###subset ForC
      response.variables.col <- 1:length(response.variables)
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
        
        col <- col.sym$col[which(col.sym$variable %in% response.v)]
        sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
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
        ylim <- range(tapply(a$mean, a$variable.name, scale))
        ylim[1] <- ylim[1] - 0.25
        ylim[2] <- ylim[2] + 0.25
        
        if(!fixed.v %in% c("mat", "lat", "PreSeasonality", "SolarRadiation")){
        
        mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim.poly <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
      
        if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)

        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.clim.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        }
        
        if(fixed.v %in% c("mat", "lat", "PreSeasonality", "SolarRadiation")){
          
          mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
          mod.clim <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          
          aictab <- aictab(list(mod = mod, mod.clim = mod.clim), sort = T)

          best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim), sort = T)$Modname[1])
          delta.aic <- as.numeric(aictab(list(mod = mod, mod.clim = mod.clim), sort = T)$Delta_AICc[2])
          delta.aic <- signif(delta.aic, digits=4)
          
          
          if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
         
          significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
          significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
          sample.size <- length(df$mean)
          
          if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        }
        
        
        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], pch = sym, yaxt = "n", ylim = ylim)
        if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
        
        
        lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
        
        first.plot <- FALSE
        
        mod.linear <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
      
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
        geog_area <- length(unique(df$geographic.area))
        
        date = Sys.Date()
        altitude = FALSE
        
        results <- data.frame(date.run = date, response = response.v, fixed = fixed.v, altitude_included = altitude, random = "geographic.area/plot.name", Age.filter = age, best.model = best.model, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq, delta.aic = delta.aic, linear.slope = slope, number.plots = number_plots, geographic.areas = geog_area)
        
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


write.csv(all.results, file = "C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/best_model_outputs/best_model_scaled_unweighted.csv", row.names = F)


### for graph with two plots, latitude only, saved to supporting information mature forests only ####

fixed.variables <- "lat"

response.variables.groups <- list(c("GPP", "NPP", "ANPP", "BNPP_root", "R_auto"),
                                  c("ANPP_foliage", "ANPP_woody_stem", "BNPP_root_fine", "R_auto_root"))

all.results = NULL

for (age in ages){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  for(fixed.v in fixed.variables){
    
    png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/supporting_information/latitude unscaled.png"), width = 5000, height = 2000, units = "px", res = 300)
    
    layout(matrix(1:3, nrow = 1), widths = c(1,1,0.25))
    par(mar = c(3,3,3,0), oma = c(3,3,0,7))
    
    print(fixed.v)
    
    fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
    
    xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
    
    for(response.variables in response.variables.groups){
      
      if(response.variables[1] == "GPP") n <- 1
      if(response.variables[1] == "ANPP_foliage") n <- 2
    
      first.plot <- TRUE
      
    for (response.v in response.variables){
      
      col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
      
      col <- col.sym$col[which(col.sym$variable %in% response.v)]
      sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
      
      if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
      if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
      if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
      
      rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
      
      fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
      
      df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
      
      # df$masl <- df$masl/1000
      
      df$fixed <- df[, fixed.v]
      
      if(response.v %in% c("GPP", "ANPP_foliage")){
      a <- ForC_simplified[ages.to.keep, ]
      a <- a[a$variable.name %in% unlist(response.variables),]
      ylim <- range(a$mean)
      ylim[1] <- ylim[1] - 0.25
      ylim[2] <- ylim[2] + 0.25
      }
      
      if(response.v == "GPP"){
        b <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables.groups),]
        no.na <- !is.na(b[, fixed.v])
        b <- b[no.na,]
        xlim <- range(b[, fixed.v])
      }
      
      
      mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
      mod.linear <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
      mod.poly <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = F)
      
      aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
      
      best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Modname[1])
      delta.aic <- as.numeric(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Delta_AICc[2])
      delta.aic <- signif(delta.aic, digits=4)
      
      if (best.model == "mod.poly") mod.full <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = F)
      if (best.model == "mod.linear") mod.full <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
      if (best.model == "mod") mod.full <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
      
      ###in this case only linear model used - override AIC results here
      mod.full <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
      
      significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      sample.size <- length(df$mean)
      
      if (best.model == "mod.poly") mod.full <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = T)
      if (best.model == "mod.linear") mod.full <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = T)
      
      ###in this case only linear model used - override AIC results here
      mod.full <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = T)
      
      newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
      newDat$fit <- predict(mod.full, newDat, re.form = NA)
     
      if(first.plot) plot(mean ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], pch = sym, ylim = ylim, xlim = xlim)
      if(!first.plot) points(mean ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
      
      lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
 
      first.plot <- FALSE
      
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      significance <- signif(significance, digits=4)
      
      Rsq <- as.data.frame(r.squaredGLMM(mod.full))
      Rsq <- signif(Rsq, digits=4)
      legend1 = "R-squared values"
      legend2 <- paste(response.v, " = ", Rsq[1])
      # legend3 <- paste(response.v, "p-value = ", significance)
      
      
      
      results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
      
      all.results <- rbind(all.results, results)
      
    }
    
    
    if(n == 1) title(paste("Major fluxes"), outer = F, line = 1)
    if(n == 2) title(paste("Subsidiary fluxes"), outer = F, line = 1)
    mtext(side = 1, line = 2, text = eval(parse(text = xaxis)), outer = T)
    mtext(side = 2, line = 1,  text = expression("Productivity (scaled values)"), outer = T) 
    
    
    
  }
    plot(1:1, type="n", axes = F)
    
    legend("topleft", legend = c("GPP", "NPP", "ANPP", "BNPP_root", "R_auto"), col = plasma(10)[c(1, 3, 5, 8, 4)], pch = c(1, 3, 5, 8, 4), xpd = T, text.col = plasma(10)[c(1, 3, 5, 8, 4)], bty = "n", title = "Major fluxes", title.col = "black")
    legend("left", legend = c("ANPP_woody_stem", "ANPP_foliage", "BNPP_root_fine", "R_auto_root"), col = plasma(10)[c(7, 9, 6, 2)], pch = c(7, 9, 6, 2), xpd = T, text.col = plasma(10)[c(7, 9, 6, 2)], bty = "n", title = "Subsidiary fluxes", title.col = "black")
    # mtext(side = 3, line = -(which(col.sym$variable %in% response.v)), text = legend2, adj = 1, col = plasma(10)[col], cex = 0.5, outer = T)
    # if(n==1) mtext(side = 3, line = 0, text = legend1, adj = 1, col = "black", cex = 0.5, outer = T)
    # mtext(side = 3, line = -7 - which(response.variables %in% response.v), text = legend3, adj = 0.95, col = plasma(8)[response.v.color], cex = 0.5, outer = T)
    
    dev.off()
}
}


################################

response.variables.groups <- list(c("GPP", "NPP", "ANPP", "BNPP_root", "R_auto"),
                                  c("ANPP_foliage", "ANPP_woody_stem", "BNPP_root_fine", "R_auto_root"))

all.results = NULL


### mature forests only ####
for (age in ages){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  for(fixed.v in fixed.variables){
    
    print(fixed.v)
    
    fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
    
    xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
   
    for(response.variables in response.variables.groups){
      
      if(response.variables[1] == "GPP") n <- 1
      if(response.variables[1] == "ANPP_foliage") n <- 2
      
      png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/Effect_of_", fixed.v, n, "_MATURE_only_poly_all.png"), width = 2255, height = 2000, units = "px", res = 300)
      
      par(mfrow = c(1,1), mar = c(3,3,3,3))
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
        
        col <- col.sym$col[which(col.sym$variable %in% response.v)]
        sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
        if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
        
        
        ##response.v.color <- response.variables.col[which(response.variables %in% response.v)]
        
        rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
        
        fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
        
        df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
        
        # df$masl <- df$masl/1000
        
        df$fixed <- df[, fixed.v]
        
        a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
        ylim <- range(tapply(a$mean, a$variable.name, scale))
        ylim[1] <- ylim[1] - 0.25
        ylim[2] <- ylim[2] + 0.25
        
        
        mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.linear <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        mod.poly <- lmer(scale(mean) ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = T)

        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], pch = sym, yaxt = "n", ylim = ylim)
        if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
        
        lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
        
        first.plot <- FALSE
        
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        significance <- signif(significance, digits=4)
        
        Rsq <- as.data.frame(r.squaredGLMM(mod.full))
        Rsq <- signif(Rsq, digits=4)
        legend1 = "R-squared values"
        legend2 <- paste(response.v, " = ", Rsq[1])
        # legend3 <- paste(response.v, "p-value = ", significance)
        legend1 <- paste(response.v)
        mtext(side = 3, line = -(which(response.variables %in% response.v)), text = legend2, adj = 0.95, col = plasma(10)[col], cex = 0.5, outer = F)
        
        
        
        results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
        
        all.results <- rbind(all.results, results)
        
      }
      
      
      if(n == 1) title(paste("Major fluxes"), outer = F, line = 1)
      if(n == 2) title(paste("Subsidiary fluxes"), outer = F, line = 1)
      mtext(side = 1, line = 2, text = eval(parse(text = xaxis)), outer = F)
      mtext(side = 2, line = 1,  text = expression("Productivity (scaled values)"), outer = F) 
      
      
      dev.off()
      
      
      
    }
    
   
    # mtext(side = 3, line = -(which(col.sym$variable %in% response.v)), text = legend2, adj = 1, col = plasma(10)[col], cex = 0.5, outer = T)
    # if(n==1) mtext(side = 3, line = 0, text = legend1, adj = 1, col = "black", cex = 0.5, outer = T)
    # mtext(side = 3, line = -7 - which(response.variables %in% response.v), text = legend3, adj = 0.95, col = plasma(8)[response.v.color], cex = 0.5, outer = T)
    
  }
}


################################ graph combined plots



fixed.variables <- c("mat", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "TempSeasonality", "length_growing_season")

response.variables <- c("GPP", "NPP", "BNPP_root", "ANPP")

all.results = NULL

### mature forests only ####
for (age in ages){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/combined_plots.png"), width = 2255, height = 3000, units = "px", res = 300)
  
  par(mfrow = c(3,2), mar = c(4,2,2,2), oma = c(0,3,0,0))
  
  pannel.nb <- 1
  
  for(fixed.v in fixed.variables){
    
    
    print(fixed.v)
    
    a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
    ylim <- range(tapply(a$mean, a$variable.name, scale))
    ylim[1] <- ylim[1] - 0.25
    ylim[2] <- ylim[2] + 0.25
    
    fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
    
    xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
    
    ###subset ForC
    
    first.plot <- TRUE
    
    for (response.v in response.variables){
      
      if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
      if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
      if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
      
      col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
      
      col <- col.sym$col[which(col.sym$variable %in% response.v)]
      sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
      
      rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
      
      fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
      
      df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
      
      # df$masl <- df$masl/1000
      
      df$fixed <- df[, fixed.v]
      
      a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
      ylim <- range(tapply(a$mean, a$variable.name, scale))
      ylim[1] <- ylim[1] - 0.25
      ylim[2] <- ylim[2] + 0.25
      
      if(!fixed.v %in% c("mat", "lat", "PreSeasonality", "SolarRadiation")){
        
        mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim.poly <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        
        if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.clim.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
      }
      
      if(fixed.v %in% c("mat", "lat", "PreSeasonality", "SolarRadiation")){
        
        mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod = mod, mod.clim = mod.clim), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.clim = mod.clim), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        
        if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
      }
      newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
      newDat$fit <- predict(mod.full, newDat, re.form = NA)

      if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], ylim = ylim, pch = sym)
      if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
      
      lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
 
      if(first.plot) {
        axis(1 ,labels = ifelse(pannel.nb %in% c(3,4), TRUE, FALSE))
        axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
      }
      
      first.plot <- FALSE
      
      # equation <-  paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2])
      
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      significance <- signif(significance, digits=4)
      
      Rsq <- as.data.frame(r.squaredGLMM(mod.full))
      Rsq <- signif(Rsq, digits=4)

      results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
      
      all.results <- rbind(all.results, results)
      
    }
    
    
    mtext(paste0("(", letters[pannel.nb], ")"), side = 3, line = -1.5, adj = 0.05)
    mtext(side = 1, line = 3, text = eval(parse(text = xaxis)), outer = F)
    # mtext(side = 2, line = 3,  text = expression("Mg C"~ha^-1~yr^-1), outer = F) 
    if(fixed.v == "map")legend("topright", legend = c("GPP", "NPP", "ANPP", "BNPP_root"), col = plasma(10)[c(1, 3, 5, 8)], pch = c(1, 3, 5, 8), xpd = T, text.col = plasma(10)[c(1, 3, 5, 8)], bty = "n", xjust = 1, cex = 0.75)
    
    pannel.nb <- pannel.nb +1
    
  }
  
  mtext(side = 2, line = 1,  text = expression("Productivity (scaled values)"), outer = T)
  
  dev.off()
}


################################ graph plots no points

response.variables.groups <- list(c("GPP", "NPP", "ANPP", "BNPP_root", "ANPP_foliage", "ANPP_woody_stem", "BNPP_root_fine", "R_auto", "R_auto_root"))
fixed.variables <- "lat"

all.results = NULL
all.legends = NULL


### mature forests only ####
for (age in ages){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  for(fixed.v in fixed.variables){
    
    print(fixed.v)
    
    fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
    
    xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
    
    for(response.variables in response.variables.groups){
      
      if(response.variables[1] == "GPP") n <- 1
      if(response.variables[1] == "ANPP_foliage") n <- 2
      
      png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/effect_of_", fixed.v, "_transparent.png"), width = 2255, height = 2000, units = "px", res = 300)
      
      par(mfrow = c(1,1), mar = c(3,3,3,3))
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
        
        col <- col.sym$col[which(col.sym$variable %in% response.v)]
        sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
        lty <- col.sym$lty[which(col.sym$variable %in% response.v)]
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
        if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
        
        
        # response.v.color <- response.variables.col[which(response.variables %in% response.v)]
        
        rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
        
        fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
        
        df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
        
        # df$masl <- df$masl/1000
        
        df$fixed <- df[, fixed.v]
        
        a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
        ylim <- range(tapply(a$mean, a$variable.name, scale))
        ylim[1] <- ylim[1] - 0.25
        ylim[2] <- ylim[2] + 0.25
        
        
        mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.linear <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod = mod, mod.linear = mod.linear), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)

        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = T)
        
        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        # if(first.plot) plot((mean) ~ fixed, data = df, xlab = "", ylab = "", col = "white", pch = sym, yaxt = "n", ylim = ylim)
        if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10, alpha = 0.3)[col], pch = sym, ylim = ylim)
        if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10, alpha = 0.3)[col], pch = sym)
        
        lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = lty)
        
        first.plot <- FALSE
        
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        significance <- signif(significance, digits=4)
        
        Rsq <- as.data.frame(r.squaredGLMM(mod.full))
        Rsq <- signif(Rsq, digits=4)
        # legend1 = "R-squared values"
        # legend2 <- paste(response.v, " = ", Rsq[1])
        # # legend3 <- paste(response.v, "p-value = ", significance)
        # legend1 <- paste(response.v)
        # mtext(side = 3, line = -(which(response.variables %in% response.v)), text = legend2, adj = 0.95, col = plasma(10)[col], cex = 0.5, outer = F)
        # 
        
        legend <- paste0(response.v, "; Rsq = ", Rsq[1])
        
        results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
        
        all.results <- rbind(all.results, results)
        all.legends <- rbind(all.legends, legend)
        
      }
      all.legends <- as.character(all.legends)
      
      # if(n == 1) title(paste("Major fluxes"), outer = F, line = 1)
      # if(n == 2) title(paste("Subsidiary fluxes"), outer = F, line = 1)
      mtext(side = 1, line = 2, text = "Latitude", outer = F)
      mtext(side = 2, line = 2,  text = expression("Productivity (scaled values)"), outer = F) 
      
      legend("topright", legend = all.legends, col = plasma(10)[c(1, 3, 5, 8, 9, 7, 6, 4, 2)], pch = c(1, 3, 5, 8, 9, 7, 6, 4, 2), xpd = T, lty = c(1, 6, 5, 1, 6, 5, 6, 1, 5), text.col = plasma(10)[c(1, 3, 5, 8, 9, 7, 6, 4, 2)], bty = "n", title.col = "black", cex = 0.75)

      
      dev.off()
      
      
      
    }
    
    
    # mtext(side = 3, line = -(which(col.sym$variable %in% response.v)), text = legend2, adj = 1, col = plasma(10)[col], cex = 0.5, outer = T)
    # if(n==1) mtext(side = 3, line = 0, text = legend1, adj = 1, col = "black", cex = 0.5, outer = T)
    # mtext(side = 3, line = -7 - which(response.variables %in% response.v), text = legend3, adj = 0.95, col = plasma(8)[response.v.color], cex = 0.5, outer = T)
    
  }
}


################################ graph grid one plot one variable climate and seasonality



fixed.variables <- c("mat", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "SolarRadiation", "TempSeasonality", "PreSeasonality", "length_growing_season")
fixed.variables.groups <- list(c("mat", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "SolarRadiation"),
                               c("TempSeasonality", "PreSeasonality", "length_growing_season"))

response.variables <- c("GPP", "NPP", "ANPP", "ANPP_woody_stem", "ANPP_foliage", "BNPP_root", "BNPP_root_fine", "R_auto", "R_auto_root")
response.variables.groups <- list(c("GPP", "NPP", "ANPP", "ANPP_woody_stem", "ANPP_foliage"),
                                  c("BNPP_root", "BNPP_root_fine", "R_auto", "R_auto_root"))

all.results = NULL
number = 1


### mature forests only ####
for (fixed.variables.group in fixed.variables.groups){
  
  type <- ifelse(fixed.variables.group %in% "mat", "climate", "seasonality")
  
for (age in ages){
  for (response.variables.group in response.variables.groups){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  
  if(type == "climate") png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/manuscript/tables_figures/grid_plots_", type, number, ".png"), width = 3000, height = 2250, units = "px", res = 300)
  if(type == "seasonality") png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/manuscript/tables_figures/grid_plots_", type, number, ".png"), width = 2000, height = 3000, units = "px", res = 300)
  
  
  if(number == 1) par(mfcol = c(5,5), mar = c(2.5,3,2,2), oma = c(2,8,0,0), xpd = T)
  if(number == 2) par(mfcol = c(4,5), mar = c(2.5,3,2,2), oma = c(2,8,0,0), xpd = T)
  if(number == 3) par(mfcol = c(5,3), mar = c(2.5,3,2,2), oma = c(2,8,0,0), xpd = T)
  if(number == 4) par(mfcol = c(4,3), mar = c(2.5,3,2,2), oma = c(2,8,0,0), xpd = T)
  number = number + 1
  pannel.nb <- 1
  
  for(fixed.v in fixed.variables.group){
    
    
    print(fixed.v)
    
    # a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
    # ylim <- range(tapply(a$mean, a$variable.name, scale))
    # ylim[1] <- ylim[1] - 0.25
    # ylim[2] <- ylim[2] + 0.25
    
    fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
    
    xaxis <- fixed.v.info$xaxis_simple[which(fixed.v.info$fixed.v %in% fixed.v)]
    
    ###subset ForC
    
    first.plot <- TRUE
    
    for (response.v in response.variables.group){
      
      if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
      if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
      if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
      
      col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
      
      col <- col.sym$col[which(col.sym$variable %in% response.v)]
      sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
      
      rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
      
      fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
      
      df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
      
      # df$masl <- df$masl/1000
      
      df$fixed <- df[, fixed.v]
      
      # a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
      # ylim <- range(tapply(a$mean, a$variable.name, scale))
      # ylim[1] <- ylim[1] - 0.25
      # ylim[2] <- ylim[2] + 0.25
      
      if(!fixed.v %in% c("mat", "lat", "PreSeasonality", "SolarRadiation")){
        
        mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim.poly <- lmer(mean ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.poly = mod.clim.poly), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        
        if (best.model == "mod") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim.poly") mod.full <- lmer(mean ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.clim") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.clim.poly") mod.full <- lmer(mean ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
      }
      
      if(fixed.v %in% c("mat", "lat", "PreSeasonality", "SolarRadiation")){
        
        mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.clim <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod = mod, mod.clim = mod.clim), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.clim = mod.clim), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        
        if (best.model == "mod") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.clim") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
      }
      newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
      newDat$fit <- predict(mod.full, newDat, re.form = NA)
      
      ylim = range(df$mean)
      ylim[1] <- ylim[1] - 2
      ylim[2] <- ylim[2] + 2
      
      plot(mean ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10, alpha = 0.4)[col], ylim = ylim, pch = sym)
      
      lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
      
      axis(1 ,labels = ifelse(pannel.nb %in% c(3,4), TRUE, FALSE))
      axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
        
      
      # equation <-  paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2])
      
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      significance <- signif(significance, digits=4)
      
      Rsq <- as.data.frame(r.squaredGLMM(mod.full))
      Rsq <- signif(Rsq, digits=4)[1]
      
      results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
      
      all.results <- rbind(all.results, results)
      if(fixed.v %in% c("mat", "TempSeasonality"))mtext(paste0(response.v), side = 3, line = 0.5, adj = 0.05, cex = 0.6)
      
      pannel.nb <- pannel.nb +1
      if(significant.effect) mtext(paste("R-sq =", Rsq), side = 3, line = -1.5, adj = 0.1, cex = 0.6)
      
      # if(fixed.v == "mat" & response.v == "GPP")legend(x = -15, y = 5, legend = c("GPP", "NPP", "ANPP", "BNPP_root"), col = plasma(10)[c(1, 3, 5, 8)], pch = c(1, 3, 5, 8), xpd = NA, text.col = plasma(10)[c(1, 3, 5, 8)], xjust = 1, cex = 0.75, inset = c(-0.4, 0), title = "Flux variables", title.col = "black")
      if(response.v %in% c("ANPP_foliage", "R_auto_root")) mtext(side = 1, line = 2.5, text = xaxis, cex = 0.6)
    }
    
    
   
    ##mtext(side = 1, line = 3, text = eval(parse(text = xaxis)), outer = F)
    # mtext(side = 2, line = 3,  text = expression("Mg C"~ha^-1~yr^-1), outer = F) 
    ##if(fixed.v == "mat")legend(x = -10, y = 0, legend = c("GPP", "NPP", "ANPP", "BNPP_root"), col = plasma(10)[c(1, 3, 5, 8)], pch = c(1, 3, 5, 8), xpd = NA, text.col = plasma(10)[c(1, 3, 5, 8)], bty = "n", xjust = 1, cex = 0.75, inset = c(-0.4, 0))
    
    
    
  }

  
  y_outer <- "expression(paste('Carbon flux (Mg C ha'^{-1}, 'yr'^{-1}, ')'))"
  
  mtext(side = 2, line = 1,  text = eval(parse(text = y_outer)), outer = T)
  
  dev.off()
}}}


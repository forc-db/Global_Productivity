
# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity")

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
## Keep only age >=100 (or 999)
age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
ages <- c("age.greater.than.100")

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0
ForC_simplified <- ForC_simplified[dist.to.keep, ]

### exclude Tura due to extreme high latitude
ForC_simplified <- ForC_simplified[!(ForC_simplified$sites.sitename %in% "Tura"),]

## prepare results table

all.results <- NULL

### ALL MODELS and individual plots ###

all.results <- NULL
all.aictab <- NULL
all.koeppen <- NULL

null <- NULL

fixed.variables <- c("mat", "map", "lat", "TempSeasonality", "TempRangeAnnual", "AnnualFrostDays", "AnnualWetDays", "Aridity", "PotentialEvapotranspiration", "VapourPressureDeficit", "SolarRadiation", "PreSeasonality", "MaxVPD", "WaterStressMonths", "length_growing_season", "CloudCover")


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
      
      png(file = paste0("Effect_of_", fixed.v, "_MATURE_only_", age, "_", n, ".png"), width = 2255, height = 2000, units = "px", res = 300)
      
      par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,0))
      print(fixed.v)
      
      fixed.v.info <- read.csv("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
      
      xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
      
      ###subset ForC
      response.variables.col <- 1:length(response.variables)
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        col.sym <- read.csv("/colsym.csv", stringsAsFactors = F)
        
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
        if(fixed.v == "mat") df$fixed <- df$fixed + 11
        if(fixed.v == "AnnualFrostDays") df$fixed <- df$fixed + 0.1
        if(fixed.v == "WaterStressMonths") df$fixed <- df$fixed + 0.1
        if(fixed.v == "lat") df$fixed <- df$fixed + 0.1
        if(fixed.v == "AnnualWetDays") df$fixed <- df$fixed + 0.1
        if(fixed.v == "VapourPressure") df$fixed <- df$fixed + 0.1
        if(fixed.v == "TempSeasonality") df$fixed <- df$fixed/100
        # ylim <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]$mean)
        
        a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
        ylim <- range(tapply(a$mean, a$variable.name, scale))
        ylim[1] <- ylim[1] - 0.25
        ylim[2] <- ylim[2] + 0.25
        
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
        delta.aic <- round(delta.aic, digits=1)

        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)

        if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.clim.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.clim.log") mod.full <- lmer(scale(mean) ~ log(fixed) + (1|geographic.area/plot.name), data = df, REML = T)
 
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
        Rsq <- round(Rsq, digits =2)
        legend2 <- paste(response.v, "r-squared = ", Rsq[1], "p-value = ", significance)
        mtext(side = 3, line = -which(response.variables %in% response.v), text = legend2, adj = 0.9, col = plasma(10)[col], cex = 0.5)
        
        number_plots <- length(unique(df$site_plot))
        geog_area <- length(unique(df$geographic.area))
        
        date = Sys.Date()
        altitude = FALSE
        
        results <- data.frame(date.run = date, response = response.v, fixed = fixed.v, altitude_included = altitude, random = "geographic.area/plot.name", Age.filter = age, best.model = best.model, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq, delta.aic = delta.aic, linear.slope = slope, number.plots = number_plots, geographic.areas = geog_area)
        
        all.results <- rbind(all.results, results)
        all.aictab <- rbind(all.aictab, aictab)
        null <- rbind(null, df)
        
      }
      
      
      title (paste("Effect of", fixed.v), outer = T, line = 1)
      mtext(side = 1, line = 3, text = eval(parse(text = xaxis)), outer = T)
      mtext(side = 2, line = 3,  text = expression("Productivity (scaled values)"), outer = T) 
      dev.off()
    }
    
  }
  
}


write.csv(all.results, file = "", row.names = F)



################################ graph combined plots

fixed.variables <- c("mat", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "TempSeasonality", "length_growing_season")

response.variables <- c("GPP", "NPP", "BNPP_root", "ANPP")

all.results = NULL

### mature forests only ####
for (age in ages){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  # png(file = paste0("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/results/figures/final_figures/final_model_fits/combined_plots.png"), width = 2255, height = 3000, units = "px", res = 300)
  jpeg(file = paste0("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/results/figures/final_figures/final_model_fits/combined_plots.jpg"), width = 2255, height = 3000, units = "px", res = 300)
  
  par(mfrow = c(3,2), mar = c(4,2,2,2), oma = c(0,3,0,0))
  
  pannel.nb <- 1
  
  for(fixed.v in fixed.variables){
    
    
    print(fixed.v)
    
    a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
    ylim <- range(tapply(a$mean, a$variable.name, scale))
    ylim[1] <- ylim[1] - 0.25
    ylim[2] <- ylim[2] + 0.25
    
    fixed.v.info <- read.csv("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/raw.data//fixedv_data.csv", stringsAsFactors = F)
    
    xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
    
    ###subset ForC
    
    first.plot <- TRUE
    
    for (response.v in response.variables){
      
      if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
      if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
      if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
      
      col.sym <- read.csv("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/raw.data//colsym.csv", stringsAsFactors = F)
      
      col <- col.sym$col[which(col.sym$variable %in% response.v)]
      sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
      
      rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
      
      fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
      
      df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
      
      # df$masl <- df$masl/1000
      
      df$fixed <- df[, fixed.v]
      if(fixed.v == "mat") df$fixed <- df$fixed + 11
      if(fixed.v == "TempSeasonality") df$fixed <- df$fixed/100
      
      a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
      ylim <- range(tapply(a$mean, a$variable.name, scale))
      ylim[1] <- ylim[1] - 0.25
      ylim[2] <- ylim[2] + 0.25
      
      # if(!fixed.v %in% c("mat", "lat", "PreSeasonality", "SolarRadiation")){
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
        
        
      newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
      newDat$fit <- predict(mod.full, newDat, re.form = NA)
      
      if(fixed.v == "mat") df$fixed <- df$fixed - 11
      if(fixed.v == "mat") newDat$fixed <- newDat$fixed - 11

      if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], ylim = ylim, pch = sym)
      if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
      
      lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
 
      if(first.plot) {
        axis(1 ,labels = ifelse(pannel.nb %in% c(3,4), TRUE, FALSE))
        axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
      }
      
      first.plot <- FALSE
    
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      significance <- signif(significance, digits=4)
      
      Rsq <- as.data.frame(r.squaredGLMM(mod.full))
      Rsq <- signif(Rsq, digits=4)

      results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
      
      all.results <- rbind(all.results, results)
      
    }
    
    
    mtext(paste0("(", letters[pannel.nb], ")"), side = 3, line = -1.5, adj = 0.05)
    mtext(side = 1, line = 3, text = eval(parse(text = xaxis)), outer = F)
    if(fixed.v == "map")legend("topright", legend = c("GPP", "NPP", "ANPP", "BNPP"), col = plasma(10)[c(1, 3, 5, 8)], pch = c(1, 3, 5, 8), xpd = T, text.col = plasma(10)[c(1, 3, 5, 8)], bty = "n", xjust = 1, cex = 0.75)
    
    pannel.nb <- pannel.nb +1
    
  }
  
  mtext(side = 2, line = 1,  text = expression("Carbon flux (standard normal deviates)"), outer = T)
  
  dev.off()
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
  
  
  if(type == "climate") png(file = paste0("/grid_plots_", type, number, ".png"), width = 3000, height = 2250, units = "px", res = 300)
  if(type == "seasonality") png(file = paste0("/grid_plots_", type, number, ".png"), width = 2000, height = 3000, units = "px", res = 300)
  
  
  if(number == 1) par(mfcol = c(5,5), mar = c(2,2,2,2), oma = c(2,8,0,0), xpd = T)
  if(number == 2) par(mfcol = c(4,5), mar = c(2,2,2,2), oma = c(2,8,0,0), xpd = T)
  if(number == 3) par(mfcol = c(5,3), mar = c(2,2,2,2), oma = c(2,8,0,0), xpd = T)
  if(number == 4) par(mfcol = c(4,3), mar = c(2,2,2,2), oma = c(2,8,0,0), xpd = T)
  number = number + 1
  pannel.nb <- 1
  
  for(fixed.v in fixed.variables.group){
    
    
    print(fixed.v)
    
    fixed.v.info <- read.csv("/fixedv_data.csv", stringsAsFactors = F)
    
    xaxis <- fixed.v.info$xaxis_simple[which(fixed.v.info$fixed.v %in% fixed.v)]
    
    ###subset ForC
    
    first.plot <- TRUE
    
    for (response.v in response.variables.group){
      
      if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
      if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
      if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
      
      col.sym <- read.csv("/colsym.csv", stringsAsFactors = F)
      
      col <- col.sym$col[which(col.sym$variable %in% response.v)]
      sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
      
      resp.v.info <- read.csv("/respv_data.csv", stringsAsFactors = F)
      respv <- resp.v.info$name[which(resp.v.info$response.v %in% response.v)]
      
      rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
      
      fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
      
      df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
      
      df$fixed <- df[, fixed.v]
      if(fixed.v == "mat") df$fixed <- df$fixed + 11
      if(fixed.v == "TempSeasonality") df$fixed <- df$fixed/100
 
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
        
        if (best.model == "mod") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.clim") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.clim.poly") mod.full <- lmer(mean ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.clim.log") mod.full <- lmer(mean ~ log(fixed) + (1|geographic.area/plot.name), data = df, REML = T)
        
      newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
      newDat$fit <- predict(mod.full, newDat, re.form = NA)
      
      ylim = range(df$mean)
      ylim[1] <- ylim[1] - 2
      ylim[2] <- ylim[2] + 2
      
      if(fixed.v == "mat") df$fixed <- df$fixed - 11
      if(fixed.v == "mat") newDat$fixed <- newDat$fixed - 11
      
      plot(mean ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10, alpha = 0.4)[col], ylim = ylim, pch = sym)
      
      lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
      
      axis(1 ,labels = ifelse(pannel.nb %in% c(3,4), TRUE, FALSE))
      axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
      
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      significance <- signif(significance, digits=2)
      
      Rsq <- as.data.frame(r.squaredGLMM(mod.full))
      Rsq <- round(Rsq, digits=2)[1]
      
      results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
      
      all.results <- rbind(all.results, results)
      if(fixed.v %in% c("mat", "TempSeasonality"))mtext(paste0(respv), side = 3, line = 0.5, adj = 0.05, cex = 0.6)
      
      pannel.nb <- pannel.nb +1
      if(significant.effect) mtext(paste("R-sq =", Rsq), side = 3, line = -1.5, adj = 0.1, cex = 0.6)
      
      if(response.v %in% c("ANPP_foliage", "R_auto_root")) mtext(side = 1, line = 2.5, text = eval(parse(text = xaxis)), cex = 0.6)
    }
    
    
    
  }

  
  y_outer <- "expression(paste('Carbon flux (Mg C ha'^{-1}, 'yr'^{-1}, ')'))"
  
  mtext(side = 2, line = 1,  text = eval(parse(text = y_outer)), outer = T)
  
  dev.off()
}}}


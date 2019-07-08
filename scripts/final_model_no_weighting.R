
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

## fixed variables list ####
fixed.variables <- c("mat", "map", "lat", "AnnualMeanTemp", "MeanDiurnalRange", "TempSeasonality", "TempRangeAnnual", "AnnualPre", "PreSeasonality", "CloudCover", "AnnualFrostDays", "AnnualWetDays", "VapourPressure", "SolarRadiation", "Aridity", "PotentialEvapotranspiration", "VapourPressureDeficit")

## prepare results table

all.results <- NULL

###comparing models


all.results <- NULL
all.aictab <- NULL
all.koeppen <- NULL

fixed.variables <- c("mat", "map", "lat", "AnnualMeanTemp", "TempSeasonality", "TempRangeAnnual", "AnnualPre", "AnnualFrostDays", "AnnualWetDays", "VapourPressure", "Aridity", "PotentialEvapotranspiration", "VapourPressureDeficit", "SolarRadiation", "PreSeasonality")



response.variables.groups <- list(c("GPP", "NPP", "BNPP_root", "BNPP_root_fine"),
                                  c("ANPP", "ANPP_foliage"),
                                  c("ANPP_woody", "ANPP_woody_stem"),
                                  c("R_auto", "R_auto_root"))

for(response.variables in response.variables.groups){
  
  if(response.variables[1] == "GPP") n <- 1
  if(response.variables[1] == "ANPP") n <- 2
  if(response.variables[1] == "ANPP_woody") n <- 3
  if(response.variables[1] == "R_auto") n <- 4
  
  ### mature forests only ####
  for (age in ages){
    
    if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
    if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
    
    for(fixed.v in fixed.variables){
      
      png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/Effect_of_", fixed.v, "_MATURE_only_", age, "_", n, ".png"), width = 2255, height = 2000, units = "px", res = 300)
      
      par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,0))
      print(fixed.v)
      
      
      
      ###subset ForC
      response.variables.col <- 1:length(response.variables)
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        col.sym <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
        
        col <- col.sym$col[which(col.sym$variable %in% response.v)]
        sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2")
        if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
        if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
        
        
        response.v.color <- response.variables.col[which(response.variables %in% response.v)]
        
        rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
        
        fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
        
        df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
        
        df$masl <- df$masl/1000
        
        df$fixed <- df[, fixed.v]
        
        if(fixed.v == "AnnualPET") df <- df[df$fixed != 0,]
        # ylim <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]$mean)
        
        a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
        ylim <- range(tapply(a$mean, a$variable.name, scale))
        ylim[1] <- ylim[1] - 0.25
        ylim[2] <- ylim[2] + 0.25
        
        mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.linear <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        mod.poly <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)

        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = T)
        
        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100), masl = c(0.5))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], pch = sym, yaxt = "n", ylim = ylim)
        if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
        
        for(masl in unique(newDat$masl)){
          i <- which(unique(newDat$masl) %in% masl)
          lines(fit ~ fixed, data = newDat[newDat$masl %in% masl,], col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2), lwd = i)}
        
        first.plot <- FALSE
        
        mod.linear <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = T)
      
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
        altitude = TRUE
        
        results <- data.frame(date.run = date, response = response.v, fixed = fixed.v, altitude_included = altitude, random = "geographic.area/plot.name", Age.filter = age, best.model = best.model, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq, delta.aic = delta.aic, linear.slope = slope, number.plots = number_plots)
        
        all.results <- rbind(all.results, results)
        all.aictab <- rbind(all.aictab, aictab)
        
      }
      
      
      title (paste("Effect of", fixed.v), outer = T, line = 1)
      mtext(side = 1, line = 3, text = fixed.v, outer = T)
      mtext(side = 2, line = 3,  text = expression("Productivity (scaled values)"), outer = T) 
      dev.off()
    }
    
  }
  
}


write.csv(all.results, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/best_model_outputs/best_model_scaled_unweighted.csv", row.names = F)

response.variables.groups <- list(c("GPP", "NPP", "ANPP", "BNPP_root", "R_auto"),
                                  c("ANPP_foliage", "ANPP_woody_stem", "BNPP_root_fine", "R_auto_root"))

all.results = NULL


### mature forests only ####
for (age in ages){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  for(fixed.v in fixed.variables){
    
    png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/Effect_of_", fixed.v, "_MATURE_only_poly_all.png"), width = 5000, height = 2000, units = "px", res = 300)
    
    layout(matrix(1:3, nrow = 1), widths = c(1,1,0.25))
    par(mar = c(3,3,3,0), oma = c(3,3,0,7))
    
    print(fixed.v)
    
    
    
    for(response.variables in response.variables.groups){
      
      if(response.variables[1] == "GPP") n <- 1
      if(response.variables[1] == "ANPP_foliage") n <- 2
    
      first.plot <- TRUE
      
    for (response.v in response.variables){
      
      col.sym <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
      
      col <- col.sym$col[which(col.sym$variable %in% response.v)]
      sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
      
      if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2")
      if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2")
      if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
      
      rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
      
      fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
      
      df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
      
      df$masl <- df$masl/1000
      
      df$fixed <- df[, fixed.v]
      
      if(response.v == "GPP"){
      a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
      ylim <- range(tapply(a$mean, a$variable.name, scale))
      ylim[1] <- ylim[1] - 0.25
      ylim[2] <- ylim[2] + 0.25
      }
      
      if(response.v == "GPP"){
        b <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables.groups),]
        no.na <- !is.na(b[, fixed.v])
        b <- b[no.na,]
        xlim <- range(b[, fixed.v])
      }
      
      
      mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
      mod.linear <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      mod.poly <- lmer(scale(mean) ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      
      aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
      
      best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Modname[1])
      delta.aic <- as.numeric(aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Delta_AICc[2])
      delta.aic <- signif(delta.aic, digits=4)
      
      if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      
      
      significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      sample.size <- length(df$mean)
      
      if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df, REML = T)
      if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = T)
      
      newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100), masl = c(0.5))
      newDat$fit <- predict(mod.full, newDat, re.form = NA)
     
      if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], pch = sym, ylim = ylim, xlim = xlim)
      if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
      
      for(masl in unique(newDat$masl)){
        i <- which(unique(newDat$masl) %in% masl)
        lines(fit ~ fixed, data = newDat[newDat$masl %in% masl,], col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2), lwd = i)}
 
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
    mtext(side = 1, line = 2, text = fixed.v, outer = T)
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
   
    for(response.variables in response.variables.groups){
      
      if(response.variables[1] == "GPP") n <- 1
      if(response.variables[1] == "ANPP_foliage") n <- 2
      
      png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/Effect_of_", fixed.v, n, "_MATURE_only_poly_all.png"), width = 2255, height = 2000, units = "px", res = 300)
      
      par(mfrow = c(1,1), mar = c(3,3,3,3))
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        col.sym <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
        
        col <- col.sym$col[which(col.sym$variable %in% response.v)]
        sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2")
        if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
        
        
        response.v.color <- response.variables.col[which(response.variables %in% response.v)]
        
        rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
        
        fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
        
        df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
        
        df$masl <- df$masl/1000
        
        df$fixed <- df[, fixed.v]
        
        a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
        ylim <- range(tapply(a$mean, a$variable.name, scale))
        ylim[1] <- ylim[1] - 0.25
        ylim[2] <- ylim[2] + 0.25
        
        
        mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.linear <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        mod.poly <- lmer(scale(mean) ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = T)
        
        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100), masl = c(0.5))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], pch = sym, yaxt = "n", ylim = ylim)
        if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
        
        for(masl in unique(newDat$masl)){
          i <- which(unique(newDat$masl) %in% masl)
          lines(fit ~ fixed, data = newDat[newDat$masl %in% masl,], col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2), lwd = i)}
        
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
      mtext(side = 1, line = 2, text = fixed.v, outer = F)
      mtext(side = 2, line = 1,  text = expression("Productivity (scaled values)"), outer = F) 
      dev.off()
      
      
      
    }
    
   
    # mtext(side = 3, line = -(which(col.sym$variable %in% response.v)), text = legend2, adj = 1, col = plasma(10)[col], cex = 0.5, outer = T)
    # if(n==1) mtext(side = 3, line = 0, text = legend1, adj = 1, col = "black", cex = 0.5, outer = T)
    # mtext(side = 3, line = -7 - which(response.variables %in% response.v), text = legend3, adj = 0.95, col = plasma(8)[response.v.color], cex = 0.5, outer = T)
    
  }
}


################################



fixed.variables <- c("AnnualMeanTemp", "TempSeasonality", "map", "PotentialEvapotranspiration")

response.variables <- c("GPP", "NPP", "BNPP_root", "ANPP")

all.results = NULL

### mature forests only ####
for (age in ages){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/combined_plots.png"), width = 2255, height = 2000, units = "px", res = 300)
  
  par(mfrow = c(2,2), mar = c(3,3,3,3), oma = c(0,3,0,0))
  
  pannel.nb <- 1
  
  for(fixed.v in fixed.variables){
    
    
    print(fixed.v)
    
    a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
    ylim <- range(tapply(a$mean, a$variable.name, scale))
    ylim[1] <- ylim[1] - 0.25
    ylim[2] <- ylim[2] + 0.25
    
    
    ###subset ForC
    
    first.plot <- TRUE
    
    for (response.v in response.variables){
      
      if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2")
      if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2")
      if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
      
      col.sym <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
      
      col <- col.sym$col[which(col.sym$variable %in% response.v)]
      sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
      
      rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
      
      fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
      
      df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
      
      df$masl <- df$masl/1000
      
      df$fixed <- df[, fixed.v]
      
      mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
      mod.linear <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      mod.poly <- lmer(scale(mean) ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      
      a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
      ylim <- range(tapply(a$mean, a$variable.name, scale))
      ylim[1] <- ylim[1] - 0.25
      ylim[2] <- ylim[2] + 0.25
      
      aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
      
      best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Modname[1])
      delta.aic <- as.numeric(aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Delta_AICc[2])
      delta.aic <- signif(delta.aic, digits=4)
      
      if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      
      
      significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      sample.size <- length(df$mean)
      
      if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df, REML = T)
      if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df, REML = T)
      
      newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100), masl = c(0.5))
      newDat$fit <- predict(mod.full, newDat, re.form = NA)

      if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], ylim = ylim, pch = sym)
      if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
      
      for(masl in unique(newDat$masl)){
        i <- which(unique(newDat$masl) %in% masl)
        lines(fit ~ fixed, data = newDat[newDat$masl %in% masl,], col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2), lwd = i)}
 
      if(first.plot) {
        axis(1 ,labels = ifelse(pannel.nb %in% c(3,4), TRUE, FALSE))
        axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
      }
      
      # if(pannel.nb == 2){
      #   legend1 <- paste(response.v)
      #   mtext(side = 3, line = -(which(response.variables %in% response.v)), text = legend1, adj = 0.95, col = plasma(10)[col], cex = 0.5, outer = F)
      # }
      
      first.plot <- FALSE
      
      # equation <-  paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2])
      
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      significance <- signif(significance, digits=4)
      
      Rsq <- as.data.frame(r.squaredGLMM(mod.full))
      Rsq <- signif(Rsq, digits=4)

      results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
      
      all.results <- rbind(all.results, results)
      
    }
    pannel.nb <- pannel.nb +1
    
    if(fixed.v == "AnnualMeanTemp") title <- "Mean Annual Temperature"
    if(fixed.v == "TempSeasonality") title <- "Temperature Seasonality"
    if(fixed.v == "map") title <- "Mean Annual Precipitation"
    if(fixed.v == "PotentialEvapotranspiration") title <- "Potential Evapotranspiration"

    title (paste(title), outer = F, line = 1)
    # mtext(side = 1, line = 3, text = fixed.v, outer = T)
    # mtext(side = 2, line = 3,  text = expression("Mg C"~ha^-1~yr^-1), outer = F) 
    if(fixed.v == "TempSeasonality")legend("topright", legend = c("GPP", "NPP", "ANPP", "BNPP_root"), col = plasma(10)[c(1, 3, 5, 8)], pch = c(1, 3, 5, 8), xpd = T, text.col = plasma(10)[c(1, 3, 5, 8)], bty = "n", xjust = 1)
    
  }
  
  mtext(side = 2, line = 1,  text = expression("Productivity (scaled values)"), outer = T)
  
  dev.off()
}


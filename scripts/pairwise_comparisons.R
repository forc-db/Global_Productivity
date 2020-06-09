
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

ForC_simplified$variable.name <- gsub("(_0|_1|_2|_3|_4|_5)", "", ForC_simplified$variable.name)

## prepare results table

all.results <- NULL

###comparing models


all.results <- NULL
all.aictab <- NULL
all.koeppen <- NULL
fixed.variables <- c("lat", "mat", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "TempSeasonality", "length_growing_season")

response.variables <- c("GPP", "NPP", "ANPP", "ANPP_woody_stem", "ANPP_foliage", "BNPP_root", "BNPP_root_fine", "R_auto", "R_auto_root")
response.variables.groups <- list(c("GPP", "NPP"),
                                  c("NPP", "ANPP"),
                                  c("NPP", "BNPP_root"),
                                  c("ANPP", "ANPP_foliage"),
                                  c("ANPP", "ANPP_woody_stem"))

all.results = NULL
comparisons = NULL
number = 1


  for (age in ages){
    all.comparisons <- data.frame(matrix(ncol = 0, nrow = 7))
    for (response.variables.group in response.variables.groups){
      
      
      if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
      
      resp1 <- ForC_simplified[ForC_simplified$variable.name %in% response.variables.group[1],]
      resp2 <- ForC_simplified[ForC_simplified$variable.name %in% response.variables.group[2],]
      
      df <- merge(resp1, resp2[, c("variable.name", "date", "mean", "citation.ID", "site_plot")], by= c("site_plot", "citation.ID", "date"))
      
      names(df)[13] <- paste(response.variables.group[1])
      names(df)[60] <- paste(response.variables.group[2])
      fixed.comparison <- NULL

      fixed.comparisons <- setNames(data.frame(matrix(ncol = 4, nrow = 1)), c("flux", paste(response.variables.group[1]), paste(response.variables.group[2]), "larger Rsq"))
      
      
      for(fixed.v in fixed.variables){
        
        
        print(fixed.v)
        
        # a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
        # ylim <- range(tapply(a$mean, a$variable.name, scale))
        # ylim[1] <- ylim[1] - 0.25
        # ylim[2] <- ylim[2] + 0.25
        
        fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
        
        xaxis <- fixed.v.info$xaxis_simple[which(fixed.v.info$fixed.v %in% fixed.v)]
        
        fixed.no.na <- !is.na(df[, fixed.v]) & !is.na(df[, "masl"])
        if (age %in% "age.greater.than.100") ages.to.keep <- df$stand.age >= 100 & !is.na(df$stand.age)
        
        df <- df[ages.to.keep & fixed.no.na, ]
        
        # df$masl <- df$masl/1000
        
        df$fixed <- df[, fixed.v]
        if(fixed.v == "mat") df$fixed <- df$fixed + 11
        
        ###subset ForC
        
        first.plot <- TRUE
        
        png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/pairwise_comparisons/pairwise_comparisons_", number, ".png"), width = 3000, height = 2000, units = "px", res = 300)
        
        par(mfrow = c(1,2), mar = c(2,2,2,2), oma = c(5,4,5,0))
        
        save.output <- setNames(data.frame(matrix(ncol = 2, nrow = 1)), c(paste(response.variables.group[1]), paste(response.variables.group[2])))
        rep <- 2
        
        for (response.v in response.variables.group){
          
          col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
          
          col <- col.sym$col[which(col.sym$variable %in% response.v)]
          sym <- col.sym$sym[which(col.sym$variable %in% response.v)]

          
          
          df$mean <- df[, response.v]
          
          # a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
          # ylim <- range(tapply(a$mean, a$variable.name, scale))
          # ylim[1] <- ylim[1] - 0.25
          # ylim[2] <- ylim[2] + 0.25
          
          # if(!fixed.v %in% c("mat", "lat", "PreSeasonality", "SolarRadiation")){
          
          mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
          mod.clim <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          # mod.clim.poly <- lmer(scale(mean) ~ poly(fixed, 2, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          mod.clim.log <- lmer(scale(mean) ~ log(fixed) + (1|geographic.area/plot.name), data = df, REML = F)
          
          
          aictab <- aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.log = mod.clim.log), sort = T)
          
          best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.log = mod.clim.log), sort = T)$Modname[1])
          
          # if (best.model == "mod.clim.poly"){ 
          #   
          #   test.delta.aic <- as.numeric(aictab(list(mod.clim = mod.clim, mod.clim.poly = mod.clim.poly))$Delta_AICc[2])
          #   
          #   best.model <- ifelse(test.delta.aic > 2, "mod.clim.poly", "mod.clim")
          # }
          
          if (best.model == "mod.clim"){ 
            
            test.delta.aic <- as.numeric(aictab(list(mod = mod, mod.clim = mod.clim))$Delta_AICc[2])
            
            best.model <- ifelse(test.delta.aic > 2, "mod.clim", "mod")
          }
          
          if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          if (best.model == "mod.clim") mod.full <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          if (best.model == "mod.clim.log") mod.full <- lmer(scale(mean) ~ log(fixed) + (1|geographic.area/plot.name), data = df, REML = F)
          
          delta.aic <- as.numeric(aictab(list(mod = mod, mod.full = mod.full))$Delta_AICc[2])
          delta.aic <- signif(delta.aic, digits=4)
          
          significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
          significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
          sample.size <- length(df$mean)
          
          if (best.model == "mod") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          if (best.model == "mod.clim") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
          if (best.model == "mod.clim.log") mod.full <- lmer(mean ~ log(fixed) + (1|geographic.area/plot.name), data = df, REML = T)
          
          # }
          
          # if(fixed.v %in% c("mat", "lat", "PreSeasonality", "SolarRadiation")){
          #   
          #   mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
          #   mod.clim <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          #   
          #   aictab <- aictab(list(mod = mod, mod.clim = mod.clim), sort = T)
          #   
          #   best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim), sort = T)$Modname[1])
          #   delta.aic <- as.numeric(aictab(list(mod = mod, mod.clim = mod.clim), sort = T)$Delta_AICc[2])
          #   delta.aic <- signif(delta.aic, digits=4)
          #   
          #   
          #   if (best.model == "mod") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          #   if (best.model == "mod.clim") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          #   
          #   significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
          #   significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
          #   sample.size <- length(df$mean)
          #   
          #   if (best.model == "mod.clim") mod.full <- lmer(mean ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = T)
          # }
          newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
          newDat$fit <- predict(mod.full, newDat, re.form = NA)
          
          ylim = range(df$mean)
          ylim[1] <- ylim[1] - 2
          ylim[2] <- ylim[2] + 2
          
          plot(mean ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10, alpha = 0.4)[col], ylim = ylim, pch = sym)
          
          lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
          
          axis(1 ,labels = TRUE)
          axis(2 ,labels = TRUE)
          
          
          # equation <-  paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2])
          
          significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
          significance <- signif(significance, digits=4)
          
          Rsq <- as.data.frame(r.squaredGLMM(mod.full))
          Rsq <- signif(Rsq, digits=4)[1]
          
          results <- data.frame(response = response.v, fixed = fixed.v, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
          fixed.comparisons[1] <- fixed.v
          fixed.comparisons[rep] <- Rsq
          rep <- rep + 1
          
          all.results <- rbind(all.results, results)
          mtext(paste0(response.v), side = 3, line = 0.5, adj = 0.05, cex = 0.6)
          
          
          if(significant.effect) mtext(paste("R-sq =", Rsq), side = 3, line = -1.5, adj = 0.1, cex = 0.6)
          
          # mtext(side = 3, text = Rsq, line = 1.5, adj = 0.05, cex = 0.6)
          
          # if(fixed.v == "mat" & response.v == "GPP")legend(x = -15, y = 5, legend = c("GPP", "NPP", "ANPP", "BNPP_root"), col = plasma(10)[c(1, 3, 5, 8)], pch = c(1, 3, 5, 8), xpd = NA, text.col = plasma(10)[c(1, 3, 5, 8)], xjust = 1, cex = 0.75, inset = c(-0.4, 0), title = "Flux variables", title.col = "black")
          # mtext(side = 1, line = 2.5, text = xaxis, cex = 0.6)
        }
        
        mtext(side = 3, text = xaxis, outer = T)
        
        dev.off()
        
        number <- number + 1
        fixed.comparisons[4] <- ifelse(fixed.comparisons[2] > fixed.comparisons[3], paste(names(fixed.comparisons[2])), paste(names(fixed.comparisons[3])))
        fixed.comparison <- rbind(fixed.comparison, fixed.comparisons)
        
        
        ##mtext(side = 1, line = 3, text = eval(parse(text = xaxis)), outer = F)
        # mtext(side = 2, line = 3,  text = expression("Mg C"~ha^-1~yr^-1), outer = F) 
        ##if(fixed.v == "mat")legend(x = -10, y = 0, legend = c("GPP", "NPP", "ANPP", "BNPP_root"), col = plasma(10)[c(1, 3, 5, 8)], pch = c(1, 3, 5, 8), xpd = NA, text.col = plasma(10)[c(1, 3, 5, 8)], bty = "n", xjust = 1, cex = 0.75, inset = c(-0.4, 0))
        
        
        
      }
      
    all.comparisons <- cbind(all.comparisons, fixed.comparison)  
      
    }}

write.csv(all.comparisons,  file = "C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/best_model_outputs/pairwise_comparisons.csv", row.names = F)

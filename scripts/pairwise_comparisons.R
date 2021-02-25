
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

###comparing models


all.results <- NULL
all.aictab <- NULL
all.koeppen <- NULL
fixed.variables <- c("lat", "mat", "TempSeasonality")


response.variables <- c("GPP", "NPP", "ANPP", "ANPP_woody_stem", "ANPP_foliage", "BNPP_root", "BNPP_root_fine", "R_auto", "R_auto_root")
response.variables.groups <- list(c("GPP", "NPP"),
                                  c("NPP", "ANPP"),
                                  c("NPP", "BNPP_root"),
                                  c("ANPP", "ANPP_foliage"),
                                  c("ANPP", "ANPP_woody_stem"),
                                  c("GPP", "R_auto"),
                                  c("BNPP_root", "R_auto_root"))

all.results = NULL
comparisons = NULL
number = 1


  for (age in ages){
    all.comparisons <- NULL
    for (response.variables.group in response.variables.groups){
      
      
      if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
      
      resp1 <- ForC_simplified[ForC_simplified$variable.name %in% response.variables.group[1],]
      resp2 <- ForC_simplified[ForC_simplified$variable.name %in% response.variables.group[2],]
      
      resp.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/respv_data.csv", stringsAsFactors = F)
      respv1 <- resp.v.info$name[which(resp.v.info$response.v %in% response.variables.group[1])]
      respv2 <- resp.v.info$name[which(resp.v.info$response.v %in% response.variables.group[2])]
      
      
      df <- merge(resp1, resp2[, c("variable.name", "date", "mean", "citation.ID", "site_plot")], by= c("site_plot", "citation.ID", "date"))
      
      names(df)[13] <- paste(response.variables.group[1])
      names(df)[60] <- paste(response.variables.group[2])
      fixed.comparison <- NULL

      fixed.comparisons <- setNames(data.frame(matrix(ncol = 9, nrow = 1)), c("C flux variable 1", "C flux variable 2", "Climate variable", "Rsq variable 1", "Rsq variable 2", "Model type variable 1", "Model type variable 2", "Number of plots", "Variable with higher Rsq"))
      
      
      for(fixed.v in fixed.variables){
        
        
        print(fixed.v)
        
        fixed.v.info <- read.csv("/fixedv_data.csv", stringsAsFactors = F)
        
        xaxis <- fixed.v.info$abbrev[which(fixed.v.info$fixed.v %in% fixed.v)]
        
        fixed.no.na <- !is.na(df[, fixed.v]) & !is.na(df[, "masl"])
        if (age %in% "age.greater.than.100") ages.to.keep <- df$stand.age >= 100 & !is.na(df$stand.age)
        
        df <- df[ages.to.keep & fixed.no.na, ]

        df$fixed <- df[, fixed.v]
        if(fixed.v == "mat") df$fixed <- df$fixed + 11
        if(fixed.v == "TempSeasonality") df$fixed <- df$fixed/100
        
        ###subset ForC
        
        first.plot <- TRUE
        
        png(file = "", width = 3000, height = 2000, units = "px", res = 300)
        
        par(mfrow = c(1,2), mar = c(2,2,2,2), oma = c(5,4,5,0))
        
        save.output <- setNames(data.frame(matrix(ncol = 2, nrow = 1)), c(paste(response.variables.group[1]), paste(response.variables.group[2])))
        rep <- 1
        
        for (response.v in response.variables.group){
          
          col.sym <- read.csv("/colsym.csv", stringsAsFactors = F)
          
          col <- col.sym$col[which(col.sym$variable %in% response.v)]
          sym <- col.sym$sym[which(col.sym$variable %in% response.v)]

          resp.v.info <- read.csv("/respv_data.csv", stringsAsFactors = F)
          respv <- resp.v.info$name[which(resp.v.info$response.v %in% response.v)]
          
          df$mean <- df[, response.v]
          
          
          mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
          mod.clim <- lmer(scale(mean) ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = df, REML = F)
          mod.clim.log <- lmer(scale(mean) ~ log(fixed) + (1|geographic.area/plot.name), data = df, REML = F)
          
          
          aictab <- aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.log = mod.clim.log), sort = T)
          
          best.model <- as.character(aictab(list(mod = mod, mod.clim = mod.clim, mod.clim.log = mod.clim.log), sort = T)$Modname[1])
          
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
          
          if (best.model == "mod") model <- "Null"
          if (best.model == "mod.clim") model <- "Lin"
          if (best.model == "mod.clim.log") model <- "Log"
          
          newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
          newDat$fit <- predict(mod.full, newDat, re.form = NA)
          
          ylim = range(df$mean)
          ylim[1] <- ylim[1] - 2
          ylim[2] <- ylim[2] + 2
          
          plot(mean ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10, alpha = 0.4)[col], ylim = ylim, pch = sym)
          
          lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
          
          axis(1 ,labels = TRUE)
          axis(2 ,labels = TRUE)

          significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
          significance <- signif(significance, digits=2)
          
          Rsq <- as.data.frame(r.squaredGLMM(mod.full))
          Rsq <- signif(Rsq, digits=2)[1]
          
          results <- data.frame(response = respv, fixed = fixed.v, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
          fixed.comparisons[3] <- xaxis
          fixed.comparisons[rep] <- respv
          fixed.comparisons[rep + 3] <- Rsq
          fixed.comparisons[rep + 5] <- model
          rep <- rep + 1
          
          all.results <- rbind(all.results, results)
          mtext(paste0(respv), side = 3, line = 0.5, adj = 0.05, cex = 0.6)
          
          
          if(significant.effect) mtext(paste("R-sq =", Rsq), side = 3, line = -1.5, adj = 0.1, cex = 0.6)
        }
        
        mtext(side = 3, text = xaxis, outer = T)
        
        dev.off()
        
        number <- number + 1
        fixed.comparisons[9] <- ifelse(fixed.comparisons[4] > fixed.comparisons[5], paste(respv1), paste(respv2))
        fixed.comparisons[8] <- length(df$mean)
        

        fixed.comparison <- rbind(fixed.comparison, fixed.comparisons)
 
      }
      
    all.comparisons <- rbind(all.comparisons, fixed.comparison)  
      
    }}

write.csv(all.comparisons,  file = "", row.names = F)

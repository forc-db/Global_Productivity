# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(MuMIn)
library(viridis)

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

library(AICcmodavg)
all.results <- NULL
all.aictab <- NULL

fixed.variables <- c("lat")


# response.variables.groups <- list(c("GPP","NPP_1","BNPP_root","ANPP_1", "ANPP_foliage","ANPP_woody", "ANPP_woody_stem"))
# response.variables.1 <- c("GPP","NPP_1","BNPP_root","ANPP_1", "ANPP_foliage","ANPP_woody", "ANPP_woody_stem")
# response.variables.2 <- c("GPP","NPP_1","BNPP_root","ANPP_1", "ANPP_foliage","ANPP_woody", "ANPP_woody_stem")

set1 <- c("NPP")
set2 <- c("R_auto")
sum <- c("GPP")
  
  ### mature forests only ####
  for (age in ages){
    
    if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
    if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
    
    for(fixed.v in fixed.variables){
      
      par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,0))
      print(fixed.v)
      
      ###subset ForC
      response.variables.col <- 1:3
      
      first.plot <- TRUE
      
      ################################ for response.v.1
      
      for(s in sum){
      
      sum.response <- ForC_simplified$variable.name %in% s
      
      fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
      
      df.sum <- ForC_simplified[sum.response & ages.to.keep & fixed.no.na, ]
      
      df.sum$masl <- df.sum$masl/1000
      
      df.sum$fixed <- df.sum[, fixed.v]
      # ylim.1 <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables.1),]$mean)
      
      mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df.sum, REML = F)
      mod.linear <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.sum, REML = F)
      mod.poly <- lmer(mean ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df.sum, REML = F)
      
      aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
      
      best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
      delta.aic <- as.numeric(aictab(list(mod.linear = mod.linear), sort = T)$Delta_AICc[2])
      delta.aic <- signif(delta.aic, digits=4)
      
      if (best.model == "mod.poly") sum.mod <- lmer(mean ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df.sum, REML = F)
      if (best.model == "mod.linear") sum.mod <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.sum, REML = F)
      if (best.model == "mod") sum.mod <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.sum, REML = F)
      
      significant.effect <- anova(mod, sum.mod)$"Pr(>Chisq)"[2] < 0.05
      significance <- anova(mod, sum.mod)$"Pr(>Chisq)"[2]
      sample.size <- length(df.sum$mean)
      
      if (best.model == "mod.poly") sum.mod <- lmer(mean ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df.sum, REML = T)
      if (best.model == "mod.linear") sum.mod <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.sum, REML = T)
      
      
      for (i in seq(along = set1)){
        for (j in seq(along = set2)){
          if (i == j){
          
          ################################ for response.v.1
          if(set1[[i]] %in% "NPP") responses.to.keep.1  <- c("NPP_1")
          if(set1[[i]] %in% "ANPP") responses.to.keep.1  <- c("ANPP_1")
          if(set1[[i]] %in% "ANPP_litterfall") responses.to.keep.1  <- c("ANPP_litterfall_1")
          if(!set1[[i]] %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep.1  <- set1[[i]]
          
          
          rows.with.response.1 <- ForC_simplified$variable.name %in% responses.to.keep.1
          
          fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
          
          df.1 <- ForC_simplified[rows.with.response.1 & ages.to.keep & fixed.no.na, ]
          
          df.1$masl <- df.1$masl/1000
          
          df.1$fixed <- df.1[, fixed.v]
          # ylim.1 <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables.1),]$mean)
          
          mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df.1, REML = F)
          mod.linear <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.1, REML = F)
          mod.poly <- lmer(mean ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df.1, REML = F)
          
          aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
          
          best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
          delta.aic <- as.numeric(aictab(list(mod.linear = mod.linear), sort = T)$Delta_AICc[2])
          delta.aic <- signif(delta.aic, digits=4)
          
          if (best.model == "mod.poly") mod.full.1 <- lmer(mean ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df.1, REML = F)
          if (best.model == "mod.linear") mod.full.1 <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.1, REML = F)
          if (best.model == "mod") mod.full.1 <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.1, REML = F)
          
          significant.effect <- anova(mod, mod.full.1)$"Pr(>Chisq)"[2] < 0.05
          significance <- anova(mod, mod.full.1)$"Pr(>Chisq)"[2]
          sample.size <- length(df.1$mean)
          
          if (best.model == "mod.poly") mod.full.1 <- lmer(mean ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df.1, REML = T)
          if (best.model == "mod.linear") mod.full.1 <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.1, REML = T)
          
          ########################################### for response.v.2
          
          if(set2[[j]] %in% "NPP") responses.to.keep.2  <- c("NPP_1")
          if(set2[[j]] %in% "ANPP") responses.to.keep.2  <- c("ANPP_1")
          if(set2[[j]] %in% "ANPP_litterfall") responses.to.keep.2  <- c("ANPP_litterfall_1")
          if(!set2[[j]] %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep.2  <- set2[[j]]
          
         
          rows.with.response.2 <- ForC_simplified$variable.name %in% responses.to.keep.2
          
          fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
          
          df.2 <- ForC_simplified[rows.with.response.2 & ages.to.keep & fixed.no.na, ]
          
          df.2$masl <- df.2$masl/1000
          
          df.2$fixed <- df.2[, fixed.v]
          # ylim.2 <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables.2),]$mean)
          
          mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df.2, REML = F)
          mod.linear <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.2, REML = F)
          mod.poly <- lmer(mean ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df.2, REML = F)
          
          aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
          
          best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
          delta.aic <- as.numeric(aictab(list(mod.linear = mod.linear), sort = T)$Delta_AICc[2])
          delta.aic <- signif(delta.aic, digits=4)
          
          if (best.model == "mod.poly") mod.full.2 <- lmer(mean ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df.2, REML = F)
          if (best.model == "mod.linear") mod.full.2 <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.2, REML = F)
          if (best.model == "mod") mod.full.2 <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.2, REML = F)
          
          significant.effect <- anova(mod, mod.full.2)$"Pr(>Chisq)"[2] < 0.05
          significance <- anova(mod, mod.full.2)$"Pr(>Chisq)"[2]
          sample.size <- length(df.2$mean)
          
          if (best.model == "mod.poly") mod.full.2 <- lmer(mean ~ poly(fixed, 2) + masl + (1|geographic.area/plot.name), data = df.2, REML = T)
          if (best.model == "mod.linear") mod.full.2 <- lmer(mean ~ poly(fixed, 1) + masl + (1|geographic.area/plot.name), data = df.2, REML = T)
          
          ####################### subset data
          
          rows.with.response.3 <- ForC_simplified$variable.name %in% c(responses.to.keep.2, responses.to.keep.1)
          df.3 <- ForC_simplified[rows.with.response.3 & ages.to.keep & fixed.no.na, ]
          df.3$fixed <- df.3[, fixed.v]
          
          newDat <- expand.grid(fixed = seq(min(df.3$fixed), max(df.3$fixed), length.out = 100), masl = c(0.5))
          
          newDat$fit.1 <- predict(mod.full.1, newDat, re.form = NA)
          newDat$fit.2 <- predict(mod.full.2, newDat, re.form = NA)
          newDat$fit.3 <- predict(sum.mod, newDat, re.form = NA)
          
          pred <- predict(sum.mod, newDat, re.form = NA)
          ci_line<-bootMer(sum.mod,FUN=function(.)
           predict(., newdata=newDat,re.form = NA), nsim=2000)
          ci_regT<-apply(ci_line$t,2,function(x) x[order(x)][c(50,1950)])
          
          newDat$stacked_plot <- newDat$fit.1 + newDat$fit.2
          
          ylim <- range(c(newDat$fit.1, newDat$fit.2, newDat$fit.3))
          
          png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/stacked_plots/", set1[[i]], "_to_", set2[[j]],"_", fixed.v, "_stacked.png"), width = 2255, height = 2000, units = "px", res = 300)
          
          plot(mean ~ fixed, data = df.1, xlab = "", ylab = "", yaxt = "n", ylim = ylim, col = plasma(5)[1])
          points(mean ~ fixed, data = df.2, ylab = "", col = plasma(5)[2])
          points(mean ~ fixed, data = df.sum, ylab = "", col = plasma(5)[3])
          
          for(masl in unique(newDat$masl)){
            i <- which(unique(newDat$masl) %in% masl)
            lines(fit.1 ~ fixed, data = newDat[newDat$masl %in% masl,], lty = ifelse(significant.effect, 1, 2), lwd = i, col = plasma(5)[1])
            lines(fit.2 ~ fixed, data = newDat[newDat$masl %in% masl,], lty = ifelse(significant.effect, 1, 2), lwd = i, col = plasma(5)[2])
            lines(fit.3 ~ fixed, data = newDat[newDat$masl %in% masl,], lty = ifelse(significant.effect, 1, 2), lwd = i, col = plasma(5)[3])
            lines(stacked_plot ~ fixed, data = newDat[newDat$masl %in% masl,], lty = ifelse(significant.effect, 1, 2), lwd = i, col = plasma(5)[4])
            lines(newDat$fixed,ci_regT[1,], col = plasma(5)[3],lty=2, lwd = i)
            lines(newDat$fixed,ci_regT[2,], col = plasma(5)[3],lty=2, lwd = i)}
          
          labels <- c(set1, set2, s, paste("sum of", set1, "and", set2))
          response.col = 1:4
          for (label in labels){
            legend <- paste(label)
            response.v.color <- response.col[which(labels %in% label)]
            mtext(side = 3, line = -which(labels %in% label), text = legend, adj = 0.95, col = plasma(5)[response.v.color], cex = 0.5, outer = F)
          }
          
          # title(paste("Stacked graphs by latitude"), outer = F, line = 1)
          mtext(side = 1, line = 3, text = "latitude", outer = F)
          mtext(side = 2, line = 3,  text = expression("Productivity Mg C"~ha^-1~yr^-1), outer = F)
          
          dev.off()
          
          # results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, best.model = best.model, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq, delta.aic = delta.aic)
          # 
          # all.results <- rbind(all.results, results)
          # all.aictab <- rbind(all.aictab, aictab)
          
        }
      }
      }
    }
    }
  }

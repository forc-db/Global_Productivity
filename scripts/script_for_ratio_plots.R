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


response.variables.groups <- list(c("GPP","NPP_1","BNPP_root","ANPP_1", "ANPP_foliage","ANPP_woody", "ANPP_woody_stem"))
response.variables.1 <- c("GPP","NPP_1","BNPP_root","ANPP_1", "ANPP_foliage","ANPP_woody", "ANPP_woody_stem")
response.variables.2 <- c("GPP","NPP_1","BNPP_root","ANPP_1", "ANPP_foliage","ANPP_woody", "ANPP_woody_stem")

for(response.variables in response.variables.groups){
  
  # if(response.variables[1] == "GPP") n <- 1
  # if(response.variables[1] == "ANPP_1") n <- 2
  # if(response.variables[1] == "ANPP_woody") n <- 3
  # if(response.variables[1] == "woody.mortality_ag") n <- 4
  
  ### mature forests only ####
  for (age in ages){
    
    if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
    if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
    
    for(fixed.v in fixed.variables){
      
      par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,0))
      print(fixed.v)
      
      ###subset ForC
      response.variables.col <- 1:length(response.variables)
      
      first.plot <- TRUE
      
      for (response.v.1 in response.variables.1){
        for (response.v.2 in response.variables.2){
          
          if (response.v.1 == response.v.2) {
            next
          }
          
        ################################ for response.v.1
        if(response.v.1 %in% "NPP") responses.to.keep.1  <- c("NPP_1")
        if(response.v.1 %in% "ANPP") responses.to.keep.1  <- c("ANPP_1")
        if(response.v.1 %in% "ANPP_litterfall") responses.to.keep.1  <- c("ANPP_litterfall_1")
        if(!response.v.1 %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep.1  <- response.v.1
        
        
        response.v.color <- response.variables.col[which(response.variables.1 %in% response.v.1)]
        
        rows.with.response.1 <- ForC_simplified$variable.name %in% responses.to.keep.1
        
        fixed.no.na <- !is.na(ForC_simplified[, fixed.v])
        
        df.1 <- ForC_simplified[rows.with.response.1 & ages.to.keep & fixed.no.na, ]
        
        df.1$fixed <- df.1[, fixed.v]
        ylim.1 <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables.1),]$mean)
        
        mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df.1, REML = F)
        mod.linear <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df.1, REML = F)
        mod.poly <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df.1, REML = F)
        
        aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod.linear = mod.linear), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        if (best.model == "mod.poly") mod.full.1 <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df.1, REML = F)
        if (best.model == "mod.linear") mod.full.1 <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df.1, REML = F)
        if (best.model == "mod") mod.full.1 <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df.1, REML = F)
        
        significant.effect <- anova(mod, mod.full.1)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full.1)$"Pr(>Chisq)"[2]
        sample.size <- length(df.1$mean)
        
        if (best.model == "mod.poly") mod.full.1 <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df.1, REML = T)
        if (best.model == "mod.linear") mod.full.1 <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df.1, REML = T)
        
        ########################################### for response.v.2
        
        if(response.v.2 %in% "NPP") responses.to.keep.2  <- c("NPP_1")
        if(response.v.2 %in% "ANPP") responses.to.keep.2  <- c("ANPP_1")
        if(response.v.2 %in% "ANPP_litterfall") responses.to.keep.2  <- c("ANPP_litterfall_1")
        if(!response.v.2 %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep.2  <- response.v.2
        
        
        response.v.color <- response.variables.col[which(response.variables.2 %in% response.v.2)]
        
        rows.with.response.2 <- ForC_simplified$variable.name %in% responses.to.keep.2
        
        fixed.no.na <- !is.na(ForC_simplified[, fixed.v])
        
        df.2 <- ForC_simplified[rows.with.response.2 & ages.to.keep & fixed.no.na, ]
        
        df.2$fixed <- df.2[, fixed.v]
        ylim.2 <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables.2),]$mean)
        
        mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df.2, REML = F)
        mod.linear <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df.2, REML = F)
        mod.poly <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df.2, REML = F)
        
        aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod.linear = mod.linear), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        if (best.model == "mod.poly") mod.full.2 <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df.2, REML = F)
        if (best.model == "mod.linear") mod.full.2 <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df.2, REML = F)
        if (best.model == "mod") mod.full.2 <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df.2, REML = F)
        
        significant.effect <- anova(mod, mod.full.2)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full.2)$"Pr(>Chisq)"[2]
        sample.size <- length(df.2$mean)
        
        if (best.model == "mod.poly") mod.full.2 <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df.2, REML = T)
        if (best.model == "mod.linear") mod.full.2 <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df.2, REML = T)
        
        ####################### subset data
        
        rows.with.response.3 <- ForC_simplified$variable.name %in% c(responses.to.keep.2, responses.to.keep.1)
        df.3 <- ForC_simplified[rows.with.response.3 & ages.to.keep & fixed.no.na, ]
        df.3$fixed <- df.3[, fixed.v]
        
        newDat <- expand.grid(fixed = seq(min(df.3$fixed), max(df.3$fixed), length.out = 100))
        
        newDat$fit.1 <- predict(mod.full.1, newDat, re.form = NA)
        newDat$fit.2 <- predict(mod.full.2, newDat, re.form = NA)
        
        newDat$ratio <- newDat$fit.1/newDat$fit.2
        
        tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/ratio_plots/", response.v.1, "_to_", response.v.2, "_", fixed.v, ".tiff"), width = 2255, height = 2000, units = "px", res = 300)
        
        plot(ratio ~ fixed, data = newDat, col = response.v.color, main = paste(response.v.1, response.v.2, fixed.v), xlab = fixed.v, ylab = paste(response.v.1, ":",response.v.2))
        # title(paste(response.v.1, response.v.2, fixed.v), outer = T, line = 1)
        # mtext(side = 1, line = 3, text = fixed.v, outer = T)
        # mtext(side = 2, line = 3,  text = paste(response.v.1, ":",response.v.2), outer = T)
        
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

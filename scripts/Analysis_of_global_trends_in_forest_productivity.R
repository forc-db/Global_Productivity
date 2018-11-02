######################################################
# Purpose: Statistical analysis to explore global trends in forest Productivity
# Developped by: Valentine Herrmann - HerrmannV@si.edu in Arpil 2018
#  R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(lme4)

# Load data ####
ForC_simplified <- read.csv("raw.data/ForC_simplified.csv", stringsAsFactors = F)
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

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0

## keep only records with min.dbh <= 10cm
min.dbh.to.keep <- ForC_simplified$min.dbh <= 10 & !is.na(ForC_simplified$min.dbh) # this doesn work great, not enough data, but maybe we can just keep the NAs?
min.dbh.to.keep <- rep(TRUE, nrow(ForC_simplified))

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


# Prepare some variables ####

## response variable list (fluxes) ####
all.response.variables <- VARIABLES[VARIABLES$variable.type %in% "flux",]$variable.name #c("GPP", "NPP", "ANPP", "ANPP_woody", "BNPP_root_fine")
all.response.variables <- gsub("(_OM|_C)", "", all.response.variables)
all.response.variables <- all.response.variables[all.response.variables %in% ForC_simplified$variable.name]
all.response.variables <- unique(gsub("_\\d", "", all.response.variables))

response.variables.groups <- list(c("NEE", "GPP", "NPP", "ANPP", "ANPP_woody", "delta.agb"),
                                  c("ANPP_foliage", "ANPP_woody_branch", "ANPP_woody_stem", "NPP_woody", "NPP_understory", "ANPP_litterfall"),
                                  c("ANPP_repro", "woody.mortality_ag","BNPP_root", "BNPP_root_fine", "BNPP_root_coarse", "TBCF", "ANPP_folivory"),
                                  c("R_auto_ag", "R_auto", "R_eco", "R_soil", "R_soil_het", "R_auto_wood", "R_auto_foliage", "R_auto_root"))
all.response.variables[!all.response.variables %in% unlist(response.variables.groups)]

## fixed variables list ####
fixed.variables <- c("mat", "map", "lat", "stand.age", "leaf.type", "leaf.phenology")

## prepare results table

all.results <- NULL


# Run analysis an plot ####

### mature forests only ####
for(fixed.v in fixed.variables){
  
  tiff(file = paste0("results/figures/Effect_of_", fixed.v, "_MATURE_only.tiff"), width = 2255, height = 2000, units = "px", res = 300)
  
  par(mfrow = c(2,2), mar = c(0,0,0,0), oma = c(5,5,2,0))
  print(fixed.v)
  
  categorical <- ifelse(fixed.v %in% c("leaf.type", "leaf.phenology"), TRUE, FALSE)
  
  ylim <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables.groups),]$mean)
  
  pannel.nb <- 1
  
  for(response.variables in response.variables.groups) {
    
    response.variables.col <- 1:length(response.variables)
    
    first.plot <- TRUE
    
    for (response.v in response.variables){ #, "ANPP_stem"
      
      print(response.v)
      
      if(response.v %in% "NEE") responses.to.keep  <- c("NEE", "NEP")
      if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
      if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
      if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1", "ANPP_litterfall_2", "ANPP_litterfall_3")
      if(!response.v %in% c("NEE", "NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
      
      
      response.v.color <- response.variables.col[which(response.variables %in% response.v)]
      
      rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
      
      fixed.no.na <- !is.na(ForC_simplified[, fixed.v])
      
      if(categorical) fixed.no.na <- fixed.no.na & !ForC_simplified[, fixed.v] %in% c("Other", "mixed")
      
      # drop age only for the analysis that has age as a fixed variable
      if(fixed.v %in% "stand.age") drop.ages.999.or.0.or.na <- ages.not.999.nor.0.nor.na
      if(!fixed.v %in% "stand.age") drop.ages.999.or.0.or.na <- rep(TRUE, nrow(ForC_simplified))
      
      # subset to keep only what we need
      df <- ForC_simplified[rows.with.response & drop.ages.999.or.0.or.na & age.greater.than.100 & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.no.na, ]
      
      
      if(all(responses.to.keep %in% c("NEE", "NEP"))) {
        # multiply NEP  by -1
        m <- match(df$variable.name, responses.to.keep)
        df$mean <- df$mean *c(1,-1)[m]
      }
      
      
      if(nrow(df) > 30 & ifelse(!categorical | (categorical & length(unique(df[, fixed.v])) >1), TRUE, FALSE)){
        df$fixed <- df[, fixed.v]
        
        
        # Deal with categorical levels
        
        
        if(categorical){
          
          if(fixed.v %in% "leaf.type") categories <- c("needleleaf", "broadleaf")
          if(fixed.v %in% "leaf.phenology") categories <- c("evergreen", "deciduous")
          if(!fixed.v %in% c("leaf.type", "leaf.phenology")) stop("Error: need to code here")
          
          
          df <- df[df[, fixed.v]%in% categories,]
          df$fixed <- factor(df$fixed, levels = categories)
          
        }
        
        # model
        
        mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df)
        
        if(fixed.v %in% "stand.age") mod.full <- lmer(mean ~ log10(fixed) + (1|geographic.area/plot.name), data = df)
        if(!fixed.v %in% "stand.age") mod.full <- lmer(mean ~ fixed + (1|geographic.area/plot.name), data = df)
        
        significant.effet <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        
        
        # plot 
        
        if(!categorical){
          if(first.plot) plot(mean ~ fixed, data = df, xlab = "", ylab = "", col = response.v.color, ylim = ylim, log = ifelse(fixed.v %in% "stand.age", "x", ""), xaxt = "n", yaxt = "n")
          if(!first.plot) points(mean ~ fixed, data = df, ylab = "", col = response.v.color) 
          
          abline(fixef(mod.full), col = response.v.color, lty = ifelse(significant.effet, 1, 2))
          
          if(first.plot) {
            axis(1 ,labels = ifelse(pannel.nb %in% c(3,4), TRUE, FALSE))
            axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
          }
        }
        
        if(categorical){
          
          df$fixed_cat <- paste(df$fixed, response.v, sep = "_")
          df$fixed_cat <- factor(df$fixed_cat, levels = apply(expand.grid(categories, response.variables), 1, paste, collapse = "_"))
          
        
            b <- boxplot(mean ~ fixed_cat, data = df,  xlab = "", ylab = "", ylim = ylim, add = !first.plot, xaxt = "n", yaxt = "n",  col = rgb(t(col2rgb(response.v.color)), maxColorValue = 255, alpha = c(255, 100)))
          
          
          if(first.plot) {
            axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
            axis(1, at = 1:nlevels(df$fixed_cat), labels = F)
          }
          # if(pannel.nb %in% c(3,4)) text(x = 1:nlevels(df$fixed_cat), y = ylim[1]-(diff(ylim)/8), labels = rep(categories, nlevels(df$fixed_cat)/2), srt = 45, xpd = T)
          
          points(c(fixef(mod.full)[1], fixef(mod.full)[1] + fixef(mod.full)[2]) ~ which(!is.na(b$stats[1,])), pch = 24, col=  "grey", bg = ifelse(significant.effet, "grey", response.v.color))
        }
        
        
        
        first.plot <- FALSE
        
        
        
        
        r <- round(fixef(mod.full), 2)
        equation <-  paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2])
        
        if (!categorical) mtext(side = 3, line = -which(response.variables %in% response.v), text = equation, adj = 0.1, col = response.v.color, cex = 0.5)
        
        if (categorical) mtext(side = 3, line = -which(response.variables %in% response.v), text = response.v, adj = 0.1, col = response.v.color, cex = 0.5)
        # dev.off()
        
        results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = ">=100 yrs", equation = equation, significant = significant.effet)
        
        all.results <- rbind(all.results, results)
      }
      
    }
    
    
    pannel.nb <- pannel.nb +1
    
    
  }
  
  if(!categorical)  legend("topright", lty = c(1,2), legend = c("significant effet", "non-significant effect"), bty = "n")
  if(categorical)  legend("topright", pch = c(24, 24, NA, NA), col= c("grey", "grey", NA, NA), pt.bg = c("grey", "white", NA, NA), fill = c(NA, NA, rgb(t(col2rgb(c("black", "black"))), maxColorValue = 255, alpha = c(255, 100))), border = c(NA, NA, "black", NA) , legend = c("significant effet", "non-significant effect", categories), bty = "n")
  
  title (paste("Effect of", fixed.v), outer = T, line = 1)
  mtext(side = 1, line = ifelse(categorical, 4, 3), text = fixed.v, outer = T)
  mtext(side = 2, line = 3,  text = expression("Mg C"~ha^-1~yr^-1), outer = T)
  
  dev.off()
  
  
  
}

#### age as an interaction ####
for(fixed.v in fixed.variables[! fixed.variables %in% "stand.age"]) {
  
  tiff(file = paste0("results/figures/Effect_of_", fixed.v, "_INTERACTION_of_AGE.tiff"), width = 1155, height = 1000, units = "px", res = 150)
  
  par(mfrow = c(2,2), mar = c(0,0,0,0), oma = c(5,5,2,0))
  print(fixed.v)
  
  categorical <- ifelse(fixed.v %in% c("leaf.type", "leaf.phenology"), TRUE, FALSE)
  
  ylim <- range(ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables.groups),]$mean)
  
  pannel.nb <- 1
  
  for(response.variables in response.variables.groups) {
    
    response.variables.col <- 1:length(response.variables)
    
    first.plot <- TRUE
    
    for (response.v in response.variables){ #, "ANPP_stem"
      
      print(response.v)
      
      if(response.v %in% "NEE") responses.to.keep  <- c("NEE", "NEP")
      if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
      if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
      if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1", "ANPP_litterfall_2", "ANPP_litterfall_3")
      if(!response.v %in% c("NEE", "NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
      
      
      response.v.color <- response.variables.col[which(response.variables %in% response.v)]
      
      rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
      
      fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "stand.age"])
      
      if(categorical) fixed.no.na <- fixed.no.na & !ForC_simplified[, fixed.v] %in% c("Other", "mixed")
      
      # drop age only for the analysis that has age as a fixed variable
      if(fixed.v %in% "stand.age") drop.ages.999.or.0.or.na <- ages.not.999.nor.0.nor.na
      if(!fixed.v %in% "stand.age") drop.ages.999.or.0.or.na <- rep(TRUE, nrow(ForC_simplified))
      
      # subset to keep only what we need
      df <- ForC_simplified[rows.with.response & drop.ages.999.or.0.or.na & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.no.na, ]
      
      
      if(all(responses.to.keep %in% c("NEE", "NEP"))) {
        # multiply NEP  by -1
        m <- match(df$variable.name, responses.to.keep)
        df$mean <- df$mean * c(1,-1)[m]
      }

      if(nrow(df) > 30 & ifelse(!categorical | (categorical & length(unique(df[, fixed.v])) >1), TRUE, FALSE)){
        df$fixed <- df[, fixed.v]
        
        
        # Deal with categorical levels
        if(categorical){
          
          if(fixed.v %in% "leaf.type") categories <- c("needleleaf", "broadleaf")
          if(fixed.v %in% "leaf.phenology") categories <- c("evergreen", "deciduous")
          if(!fixed.v %in% c("leaf.type", "leaf.phenology")) stop("Error: need to code here")
          
          
          df <- df[df[, fixed.v]%in% categories,]
          df$fixed <- factor(df$fixed, levels = categories)
          
        }
        
        # model
        
        # mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df)
        # if(fixed.v %in% "stand.age") mod.full <- lmer(mean ~ log10(fixed) + (1|geographic.area/plot.name), data = df)   # if(!fixed.v %in% "stand.age") mod.full <- lmer(mean ~ fixed + (1|geographic.area/plot.name), data = df)
        
        mod.full <- lmer(mean ~ fixed * stand.age + (1|geographic.area/plot.name), data = df)
        
        significant.effet.of.interaction <- drop1(mod.full)$AIC[2] > drop1(mod.full)$AIC[1]
        
        
        if(!significant.effet.of.interaction) {
          
          mod.full <- lmer(mean ~ fixed + stand.age + (1|geographic.area/plot.name), data = df)
        
          significant.effet.of.additive <- drop1(mod.full)$AIC[3] > drop1(mod.full)$AIC[1]
         
          if(!significant.effet.of.additive) {
            mod.full <- lmer(mean ~ fixed + (1|geographic.area/plot.name), data = df)
            significant.effet <- drop1(mod.full)$AIC[2] > drop1(mod.full)$AIC[1]
          } 
          
          if(significant.effet.of.additive) { # just to know if there is a significant effect of the main fixed effect
            significant.effet <-  drop1(mod.full)$AIC[2] > drop1(mod.full)$AIC[1]
          }
         
        }
        
        if(significant.effet.of.interaction) { # just to know if there is a significant effect of the main fixed effect
          significant.effet.of.additive <- FALSE
          significant.effet <- TRUE
        }
        
        # predict and plot if significant effect of age (with or without intereaction) + plot ####
    
        
        if (significant.effet.of.interaction | significant.effet.of.additive) {
          # predict 
          
          if(!categorical) newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100), stand.age = c(50, 150, 500))
          
          if(categorical) newDat <- expand.grid(fixed = levels(df$fixed), stand.age = c(50, 150, 500))
          
          newDat$fit <- predict(mod.full, newDat, re.form = NA)
          
          # plot
          
          if(!categorical){
            if(first.plot) plot(mean ~ fixed, data = df, xlab = "", ylab = "", col = response.v.color, ylim = ylim, log = ifelse(fixed.v %in% "stand.age", "x", ""), xaxt = "n", yaxt = "n")
            if(!first.plot) points(mean ~ fixed, data = df, ylab = "", col = response.v.color) 
            
            for(stand.age in unique(newDat$stand.age)){
              i <- which(unique(newDat$stand.age) %in% stand.age)
              lines(fit ~ fixed, data = newDat[newDat$stand.age %in% stand.age,], col = response.v.color, lty = ifelse(significant.effet.of.interaction, 1, 2), lwd = i)
              
            }
            
            if(first.plot) {
              axis(1 ,labels = ifelse(pannel.nb %in% c(3,4), TRUE, FALSE))
              axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
            }
          }
          
          if(categorical){
            
            df$fixed_cat <- paste(df$fixed, response.v, sep = "_")
            df$fixed_cat <- factor(df$fixed_cat, levels = apply(expand.grid(categories, response.variables), 1, paste, collapse = "_"))
            
            b <- boxplot(mean ~ fixed_cat, data = df,  xlab = "", ylab = "", ylim = ylim, add = !first.plot, xaxt = "n", yaxt = "n",  col = rgb(t(col2rgb(response.v.color)), maxColorValue = 255, alpha = c(255, 100)))
            
            
            if(first.plot) {
              axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
              axis(1, at = 1:nlevels(df$fixed_cat), labels = F)
            }
            # if(pannel.nb %in% c(3,4)) text(x = 1:nlevels(df$fixed_cat), y = ylim[1]-(diff(ylim)/8), labels = rep(categories, nlevels(df$fixed_cat)/2), srt = 45, xpd = T)
            
            
            for(stand.age in unique(newDat$stand.age)){
              i <- which(unique(newDat$stand.age) %in% stand.age)
              # lines(fit ~ fixed, data = newDat[newDat$stand.age %in% stand.age,], col = response.v.color, lty = ifelse(significant.effet.of.interaction, 1, 2), lwd = i)
              points(fit ~ which(!is.na(b$stats[1,])),  data = newDat[newDat$stand.age %in% stand.age,], pch = 24, col=  "grey", bg = ifelse(significant.effet, "grey", response.v.color), cex = i/2)
            }
            
            
          }
          
          # add equation
          
          r <- round(fixef(mod.full), 2)
          
          if(significant.effet.of.interaction) equation <- paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2], " + age x", r[3], "+", r[4], " x interaction" )
          if(significant.effet.of.additive) equation <-  paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2], " + age x", r[3])
          
          
          if(!categorical) mtext(side = 3, line = -which(response.variables %in% response.v), text = equation, adj = 0.1, col = response.v.color, cex = 0.5)
          if(categorical) mtext(side = 3, line = -which(response.variables %in% response.v), text = response.v, adj = 0.1, col = response.v.color, cex = 0.5)
          
        }
        
        # plot if no effect of age, plot
        
        if(!significant.effet.of.interaction & !significant.effet.of.additive){
          if(!categorical){
          if(first.plot) plot(mean ~ fixed, data = df, xlab = "", ylab = "", col = response.v.color, ylim = ylim, log = ifelse(fixed.v %in% "stand.age", "x", ""), xaxt = "n", yaxt = "n")
          if(!first.plot) points(mean ~ fixed, data = df, ylab = "", col = response.v.color) 
          
          abline(fixef(mod.full), col = response.v.color, lty = ifelse(significant.effet, 1, 2))
          
          if(first.plot) {
            axis(1 ,labels = ifelse(pannel.nb %in% c(3,4), TRUE, FALSE))
            axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
          }
        }
        
          if(categorical){
          
          df$fixed_cat <- paste(df$fixed, response.v, sep = "_")
          df$fixed_cat <- factor(df$fixed_cat, levels = apply(expand.grid(categories, response.variables), 1, paste, collapse = "_"))
          
          categorical.color <- 
            
            b <- boxplot(mean ~ fixed_cat, data = df,  xlab = "", ylab = "", ylim = ylim, add = !first.plot, xaxt = "n", yaxt = "n",  col = rgb(t(col2rgb(response.v.color)), maxColorValue = 255, alpha = c(255, 100)))
          
          
          if(first.plot) {
            axis(2 ,labels = ifelse(pannel.nb %in% c(1,3), TRUE, FALSE))
            axis(1, at = 1:nlevels(df$fixed_cat), labels = F)
          }
          # if(pannel.nb %in% c(3,4)) text(x = 1:nlevels(df$fixed_cat), y = ylim[1]-(diff(ylim)/8), labels = rep(categories, nlevels(df$fixed_cat)/2), srt = 45, xpd = T)
          
          points(c(fixef(mod.full)[1], fixef(mod.full)[1] + fixef(mod.full)[2]) ~ which(!is.na(b$stats[1,])), pch = 24, col=  "grey", bg = ifelse(significant.effet, "grey", response.v.color))
        }
        
          
          r <- round(fixef(mod.full), 2)
          equation <-  paste(response.v, "=", r[1], "+", fixed.v,  "x", r[2])
          
          if(!categorical)  mtext(side = 3, line = -which(response.variables %in% response.v), text = equation, adj = 0.1, col = response.v.color, cex = 0.5)
          if(categorical)  mtext(side = 3, line = -which(response.variables %in% response.v), text = response.v, adj = 0.1, col = response.v.color, cex = 0.5)
        }
        
        
        first.plot <- FALSE
        
       
        
        results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = ">=100 yrs", equation = equation, significant = significant.effet)
        
        all.results <- rbind(all.results, results)
      }
      
    } # for (response.v in response.variables)
    
    
    pannel.nb <- pannel.nb +1
    
    
  } # for(response.variables in response.variables.groups)
  
  if(!categorical)  legend("topright", lty = c(1,2), legend = c("significant effet", "non-significant effect"), bty = "n")
  if(categorical)  legend("topright", pch = c(24, 24, NA, NA), col= c("grey", "grey", NA, NA), pt.bg = c("grey", "white", NA, NA), fill = c(NA, NA, rgb(t(col2rgb(c("black", "black"))), maxColorValue = 255, alpha = c(255, 100))), border = c(NA, NA, "black", NA) , legend = c("significant effet", "non-significant effect", categories), bty = "n")
  
  title (paste("Effect of", fixed.v), outer = T, line = 1)
  mtext(side = 1, line = ifelse(categorical, 4, 3), text = fixed.v, outer = T)
  mtext(side = 2, line = 3,  text = expression("Mg C"~ha^-1~yr^-1), outer = T)
  
  dev.off()
  
  
  
} # for(fixed.v in fixed.variables[! fixed.variables %in% "stand.age"])

# look at results ####

all.results

# Effect of MAT + AGE####
first.plot <- TRUE
for (response.v in c("GPP", "NPP", "ANPP", "ANPP_woody")){
  fixed.v = "MAT + AGE"
  
  if(response.v %in% "GPP") responses.to.keep  <- response.v
  if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
  if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_woody") responses.to.keep  <- response.v
  
  rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
  
  fixed.no.na <- !is.na(ForC_simplified$mat) & !is.na(ForC_simplified$stand.age)
  
  df <- ForC_simplified[rows.with.response & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.no.na, ]
  
  mod.full <- lmer(mean ~ mat * stand.age +(1|geographic.area/plot.name), data = df)
  significant.effet.interaction <- which.min(drop1(mod.full)$AIC) == 1
  
  if(!significant.effet.interaction) {
    results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = "none", equation = NA, significant = significant.effet.interaction)
    all.results <- rbind(all.results, results)
  }
  
  if( significant.effet.interaction) stop ("code here")
  mod.full <- lmer(mean ~ mat * stand.age +(1|geographic.area/plot.name), data = df)
  significant.effet <- which.min(drop1(mod.full)$AIC) == 1
  
  max.effect.v <- rownames(drop1(mod.full))[which.max(drop1(mod.full)$AIC)]
  other.v <- rownames(drop1(mod.full))[-which.max(drop1(mod.full)$AIC)][2]
  
  plot(df$mean ~ df$mat, main = paste(response.v, "vs", max.effect.v), ylab = response.v, cex = sqrt(df$stand.age)/10)
  
  
  newDat1 <- data.frame(mat = seq(min(df$mat), max(df$mat), length.out = 100),
                        stand.age = quantile(df$stand.age, 0.25))
  newDat2 <- data.frame(mat = seq(min(df$mat), max(df$mat), length.out = 100),
                        stand.age = quantile(df$stand.age, 0.5))
  newDat3 <-data.frame(mat = seq(min(df$mat), max(df$mat), length.out = 100),
                       stand.age = quantile(df$stand.age, 0.75))
  
  fit1 <- predict(mod.full, newDat1, re.form =  NA)
  fit2 <- predict(mod.full, newDat2, re.form =  NA)
  fit3 <- predict(mod.full, newDat3, re.form =  NA)
  
  lines(fit1~ newDat1$mat, lwd = 1)
  lines(fit2~ newDat1$mat, lwd = 2)
  lines(fit3~ newDat1$mat, lwd = 3)
  
  legend("topright", lwd = 1:3, legend = c("age 1st quartile", "age median", "age 3rd quartile"), bty = "n")
  
  r <- round(fixef(mod.full), 2)
  equation <-  paste(r[1], "+", fixed.v,  "x", r[2])
  
  mtext(side = 3, line = -1, text = equation, adj = 0.1, cex = 0.5)
  
  results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = ">=100 yrs", equation = equation, significant = significant.effet)
  
  all.results <- rbind(all.results, results)
}

# Effect of LAT + AGE####
first.plot <- TRUE
for (response.v in c("GPP", "NPP", "ANPP", "ANPP_woody")){
  fixed.v = "LAT + AGE"
  
  if(response.v %in% "GPP") responses.to.keep  <- response.v
  if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
  if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_woody") responses.to.keep  <- response.v
  
  rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
  
  fixed.no.na <- !is.na(ForC_simplified$lat) & !is.na(ForC_simplified$stand.age)
  
  df <- ForC_simplified[rows.with.response & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.no.na, ]
  
  mod.full <- lmer(mean ~ lat * stand.age +(1|geographic.area/plot.name), data = df)
  significant.effet.interaction <- which.min(drop1(mod.full)$AIC) == 1
  
  if(!significant.effet.interaction) {
    results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = "none", equation = NA, significant = significant.effet.interaction)
    all.results <- rbind(all.results, results)
  }
  
  # if( significant.effet.interaction) stop ("code here")
  # mod.full <- lmer(mean ~ lat * stand.age +(1|geographic.area/plot.name), data = df)
  # significant.effet <- which.min(drop1(mod.full)$AIC) == 1
  # 
  # max.effect.v <- rownames(drop1(mod.full))[which.max(drop1(mod.full)$AIC)]
  # other.v <- rownames(drop1(mod.full))[-which.max(drop1(mod.full)$AIC)][2]
  # 
  # plot(df$mean ~ df[, max.effect.v], main = paste(response.v, "vs", max.effect.v), ylab = response.v)
  # 
  # newDat1 <- data.frame(max.effect.v = seq(min(df[, max.effect.v]), max(df[,max.effect.v]), length.out = 100),
  #                      other.v = quantile(df[, other.v], 0.25))
  # newDat2 <- data.frame(max.effect.v = seq(min(df[, max.effect.v]), max(df[,max.effect.v]), length.out = 100),
  #                       other.v = quantile(df[, other.v], 0.25))
  # 
  # abline(fixef(mod.full), col = "red", lty = ifelse(significant.effet, 1, 2))
  # 
  # 
  # legend("topright", col = "red", lty = c(1,2), legend = c("significant effet", "non-significant effect"), bty = "n")
  # 
  # r <- round(fixef(mod.full), 2)
  # equation <-  paste(r[1], "+", fixed.v,  "x", r[2])
  # 
  # mtext(side = 3, line = -1, text = equation, adj = 0.1)
  # 
  # results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = ">=100 yrs", equation = equation, significant = significant.effet)
  # 
  # all.results <- rbind(all.results, results)
}


# SAVE all.results ####
write.csv(all.results, file = "results/tables/global_trend_models.csv", row.names = F)



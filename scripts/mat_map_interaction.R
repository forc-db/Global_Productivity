

# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity")

# Load libaries ####
library(lme4)
library(MuMIn)
library(AICcmodavg)
library(piecewiseSEM)
library(viridis)
library(mgcv)

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

ForC_simplified$SolarRadiation <- ForC_simplified$SolarRadiation/1000


# Control for some factors ####
## Keep only age >=100
age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
ages <- c("age.greater.than.100")

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0

## keep only records with min.dbh <= 10cm
min.dbh.to.keep <- ForC_simplified$min.dbh <= 10 ##& !is.na(ForC_simplified$min.dbh) # this doesn work great, not enough data, but maybe we can just keep the NAs?
min.dbh.to.keep <- rep(TRUE, nrow(ForC_simplified))

ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)

fixed.no.na <- !is.na(ForC_simplified[, "map"]) & !is.na(ForC_simplified[, "masl"]) & !is.na(ForC_simplified[, "mat"])
ForC_simplified <- ForC_simplified[dist.to.keep & fixed.no.na & ages.to.keep, ]

### exclude Tura due to extreme high latitude
ForC_simplified <- ForC_simplified[!(ForC_simplified$sites.sitename %in% "Tura"),]

## prepare results table

all.results <- NULL
best.results <- NULL
outputs <- NULL

effects <- c("mat", "map", "(1|geographic.area/plot.name)")
pannel.nb <- 1
p.table <- NULL


response.variables <- c("GPP", "NPP", "ANPP", "ANPP_woody_stem", "ANPP_foliage", "BNPP_root", "BNPP_root_fine", "R_auto", "R_auto_root")

############################## this section of code is useful in determining whether there are significant interactive effects or not
####################it is necessary to look at both the AIC and Pvalues

par(mfrow = c(3,3), mar = c(1,0,0,2), oma = c(5,8,2,0), xpd = T)

for (response.v in response.variables){
  
  if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
  if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
  if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
  if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
  
  
  rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
  
  fixed.no.na <- !is.na(ForC_simplified[, "map"]) & !is.na(ForC_simplified[, "mat"])
  df <- ForC_simplified[rows.with.response & fixed.no.na, ]
  df$masl <- (df$masl/1000)
  
  mod.null <- lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
  mod.single <- lmer(mean ~ mat + (1|geographic.area/plot.name), data = df, REML = F)
  mod.add <- lmer(mean ~ mat + map + (1|geographic.area/plot.name), data = df, REML = F)
  mod.int <- lmer(mean ~ mat * map + (1|geographic.area/plot.name), data = df, REML = F) 
  aictab <- aictab(list(mod.single = mod.single, mod.add = mod.add, mod.int = mod.int), sort = F)
  Rsq.s <- as.data.frame(r.squaredGLMM(mod.single))
  Rsq.s <- signif(Rsq.s, digits=4)[1]
  Rsq.a <- as.data.frame(r.squaredGLMM(mod.add))
  Rsq.a <- signif(Rsq.a, digits=4)[1]
  Rsq.i <- as.data.frame(r.squaredGLMM(mod.int))
  Rsq.i <- signif(Rsq.i, digits=4)[1]
  BIC <- BIC(mod.single, mod.add, mod.int)
  best.model <- as.character(aictab(list(mod.single = mod.single, mod.add = mod.add, mod.int = mod.int), sort = T)$Modname[1])
  
  p.sing <- anova(mod.null, mod.single)$"Pr(>Chisq)"[2]
  p.sing <- signif(p.sing, digits = 2)
  
  p.addi <- anova(mod.add, mod.single)$"Pr(>Chisq)"[2]
  p.addi <- signif(p.addi, digits = 2)
  
  p.int <- anova(mod.int, mod.add)$"Pr(>Chisq)"[2]
  p.int <- signif(p.int, digits = 2)
  
  p.values <- data.frame(response.v = response.v, p.sing = p.sing, p.addi = p.addi, p.int = p.int)
  
  p.table <- rbind(p.table, p.values)
  
  
  aictab$Rsq <- c(Rsq.s, Rsq.a, Rsq.i)
  
  aictab <- cbind(aictab, BIC)
  anova <- as.data.frame(anova(mod.null, mod.single, mod.add, mod.int))
  anova$flux <- response.v
  significance <- anova(mod.single, mod.add, mod.int)$"Pr(>Chisq)"[2]
  significance <- signif(significance, digits=4)
  
  
  mod.full <- lmer(mean ~ mat * map + (1|geographic.area/plot.name), data = df, REML = F)
  
  significant.effect.of.interaction <- drop1(mod.full)$AIC[2] > drop1(mod.full)$AIC[1]
  if(significant.effect.of.interaction) dAIC <- drop1(mod.full)$AIC[2] - drop1(mod.full)$AIC[1]
  
  if(!significant.effect.of.interaction) {

    mod.full <- lmer(mean ~ mat + map + (1|geographic.area/plot.name), data = df, REML = F)
    mod <- lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
    
    significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
    
    p.add <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
    p.add <- signif(p.add, digits = 2)

    significant.effect.of.additive <- drop1(mod.full)$AIC[3] > drop1(mod.full)$AIC[1]
    dAIC <- drop1(mod.full)$AIC[3] - drop1(mod.full)$AIC[1]

    if(!significant.effect.of.additive) {
      mod.full <- lmer(mean ~ mat + (1|geographic.area/plot.name), data = df)
      significant.effect <- drop1(mod.full)$AIC[2] > drop1(mod.full)$AIC[1]
      dAIC <- drop1(mod.full)$AIC[2] - drop1(mod.full)$AIC[1]
    }

    if(significant.effect.of.additive) { # just to know if there is a significant effect of the main fixed effect
      significant.effect <-  drop1(mod.full)$AIC[2] > drop1(mod.full)$AIC[1]
    }

  }
  
  if(significant.effect.of.interaction) { # just to know if there is a significant effect of the main fixed effect
    significant.effect.of.additive <- FALSE
    significant.effect <- TRUE
  }
  
  # significant.effect.of.additive <- TRUE

  ylim = range(df$mean)
  xlim = c(-10, 28)# range(df$mat)
  
  if (significant.effect.of.interaction | significant.effect.of.additive | significant.effect) {
    # predict 
    first.plot <- TRUE
    lower_bound <- c(0, 1001, 2001, 3001)
    upper_bound <- c(1000, 2000, 3000, 7500)
    midpoint <- c(500, 1500, 2500, 3500)

    for (i in seq(along = lower_bound)){
      map.in.bin <- df[df$map > lower_bound[[i]] & df$map < upper_bound[[i]], ]
      if(first.plot) plot(mean ~ mat, data = map.in.bin, xlab = "", ylab = "", ylim = ylim, col = plasma(5)[i], xlim = xlim, las = 1, tck = 0.02, mgp = c(3, 0.5, 0))
      if(!first.plot) points(mean ~ mat, data = map.in.bin, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = ylim, col = plasma(5)[i], xlim = xlim) 
      newDat <- expand.grid(mat = seq(min(map.in.bin$mat), max(map.in.bin$mat), length.out = 100), map = midpoint[[i]])
      newDat$fit <- predict(mod.full, newDat, re.form = NA)
      for(map in unique(newDat$map)){
            j <- which(unique(newDat$map) %in% map)
            lines(fit ~ mat, data = newDat[newDat$map %in% map,], lty = ifelse(significant.effect.of.interaction|significant.effect, 1, 2), lwd = i, col = plasma(5)[i], xlim = xlim)

      }
      
      first.plot <- FALSE
    }

    if(response.v == "GPP") legend(x = -33, y = 43, lwd = c(1:4), legend = c(500, 1500, 2500, 3500), col = plasma(5)[1:4], inset = c(-0.4, 0), xpd = NA, title = "MAP (mm)", title.col = "black")
    # title (paste(response.v), outer = T, line = 1)
    mtext(side = 1, line = 3, text = expression(paste("Mean Annual Temperature (", degrees, ")")), outer = T)
    mtext(side = 2, line = 3,  text = expression(paste("Carbon flux (Mg C"~ha^-1~yr^-1,")")), outer = T)
    
    # add equation
    Rsq <- as.data.frame(r.squaredGLMM(mod.full))
    Rsq <- signif(Rsq, digits=4)[1]
    
    r <- round(fixef(mod.full), 4)
    fixed1.coef <- r[2]
    fixed2.coef <- r[3]
    int.coef <- r[4]
    
    if(pannel.nb %in% c(1:6)) axis(1, labels = F, tck = 0.02)
    if(pannel.nb %in% c(7:9)) axis(1, tck = 0.02)
    
    resp.v.info <- read.csv("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/raw.data/respv_data.csv", stringsAsFactors = F)
    respv <- resp.v.info$name[which(resp.v.info$response.v %in% response.v)]
    
    
    mtext(side = 3, line = -1, text = expression(paste("(", bold(letters[pannel.nb]), ") ", respv)), adj = 0.1, cex = 0.5)
    if(significant.effect.of.interaction) mtext(side = 3, line = -2, text = "Significant interactive effect", adj = 0.1, cex = 0.5)
    if(!significant.effect.of.interaction & significant.effect.of.additive) mtext(side = 3, line = -2, text = "Significant additive effect", adj = 0.1, cex = 0.5)
    if(!significant.effect.of.interaction & !significant.effect.of.additive & significant.effect) mtext(side = 3, line = -2, text = "Significant effect of MAT", adj = 0.1, cex = 0.5)
    
    results <- data.frame(response = response.v, fixed1.coef = fixed1.coef, fixed2.coef = fixed2.coef, int.coef = int.coef, Rsq = Rsq, dAIC = dAIC, significance = significance, best.model = best.model)
    
    all.results <- all.results <- rbind(all.results, results)
    outputs <- rbind(outputs, anova)
    pannel.nb <- pannel.nb +1
  }
  
}

dev.off()
# write.csv(all.results, "")
# write.csv(p.table, "")

########################################## code to correctly plot
Rsqs = NULL

response.variables <- c("GPP", "NPP", "ANPP", "ANPP_woody_stem", "ANPP_foliage", "BNPP_root", "BNPP_root_fine", "R_auto", "R_auto_root")
png(file = "C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/results/figures/final_figures/interactions/mat_map_interaction.png", width = 2255, height = 2000, units = "px", res = 300)
jpeg(file = "C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/results/figures/final_figures/interactions/mat_map_interaction.jpg", width = 2255, height = 2000, units = "px", res = 300)

par(mfrow = c(3,3), mar = c(2,0,0,2), oma = c(5,8,2,0), xpd = T)

pannel.nb <- 1

for (response.v in response.variables){
  
  if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
  if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
  if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
  if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
  
  
  rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
  
  fixed.no.na <- !is.na(ForC_simplified[, "map"]) & !is.na(ForC_simplified[, "mat"])
  df <- ForC_simplified[rows.with.response & fixed.no.na, ]
  df$masl <- (df$masl/1000)
  
  if(response.v %in% c("NPP", "ANPP_woody_stem")) mod.full <- lmer(mean ~ mat * map + (1|geographic.area/plot.name), data = df, REML = F)
  if(response.v %in% c("GPP", "ANPP", "R_auto")) mod.full <- lmer(mean ~ mat + map + (1|geographic.area/plot.name), data = df, REML = F)
  if(response.v %in% c("ANPP_foliage", "BNPP_root", "BNPP_root_fine", "R_auto_root")) mod.full <- lmer(mean ~ mat + (1|geographic.area/plot.name), data = df, REML = F)

  ylim = range(df$mean)
  xlim = c(-10, 28)
  
  first.plot <- TRUE
  lower_bound <- c(0, 1001, 2001, 3001)
  upper_bound <- c(1000, 2000, 3000, 7500)
  midpoint <- c(500, 1500, 2500, 3500)
  
  for (i in seq(along = lower_bound)){
    map.in.bin <- df[df$map > lower_bound[[i]] & df$map < upper_bound[[i]], ]
    if(first.plot) plot(mean ~ mat, data = map.in.bin, xlab = "", ylab = "", ylim = ylim, col = plasma(5)[i], xlim = xlim, las = 1, tck = 0.02, mgp = c(3, 0.5, 0))
    if(!first.plot) points(mean ~ mat, data = map.in.bin, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = ylim, col = plasma(5)[i], xlim = xlim) 
    newDat <- expand.grid(mat = seq(min(map.in.bin$mat), max(map.in.bin$mat), length.out = 100), map = midpoint[[i]])
    newDat$fit <- predict(mod.full, newDat, re.form = NA)
    for(map in unique(newDat$map)){
      j <- which(unique(newDat$map) %in% map)
      lines(fit ~ mat, data = newDat[newDat$map %in% map,], lty = ifelse(significant.effect.of.interaction|significant.effect, 1, 2), lwd = i, col = plasma(5)[i], xlim = xlim)
    }
    first.plot <- FALSE
  }
  
  if(response.v == "GPP") legend(x = -33, y = 43, lwd = c(1:4), legend = c(500, 1500, 2500, 3500), col = plasma(5)[1:4], inset = c(-0.4, 0), xpd = NA, title = "MAP (mm)", title.col = "black")
  # title (paste(response.v), outer = T, line = 1)
  mtext(side = 1, line = 3, text = expression(paste("Mean Annual Temperature (", degree, "C)")), outer = T)
  mtext(side = 2, line = 3,  text = expression(paste("Carbon flux (Mg C"~ha^-1~yr^-1,")")), outer = T)
  
  # add equation
  Rsq <- as.data.frame(r.squaredGLMM(mod.full))
  Rsq <- signif(Rsq, digits=4)[1]
  
  r <- round(fixef(mod.full), 4)
  fixed1.coef <- r[2]
  fixed2.coef <- r[3]
  int.coef <- r[4]
  

  if(pannel.nb %in% c(1:6)) axis(1, labels = F, tck = 0.02)
  if(pannel.nb %in% c(7:9)) axis(1, labels = F, tck = 0.02)
  
  resp.v.info <- read.csv("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/raw.data/respv_data.csv", stringsAsFactors = F)
  respv <- resp.v.info$name[which(resp.v.info$response.v %in% response.v)]
  
  mtext(side = 3, line = -1, text = paste0("(", letters[pannel.nb], ") ", respv), adj = 0.1, cex = 0.5)
  if(response.v %in% c("NPP", "ANPP_woody_stem")) mtext(side = 3, line = -2, text = "Significant interactive effect", adj = 0.1, cex = 0.5)
  if(response.v %in% c("GPP", "ANPP", "R_auto")) mtext(side = 3, line = -2, text = "Significant additive effect only", adj = 0.1, cex = 0.5)
  if(response.v %in% c("ANPP_foliage", "BNPP_root", "BNPP_root_fine", "R_auto_root")) mtext(side = 3, line = -2, text = "Significant effect of MAT only", adj = 0.1, cex = 0.5)
  Rsqs <- rbind(Rsqs, Rsq)
  pannel.nb <- pannel.nb +1
}

dev.off()

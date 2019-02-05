# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

library(lme4)
library(MuMIn)


ANPP_woody_stem_and_canopy <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_woody_and_canopy_WorldClim_CRU.csv")
ANPP_woody_stem_and_canopy <- ANPP_woody_stem_and_canopy[-121,]


ANPP_woody_stem_and_canopy$ratio_canopy_to_wood <- NA
ANPP_woody_stem_and_canopy$ratio_canopy_to_wood <- (ANPP_woody_stem_and_canopy$ANPP_woody_stem/ANPP_woody_stem_and_canopy$ANPP_canopy)

fixed.variables <- c("lat", "AnnualMeanTemp", "MeanDiurnalRange", "Isothermality","TempSeasonality", "MaxTWarmestMonth", "MinTColdestMonth", "TempRangeAnnual", "MeanTWetQ", "MeanTDryQ","MeanTWarmQ","MeanTColdQ", "AnnualPre","PreWetMonth", "PreDryMonth", "PreSeasonality", "PreWetQ", "PreDryQ", "PreWarmQ", "PreColdQ", "CloudCover", "AnnualFrostDays","AnnualPET", "AnnualWetDays")

fixed.variables.groups <- list(c("lat", "AnnualMeanTemp", "MeanDiurnalRange", "Isothermality"),
                                  c("TempSeasonality", "MaxTWarmestMonth", "MinTColdestMonth", "TempRangeAnnual"),
                                  c("MeanTWetQ", "MeanTDryQ","MeanTWarmQ","MeanTColdQ"),
                                  c("AnnualPre","PreWetMonth", "PreDryMonth", "PreSeasonality"),
                                  c("PreWetQ", "PreDryQ", "PreWarmQ", "PreColdQ"),
                               c("CloudCover", "AnnualFrostDays","AnnualPET", "AnnualWetDays"))

## give leaf type
broadleaf_codes <- c("2TEB", "2TDB", "2TB")
needleleaf_codes <- c("2TEN", "2TDN", "2TN")
mixed <- c("2TD", "2TE", "2TM", "2TREE")

ANPP_woody_stem_and_canopy$leaf.type <- ifelse(ANPP_woody_stem_and_canopy$dominant.veg %in% broadleaf_codes, "broadleaf",
                                    ifelse(ANPP_woody_stem_and_canopy$dominant.veg %in% needleleaf_codes, "needleleaf",
                                           ifelse(ANPP_woody_stem_and_canopy$dominant.veg %in% mixed, "mixed", "Other")))

## give leaf phenology
evergreen_codes <- c("2TE", "2TEB", "2TEN")
deciduous_codes <- c("2TDN", "2TDB", "2TD")


ANPP_woody_stem_and_canopy$leaf.phenology <- ifelse(ANPP_woody_stem_and_canopy$dominant.veg %in% evergreen_codes, "evergreen",
                                         ifelse(ANPP_woody_stem_and_canopy$dominant.veg %in% deciduous_codes, "deciduous", "Other"))

df<- ANPP_woody_stem_and_canopy

all.results <- NULL
par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,2))
for (fixed.variable in fixed.variables){
df$fixed <- df[, fixed.variable]
mod <-  lmer(ratio_canopy_to_wood ~ 1 + (1|geographic.area/plot.name), data = ANPP_woody_stem_and_canopy)

mod.full <- lmer(ratio_canopy_to_wood ~ get(paste0(fixed.variable)) + (1|geographic.area/plot.name), data = ANPP_woody_stem_and_canopy)

significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05

significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
significance <- signif(significance, digits=4)
legend1 <- paste0("p-value = ", significance)

Rsq <- as.data.frame(r.squaredGLMM(mod.full))
Rsq <- signif(Rsq, digits=4)
legend2 <- paste0("r-squared = ", Rsq[1])

sample.size <- length(ANPP_woody_stem_and_canopy$ratio_canopy_to_wood)

tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/foliage_woody/mixed_effects_model/ANPP_canopy_to ANPP_woody_stem_mixed_model_", fixed.variable, ".tiff"), width = 2255, height = 2000, units = "px", res = 300)

plot <- plot(ratio_canopy_to_wood ~ get(paste0(fixed.variable)), data = ANPP_woody_stem_and_canopy, ylim = range(0:4),
             xlab = paste0(fixed.variable),
             ylab = "ANPP_canopy:ANPP_woody_stem",
             main = paste0("ANPP_canopy:ANPP_woody_stem and ", fixed.variable))
abline(fixef(mod.full))
#title(paste0("Relationship between ANPP_canopy:ANPP_woody_stem and ", fixed.variable), outer = T, line = 1)
#mtext(side = 1, line = 3, text = fixed.variable, outer = T)
#mtext(side = 2, text = expression("ANPP_canopy:ANPP_woody_stem"), line = 3, outer = T)

legend("topright", legend=c(legend1, legend2))

results <- data.frame(fixed = fixed.variable, random = "geographic.area/plot.name", significant = significant.effect, p.value = significance, sample.size = sample.size)

results <- cbind(results, Rsq)

all.results <- rbind(all.results, results)

dev.off()

}

for (fixed.variable in fixed.variables){
  
  tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/foliage_woody/mixed_effects_model/residuals/residuals", fixed.variable, ".tiff"), width = 2255, height = 2000, units = "px", res = 300)
  mod <-  lmer(ratio_canopy_to_wood ~ 1 + (1|geographic.area/plot.name), data = ANPP_woody_stem_and_canopy)
  
  mod.full <- lmer(ratio_canopy_to_wood ~ get(paste0(fixed.variable)) + (1|geographic.area/plot.name), data = ANPP_woody_stem_and_canopy)
  par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,2))
  plot(residuals(mod.full))
  
  title <- paste0("Residuals ", fixed.variable)
  title(title, outer = T, line = 1)
  dev.off()
}

write.csv(all.results, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/canopy_vs_woody_mixed_model_results.csv")


all.results <- NULL
par(mfrow = c(1,1), mar = c(0,0,0,0))
for (fixed.variable in fixed.variables){
  mod <-  lm(ratio_canopy_to_wood ~ 1, data = ANPP_woody_stem_and_canopy)
  
  mod.full <- lm(ratio_canopy_to_wood ~ get(paste0(fixed.variable)), data=ANPP_woody_stem_and_canopy)
  
  significant.effect <- anova(mod.full)$"Pr(>F)"[1] < 0.05
  
  significance <- anova(mod.full)$"Pr(>F)"[1]
  significance <- signif(significance, digits=4)
  legend1 <- paste0("p-value = ", significance)
  
  Rsq <- summary(mod.full)$r.squared
  Rsq <- signif(Rsq, digits=4)
  legend2 <- paste0("r-squared = ", Rsq[1])
  
  Fstat <- summary(mod.full)$fstatistic
  
  sample.size <- length(ANPP_woody_stem_and_canopy$ratio_canopy_to_wood)
  
  tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/foliage_woody/linear_model/ANPP_canopy_to ANPP_woody_stem_linear_model_", fixed.variable, ".tiff"), width = 2255, height = 2000, units = "px", res = 300)
  
  plot(ratio_canopy_to_wood ~ get(paste0(fixed.variable)), data = ANPP_woody_stem_and_canopy, ylim = range(0:4),
       xlab = paste0(fixed.variable),
       ylab = "ANPP_canopy:ANPP_woody_stem",
       main = paste0("ANPP_canopy:ANPP_woody_stem and ", fixed.variable))
  abline(mod.full)
  #title(paste0("Relationship between ANPP_canopy:ANPP_woody_stem and ", fixed.variable), outer = T, line = 1)
  #mtext(side = 1, line = 3, text = fixed.variable, outer = T)
  #mtext(side = 2, text = expression("ANPP_canopy:ANPP_woody_stem"), line = 3, outer = T)
  
  legend("topright", legend=c(legend1, legend2))
  
  results <- data.frame(fixed = fixed.variable, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
  
  results <- cbind(results, Rsq)
  
  all.results <- rbind(all.results, results)
  
  dev.off()
  
}

par(mfrow = c(2,2), mar = c(0,0,0,0), oma = c(5,5,2,2))

for (fixed.variable in fixed.variables){
  
  tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/foliage_woody/linear_model/residuals/residuals", fixed.variable, ".tiff"), width = 2255, height = 2000, units = "px", res = 300)
  mod <-  lm(ratio_canopy_to_wood ~ 1, data = ANPP_woody_stem_and_canopy)
  
  mod.full <- lm(ratio_canopy_to_wood ~ get(paste0(fixed.variable)), data = ANPP_woody_stem_and_canopy)
  par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,2))
  plot(residuals(mod.full))
  
  title <- paste0("Residuals ", fixed.variable)
  title(title, outer = T, line = 1)
  
  dev.off()
}


write.csv(all.results, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/canopy_vs_woody_linear_model_results.csv")

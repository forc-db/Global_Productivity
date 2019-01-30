# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)
ForC_simplified$site_plot <- paste0(ForC_simplified$sites.sitename," ", ForC_simplified$plot.name)
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)
ForC_simplified <- ForC_simplified[which(ForC_simplified$stand.age >= 100), ]

ForC_simplified$biomes <- NA
ForC_simplified$biomes <- ifelse((grepl("Temperate", ForC_simplified$FAO.ecozone, fixed = TRUE)), "temperate", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Boreal", ForC_simplified$FAO.ecozone, fixed = TRUE)), "boreal", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Tropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "tropical", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Subtropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "subtropical", ForC_simplified$biomes)

ANPP_woody <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_woody"),]
ANPP_foliage <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_foliage"),]
ANPP_woody_stem <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_woody_stem"),]
ANPP_litterfall <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_litterfall_0", "ANPP_litterfall_1", "ANPP_litterfall_2"),]
ANPP_litterfall$variable.name <- gsub("(\\w*)(_1$|_2$|_0$)", "\\1", ANPP_litterfall$variable.name, perl = T)

ANPP_woody_and_foliage <- merge(ANPP_foliage, ANPP_woody[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))
ANPP_woody_stem_and_foliage <- merge(ANPP_foliage, ANPP_woody_stem[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))
ANPP_woody_and_litterfall <- merge(ANPP_litterfall, ANPP_woody[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))
ANPP_woody_stem_and_litterfall <- merge(ANPP_litterfall, ANPP_woody_stem[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))

write.csv(ANPP_woody_and_foliage, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_woody_and_foliage.csv")
write.csv(ANPP_woody_stem_and_foliage, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_woody_stem_and_foliage.csv")
write.csv(ANPP_woody_and_litterfall, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_woody_and_litterfall.csv")
write.csv(ANPP_woody_stem_and_litterfall, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_woody_stem_and_litterfall.csv")

subtropical_waf <- ANPP_woody_and_foliage[ANPP_woody_and_foliage$biomes == "subtropical",]
tropical_waf <- ANPP_woody_and_foliage[ANPP_woody_and_foliage$biomes == "tropical",]
boreal_waf <- ANPP_woody_and_foliage[ANPP_woody_and_foliage$biomes == "boreal",]
temperate_waf <- ANPP_woody_and_foliage[ANPP_woody_and_foliage$biomes == "temperate",]

subtropical_wsaf <- ANPP_woody_stem_and_foliage[ANPP_woody_stem_and_foliage$biomes == "subtropical",]
tropical_wsaf <- ANPP_woody_stem_and_foliage[ANPP_woody_stem_and_foliage$biomes == "tropical",]
boreal_wsaf <- ANPP_woody_stem_and_foliage[ANPP_woody_stem_and_foliage$biomes == "boreal",]
temperate_wsaf <- ANPP_woody_stem_and_foliage[ANPP_woody_stem_and_foliage$biomes == "temperate",]

tropicalModwaf <- lm(mean.x ~ 0 + mean.y, data=tropical_waf)
subtropicalModwaf <- lm(mean.x ~ 0 + mean.y, data=subtropical_waf)
borealModwaf <- lm(mean.x ~ 0 + mean.y, data=boreal_waf)
temperateModwaf <- lm(mean.x ~ 0 + mean.y, data=temperate_waf)
tropicalModwsaf <- lm(mean.x ~ 0 + mean.y, data=tropical_wsaf)
subtropicalModwsaf <- lm(mean.x ~ 0 + mean.y, data=subtropical_wsaf)
borealModwsaf <- lm(mean.x ~ 0 + mean.y, data=boreal_wsaf)
temperateModwsaf <- lm(mean.x ~ 0 + mean.y, data=temperate_wsaf)

par(mfrow = c(2,2), mar = c(0,0,0,0), oma = c(5,5,2,0))
plot(tropical_waf$mean.y, tropical_waf$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:15))
#abline(tropicalMod)
abline(tropicalModwaf)
plot(subtropical_waf$mean.y, subtropical_waf$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(subtropicalMod)
abline(subtropicalModwaf)
plot(temperate_waf$mean.y, temperate_waf$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(temperateMod)
abline(temperateModwaf)
plot(boreal_waf$mean.y, boreal_waf$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(borealMod)
abline(borealModwaf)

par(mfrow = c(2,2), mar = c(0,0,0,0), oma = c(5,5,2,0))
plot(tropical_wsaf$mean.y, tropical_wsaf$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(tropicalMod)
abline(tropicalModwsaf)
plot(subtropical_wsaf$mean.y, subtropical_wsaf$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(subtropicalMod)
abline(subtropicalModwsaf)
plot(temperate_wsaf$mean.y, temperate_wsaf$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(temperateMod)
abline(temperateModwsaf)
plot(boreal_wsaf$mean.y, boreal_wsaf$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(borealMod)
abline(borealModwaf)

ANPP_all <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_foliage", "ANPP_litterfall_0", "ANPP_litterfall_1", "ANPP_litterfall_2"),]
ANPP_all$variable.name <- gsub("(\\w*)(_1$|_2$|_0$)", "\\1", ANPP_all$variable.name, perl = T)
ANPP_all$variable.name <- gsub("litterfall", "foliage", ANPP_all$variable.name, perl = T)

ANPP_woody_all <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_woody", "ANPP_woody_stem"),]
ANPP_woody_all$variable.name <- gsub("_stem", "\\1", ANPP_woody_all$variable.name, perl = T)

ANPP_all <- merge(ANPP_all, ANPP_woody_all[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))

subtropical_all <- ANPP_all[which(ANPP_all$biomes == "subtropical"),]
tropical_all <- ANPP_all[which(ANPP_all$biomes == "tropical"),]
boreal_all <- ANPP_all[which(ANPP_all$biomes == "boreal"),]
temperate_all <- ANPP_all[which(ANPP_all$biomes == "temperate"),]

tropicalModall <- lm(mean.x ~ 0 + mean.y, data=tropical_all)
subtropicalModall <- lm(mean.x ~ 0 + mean.y, data=subtropical_all)
borealModall <- lm(mean.x ~ 0 + mean.y, data=boreal_all)
temperateModall <- lm(mean.x ~ 0 + mean.y, data=temperate_all)

par(mfrow = c(2,2), mar = c(0,0,0,0), oma = c(5,5,2,0))
plot(tropical_all$mean.y, tropical_all$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(tropicalMod)
abline(tropicalModall)
plot(subtropical_all$mean.y, subtropical_all$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(subtropicalMod)
abline(subtropicalModall)
plot(temperate_all$mean.y, temperate_all$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(temperateMod)
abline(temperateModall)
plot(boreal_all$mean.y, boreal_all$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
#abline(borealMod)
abline(borealModall)


dev.off()

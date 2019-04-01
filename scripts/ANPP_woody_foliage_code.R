# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)
ForC_simplified$site_plot <- paste0(ForC_simplified$sites.sitename," ", ForC_simplified$plot.name)
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)
ForC_simplified <- ForC_simplified[which(ForC_simplified$stand.age >= 50), ]
dist.to.keep <- ForC_simplified$managed %in% 0
ForC_simplified <- ForC_simplified[which(dist.to.keep),]

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
ANPP_canopy <- rbind(ANPP_litterfall, ANPP_foliage)

ANPP_woody_and_foliage <- merge(ANPP_foliage, ANPP_woody[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))
ANPP_woody_stem_and_foliage <- merge(ANPP_foliage, ANPP_woody_stem[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))
ANPP_woody_and_litterfall <- merge(ANPP_litterfall, ANPP_woody[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))
ANPP_woody_stem_and_litterfall <- merge(ANPP_litterfall, ANPP_woody_stem[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))

ANPP_woody_and_canopy <- merge(ANPP_canopy, ANPP_woody[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))
ANPP_woody_stem_and_canopy <- merge(ANPP_canopy, ANPP_woody_stem[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))

#####now working with ANPP_woody_stem_and_canopy

names(ANPP_woody_stem_and_canopy)[13] <- "ANPP_canopy"
names(ANPP_woody_stem_and_canopy)[41] <- "ANPP_woody_stem"

write.csv(ANPP_woody_stem_and_canopy, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_woody_stem_and_canopy.csv")


subtropical_wsac <- ANPP_woody_stem_and_canopy[which(ANPP_woody_stem_and_canopy$biomes == "subtropical"),]
tropical_wsac <- ANPP_woody_stem_and_canopy[which(ANPP_woody_stem_and_canopy$biomes == "tropical"),]
boreal_wsac <- ANPP_woody_stem_and_canopy[which(ANPP_woody_stem_and_canopy$biomes == "boreal"),]
temperate_wsac <- ANPP_woody_stem_and_canopy[which(ANPP_woody_stem_and_canopy$biomes == "temperate"),]

tropicalModwsac <- lm(ANPP_canopy ~ 0 + ANPP_woody_stem, data=tropical_wsac)
subtropicalModwsac <- lm(ANPP_canopy ~ 0 + ANPP_woody_stem, data=subtropical_wsac)
borealModwsac <- lm(ANPP_canopy ~ 0 + ANPP_woody_stem, data=boreal_wsac)
temperateModwsac <- lm(ANPP_canopy ~ 0 + ANPP_woody_stem, data=temperate_wsac)

biomes <- c("tropical", "subtropical", "temperate", "boreal")

tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/foliage_woody/ANPP_canopy_woody_by_biome.tiff"), width = 2255, height = 2000, units = "px", res = 300)

par(mfrow = c(2,2), mar = c(0,0,0,0), oma = c(5,5,2,0))

for (biome in biomes){
  samplex <- get(paste0(biome, "_wsac"))
  sample.size <- length(samplex$ANPP_canopy)
  legend <- paste0(biome, " sample size = ", sample.size)
  plot(ANPP_canopy ~ ANPP_woody_stem, data = get(paste0(biome, "_wsac")), pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
  abline(get(paste0(biome, "Modwsac")))
  mtext(side = 1, line = -3, text = legend, adj = 0.5, col = "black", cex = 0.75)
}

title (paste("Relationship between canopy ANPP and woody stem ANPP"), outer = T, line = 1)
mtext(side = 1, line = 3, text = expression("Woody stem ANPP Mg C"~ha^-1~yr^-1), outer = T)
mtext(side = 2, line = 3,  text = expression("Canopy ANPP Mg C"~ha^-1~yr^-1), outer = T)
  
dev.off()

##########################################################################################################################

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
abline(tropicalModall)
plot(subtropical_all$mean.y, subtropical_all$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
abline(subtropicalModall)
plot(temperate_all$mean.y, temperate_all$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
abline(temperateModall)
plot(boreal_all$mean.y, boreal_all$mean.x, pch = 16, col = "blue", xlim = range(0:15), ylim = range(0:6))
abline(borealModall)


dev.off()


########################################

# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)
ForC_simplified$site_plot <- paste0(ForC_simplified$sites.sitename," ", ForC_simplified$plot.name)
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)
ForC_simplified <- ForC_simplified[which(ForC_simplified$stand.age >= 50), ]
dist.to.keep <- ForC_simplified$managed %in% 0
ForC_simplified <- ForC_simplified[which(dist.to.keep),]

ForC_simplified$biomes <- NA
ForC_simplified$biomes <- ifelse((grepl("Temperate", ForC_simplified$FAO.ecozone, fixed = TRUE)), "temperate", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Boreal", ForC_simplified$FAO.ecozone, fixed = TRUE)), "boreal", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Tropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "tropical", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Subtropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "subtropical", ForC_simplified$biomes)

ANPP_2 <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_2"),]
ANPP_1 <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_1"),]

ANPP <- merge(ANPP_1, ANPP_2[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))

ANPP_woody <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_woody"),]
ANPP_woody_stem <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_woody_stem"),]
ANPP_woody_branch <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_woody_branch"),]
ANPP_woody_and_branch <- merge(ANPP_woody_stem, ANPP_woody_branch[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))

ANPP_woody_and_branch$ratio <- ANPP_woody_and_branch$mean.x/ANPP_woody_and_branch$mean.y
ANPP$ratio <- ANPP$mean.x/ANPP$mean.y

write.csv(ANPP_woody_and_branch, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_woody_and_branch.csv")

write.csv(ANPP, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_1_2.csv")


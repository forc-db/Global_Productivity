# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

library(ggplot2)
library(ggpubr)
library(cowplot)
library(gridExtra)

ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)
ForC_simplified$site_plot <- paste0(ForC_simplified$sites.sitename," ", ForC_simplified$plot.name)
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)
ForC_simplified <- ForC_simplified[which(ForC_simplified$stand.age >= 100), ]
dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0
ForC_simplified <- ForC_simplified[which(dist.to.keep),]

alt.to.keep <- ForC_simplified$masl <= 1000 & !is.na(ForC_simplified$masl)
ForC_simplified <- ForC_simplified[alt.to.keep, ]

ForC_simplified$lat <- abs(ForC_simplified$lat)

koeppen_prob <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/forest_area_koeppen.csv", stringsAsFactors = F)
ForC_simplified$weight <- koeppen_prob$prob[match(ForC_simplified$Koeppen, koeppen_prob$koeppen)]


ForC_simplified$biomes <- NA
ForC_simplified$biomes <- ifelse((grepl("Temperate", ForC_simplified$FAO.ecozone, fixed = TRUE)), "temperate", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Boreal", ForC_simplified$FAO.ecozone, fixed = TRUE)), "boreal", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Tropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "tropical", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Subtropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "subtropical", ForC_simplified$biomes)


fixed.variables <- c("mat", "map", "lat", "AnnualMeanTemp", "AnnualPre", "TempSeasonality", "TempRangeAnnual", "VapourPressure", "VapourPressureDeficit")

set1 <- c("GPP", "NPP_1", "ANPP_1", "ANPP_foliage", "NPP_1", "NPP_1", "NPP_1")
set2 <- c("NPP_1", "ANPP_2", "BNPP_root", "ANPP_woody_stem", "BNPP_root", "ANPP_foliage", "ANPP_woody_stem")


for (i in seq(along = set1)){
  for (j in seq(along = set2)){
    if (i == j){
      resp1 <- ForC_simplified[ForC_simplified$variable.name %in% set1[[i]],]
      resp2 <- ForC_simplified[ForC_simplified$variable.name %in% set2[[j]],]
      
      df <- merge(resp1, resp2[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot"))
      
      df$ratio <- df$mean.x/df$mean.y
      
      range <- quantile(df$ratio, 0.99)
      
      df <- df[df$biomes %in% c("boreal", "temperate", "tropical"),]
      
      my_comparisons <- list( c("boreal", "tropical"), c("boreal", "temperate"), c("tropical", "temperate"))
      
      png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/boxplot_comparisons/boxplot_", set1[[i]], "_", set2[[j]], ".png"), width = 2255, height = 2000, units = "px", res = 300)
      p <- ggboxplot(df, x = "biomes", y = "ratio",
                     color = "biomes", palette = "jco", outlier.shape = NA, ylim = c(0, range), order = c("boreal", "temperate", "tropical")) +
        stat_compare_means(comparisons = my_comparisons, hide.ns = T, label = "p.signif", label.y = c(range, range - 0.25 , range - 0.5), tip.length = 0.01)
      print(p)
      dev.off()
        
      }
      
    }
  }

#########################

set1 <- "ANPP_1"
set2 <- "BNPP_root"


for (i in seq(along = set1)){
  for (j in seq(along = set2)){
    if (i == j){
      resp1 <- ForC_simplified[ForC_simplified$variable.name %in% set1[[i]],]
      resp2 <- ForC_simplified[ForC_simplified$variable.name %in% set2[[j]],]
      
      df <- merge(resp1, resp2[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot"))
      
      df$ratio <- df$mean.x/df$mean.y
      
      fixed.no.na <- !is.na(df[, "lat"]) & !is.na(df[, "masl"])
      if(length(fixed.no.na > 0))  df <- df[fixed.no.na, ]
      
      mod <-  lmer(ratio ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
      mod.linear <- lmer(ratio ~ poly(lat, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      
      best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
      
      if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(lat, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      if (best.model == "mod") mod.full <- lmer(ratio ~ poly(lat, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      
      significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      sample.size <- length(df$ratio)
      
      if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(lat, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = T)
      
      range <- quantile(df$ratio, 0.99)
      
      df <- df[df$biomes %in% c("boreal", "temperate", "tropical"),]
      
      my_comparisons <- list( c("boreal", "tropical"), c("boreal", "temperate"), c("tropical", "temperate"))
      
      png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/boxplot_comparisons/boxplot_ANPP_BNPP.png"), width = 3000, height = 2000, units = "px", res = 300)
      p2 <- ggboxplot(df, x = "biomes", y = "ratio", ylab = "ANPP:BNPP", xlab = "", outlier.shape = NA, ylim = c(0, 6.5), order = c("boreal", "temperate", "tropical"), add = "jitter") +
        stat_compare_means(comparisons = my_comparisons, hide.ns = T, label = "p.signif", label.y = c(6.25, 6, 5.75), tip.length = 0.01)
      print(p2)
      dev.off()
      
    }
  }
}

set1 <- "ANPP_foliage"
set2 <- "ANPP_woody_stem"


for (i in seq(along = set1)){
  for (j in seq(along = set2)){
    if (i == j){
      resp1 <- ForC_simplified[ForC_simplified$variable.name %in% set1[[i]],]
      resp2 <- ForC_simplified[ForC_simplified$variable.name %in% set2[[j]],]
      
      df <- merge(resp1, resp2[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot"))
      
      df$ratio <- df$mean.x/df$mean.y
      
      fixed.no.na <- !is.na(df[, "lat"]) & !is.na(df[, "masl"])
      if(length(fixed.no.na > 0)) df <- df[fixed.no.na, ]
      
      mod <-  lmer(ratio ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
      mod.linear <- lmer(ratio ~ poly(lat, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      
      best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
      
      if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(lat, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      if (best.model == "mod") mod.full <- lmer(ratio ~ poly(lat, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
      
      significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
      significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
      sample.size <- length(df$ratio)
      
      if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(lat, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = T)
      
      range <- quantile(df$ratio, 0.99)
      
      df <- df[df$biomes %in% c("boreal", "temperate", "tropical"),]
      
      my_comparisons <- list( c("boreal", "tropical"), c("boreal", "temperate"), c("tropical", "temperate"))
      
      png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/boxplot_comparisons/boxplot_foliage_woodystem.png"), width = 3000, height = 2000, units = "px", res = 300)
      p2 <- ggboxplot(df, x = "biomes", y = "ratio", ylab = "ANPP foliage: ANPP woody stem", xlab = "", outlier.shape = NA, ylim = c(0, 4), order = c("boreal", "temperate", "tropical"), add = "jitter") +
        stat_compare_means(comparisons = my_comparisons, hide.ns = T, label = "p.signif", label.y = c(4, 3.75, 3.5), tip.length = 0.01)
      print(p2)
      dev.off()
      
    }
  }
}

# png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/boxplot_comparisons/boxplot_foliage_woody_combined.png"), width = 3000, height = 2000, units = "px", res = 300)
# p <- grid.arrange(p1, p2, nrow = 1)
# dev.off()


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
ForC_simplified$lat <- abs(ForC_simplified$lat)

koeppen_prob <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/forest_area_koeppen.csv", stringsAsFactors = F)
ForC_simplified$weight <- koeppen_prob$prob[match(ForC_simplified$Koeppen, koeppen_prob$koeppen)]


ForC_simplified$biomes <- NA
ForC_simplified$biomes <- ifelse((grepl("Temperate", ForC_simplified$FAO.ecozone, fixed = TRUE)), "temperate", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Boreal", ForC_simplified$FAO.ecozone, fixed = TRUE)), "boreal", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Tropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "tropical", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Subtropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "subtropical", ForC_simplified$biomes)


fixed.variables <- c("mat", "map", "lat", "AnnualMeanTemp", "AnnualPre", "TempSeasonality", "TempRangeAnnual", "VapourPressure", "VapourPressureDeficit")

set1 <- c("GPP", "NPP_1", "ANPP_1", "ANPP_foliage", "ANPP_foliage", "NPP_1", "ANPP_2")
set2 <- c("NPP_1", "ANPP_2", "BNPP_root", "ANPP_woody", "ANPP_woody_stem", "BNPP_root", "BNPP_root")


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

set1 <- c("ANPP_foliage", "ANPP_foliage")
set2 <- c("ANPP_woody", "ANPP_woody_stem")


for (i in seq(along = set1)){
  for (j in seq(along = set2)){
    if (i == j){
      resp1 <- ForC_simplified[ForC_simplified$variable.name %in% set1[[i]],]
      resp2 <- ForC_simplified[ForC_simplified$variable.name %in% set2[[j]],]
      
      df <- merge(resp1, resp2[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age", "weight")], by= c("site_plot", "date"))
      
      df$ratio <- df$mean.x/df$mean.y
      
      range <- quantile(df$ratio, 0.99)
      
      for (fixed.variable in fixed.variables){
        df$fixed <- df[, fixed.variable]
        
        fixed.no.na <- !is.na(df[, fixed.variable]) & !is.na(df[, "masl"])
        
        df <- df[fixed.no.na, ]
        
        df$masl <- df$masl/1000
        
        mod <-  lmer(ratio ~ 1 + (1|geographic.area/plot.name), data = df, REML = F, weights = weight.x)
        
        mod.full <- lmer(ratio ~ fixed + masl + (1|geographic.area/plot.name), data = df, REML = F, weights = weight.x)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        significance <- signif(significance, digits=4)
        legend1 <- paste0("p-value = ", significance)
        
        Rsq <- as.data.frame(r.squaredGLMM(mod.full))
        Rsq <- signif(Rsq, digits=4)
        legend2 <- paste0("r-squared = ", Rsq[1])
        
        sample.size <- length(df$ratio)
        
        png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/foliage_woody_ratio/", set1[[i]], "_", set2[[j]], "_", fixed.variable, ".png"), width = 2255, height = 2000, units = "px", res = 300)
        
        par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,2))
        
        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100), masl = c(0.5))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        plot <- plot(ratio ~ fixed, data = df)
        # xlab = paste0(fixed.variable),
        # ylab = paste0(set1[[i]], ":", set2[[j]]))
        
        for(masl in unique(newDat$masl)){
          i <- which(unique(newDat$masl) %in% masl)
          lines(fit ~ fixed, data = newDat[newDat$masl %in% masl,], col = "red", lwd = i)}
        
        #title(paste0("Relationship between ANPP_canopy:ANPP_woody_stem and ", fixed.variable), outer = T, line = 1)
        mtext(side = 1, line = 3, text = paste0(fixed.variable), outer = T)
        mtext(side = 2, text = paste0(set1[[i]], ":", set2[[j]]), line = 3, outer = T)
        
        legend("topright", legend=c(legend1, legend2))
        
        
        dev.off()
        
      }
      
    }
  }
}
#########################

set1 <- "ANPP_foliage"
set2 <- "ANPP_woody"


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
      
      p1 <- ggboxplot(df, x = "biomes", y = "ratio", outlier.shape = NA, ylab = "ANPP foliage: ANPP woody", xlab = "", ylim = c(0, range), order = c("boreal", "temperate", "tropical")) +
        stat_compare_means(comparisons = my_comparisons, hide.ns = T, label = "p.signif", label.y = c(range, range - 0.25 , range - 0.5), tip.length =  0.01)
      
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
      
      range <- quantile(df$ratio, 0.99)
      
      df <- df[df$biomes %in% c("boreal", "temperate", "tropical"),]
      
      my_comparisons <- list( c("boreal", "tropical"), c("boreal", "temperate"), c("tropical", "temperate"))
      
      png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/boxplot_comparisons/boxplot_foliage_woodystem.png"), width = 3000, height = 2000, units = "px", res = 300)
      p2 <- ggboxplot(df, x = "biomes", y = "ratio", ylab = "ANPP foliage: ANPP woody stem", xlab = "", outlier.shape = NA, ylim = c(0, 4), order = c("boreal", "temperate", "tropical")) +
        stat_compare_means(comparisons = my_comparisons, hide.ns = T, label = "p.signif", label.y = c(4, 3.75, 3.5), tip.length = 0.01)
      print(p2)
      dev.off()
      
    }
  }
}

# png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/boxplot_comparisons/boxplot_foliage_woody_combined.png"), width = 3000, height = 2000, units = "px", res = 300)
# p <- grid.arrange(p1, p2, nrow = 1)
# dev.off()


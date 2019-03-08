# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)
ForC_simplified$site_plot <- paste0(ForC_simplified$sites.sitename," ", ForC_simplified$plot.name)
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)
ForC_simplified <- ForC_simplified[which(ForC_simplified$stand.age >= 50), ]
dist.to.keep <- ForC_simplified$managed %in% 0
ForC_simplified <- ForC_simplified[which(dist.to.keep),]
ForC_simplified$lat <- abs(ForC_simplified$lat)


ForC_simplified$biomes <- NA
ForC_simplified$biomes <- ifelse((grepl("Temperate", ForC_simplified$FAO.ecozone, fixed = TRUE)), "temperate", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Boreal", ForC_simplified$FAO.ecozone, fixed = TRUE)), "boreal", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Tropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "tropical", ForC_simplified$biomes)
ForC_simplified$biomes <- ifelse((grepl("Subtropical", ForC_simplified$FAO.ecozone, fixed = TRUE)), "subtropical", ForC_simplified$biomes)
# 
# ANPP_woody <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_woody"),]
# ANPP_foliage <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_foliage"),]
# ANPP_woody_stem <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_woody_stem"),]
# 
# ANPP_woody_and_foliage <- merge(ANPP_foliage, ANPP_woody[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))
# ANPP_woody_stem_and_foliage <- merge(ANPP_foliage, ANPP_woody_stem[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))

set1 <- c("GPP", "NPP_1", "ANPP_1", "ANPP_foliage", "ANPP_foliage")
set2 <- c("NPP_1", "ANPP_1", "BNPP_root", "ANPP_woody", "ANPP_woody_stem")


for (i in seq(along = set1)){
  for (j in seq(along = set2)){
    if (i == j){
      resp1 <- ForC_simplified[ForC_simplified$variable.name %in% set1[[i]],]
      resp2 <- ForC_simplified[ForC_simplified$variable.name %in% set2[[j]],]
      
      df <- merge(resp1, resp2[, c("variable.name", "date", "start.date", "end.date", "mean", "citation.ID", "site_plot", "stand.age")], by= c("site_plot", "citation.ID", "stand.age"))
      
      df$ratio <- df$mean.x/df$mean.y
      
      df <- df[df$biomes %in% c("boreal", "temperate", "tropical"),]
      
      my_comparisons <- list( c("boreal", "tropical"), c("boreal", "temperate"), c("tropical", "temperate"))
      
      tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/foliage_woody/boxplot_", set1[[i]], "_", set2[[j]], ".tiff"), width = 2255, height = 2000, units = "px", res = 300)
      p <- ggboxplot(df, x = "biomes", y = "ratio",
                     color = "biomes", palette = "jco", outlier.shape = NA, ylim = c(0,4), order = c("boreal", "temperate", "tropical")) +
        stat_compare_means(comparisons = my_comparisons, label.y = c(3, 4, 3.5))
      print(p)
      dev.off()
    }
  }
}

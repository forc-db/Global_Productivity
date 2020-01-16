# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/becky/Dropbox (Smithsonian)/GitHub/ForC")
library(MuMIn)

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)

VARIABLES <- read.csv(paste0(dirname(getwd()), "/ForC/data/ForC_variables.csv"), stringsAsFactors = F)

variables.to.keep <- ForC_simplified$variable.name %in% c("GPP", "NPP_1", "ANPP_1", "ANPP_2", "BNPP_root", "BNPP_root_fine", "ANPP_foliage", "ANPP_woody_stem", "R_auto", "R_auto_root")
ForC_simplified <- ForC_simplified[variables.to.keep,]

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0
ForC_simplified <- ForC_simplified[dist.to.keep, ]

age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
ForC_simplified <- ForC_simplified[age.greater.than.100, ]

## take absolute value of latitude
ForC_simplified$lat <- abs(ForC_simplified$lat)


ForC_simplified <- ForC_simplified[, c("lat", "mat", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "TempSeasonality", "length_growing_season")]

variables1 <- c("lat", "mat", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "TempSeasonality", "length_growing_season")
variables2 <- c("lat", "mat", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "TempSeasonality", "length_growing_season")

fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
labels <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v.info)]

# pairs(ForC_simplified[,1:7], cex = 0.5, upper.panel = panel.cor,
      # na.action = na.omit,
      # labels = c("Latitude", expression(paste('Mean annual \ntemperature \n(degrees Celsius)')), expression(paste('Mean annual \nprecipitation \n(mm/yr)')), expression(paste('Potential \nevapotranspiration \n(mm/yr)')), expression(paste('Vapour pressure \n(kPa)')),expression(paste('Temperature \nseasonality')) , expression(paste('Length of growing \nseason (months)'))),
      # label.pos = 0.4)

panel.number <- 1

png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/Github/Global_Productivity/results/figures/final_figures/supporting_information/climate_regressions.png"), width = 3500, height = 3000, units = "px", res = 300)

par(mfrow = c(7,7), mar = c(3,3,0.5,0.5), oma = c(0,0,0,0))

for (i in seq(along = variables1)){
  for (j in seq(along = variables2)){
  print(i)
  print(j)
  
  df <- ForC_simplified
  # 
  df$var1 <- df[, variables1[[i]]]
  df$var2 <- df[, variables2[[j]]]
  # 
  fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/Github/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
  xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% variables2[[j]])]
  yaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% variables1[[i]])]
  # 
  if(i!=j) fit <- lm(var1 ~ var2 , data = df)
  if(i!=j) significant.effect <- summary(fit)$coefficients[2,4] < 0.05
  ### this tests whether any correlations are non-significant; if they are non-significant they can be filtered out when graphed
  ifelse(i==j, NA, 
         ifelse(significant.effect == FALSE, print("significant effect false"), NA))
  if(i!=j) Rsq <- as.data.frame(r.squaredGLMM(fit))
  if(i!=j) Rsq <- signif(Rsq, digits=4)
  if(i!=j) legend <- Rsq[1]
  # # significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
  # 
  # 
  print(panel.number)
  if (i == j) plot(NULL, xlim=c(0,1), ylim=c(0,1), xaxt = "n", yaxt = "n")
  if (i < j) plot(NULL, xlim=c(0,1), ylim=c(0,1), xaxt = "n", yaxt = "n")
  if (i < j) legend("center", legend = legend, cex = 2, bty = "n")
  if (i > j) plot(var1 ~ var2, data = df, yaxt = ifelse(panel.number %in% c(1, 8, 15, 22, 29, 36, 43), "s", "n"), xaxt = ifelse(panel.number %in% c(43:49), "s", "n"), cex= 0.5)
  # # if(panel.number == c(43:49))
  # if (i < j) mtext(text = legend, side = 3, adj = 0.5, line = -5, cex = 1)
  if (i > j) abline(fit, lty = ifelse(significant.effect, 1, 2))
  if (panel.number %in% c(1, 8, 15, 22, 29, 36, 43)) mtext(side = 2, line = 2, text = eval(parse(text = yaxis)), outer = F, cex = 0.5)
  if (panel.number %in% c(43:49)) mtext(side = 1, line = 2,  text = eval(parse(text = xaxis)), outer = F, cex = 0.5)
  # 
  panel.number <- panel.number +1
  
  }}

dev.off()

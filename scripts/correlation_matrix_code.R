# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("")
library(MuMIn)

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)

variables.to.keep <- ForC_simplified$variable.name %in% c("GPP", "NPP_1", "ANPP_1", "ANPP_2", "BNPP_root", "BNPP_root_fine", "ANPP_foliage", "ANPP_woody_stem", "R_auto", "R_auto_root")
ForC_simplified <- ForC_simplified[variables.to.keep,]

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0
ForC_simplified <- ForC_simplified[dist.to.keep, ]

age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
ForC_simplified <- ForC_simplified[age.greater.than.100, ]

## take absolute value of latitude
ForC_simplified$lat <- abs(ForC_simplified$lat)
ForC_simplified$TempSeasonality <- ForC_simplified$TempSeasonality/100


ForC_simplified <- ForC_simplified[, c("lat", "mat", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "TempSeasonality", "length_growing_season")]

##two groups of variables, needs to be identical + in the same order
variables1 <- c("lat", "mat", "TempSeasonality", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "length_growing_season")
variables2 <- c("lat", "mat", "TempSeasonality", "map", "PotentialEvapotranspiration", "VapourPressureDeficit", "length_growing_season")

fixed.v.info <- read.csv("fixedv_data.csv", stringsAsFactors = F)
labels <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v.info)]

panel.number <- 1

png(file = "", width = 3500, height = 3000, units = "px", res = 300)

##creates grid of plots, mfrow should equal number of variables
par(mfrow = c(7,7), mar = c(2.5,2.5,0,0), oma = c(1,1,0,0))


##loops through variables, pairs each variable with each other variable, but skips if they're the same
for (i in seq(along = variables1)){
  for (j in seq(along = variables2)){
  print(i)
  print(j)
  
  df <- ForC_simplified
  # 
  df$var1 <- df[, variables1[[i]]]
  df$var2 <- df[, variables2[[j]]]
  # 
  fixed.v.info <- read.csv("fixedv_data.csv", stringsAsFactors = F)
  xaxis <- fixed.v.info$xaxis_simple[which(fixed.v.info$fixed.v %in% variables2[[j]])]
  yaxis <- fixed.v.info$xaxis_simple[which(fixed.v.info$fixed.v %in% variables1[[i]])]
  centre <- fixed.v.info$abbrev[which(fixed.v.info$fixed.v %in% variables2[[j]])]
  # 
  if(i!=j) fit <- lm(var1 ~ var2 , data = df)
  
  if(i!=j) pearsons.r <- rcorr(df$var1, df$var2)
  if(i!=j) pearsons.r <- pearsons.r$r[2]
  if(i!=j) pearsons.r <- round(pearsons.r, digits = 2)
  if(i!=j) significant.effect <- summary(fit)$coefficients[2,4] < 0.05
  
  ### this tests whether any correlations are non-significant; if they are non-significant they can be filtered out when graphed
  ifelse(i==j, NA, 
         ifelse(significant.effect == FALSE, print("significant effect false"), NA))
  if(i!=j) Rsq <- as.data.frame(r.squaredGLMM(fit))
  if(i!=j) Rsq <- round(Rsq, digits=2)
  if(i!=j) legend <- Rsq[1]
 
  print(panel.number)
  if (i == j) plot(NULL, xlim=c(0,1), ylim=c(0,1), xaxt = "n", yaxt = "n")
  if (i == j) legend("center", legend = centre, cex = 2, bty = "n")
  if (i < j) plot(NULL, xlim=c(0,1), ylim=c(0,1), xaxt = "n", yaxt = "n")
  if (i < j) legend("center", legend = pearsons.r, cex = 2, bty = "n")
  if (i > j) plot(var1 ~ var2, data = df, yaxt = ifelse(panel.number %in% c(1, 8, 15, 22, 29, 36, 43), "s", "n"), xaxt = ifelse(panel.number %in% c(43:49), "s", "n"), cex= 0.5, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1))

  if (i > j) abline(fit, lty = ifelse(significant.effect, 1, 2))
  if (panel.number %in% c(1, 8, 15, 22, 29, 36, 43)) mtext(side = 2, line = 2.5, text = eval(parse(text = yaxis)), outer = F, cex = 0.75)
  if (panel.number %in% c(43:49)) mtext(side = 1, line = 2.5,  text = eval(parse(text = xaxis)), outer = F, cex = 0.75)
  # 
  panel.number <- panel.number +1
  
  }}

dev.off()

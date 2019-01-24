######################################################
# Purpose: Statistical analysis to explore global trends in forest Productivity
# Developped by: Valentine Herrmann - HerrmannV@si.edu in Arpil 2018
#  R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/becky/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(graphics)

# Load data ####
ForestGEO_ANPP <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ForestGEO_ANPP.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


# Prepare data ####

## change to numerics what it needs to
ForestGEO_ANPP$min.dbh <- as.numeric(ForestGEO_ANPP$min.dbh)

## take absolute value of latitude
ForestGEO_ANPP$LAT <- abs(ForestGEO_ANPP$LAT)



# Control for some factors ####

## give leaf type
#broadleaf_codes <- c("2TEB", "2TDB", "2TB")
#needleleaf_codes <- c("2TEN", "2TDN", "2TN")
#mixed <- c("2TD", "2TE", "2TM", "2TREE")

#ForC_simplified$leaf.type <- ifelse(ForC_simplified$dominant.veg %in% broadleaf_codes, "broadleaf",
#                                    ifelse(ForC_simplified$dominant.veg %in% needleleaf_codes, "needleleaf",
#                                          ifelse(ForC_simplified$dominant.veg %in% mixed, "mixed", "Other")))

## give leaf phenology
#evergreen_codes <- c("2TE", "2TEB", "2TEN")
#deciduous_codes <- c("2TDN", "2TDB", "2TD")


#ForC_simplified$leaf.phenology <- ifelse(ForC_simplified$dominant.veg %in% evergreen_codes, "evergreen",
#                                         ifelse(ForC_simplified$dominant.veg %in% deciduous_codes, "deciduous", "Other"))



## fixed variables list ####
fixed.variables <- c("MAT", "MAP", "LAT")

## prepare results table

all.results <- NULL


# Run analysis an plot ####

### mature forests only ####

min.dbhs <- c("1cm", "10cm")
#min.dbh = "1cm"
#fixed.v = "lat"
df <- ForestGEO_ANPP

for(min.dbh in min.dbhs){
  
  df <- ForestGEO_ANPP
  
  if(min.dbh %in% "1cm") df$mean <- ForestGEO_ANPP$ANPP_total_1cm
  if(min.dbh %in% "10cm") df$mean <- ForestGEO_ANPP$ANPP_total_10cm
  

for(fixed.v in fixed.variables){
  
  df$fixed <- df[, fixed.v]
    
    tiff(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/Forest_GEO/Effect_of_", fixed.v, "_min_dbh_", min.dbh, ".tiff"), width = 2255, height = 2000, units = "px", res = 300)
    
  # model
  
  mod <-  lmer(mean ~ 1 + (1|site), data = df)
  
  mod.full <- lmer(mean ~ fixed + (1|site), data = df)
  
  significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
  
  
  # plot 
  
    title <- paste("Effect of", fixed.v, "dbh >", min.dbh)
    plot(mean ~ fixed, data = df, main = title, xlab = fixed.v, ylab = expression("ANPP Mg C"~ha^-1~yr^-1), xaxt = "n", yaxt = "n")
    abline(fixef(mod.full), lty = ifelse(significant.effect, 1, 2))
    axis(1, labels = TRUE)
    axis(2, labels = TRUE)
    #title(paste("Effect of", fixed.v, "dbh >", min.dbh), outer = T, line = 1)
    #mtext(text = fixed.v, side = 1, line = 3, outer = T)
    #mtext(text = expression("ANPP Mg C"~ha^-1~yr^-1), side = 2, line = 3,  , outer = T)

  results <- data.frame(fixed = fixed.v, significant = significant.effect, mindbh = min.dbh)
  
  all.results <- rbind(all.results, results)
  
  dev.off()
} 
}        
          

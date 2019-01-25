rm(list = ls())

setwd("C:/Users/becky/Dropbox (Smithsonian)/GitHub/ForC")

###code below extracts CTFS sites from ForC_simplified if required

#ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)
#ForestGEO_ANPP <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ForestGEO_ANPP.csv", stringsAsFactors = F)


#CTFS_plots <- ForC_simplified[grepl("CTFS", c(ForC_simplified$sites.sitename, ForC_simplified$plot.name), perl = T, ignore.case = T),]
#CTFS_plots_ANPP <- CTFS_plots[CTFS_plots$variable.name == "ANPP_woody_stem",]

#write.csv(CTFS_plots_ANPP, file = "C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/CTFS_plots_ANPP.csv")


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/becky/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(graphics)

# Load data ####
ForestGEO_ANPP <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/CTFS_plots_ANPP_woody.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


# Prepare data ####

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


#min.dbh = "1cm"
#fixed.v = "lat"
df <- ForestGEO_ANPP

  
  df <- ForestGEO_ANPP
  
  df$mean <- ForestGEO_ANPP$ANPP_total_1cm
  
  
  for(fixed.v in fixed.variables){
    
    df$fixed <- df[, fixed.v]
    
    tiff(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/Forest_GEO/Effect_of_", fixed.v, "_FGEO_ForC.tiff"), width = 2255, height = 2000, units = "px", res = 300)
    
    # model
    
    mod <-  lmer(mean ~ 1 + (1|site), data = df)
    
    mod.full <- lmer(mean ~ fixed + (1|site), data = df)
    
    significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
    
    
    # plot 
    
    title <- paste("Effect of", fixed.v)
    plot(mean ~ fixed, data = df, main = title, xlab = fixed.v, ylab = expression("ANPP Mg C"~ha^-1~yr^-1), xaxt = "n", yaxt = "n")
    abline(fixef(mod.full), lty = ifelse(significant.effect, 1, 2))
    axis(1, labels = TRUE)
    axis(2, labels = TRUE)
    #title(paste("Effect of", fixed.v, "dbh >", min.dbh), outer = T, line = 1)
    #mtext(text = fixed.v, side = 1, line = 3, outer = T)
    #mtext(text = expression("ANPP Mg C"~ha^-1~yr^-1), side = 2, line = 3,  , outer = T)
    
    results <- data.frame(fixed = fixed.v, significant = significant.effect)
    
    all.results <- rbind(all.results, results)
    
    dev.off()
  } 
   


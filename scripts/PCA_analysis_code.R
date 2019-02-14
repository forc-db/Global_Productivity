mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

summary(mtcars.pca)
######################################################
# Purpose: Statistical analysis to explore global trends in forest Productivity
# Developped by: Valentine Herrmann - HerrmannV@si.edu in Arpil 2018
#  R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(devtools)
library(MuMIn)

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_WorldClim_CRU.csv", stringsAsFactors = F)
ANPP <- read.csv("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/ANPP_woody_and_canopy_WorldClim_CRU.csv", stringsAsFactors = F)
ForC_simplified <- ForC_simplified[,-c(43:46, 51:54)]
ForC_simplified <- ForC_simplified[,-c(38, 40:41, 44:45)]
ForC_simplified <- ForC_simplified[,-c(43)]
VARIABLES <- read.csv(paste0(dirname(getwd()), "/ForC/data/ForC_variables.csv"), stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


# Prepare data ####

## change to numerics what it needs to
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)
ForC_simplified$min.db <- as.numeric(ForC_simplified$min.db)

## make plot and geographic areas factors
ForC_simplified$plot.name <- addNA(ForC_simplified$plot.name)
ForC_simplified$geographic.area <- addNA(ForC_simplified$geographic.area)

## change sign of NEE 
ForC_simplified[grepl("NEE", ForC_simplified$variable.name,ignore.case = F),]$mean <- -ForC_simplified[grepl("NEE", ForC_simplified$variable.name,ignore.case = F),]$mean

## take absolute value of latitude
ForC_simplified$lat <- abs(ForC_simplified$lat)


# Control for some factors ####


## keep all ages except na, 0 and 999
ages.not.999.nor.0.nor.na <- !ForC_simplified$stand.age %in% 999 &  !ForC_simplified$stand.age %in% 0 & !is.na(ForC_simplified$stand.age)

## Keep only age >=100 (or 999)
age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
age.greater.than.200 <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
ages <- c("age.greater.than.100", "age.greater.than.200")

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0

## keep only records with min.dbh <= 10cm
min.dbh.to.keep <- ForC_simplified$min.dbh <= 10 ##& !is.na(ForC_simplified$min.dbh) # this doesn work great, not enough data, but maybe we can just keep the NAs?
min.dbh.to.keep <- rep(TRUE, nrow(ForC_simplified))

## give leaf type
broadleaf_codes <- c("2TEB", "2TDB", "2TB")
needleleaf_codes <- c("2TEN", "2TDN", "2TN")
mixed <- c("2TD", "2TE", "2TM", "2TREE")

ForC_simplified$leaf.type <- ifelse(ForC_simplified$dominant.veg %in% broadleaf_codes, "broadleaf",
                                    ifelse(ForC_simplified$dominant.veg %in% needleleaf_codes, "needleleaf",
                                           ifelse(ForC_simplified$dominant.veg %in% mixed, "mixed", "Other")))

## give leaf phenology
evergreen_codes <- c("2TE", "2TEB", "2TEN")
deciduous_codes <- c("2TDN", "2TDB", "2TD")


ForC_simplified$leaf.phenology <- ifelse(ForC_simplified$dominant.veg %in% evergreen_codes, "evergreen",
                                         ifelse(ForC_simplified$dominant.veg %in% deciduous_codes, "deciduous", "Other"))


# Prepare some variables ####

## response variable list (fluxes) ####
all.response.variables <- VARIABLES[c(4, 7:18, 25:32, 37:38, 51:52),]$variable.name
all.response.variables <- gsub("(_OM|_C)", "", all.response.variables)
all.response.variables <- all.response.variables[all.response.variables %in% ForC_simplified$variable.name]
all.response.variables <- unique(gsub("_\\d", "", all.response.variables))

response.variables.groups <- list(c("GPP", "NPP", "ANPP"),
                                  c("ANPP_foliage", "ANPP_woody", "BNPP_root"))



all.response.variables[!all.response.variables %in% unlist(response.variables.groups)]

## fixed variables list ####
fixed.variables <- c("mat", "map", "lat", "stand.age", "leaf.type", "leaf.phenology", "AnnualMeanTemp", "MeanDiurnalRange", "TempSeasonality", "MaxTWarmestMonth", "MinTColdestMonth", "TempRangeAnnual", "MeanTWetQ", "MeanTDryQ", "MeanTWarmQ", "MeanTColdQ","AnnualPre", "PreWetMonth", "PreDryMonth", "PreSeasonality", "PreWetQ", "PreDryQ", "PreWarmQ", "PreColdQ", "CloudCover", "AnnualFrostDays", "AnnualPET", "AnnualWetDays")

## prepare results table

all.results <- NULL
ForC_simplified$variable.name <- gsub("(_OM|_C)", "", ForC_simplified$variable.name)
ForC_simplified$variable.name <- gsub("(_0|_1|_2)", "", ForC_simplified$variable.name)
all.response.variables <- gsub("(_OM|_C)", "", all.response.variables)

ForC_simplified <- ForC_simplified[dist.to.keep & min.dbh.to.keep, ]
ForC_simplified <- ForC_simplified[ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age),]
ForC_simplified <- ForC_simplified[!ForC_simplified$sites.sitename %in% "Paracou B", ]
ForC_simplified$leaf.type.phenology <- NA
ForC_simplified$leaf.type.phenology <- paste(ForC_simplified$leaf.type, ForC_simplified$leaf.phenology, sep = "_")

for(response.v in all.response.variables){
print(response.v)
df <- ForC_simplified[ForC_simplified$variable.name %in% response.v,]
leaf.type <- df$leaf.type
leaf.phenology <- df$leaf.phenology
leaf.type.phenology <- df$leaf.type.phenology

df_pca <- prcomp(df[, c(36:44)], center = TRUE,scale. = TRUE)
for (n in 2){
tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/pca/pca_biplot_", response.v, "_", n, ".tiff"), width = 2255, height = 2000, units = "px", res = 300)
print(ggbiplot::ggbiplot(df_pca, choices = (n-1):(n), groups = leaf.type.phenology) + ggtitle(paste0("PCA plot for ", response.v)) + scale_x_continuous(expand = c(.3, .3)) + scale_y_continuous(expand = c(.3, .3)))
dev.off()
}
tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/pca/pca_screeplot_", response.v, ".tiff"))
print(ggbiplot::ggscreeplot(df_pca, type = c("cev")) + ggtitle(paste0("PCA scree plot for ", response.v)))
dev.off()

pca <- abs(df_pca$rotation)
pca <- as.data.frame(sweep(pca, 2, colSums(pca), "/")[, c(1:5)])

write.csv(pca, file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/pca/pca_", response.v, ".csv"))

axes <- predict(df_pca, newdata = df)
df <- cbind(df, axes)

#for (n in 1:2){
  mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df)
  mod.full <-  lmer(mean ~ PC1 + (1|geographic.area/plot.name), data = df)
  significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
  significance <- signif(significance, digits=4)
  legend1 <- paste0("p-value = ", significance)
  
  Rsq <- as.data.frame(r.squaredGLMM(mod.full))
  Rsq <- signif(Rsq, digits=4)
  legend2 <- paste0("r-squared = ", Rsq[1])
  
  tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/pca/pca_regression_", response.v, "_PC1.tiff"), width = 2255, height = 2000, units = "px", res = 300)
  plot(mean ~ PC1, data = df,
       xlab = "PC1",
       ylab = paste0(response.v),
       main = paste0(response.v))
  abline(fixef(mod.full))
  legend("topleft", legend=c(legend1, legend2))
  dev.off()
#}

}


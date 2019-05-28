

# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(MuMIn)
library(ggplot2)
library(viridis)

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)
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
# ForC_simplified$lat <- abs(ForC_simplified$lat)


# Control for some factors ####


## keep all ages except na, 0 and 999
ages.not.999.nor.0.nor.na <- !ForC_simplified$stand.age %in% 999 &  !ForC_simplified$stand.age %in% 0 & !is.na(ForC_simplified$stand.age)

## Keep only age >=100 (or 999)
age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
age.greater.than.200 <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
ages <- c("age.greater.than.100")

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0
ForC_simplified <- ForC_simplified[dist.to.keep, ]

## keep only records with min.dbh <= 10cm
ForC_simplified$min.dbh <- as.numeric(ForC_simplified$min.dbh)
# min.dbh.to.keep <- ForC_simplified$min.dbh <= 10 & is.na(ForC_simplified$min.dbh) # this doesn work great, not enough data, but maybe we can just keep the NAs?
min.dbh.to.remove <- ForC_simplified$min.dbh >= 10 & !is.na(ForC_simplified$min.dbh)
ForC_simplified <- ForC_simplified[-min.dbh.to.remove, ]

###keep only altitude below 500 metres
# 
# alt.to.keep <- ForC_simplified$masl <= 500 & !is.na(ForC_simplified$masl)
# ForC_simplified <- ForC_simplified[alt.to.keep, ]

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

### exclude Tura due to extreme high latitude
ForC_simplified <- ForC_simplified[!(ForC_simplified$sites.sitename %in% "Tura"),]

# Prepare some variables ####

## response variable list (fluxes) ####
all.response.variables <- VARIABLES[c(4, 7:18, 25:32, 37:38, 51:52),]$variable.name
all.response.variables <- gsub("(_OM|_C)", "", all.response.variables)
all.response.variables <- gsub("(_0|_1|_2|_3|_4|_5)", "", all.response.variables)
all.response.variables <- all.response.variables[all.response.variables %in% ForC_simplified$variable.name]
all.response.variables <- unique(gsub("_\\d", "", all.response.variables))

response.variables.groups <- list(c("GPP", "NPP", "BNPP_root", "BNPP_root_fine"),
                                  c("ANPP_1", "ANPP_foliage", "ANPP_repro"),
                                  c("ANPP_woody", "ANPP_woody_stem", "ANPP_woody_branch"))

# response.variables.groups <- list(c("ANPP_1"))

all.response.variables[!all.response.variables %in% unlist(response.variables.groups)]

## fixed variables list ####
fixed.variables <- c("AnnualMeanTemp")

## prepare results table

all.results <- NULL

ForC_simplified$variable.name <- gsub("(_0|_1|_2)", "", ForC_simplified$variable.name)

ForC_simplified <- ForC_simplified[ForC_simplified$variable.name %in% c("GPP", "NPP", "BNPP_root", "BNPP_root_fine", "ANPP", "ANPP_foliage", "ANPP_woody", "ANPP_woody_stem"),]

fixed.v <- c("AnnualMeanTemp")
fixed.no.na <- !is.na(ForC_simplified[, fixed.v])
ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)

ForC_simplified <- ForC_simplified[ages.to.keep & fixed.no.na, ]

lon <- ForC_simplified$lon
lat <- ForC_simplified$lat
variable.name <- ForC_simplified$variable.name

png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/maps/distribution_all_samples.png"), width = 2255, height = 2000, units = "px", res = 300)

mapWorld <- borders("world", colour="gray50", fill="gray50")
mp <- ggplot(data = ForC_simplified) +   mapWorld
mp <- mp+ geom_point(aes(x=lon, y=lat, color= variable.name), size=1) + scale_colour_viridis(discrete = T, option = "plasma") + facet_wrap(vars(variable.name))
print(mp)

dev.off()

################# all plots on one

mapWorld <- borders("world", colour="gray50", fill="gray50")
mp <- ggplot() +   mapWorld
mp <- mp + geom_point(aes(x=lon, y=lat, shape = variable.name, color = variable.name), size=2) + scale_colour_viridis(discrete = T, option = "plasma") + scale_shape_manual(values = 1:8)
ggsave("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/maps/distribution_all_variables.png", plot = mp, width = 14, height = 7)





####################### all plots individually

# 
# 
# ### mature forests only ####
# for(response.variables in response.variables.groups){
#   
#   if(response.variables[1] == "GPP") n <- 1
#   if(response.variables[1] == "ANPP_1") n <- 2
#   if(response.variables[1] == "ANPP_woody") n <- 3
#   if(response.variables[1] == "woody.mortality_ag") n <- 4
#   
#   for (age in ages){
#     
#     if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
#     if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
#     
#     for(fixed.v in fixed.variables){
#       
#       # tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/test/best_model/Effect_of_", fixed.v, "_MATURE_only_", age, "_", n, ".tiff"), width = 2255, height = 2000, units = "px", res = 300)
#       
#       print(fixed.v)
#       
#       
#       
#       ###subset ForC
#       
#       
#       first.plot <- TRUE
#       
#       for (response.v in response.variables){
#         
#         if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1")
#         if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1")
#         if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
#         if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
#         
#         
#         rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
#         
#         fixed.no.na <- !is.na(ForC_simplified[, fixed.v])
#         
#         df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
#         
#         df$fixed <- df[, fixed.v]
#         
#         lon <- df$lon
#         lat <- df$lat
#         # 
#         tiff(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/maps/distribution_", response.v, "_sample.tiff"), width = 2255, height = 2000, units = "px", res = 300)
#         
#         mapWorld <- borders("world", colour="gray50", fill="gray50")
#         mp <- ggplot() +   mapWorld
#         mp <- mp+ geom_point(aes(x=lon, y=lat, color= variable.name), size=1) + ggtitle(paste0(response.v))
#         print(mp)
#         
#         dev.off()
#         
#       }
#       
#     }
#     
#   }
# }
# 
# 



# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity")

# Load libaries ####
library(lme4)
library(MuMIn)
library(ggplot2)
library(viridis)
library(maps)

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified_WorldClim_CRU_refined.csv", stringsAsFactors = F)


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
ages <- c("age.greater.than.100")

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0
ForC_simplified <- ForC_simplified[dist.to.keep, ]

## keep only records with min.dbh <= 10cm
ForC_simplified$min.dbh <- as.numeric(ForC_simplified$min.dbh)
# min.dbh.to.keep <- ForC_simplified$min.dbh <= 10 & is.na(ForC_simplified$min.dbh) # this doesn work great, not enough data, but maybe we can just keep the NAs?
min.dbh.to.remove <- ForC_simplified$min.dbh >= 10 & !is.na(ForC_simplified$min.dbh)
ForC_simplified <- ForC_simplified[-min.dbh.to.remove, ]

### exclude Tura due to extreme high latitude
ForC_simplified <- ForC_simplified[!(ForC_simplified$sites.sitename %in% "Tura"),]

# Prepare some variables ####

ForC_simplified$variable.name[ForC_simplified$variable.name %in% "BNPP_root"] <- "BNPP"
ForC_simplified$variable.name[ForC_simplified$variable.name %in% "ANPP_foliage"] <- "ANPP foliage"
ForC_simplified$variable.name[ForC_simplified$variable.name %in% "ANPP_woody_stem"] <- "ANPP stem"
ForC_simplified$variable.name[ForC_simplified$variable.name %in% "BNPP_root_fine"] <- "BNPP fine root"
ForC_simplified$variable.name[ForC_simplified$variable.name %in% "R_auto"] <- "R auto"
ForC_simplified$variable.name[ForC_simplified$variable.name %in% "R_auto_root"] <- "R root"

response.variables.groups <- list(c("GPP", "NPP", "BNPP root", "BNPP_root_fine"),
                                  c("ANPP", "ANPP_foliage", "ANPP_repro"),
                                  c("ANPP_woody", "ANPP_woody_stem", "ANPP_woody_branch"))

# response.variables.groups <- list(c("ANPP_1"))

all.response.variables[!all.response.variables %in% unlist(response.variables.groups)]

## fixed variables list ####
fixed.variables <- c("AnnualMeanTemp")

## prepare results table

all.results <- NULL

ForC_simplified$variable.name <- gsub("(_0|_1|_2|_3|_4|_5)", "", ForC_simplified$variable.name)

ForC_simplified <- ForC_simplified[ForC_simplified$variable.name %in% c("GPP", "NPP", "ANPP", "ANPP stem", "ANPP foliage", "BNPP", "BNPP fine root", "R auto", "R root"),]

fixed.v <- c("AnnualMeanTemp")
fixed.no.na <- !is.na(ForC_simplified[, fixed.v])
ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)

ForC_simplified <- ForC_simplified[ages.to.keep & fixed.no.na, ]

lon <- ForC_simplified$lon
lat <- ForC_simplified$lat

col.sym <- read.csv("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)

ForC_simplified <- merge(ForC_simplified, col.sym[, c("maps.name", "col", "sym")], by.x = "variable.name", by.y = "maps.name")
ForC_simplified$col <- as.character(ForC_simplified$col)
ForC_simplified$sym <- as.character(ForC_simplified$sym)

ForC_simplified$variable.name <- factor(ForC_simplified$variable.name, levels = c("GPP", "NPP", "ANPP", "ANPP stem", "ANPP foliage", "BNPP", "BNPP fine root", "R auto", "R root"))

png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/maps/distribution_all_samples.png"), width = 2000, height = 3000, units = "px", res = 300)

mapWorld <- borders("world", colour="gray50", fill="gray50")

mp <- ggplot(data = ForC_simplified) + mapWorld + aes(x=lon, y=lat)
mp <- mp+ geom_point(aes(color = variable.name, shape = variable.name), size=1) + facet_wrap(vars(variable.name), ncol = 2) + theme(legend.position = "none")
mp <- mp + scale_color_manual(values = plasma(10)[c(1,3,5,7,9,8,6,4,2)]) + scale_shape_manual(values = c(1:9)[c(1,3,5,7,9,8,6,4,2)])  + labs(color = "Variable", shape = "Variable")
                  
print(mp)

dev.off()

################# all plots on one

mapWorld <- borders("world", colour="gray50", fill="gray50")
mp <- ggplot(data = ForC_simplified) +   mapWorld
mp <- mp + geom_point(aes(x=lon, y=lat, shape = variable.name, color = variable.name), size=2) + scale_color_manual(values = plasma(10)[c(1,3,5,7,9,8,6,4,2)]) + scale_shape_manual(values = c(1:9)[c(1,3,5,7,9,8,6,4,2)]) + labs(color = "Variable", shape = "Variable") + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.35), legend.text = element_text(size = 12))
ggsave("C:/Users/gyrcbm/Documents/GitHub/Global_Productivity/results/figures/final_figures/maps/distribution_all_variables.jpg", plot = mp, width = 14, height = 7)





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
#       # tiff(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/test/best_model/Effect_of_", fixed.v, "_MATURE_only_", age, "_", n, ".tiff"), width = 2255, height = 2000, units = "px", res = 300)
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
#         tiff(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/maps/distribution_", response.v, "_sample.tiff"), width = 2255, height = 2000, units = "px", res = 300)
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


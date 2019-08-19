

# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(MuMIn)
library(AICcmodavg)
library(piecewiseSEM)

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
ForC_simplified$lat <- abs(ForC_simplified$lat)

ForC_simplified$SolarRadiation <- ForC_simplified$SolarRadiation/1000


# Control for some factors ####


## keep all ages except na, 0 and 999
ages.not.999.nor.0.nor.na <- !ForC_simplified$stand.age %in% 999 &  !ForC_simplified$stand.age %in% 0 & !is.na(ForC_simplified$stand.age)

## Keep only age >=100 (or 999)
age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
age.greater.than.200 <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
ages <- c("age.greater.than.100")

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0

## keep only records with min.dbh <= 10cm
min.dbh.to.keep <- ForC_simplified$min.dbh <= 10 ##& !is.na(ForC_simplified$min.dbh) # this doesn work great, not enough data, but maybe we can just keep the NAs?
min.dbh.to.keep <- rep(TRUE, nrow(ForC_simplified))

ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)


fixed.no.na <- !is.na(ForC_simplified[, "map"]) & !is.na(ForC_simplified[, "masl"]) & !is.na(ForC_simplified[, "mat"]) & !is.na(ForC_simplified[, "WaterStressMonths"])


ForC_simplified <- ForC_simplified[dist.to.keep & fixed.no.na & ages.to.keep, ]

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


all.response.variables[!all.response.variables %in% unlist(response.variables.groups)]


## prepare results table

all.results <- NULL
best.results <- NULL

effects <- c("mat", "length_growing_season", "map", "Aridity", "PotentialEvapotranspiration", "VapourPressureDeficit", "SolarRadiation", "PreSeasonality", "MaxVPD", "WaterStressMonths", "(1|geographic.area/plot.name)")


response.variables <- c("GPP", "NPP", "BNPP_root", "BNPP_root_fine", "ANPP", "ANPP_foliage", "ANPP_woody_stem","R_auto", "R_auto_root")
        
        for (response.v in response.variables){
          
          if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1")
          if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1")
          if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
          if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
          
          
          rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
          
          df <- ForC_simplified[rows.with.response, ]

          df$masl <- (df$masl/1000)
          
          #create all combinations of random / fixed effects
          
          response <- "mean"
          
          uni_comb <-
            unlist( sapply( seq_len(length(1)), 
                            function(x) {
                              apply( combn(effects, 2), 2, function(x) paste(x, collapse = "+"))
                            }))
          
          uni_comb <- expand.grid(response, uni_comb) 
          uni_comb <- uni_comb[grepl("1", uni_comb$Var2), ] #keep random effect
          uni_comb <- uni_comb[grepl("mat", uni_comb$Var2), ] #keep mat
        
          
          add_comb <- 
            unlist( sapply( seq_len(length(1)), 
                            function(x) {
                              apply( combn(effects, 3), 2, function(x) paste(x, collapse = "+"))
                            }))
         
          
          add_comb <- expand.grid(response, add_comb) 
          add_comb <- add_comb[grepl("1", add_comb$Var2), ] #keep random effect
          add_comb <- add_comb[grepl("mat", add_comb$Var2), ] #keep mat
          
          int_comb <-
            unlist( sapply( seq_len(length(1)), 
                            function(x) {
                              apply( combn(effects, 2), 2, function(x) paste(x, collapse = "*"))
                            }))
          int_comb <- expand.grid(response, int_comb) 
          int_comb <- int_comb[!(grepl("1", int_comb$Var2)), ]
          int_comb <- int_comb[!(grepl("masl", int_comb$Var2)), ]
          
          int_comb <- as.character(int_comb$Var2)
          
          int_comb <- append(int_comb, c("(1|geographic.area/plot.name)"))
          
          int_comb <-
            unlist( sapply( seq_len(length(1)), 
                            function(x) {
                              apply( combn(int_comb, 2), 2, function(x) paste(x, collapse = "+"))
                            }))
          
          int_comb <- expand.grid(response, int_comb) 
          int_comb <- int_comb[grepl("1", int_comb$Var2), ] #keep random effect
          int_comb <- int_comb[grepl("mat", int_comb$Var2), ] #keep mat
          
          var_comb <- rbind(uni_comb, add_comb, int_comb)
          
          uni_comb <-
            unlist( sapply( seq_len(length(1)), 
                            function(x) {
                              apply( combn(effects, 2), 2, function(x) paste(x, collapse = "+"))
                            }))
          
          uni_comb <- expand.grid(response, uni_comb) 
          uni_comb <- uni_comb[grepl("1", uni_comb$Var2), ] #keep random effect
          uni_comb <- uni_comb[grepl("length_growing_season", uni_comb$Var2), ] #keep mat
          
          
          add_comb <- 
            unlist( sapply( seq_len(length(1)), 
                            function(x) {
                              apply( combn(effects, 3), 2, function(x) paste(x, collapse = "+"))
                            }))
          
          
          add_comb <- expand.grid(response, add_comb) 
          add_comb <- add_comb[grepl("1", add_comb$Var2), ] #keep random effect
          add_comb <- add_comb[grepl("length_growing_season", add_comb$Var2), ] #keep mat
          
          int_comb <-
            unlist( sapply( seq_len(length(1)), 
                            function(x) {
                              apply( combn(effects, 2), 2, function(x) paste(x, collapse = "*"))
                            }))
          int_comb <- expand.grid(response, int_comb) 
          int_comb <- int_comb[!(grepl("1", int_comb$Var2)), ]
          int_comb <- int_comb[!(grepl("masl", int_comb$Var2)), ]
          
          int_comb <- as.character(int_comb$Var2)
          
          int_comb <- append(int_comb, c("(1|geographic.area/plot.name)"))
          
          int_comb <-
            unlist( sapply( seq_len(length(1)), 
                            function(x) {
                              apply( combn(int_comb, 2), 2, function(x) paste(x, collapse = "+"))
                            }))
          
          int_comb <- expand.grid(response, int_comb) 
          int_comb <- int_comb[grepl("1", int_comb$Var2), ] #keep random effect
          int_comb <- int_comb[grepl("length_growing_season", int_comb$Var2), ] #keep mat
          
          var_comb <- rbind(var_comb, uni_comb, add_comb, int_comb)
          
          formula_vec <- sprintf("%s ~ %s", var_comb$Var1, var_comb$Var2)
          
          # create list of model outputs
          lmm_all <- lapply(formula_vec, function(x){
            fit1 <- lmer(x, data = df, REML=FALSE)
            return(fit1)
          })
          names(lmm_all) <- formula_vec
          
          var_aic <- aictab(lmm_all, sort = FALSE) #rank based on AICc
          rsq <- rsquared(lmm_all)
          
          output <- cbind(var_aic, rsq)
          
          output <- output[order(output$Delta_AICc),]
          output$Modnames <- as.character(output$Modnames)
          output$response.variable <- response.v
          mat_map <- output[output$Modnames %in% "mean ~ mat*map+(1|geographic.area/plot.name)",]
          
          results <- output[1:8, c(1, 3, 4, 12, 13, 14)]
          results <- rbind(results, mat_map[, c(1, 3, 4, 12, 13, 14)])
          best.result <- output[1, c(1, 3, 4, 12, 13, 14)]
          
          
          all.results <- all.results <- rbind(all.results, results)
          best.results <- best.results <- rbind(best.results, best.result)
          
          
        }

write.csv(all.results, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/best_model_outputs/AIC_multivariate_interactions_table.csv", row.names = F)

all.results = NULL

for(response.v in response.variables){
  
  if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2")
  if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
  if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
  
  
  rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
  
  df <- ForC_simplified[rows.with.response, ]
  
  df$masl <- (df$masl/1000)
  
  mod <- best.results[best.results$response.variable %in% response.v,]$Modnames
  mod.full <- lmer(mod, data = df, REML = T)
  
  mod.null <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = T)
  
  significant.effect <- anova(mod.null, mod.full)$"Pr(>Chisq)"[2] < 0.05
  significance <- anova(mod.null, mod.full)$"Pr(>Chisq)"[2]
  sample.size <- length(df$mean)
  
  r <- round(fixef(mod.full), 4)
  fixed1.coef <- r[2]
  fixed2.coef <- r[3]
  int.coef <- r[5]
  
  significance <- anova(mod.null, mod.full)$"Pr(>Chisq)"[2]
  significance <- signif(significance, digits=4)
  
  Rsq <- as.data.frame(r.squaredGLMM(mod.full))
  Rsq <- signif(Rsq, digits=4)
  
  results <- data.frame(response = response.v, mod = mod, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq, fixed1.coef = fixed1.coef, fixed2.coef = fixed2.coef, int.coef = int.coef)
  
  all.results <- all.results <- rbind(all.results, results)
}

write.csv(all.results, file = "C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/best_model_outputs/AIC_multivariate_interactions_best.csv", row.names = F)

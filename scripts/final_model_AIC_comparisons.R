

# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("")

# Load libaries ####
library(lme4)
library(MuMIn)
library(AICcmodavg)
library(piecewiseSEM)

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
ForC_simplified$lat <- abs(ForC_simplified$lat)

ForC_simplified$SolarRadiation <- ForC_simplified$SolarRadiation/1000


# Control for some factors ####

## Keep only age >=100 (or 999)
age.greater.than.100 <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
ages <- c("age.greater.than.100")

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0

ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
fixed.no.na <- !is.na(ForC_simplified[, "map"]) & !is.na(ForC_simplified[, "masl"]) & !is.na(ForC_simplified[, "mat"])
ForC_simplified <- ForC_simplified[dist.to.keep & fixed.no.na & ages.to.keep, ]


### exclude Tura due to extreme high latitude
ForC_simplified <- ForC_simplified[!(ForC_simplified$sites.sitename %in% "Tura"),]

## prepare results table

all.results <- NULL
best.results <- NULL

response.variables <- c("GPP", "NPP", "BNPP_root", "BNPP_root_fine", "ANPP", "ANPP_foliage", "ANPP_woody_stem","R_auto", "R_auto_root")

effects <- c("poly(mat, 1, raw = T)", "poly(map, 1, raw = T)", "poly(lat, 1, raw = T)", "poly(TempSeasonality, 1, raw = T)", "poly(TempRangeAnnual, 1, raw = T)", "poly(AnnualFrostDays, 1, raw = T)", "poly(AnnualWetDays, 1, raw = T)", "poly(Aridity, 1, raw = T)", "poly(PotentialEvapotranspiration, 1, raw = T)", "poly(VapourPressureDeficit, 1, raw = T)", "poly(SolarRadiation, 1, raw = T)", "poly(PreSeasonality, 1, raw = T)", "poly(mat, 2, raw = T)", "poly(map, 2, raw = T)", "poly(lat, 2, raw = T)", "poly(TempSeasonality, 2, raw = T)", "poly(TempRangeAnnual, 2, raw = T)", "poly(AnnualFrostDays, 2, raw = T)", "poly(AnnualWetDays, 2, raw = T)", "poly(Aridity, 2, raw = T)", "poly(PotentialEvapotranspiration, 2, raw = T)", "poly(VapourPressureDeficit, 2, raw = T)", "poly(SolarRadiation, 2, raw = T)", "poly(PreSeasonality, 2, raw = T)", "(1|geographic.area/plot.name)")

for (response.v in response.variables){
  
  if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1")
  if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_litterfall") responses.to.keep  <- c("ANPP_litterfall_1")
  if(!response.v %in% c("NPP", "ANPP", "ANPP_litterfall")) responses.to.keep  <- response.v
  
  
  rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
  
  df <- ForC_simplified[rows.with.response, ]
  
  df$masl <- (df$masl/1000)
  
  #create all combinations of random / fixed effects
  
  response <- "mean"
  
  comb <-
    unlist( sapply( seq_len(length(1)), 
                    function(x) {
                      apply( combn(effects, 2), 2, function(x) paste(x, collapse = "+"))
                    }))
  
  comb <- expand.grid(response, comb) 
  comb <- comb[grepl("geographic", comb$Var2), ] #keep random effect
  
  null <- "1 + (1|geographic.area/plot.name)"
  null <- expand.grid(response, null)
  
  var_comb <- rbind(comb, null)
  
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
  
  results <- output[1:8, c(1, 3, 4, 12, 13, 14)]
  best.result <- output[1, c(1, 3, 4, 12, 13, 14)]
  
  
  all.results <- all.results <- rbind(all.results, results)
  best.results <- best.results <- rbind(best.results, best.result)
  
  
}

write.csv(all.results, file = "AIC_table.csv", row.names = F)
######################################################
# Purpose: Statistical analysis to explore global trends in forest Productivity
# Inputs:
# - ForC_simplified table
# Outputs:
# - 
# Developped by: Valentine Herrmann - HerrmannV@si.edu in Arpil 2018
#  R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(lme4)

# Load data ####
ForC_simplified <- read.csv("raw.data/ForC_simplified.csv", stringsAsFactors = F)

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

# Control for some factors ####

## Keep only age >=100 (or 999)
age.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0

## keep only records with min.dbh <= 10cm
min.dbh.to.keep <- ForC_simplified$min.dbh <= 10 & !is.na(ForC_simplified$min.dbh)
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
## prepare results table

all.results <- NULL
# Run analysis ####

# Effect of MAT ####

for (response.v in c("GPP", "NPP", "ANPP", "ANPP_woody", "BNPP_root_fine")){ #, "ANPP_stem"
fixed.v = "MAT"

if(response.v %in% "GPP") variable.to.keep  <- response.v
if(response.v %in% "NPP") variable.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
if(response.v %in% "ANPP") variable.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
if(response.v %in% "ANPP_woody") variable.to.keep  <- response.v
if(response.v %in% "BNPP_root_fine") variable.to.keep  <- response.v

response.to.keep <- ForC_simplified$variable.name %in% variable.to.keep

fixed.to.keep <- !is.na(ForC_simplified$mat)

df <- ForC_simplified[response.to.keep & age.to.keep & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.to.keep, ]


tiff(paste0("figures/Effect_of_Mean_Annual_Temperature_on", response.v, ".tiff"), width = 2250, height = 1500, units = "px", res = 300)
plot(mean ~ mat, data = df, main = paste(response.v, "vs", fixed.v), ylab = response.v)

mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df)
mod.full <- lmer(mean ~ mat + (1|geographic.area/plot.name), data = df)
significant.effet <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
abline(fixef(mod.full), col = "red", lty = ifelse(significant.effet, 1, 2))


legend("topright", col = "red", lty = c(1,2), legend = c("significant effet", "non-significant effect"), bty = "n")

r <- round(fixef(mod.full), 2)
equation <-  paste(r[1], "+", fixed.v,  "x", r[2])

mtext(side = 3, line = -1, text = equation, adj = 0.1)

dev.off()
results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = ">=100 yrs", equation = equation, significant = significant.effet)

all.results <- rbind(all.results, results)
}

# Effect of MAT + AGE####

for (response.v in c("GPP", "NPP", "ANPP", "ANPP_woody")){
  fixed.v = "MAT + AGE"
  
  if(response.v %in% "GPP") variable.to.keep  <- response.v
  if(response.v %in% "NPP") variable.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
  if(response.v %in% "ANPP") variable.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_woody") variable.to.keep  <- response.v
  
  response.to.keep <- ForC_simplified$variable.name %in% variable.to.keep
  
  fixed.to.keep <- !is.na(ForC_simplified$mat) & !is.na(ForC_simplified$stand.age)
  
  df <- ForC_simplified[response.to.keep & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.to.keep, ]
  
  mod.full <- lmer(mean ~ mat * stand.age +(1|geographic.area/plot.name), data = df)
  significant.effet.interaction <- which.min(drop1(mod.full)$AIC) == 1
  
  if(!significant.effet.interaction) {
    results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = "none", equation = NA, significant = significant.effet.interaction)
    all.results <- rbind(all.results, results)
  }
  
  if( significant.effet.interaction) stop ("code here")
  mod.full <- lmer(mean ~ mat * stand.age +(1|geographic.area/plot.name), data = df)
  significant.effet <- which.min(drop1(mod.full)$AIC) == 1

  max.effect.v <- rownames(drop1(mod.full))[which.max(drop1(mod.full)$AIC)]
  other.v <- rownames(drop1(mod.full))[-which.max(drop1(mod.full)$AIC)][2]

  plot(df$mean ~ df$mat, main = paste(response.v, "vs", max.effect.v), ylab = response.v, cex = sqrt(df$stand.age)/10)

  
  newDat1 <- data.frame(mat = seq(min(df$mat), max(df$mat), length.out = 100),
                       stand.age = quantile(df$stand.age, 0.25))
  newDat2 <- data.frame(mat = seq(min(df$mat), max(df$mat), length.out = 100),
                        stand.age = quantile(df$stand.age, 0.5))
  newDat3 <-data.frame(mat = seq(min(df$mat), max(df$mat), length.out = 100),
                       stand.age = quantile(df$stand.age, 0.75))

  fit1 <- predict(mod.full, newDat1, re.form =  NA)
  fit2 <- predict(mod.full, newDat2, re.form =  NA)
  fit3 <- predict(mod.full, newDat3, re.form =  NA)
  
  lines(fit1~ newDat1$mat, lwd = 1)
  lines(fit2~ newDat1$mat, lwd = 2)
  lines(fit3~ newDat1$mat, lwd = 3)
  
  legend("topright", lwd = 1:3, legend = c("age 1st quartile", "age median", "age 3rd quartile"), bty = "n")

  r <- round(fixef(mod.full), 2)
  equation <-  paste(r[1], "+", fixed.v,  "x", r[2])

  mtext(side = 3, line = -1, text = equation, adj = 0.1)

  results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = ">=100 yrs", equation = equation, significant = significant.effet)

  all.results <- rbind(all.results, results)
}

# Effect of LAT ####

for (response.v in c("GPP", "NPP", "ANPP", "ANPP_woody")){
  fixed.v = "LAT"
  
  if(response.v %in% "GPP") variable.to.keep  <- response.v
  if(response.v %in% "NPP") variable.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
  if(response.v %in% "ANPP") variable.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_woody") variable.to.keep  <- response.v
  
  response.to.keep <- ForC_simplified$variable.name %in% variable.to.keep
  
  fixed.to.keep <- !is.na(ForC_simplified$lat)
  
  df <- ForC_simplified[response.to.keep & age.to.keep & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.to.keep, ]
  
  
  tiff(paste0("figures/Effect_of_Latitude_on", response.v, ".tiff"), width = 2250, height = 1500, units = "px", res = 300)
  
  plot(mean ~ abs(lat), data = df, main = paste(response.v, "vs", fixed.v), ylab = response.v)
  
  mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df)
  mod.full <- lmer(mean ~ lat + (1|geographic.area/plot.name), data = df)
  significant.effet <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
  abline(fixef(mod.full), col = "red", lty = ifelse(significant.effet, 1, 2))
  
  
  legend("topright", col = "red", lty = c(1,2), legend = c("significant effet", "non-significant effect"), bty = "n")
  
  r <- round(fixef(mod.full), 2)
  equation <-  paste(r[1], "+", fixed.v,  "x", r[2])
  
  mtext(side = 3, line = -1, text = equation, adj = 0.1)
  
  dev.off()
  results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = ">=100 yrs", equation = equation, significant = significant.effet)
  
  all.results <- rbind(all.results, results)
}

# Effect of LAT + AGE####

for (response.v in c("GPP", "NPP", "ANPP", "ANPP_woody")){
  fixed.v = "LAT + AGE"
  
  if(response.v %in% "GPP") variable.to.keep  <- response.v
  if(response.v %in% "NPP") variable.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
  if(response.v %in% "ANPP") variable.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_woody") variable.to.keep  <- response.v
  
  response.to.keep <- ForC_simplified$variable.name %in% variable.to.keep
  
  fixed.to.keep <- !is.na(ForC_simplified$lat) & !is.na(ForC_simplified$stand.age)
  
  df <- ForC_simplified[response.to.keep & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.to.keep, ]
  
  mod.full <- lmer(mean ~ lat * stand.age +(1|geographic.area/plot.name), data = df)
  significant.effet.interaction <- which.min(drop1(mod.full)$AIC) == 1
  
  if(!significant.effet.interaction) {
    results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = "none", equation = NA, significant = significant.effet.interaction)
    all.results <- rbind(all.results, results)
  }
  
  # if( significant.effet.interaction) stop ("code here")
  # mod.full <- lmer(mean ~ lat * stand.age +(1|geographic.area/plot.name), data = df)
  # significant.effet <- which.min(drop1(mod.full)$AIC) == 1
  # 
  # max.effect.v <- rownames(drop1(mod.full))[which.max(drop1(mod.full)$AIC)]
  # other.v <- rownames(drop1(mod.full))[-which.max(drop1(mod.full)$AIC)][2]
  # 
  # plot(df$mean ~ df[, max.effect.v], main = paste(response.v, "vs", max.effect.v), ylab = response.v)
  # 
  # newDat1 <- data.frame(max.effect.v = seq(min(df[, max.effect.v]), max(df[,max.effect.v]), length.out = 100),
  #                      other.v = quantile(df[, other.v], 0.25))
  # newDat2 <- data.frame(max.effect.v = seq(min(df[, max.effect.v]), max(df[,max.effect.v]), length.out = 100),
  #                       other.v = quantile(df[, other.v], 0.25))
  # 
  # abline(fixef(mod.full), col = "red", lty = ifelse(significant.effet, 1, 2))
  # 
  # 
  # legend("topright", col = "red", lty = c(1,2), legend = c("significant effet", "non-significant effect"), bty = "n")
  # 
  # r <- round(fixef(mod.full), 2)
  # equation <-  paste(r[1], "+", fixed.v,  "x", r[2])
  # 
  # mtext(side = 3, line = -1, text = equation, adj = 0.1)
  # 
  # results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = ">=100 yrs", equation = equation, significant = significant.effet)
  # 
  # all.results <- rbind(all.results, results)
}

# Effect of leaf.type ####

for (response.v in c("GPP", "NPP", "ANPP", "ANPP_woody")){
  fixed.v = "leaf type"
  
  if(response.v %in% "GPP") variable.to.keep  <- response.v
  if(response.v %in% "NPP") variable.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
  if(response.v %in% "ANPP") variable.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_woody") variable.to.keep  <- response.v
  
  response.to.keep <- ForC_simplified$variable.name %in% variable.to.keep
  
  fixed.to.keep <- !ForC_simplified$leaf.type %in% "other"
  
  df <- ForC_simplified[response.to.keep & age.to.keep & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.to.keep, ]
  
  tiff(paste0("figures/Effect_of_leaf_type_on", response.v, ".tiff"), width = 2250, height = 1500, units = "px", res = 300)
  
  
  boxplot(mean ~ leaf.type, data = df, main = paste(response.v, "vs", fixed.v), ylab = response.v)
  
  mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df)
  mod.full <- lmer(mean ~ leaf.type + (1|geographic.area/plot.name), data = df)
  significant.effet <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
  points(c(fixef(mod.full)[1], fixef(mod.full)[1] + fixef(mod.full)[c(2:3)]) ~ c(1:3), col = "red", pch = ifelse(significant.effet, 16, 1))
  
  legend("topright", col = "red", pch = c(16,1), legend = c("significant effet", "non-significant effect"), bty = "n")
  
  dev.off()
}

# Effect of AGE ####

for (response.v in c("GPP", "NPP", "ANPP", "ANPP_woody")){
  fixed.v = "AGE"
  
  if(response.v %in% "GPP") variable.to.keep  <- response.v
  if(response.v %in% "NPP") variable.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
  if(response.v %in% "ANPP") variable.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_woody") variable.to.keep  <- response.v
  
  response.to.keep <- ForC_simplified$variable.name %in% variable.to.keep
  
  fixed.to.keep <- !is.na(ForC_simplified$stand.age)
  
  df <- ForC_simplified[response.to.keep & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.to.keep, ]
  
  tiff(paste0("figures/Effect_of_stand_age_on", response.v, ".tiff"), width = 2250, height = 1500, units = "px", res = 300)
  
  
  plot(mean ~ stand.age, data = df, main = paste(response.v, "vs", fixed.v), ylab = response.v)
  
  mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df)
  mod.full <- lmer(mean ~ stand.age + (1|geographic.area/plot.name), data = df)
  significant.effet <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
  abline(fixef(mod.full), col = "red", lty = ifelse(significant.effet, 1, 2))
  
  
  legend("topright", col = "red", lty = c(1,2), legend = c("significant effet", "non-significant effect"), bty = "n")
  
  
  r <- round(fixef(mod.full), 2)
  equation <-  paste(r[1], "+", fixed.v,  "x", r[2])
  
  mtext(side = 3, line = -1, text = equation, adj = 0.1)
  
  dev.off()
  results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = "none", equation = equation, significant = significant.effet)
  
  all.results <- rbind(all.results, results)
}

# Effect of MAP ####

for (response.v in c("GPP", "NPP", "ANPP", "ANPP_woody")){
  fixed.v = "MAP"
  
  if(response.v %in% "GPP") variable.to.keep  <- response.v
  if(response.v %in% "NPP") variable.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
  if(response.v %in% "ANPP") variable.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_woody") variable.to.keep  <- response.v
  
  response.to.keep <- ForC_simplified$variable.name %in% variable.to.keep
  
  fixed.to.keep <- !is.na(ForC_simplified$map)
  
  df <- ForC_simplified[response.to.keep & age.to.keep & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.to.keep, ]
  
  tiff(paste0("figures/Effect_of_Mean_annual_Precipitation_on", response.v, ".tiff"), width = 2250, height = 1500, units = "px", res = 300)
  
  
  plot(mean ~ map, data = df, main = paste(response.v, "vs", fixed.v), ylab = response.v)
  
  mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df)
  mod.full <- lmer(mean ~ map + (1|geographic.area/plot.name), data = df)
  significant.effet <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
  abline(fixef(mod.full), col = "red", lty = ifelse(significant.effet, 1, 2))
  
  
  legend("topright", col = "red", lty = c(1,2), legend = c("significant effet", "non-significant effect"), bty = "n")
  
  
  
  r <- round(fixef(mod.full), 2)
  equation <-  paste(r[1], "+", fixed.v,  "x", r[2])
  
  mtext(side = 3, line = -1, text = equation, adj = 0.1)
  
  dev.off()
  
  results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = ">=100 yrs", equation = equation, significant = significant.effet)
  
  all.results <- rbind(all.results, results)
  
}

# Effect of leaf.phenology ####

for (response.v in c("GPP", "NPP", "ANPP", "ANPP_woody")){
  fixed.v = "leaf phenology"
  
  if(response.v %in% "GPP") variable.to.keep  <- response.v
  if(response.v %in% "NPP") variable.to.keep  <- c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5")
  if(response.v %in% "ANPP") variable.to.keep  <- c("ANPP_0", "ANPP_1", "ANPP_2")
  if(response.v %in% "ANPP_woody") variable.to.keep  <- response.v
  
  response.to.keep <- ForC_simplified$variable.name %in% variable.to.keep
  
  fixed.to.keep <- !ForC_simplified$leaf.phenology %in% "other"
  
  df <- ForC_simplified[response.to.keep & age.to.keep & dist.to.keep & min.dbh.to.keep & dist.to.keep & fixed.to.keep, ]
  
  tiff(paste0("figures/Effect_of_Leaf_phenologye_on", response.v, ".tiff"), width = 2250, height = 1500, units = "px", res = 300)
  
  
  boxplot(mean ~ leaf.phenology, data = df, main = paste(response.v, "vs", fixed.v), ylab = response.v)
  
  mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df)
  mod.full <- lmer(mean ~ leaf.phenology + (1|geographic.area/plot.name), data = df)
  significant.effet <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
  points(c(fixef(mod.full)[1], fixef(mod.full)[1] + fixef(mod.full)[c(2:3)]) ~ c(1:3), col = "red", pch = ifelse(significant.effet, 16, 1))
  
  legend("topright", col = "red", pch = c(16,1), legend = c("significant effet", "non-significant effect"), bty = "n")
  
  dev.off()
}

### look at results ####

all.results

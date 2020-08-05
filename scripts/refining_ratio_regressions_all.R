
# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/becky/Dropbox (Smithsonian)/GitHub/ForC")

# Load libaries ####
library(lme4)
library(MuMIn)
library(plyr)
library(merTools)
library(visreg)
library(r2glmm)
library(nlme)
library(viridis)
library(AICcmodavg)

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

ForC_simplified$site_plot <- paste0(ForC_simplified$sites.sitename," ", ForC_simplified$plot.name)
ForC_simplified$site_plot_year_cite <- paste0(ForC_simplified$sites.sitename," ", ForC_simplified$plot.name, " ", ForC_simplified$stand.age, " ", ForC_simplified$citation.ID)

# Control for some factors ####


## keep all ages except na, 0 and 999
ages.not.999.nor.0.nor.na <- !ForC_simplified$stand.age %in% 999 &  !ForC_simplified$stand.age %in% 0 & !is.na(ForC_simplified$stand.age)

## Keep only age >=100 (or 999)
ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
ForC_simplified <- ForC_simplified[ages.to.keep, ]

## keep only stands that are NOT too strongly influence by management/ disturbance

dist.to.keep <- ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0
ForC_simplified <- ForC_simplified[dist.to.keep, ]

alt.to.keep <- ForC_simplified$masl <= 1000 & !is.na(ForC_simplified$masl)
ForC_simplified <- ForC_simplified[alt.to.keep, ]

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

all.results <- NULL
all.aictab <- NULL

ForC_simplified_base <- ForC_simplified[ForC_simplified$variable.name %in% c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0","GPP", "R_auto"),]

GPP <- ForC_simplified_base[ForC_simplified_base$variable.name %in% c("GPP"),]

NPP <- ForC_simplified_base[ForC_simplified_base$variable.name %in% c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0"),]
NPP <- NPP %>%
  dplyr::mutate_at("variable.name", factor, levels = c("NPP_0", "NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5")) %>%
  arrange(desc(variable.name)) %>%
  dplyr::group_by(site_plot_year_cite) %>%
  dplyr::filter(variable.name == variable.name[1]) %>%
  dplyr::ungroup()

R_auto <- ForC_simplified_base[ForC_simplified_base$variable.name %in% c("R_auto"),]

merge1 <- merge(GPP[, c("plot.name", "variable.name", "mean", "site_plot_year_cite", "lat", "mat", "map", "TempSeasonality", "geographic.area", "masl")], NPP[, c("plot.name", "variable.name", "mean", "site_plot_year_cite", "lat", "mat", "map", "TempSeasonality", "geographic.area", "masl")], by= c("plot.name", "site_plot_year_cite"), all.x = TRUE, all.y = TRUE)
merge2 <- merge(merge1, R_auto[, c("variable.name", "mean", "site_plot_year_cite")], by= c("site_plot_year_cite"), all = TRUE)

names(merge2)[c(3:4, 11:12, 19:20)] <- c("GPP", "GPP_mean", "NPP", "NPP_mean", "R_auto", "R_mean")

merge2$NPP_mean[is.na(merge2$NPP_mean)] <- (merge2$GPP_mean - merge2$R_mean)[is.na(merge2$NPP_mean)]
merge2$R_mean[is.na(merge2$R_mean)] <- (merge2$GPP_mean - merge2$NPP_mean)[is.na(merge2$R_mean)]
merge2$GPP_mean[is.na(merge2$GPP_mean)] <- (merge2$R_mean + merge2$NPP_mean)[is.na(merge2$GPP_mean)]

final_sheet <- merge2[!is.na(merge2$NPP_mean) & !is.na(merge2$GPP_mean) & !is.na(merge2$R_mean),]

final_sheet$ANPP <- as.character(final_sheet$ANPP)
final_sheet$NPP <- as.character(final_sheet$NPP)

final_sheet$GPP[is.na(final_sheet$GPP)] <- "GPP_derived"
final_sheet$NPP[is.na(final_sheet$NPP)] <- "NPP_derived"
final_sheet$R_auto[is.na(final_sheet$R_auto)] <- "R_derived"

final_sheet$lat.x[is.na(final_sheet$lat.x)] <- (final_sheet$lat.y)[is.na(final_sheet$lat.x)]
final_sheet$mat.x[is.na(final_sheet$mat.x)] <- (final_sheet$mat.y)[is.na(final_sheet$mat.x)]
final_sheet$map.x[is.na(final_sheet$map.x)] <- (final_sheet$map.y)[is.na(final_sheet$map.x)]
final_sheet$TempSeasonality.x[is.na(final_sheet$TempSeasonality.x)] <- (final_sheet$TempSeasonality.y)[is.na(final_sheet$TempSeasonality.x)]
final_sheet$geographic.area.x[is.na(final_sheet$geographic.area.x)] <- (final_sheet$geographic.area.y)[is.na(final_sheet$geographic.area.x)]
final_sheet$masl.x[is.na(final_sheet$masl.x)] <- (final_sheet$masl.y)[is.na(final_sheet$masl.x)]

# check <- merge2[is.na(merge2$ANPP_mean) | is.na(merge2$BNPP_mean) | is.na(merge2$NPP_mean),]

final_sheet <- final_sheet[-(13:18)]
names(final_sheet)[5:10] <- c("lat", "mat", "map", "TempSeasonality", "geographic.area", "masl")

final_sheet$ratio <- log(final_sheet$NPP_mean/final_sheet$R_mean)

fixed.variables <- c("lat", "mat", "map", "TempSeasonality")

png(file = "C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/ratio_regressions/log ratios/all.png", width = 3000, height = 2000, units = "px", res = 300)

par(mfrow = c(3,4), mar = c(1,1,3,0.5), oma = c(5,4,5,0))

n <- 1

for(fixed.v in fixed.variables){

  final_sheet$fixed <- final_sheet[, fixed.v]
  
  mod <-  lmer(ratio ~ 1 + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  mod.linear <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  
  best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
  
  if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  if (best.model == "mod") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  
  significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
  significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
  sample.size <- length(final_sheet$ratio)
  
  if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = T)
  
  newDat <- expand.grid(fixed = seq(min(final_sheet$fixed), max(final_sheet$fixed), length.out = 100))
  newDat$fit <- predict(mod.full, newDat, re.form = NA)
  
  ylim <- range(exp(final_sheet$ratio))
  ylim[1] <- ylim[1] - 0.25
  ylim[2] <- ylim[2] + 0.25
  
  
  if(n == 1) plot(exp(ratio) ~ fixed, data = final_sheet, ylab = "", ylim = ylim)
  if(n != 1) plot(exp(ratio) ~ fixed, data = final_sheet, ylab = "", ylim = ylim, yaxt = "n")
  
  lines(exp(fit) ~ fixed, data = newDat, lty = ifelse(significant.effect, 1, 2))
  
  mod.linear <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = T)
  
  r <- round(fixef(mod.linear), 5)
  
  significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
  significance <- signif(significance, digits=4)
  
  Rsq <- as.data.frame(r.squaredGLMM(mod.full))
  Rsq <- signif(Rsq, digits=2)[1]
  
  if(significant.effect) mtext(paste("R-sq =", Rsq), side = 3, line = -1.5, adj = 0.1, cex = 0.6)
  if(fixed.v %in% "lat")mtext("NPP:Autotrophic respiration", side = 3, line = 0.5, adj = 0, cex = 0.6)
  
  
  # if(fixed.v %in% "mat") mtext(side = 2, line = 2,  text = paste("Ratio", set1[[i]], "to", set2[[i]]))
  # panel.number <- panel.number + 1
  n <- n + 1
}

#######################################################################################################

ForC_simplified_base <- ForC_simplified[ForC_simplified$variable.name %in% c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0","ANPP_1", "ANPP_2", "ANPP_0", "BNPP_root"),]

ANPP <- ForC_simplified_base[ForC_simplified_base$variable.name %in% c("ANPP_1", "ANPP_2", "ANPP_0"),]
ANPP <- ANPP %>%
  dplyr::mutate_at("variable.name", factor, levels = c("ANPP_0", "ANPP_1", "ANPP_2")) %>%
  arrange(desc(variable.name)) %>%
  dplyr::group_by(site_plot_year_cite) %>%
  dplyr::filter(variable.name == variable.name[1]) %>%
  dplyr::ungroup()

NPP <- ForC_simplified_base[ForC_simplified_base$variable.name %in% c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0"),]
NPP <- NPP %>%
  dplyr::mutate_at("variable.name", factor, levels = c("NPP_0", "NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5")) %>%
  arrange(desc(variable.name)) %>%
  dplyr::group_by(site_plot_year_cite) %>%
  dplyr::filter(variable.name == variable.name[1]) %>%
  dplyr::ungroup()

BNPP <- ForC_simplified_base[ForC_simplified_base$variable.name %in% c("BNPP_root"),]

merge1 <- merge(NPP[, c("plot.name", "variable.name", "mean", "site_plot_year_cite", "lat", "mat", "map", "TempSeasonality", "geographic.area", "masl")], ANPP[, c("plot.name", "variable.name", "mean", "site_plot_year_cite", "lat", "mat", "map", "TempSeasonality", "geographic.area", "masl")], by= c("plot.name", "site_plot_year_cite"), all.x = TRUE, all.y = TRUE)
merge2 <- merge(merge1, BNPP[, c("variable.name", "mean", "site_plot_year_cite")], by= c("site_plot_year_cite"), all = TRUE)

names(merge2)[c(3:4, 11:12, 19:20)] <- c("NPP", "NPP_mean", "ANPP", "ANPP_mean", "BNPP", "BNPP_mean")

merge2$BNPP_mean[is.na(merge2$BNPP_mean)] <- (merge2$NPP_mean - merge2$ANPP_mean)[is.na(merge2$BNPP_mean)]
merge2$ANPP_mean[is.na(merge2$ANPP_mean)] <- (merge2$NPP_mean - merge2$BNPP_mean)[is.na(merge2$ANPP_mean)]
merge2$NPP_mean[is.na(merge2$NPP_mean)] <- (merge2$ANPP_mean + merge2$BNPP_mean)[is.na(merge2$NPP_mean)]

final_sheet <- merge2[!is.na(merge2$ANPP_mean) & !is.na(merge2$BNPP_mean) & !is.na(merge2$NPP_mean),]

final_sheet$ANPP <- as.character(final_sheet$ANPP)
final_sheet$NPP <- as.character(final_sheet$NPP)

final_sheet$BNPP[is.na(final_sheet$BNPP)] <- "BNPP_derived"
final_sheet$ANPP[is.na(final_sheet$ANPP)] <- "ANPP_derived"
final_sheet$NPP[is.na(final_sheet$NPP)] <- "NPP_derived"

final_sheet$lat.x[is.na(final_sheet$lat.x)] <- (final_sheet$lat.y)[is.na(final_sheet$lat.x)]
final_sheet$mat.x[is.na(final_sheet$mat.x)] <- (final_sheet$mat.y)[is.na(final_sheet$mat.x)]
final_sheet$map.x[is.na(final_sheet$map.x)] <- (final_sheet$map.y)[is.na(final_sheet$map.x)]
final_sheet$TempSeasonality.x[is.na(final_sheet$TempSeasonality.x)] <- (final_sheet$TempSeasonality.y)[is.na(final_sheet$TempSeasonality.x)]
final_sheet$geographic.area.x[is.na(final_sheet$geographic.area.x)] <- (final_sheet$geographic.area.y)[is.na(final_sheet$geographic.area.x)]
final_sheet$masl.x[is.na(final_sheet$masl.x)] <- (final_sheet$masl.y)[is.na(final_sheet$masl.x)]

# check <- merge2[is.na(merge2$ANPP_mean) | is.na(merge2$BNPP_mean) | is.na(merge2$NPP_mean),]

final_sheet <- final_sheet[-(13:18)]
names(final_sheet)[5:10] <- c("lat", "mat", "map", "TempSeasonality", "geographic.area", "masl")

final_sheet$ratio <- log(final_sheet$ANPP_mean/final_sheet$BNPP_mean)

fixed.variables <- c("lat", "mat", "map", "TempSeasonality")

n <- 1

for(fixed.v in fixed.variables){
  
  final_sheet$fixed <- final_sheet[, fixed.v]
  
  mod <-  lmer(ratio ~ 1 + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  mod.linear <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  
  best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
  
  if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  if (best.model == "mod") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  
  cooksd <- cooks.distance(mod.full)

  # plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
  # abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  # text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

  influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
  rows.to.keep<-which(rownames(final_sheet) %in% influential)
  ifelse(length(influential > 0), dfsubset <- final_sheet[-rows.to.keep,], dfsubset <- final_sheet)
  
  mod <-  lmer(ratio ~ 1 + (1|geographic.area/plot.name), data = dfsubset, REML = F)
  mod.linear <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = dfsubset, REML = F)
  
  best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
  
  if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = dfsubset, REML = F)
  if (best.model == "mod") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = dfsubset, REML = F)
  
  
  significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
  significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
  sample.size <- length(dfsubset$ratio)
  
  if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T)+ (1|geographic.area/plot.name), data = dfsubset, REML = T)
  
  newDat <- expand.grid(fixed = seq(min(dfsubset$fixed), max(dfsubset$fixed), length.out = 100))
  newDat$fit <- predict(mod.full, newDat, re.form = NA)
  
  ylim <- range(exp(dfsubset$ratio))
  ylim[1] <- ylim[1] - 0.25
  ylim[2] <- ylim[2] + 0.25
  
  
  if(n == 1) plot(exp(ratio) ~ fixed, data = dfsubset, ylab = "", ylim = ylim)
  if(n != 1) plot(exp(ratio) ~ fixed, data = dfsubset, ylab = "", ylim = ylim, yaxt = "n")
  
  
  lines(exp(fit) ~ fixed, data = newDat, lty = ifelse(significant.effect, 1, 2))
  
  mod.linear <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = dfsubset, REML = T)
  
  r <- round(fixef(mod.linear), 5)
  
  significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
  significance <- signif(significance, digits=4)
  
  Rsq <- as.data.frame(r.squaredGLMM(mod.full))
  Rsq <- signif(Rsq, digits=2)[1]
  
  if(significant.effect) mtext(paste("R-sq =", Rsq), side = 3, line = -1.5, adj = 0.1, cex = 0.6)
  if(fixed.v %in% "lat")mtext("ANPP:BNPP", side = 3, line = 0.5, adj = 0, cex = 0.6)
  
  
  # if(fixed.v %in% "mat") mtext(side = 2, line = 2,  text = paste("Ratio", set1[[i]], "to", set2[[i]]))
  # panel.number <- panel.number + 1
  
  n <- n + 1
}


#######################################################################################################

ForC_simplified_base <- ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_1", "ANPP_woody_stem", "ANPP_foliage"),]

ANPP <- ForC_simplified_base[ForC_simplified_base$variable.name %in% c("ANPP_1"),]

ANPP_stem <- ForC_simplified_base[ForC_simplified_base$variable.name %in% c("ANPP_woody_stem"),]

ANPP_foliage <- ForC_simplified_base[ForC_simplified_base$variable.name %in% c("ANPP_foliage"),]

merge1 <- merge(ANPP[, c("plot.name", "variable.name", "mean", "site_plot_year_cite", "lat", "mat", "map", "TempSeasonality", "geographic.area", "masl")], ANPP_stem[, c("plot.name", "variable.name", "mean", "site_plot_year_cite", "lat", "mat", "map", "TempSeasonality", "geographic.area", "masl")], by= c("plot.name", "site_plot_year_cite"), all.x = TRUE, all.y = TRUE)
merge2 <- merge(merge1, ANPP_foliage[, c("variable.name", "mean", "site_plot_year_cite")], by= c("site_plot_year_cite"), all = TRUE)

names(merge2)[c(3:4, 11:12, 19:20)] <- c("ANPP", "ANPP_mean", "stem", "stem_mean", "foliage", "foliage_mean")

merge2$stem_mean[is.na(merge2$stem_mean)] <- (merge2$ANPP_mean - merge2$foliage_mean)[is.na(merge2$stem_mean)]
merge2$foliage_mean[is.na(merge2$foliage_mean)] <- (merge2$ANPP_mean - merge2$stem_mean)[is.na(merge2$foliage_mean)]
merge2$ANPP_mean[is.na(merge2$ANPP_mean)] <- (merge2$foliage_mean + merge2$stem_mean)[is.na(merge2$ANPP_mean)]

final_sheet <- merge2[!is.na(merge2$ANPP_mean) & !is.na(merge2$foliage_mean) & !is.na(merge2$stem_mean),]

final_sheet$ANPP[is.na(final_sheet$ANPP)] <- "ANPP_derived"
final_sheet$stem[is.na(final_sheet$stem)] <- "stem_derived"
final_sheet$foliage[is.na(final_sheet$foliage)] <- "foliage_derived"

final_sheet$lat.x[is.na(final_sheet$lat.x)] <- (final_sheet$lat.y)[is.na(final_sheet$lat.x)]
final_sheet$mat.x[is.na(final_sheet$mat.x)] <- (final_sheet$mat.y)[is.na(final_sheet$mat.x)]
final_sheet$map.x[is.na(final_sheet$map.x)] <- (final_sheet$map.y)[is.na(final_sheet$map.x)]
final_sheet$TempSeasonality.x[is.na(final_sheet$TempSeasonality.x)] <- (final_sheet$TempSeasonality.y)[is.na(final_sheet$TempSeasonality.x)]
final_sheet$geographic.area.x[is.na(final_sheet$geographic.area.x)] <- (final_sheet$geographic.area.y)[is.na(final_sheet$geographic.area.x)]
final_sheet$masl.x[is.na(final_sheet$masl.x)] <- (final_sheet$masl.y)[is.na(final_sheet$masl.x)]

# check <- merge2[is.na(merge2$ANPP_mean) | is.na(merge2$BNPP_mean) | is.na(merge2$NPP_mean),]

final_sheet <- final_sheet[-(13:18)]
names(final_sheet)[5:10] <- c("lat", "mat", "map", "TempSeasonality", "geographic.area", "masl")

final_sheet$ratio <- log(final_sheet$stem_mean/final_sheet$foliage_mean)

fixed.variables <- c("lat", "mat", "map", "TempSeasonality")

n <- 1

for(fixed.v in fixed.variables){
  
  fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
  xaxis_simple <- fixed.v.info$xaxis_simple[which(fixed.v.info$fixed.v %in% fixed.v)]
  
  final_sheet$fixed <- final_sheet[, fixed.v]
  
  mod <-  lmer(ratio ~ 1 + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  mod.linear <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  
  best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
  
  if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  if (best.model == "mod") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = F)
  
  significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
  significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
  sample.size <- length(final_sheet$ratio)
  
  if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = T)
  
  newDat <- expand.grid(fixed = seq(min(final_sheet$fixed), max(final_sheet$fixed), length.out = 100), masl = c(0.5))
  newDat$fit <- predict(mod.full, newDat, re.form = NA)
  
  ylim <- range(exp(final_sheet$ratio))
  ylim[1] <- ylim[1] - 0.25
  ylim[2] <- ylim[2] + 0.25
  
  
  if(n == 1) plot(exp(ratio) ~ fixed, data = final_sheet, ylab = "", ylim = ylim)
  if(n != 1) plot(exp(ratio) ~ fixed, data = final_sheet, ylab = "", ylim = ylim, yaxt = "n")
  
  
  lines(exp(fit) ~ fixed, data = newDat, lty = ifelse(significant.effect, 1, 2))
  
  mod.linear <- lmer(ratio ~ poly(fixed, 1, raw = T) + (1|geographic.area/plot.name), data = final_sheet, REML = T)
  
  r <- round(fixef(mod.linear), 5)
  
  significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
  significance <- signif(significance, digits=4)
  
  Rsq <- as.data.frame(r.squaredGLMM(mod.full))
  Rsq <- signif(Rsq, digits=2)[1]
  
  if(significant.effect) mtext(paste("R-sq =", Rsq), side = 3, line = -1.5, adj = 0.1, cex = 0.6)
  if(fixed.v %in% "lat") mtext("ANPP stem:ANPP foliage", side = 3, line = 0.5, adj = 0, cex = 0.6)
  
  
  mtext(text = eval(parse(text = xaxis_simple)), side = 1, line = 2.5, cex = 0.6)
  
  # if(fixed.v %in% "mat") mtext(side = 2, line = 2,  text = paste("Ratio", set1[[i]], "to", set2[[i]]))
  # panel.number <- panel.number + 1
  
  n <- n+1
}

mtext(text = "Flux ratio", side = 2, outer= T, line = 2)

dev.off()

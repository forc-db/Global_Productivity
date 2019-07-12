
# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/ForC")

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


# Control for some factors ####


## keep all ages except na, 0 and 999
ages.not.999.nor.0.nor.na <- !ForC_simplified$stand.age %in% 999 &  !ForC_simplified$stand.age %in% 0 & !is.na(ForC_simplified$stand.age)

## Keep only age >=100 (or 999)
ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)


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

fixed.variables <- c("mat", "map", "lat", "AnnualMeanTemp", "TempSeasonality", "TempRangeAnnual", "PotentialEvapotranspiration", "VapourPressureDeficit")

set1 <- c("GPP", "ANPP_1", "ANPP_foliage", "ANPP_foliage", "ANPP_woody_stem", "ANPP_1", "BNPP_root")
set2 <- c("NPP_1", "BNPP_root", "ANPP_woody_stem", "NPP_1", "NPP_1", "NPP_1", "NPP_1")

# set1 <- "ANPP_foliage"
# set2 <- "ANPP_woody_stem"


for (i in seq(along = set1)){
  # for (j in seq(along = set2)){
    # if (i == j){
      print(i)
      resp1 <- ForC_simplified[ForC_simplified$variable.name %in% set1[[i]],]
      resp2 <- ForC_simplified[ForC_simplified$variable.name %in% set2[[i]],]
      
      df <- merge(resp1, resp2[, c("variable.name", "date", "mean", "citation.ID", "site_plot")], by= c("site_plot"))
      
      df$ratio <- df$mean.x/df$mean.y
      
      df$masl <- df$masl/1000
      
      for(fixed.v in fixed.variables){
        
        print(fixed.v)
        
        fixed.no.na <- !is.na(df[, fixed.v]) & !is.na(df[, "masl"])
        ages.to.keep <- df$stand.age >= 100 & !is.na(df$stand.age)
        
        df <- df[fixed.no.na & ages.to.keep, ]
        
        df$fixed <- df[, fixed.v]
        
        ##remove extreme outliers 
        
        # outliers <- boxplot(df$ratio, plot=FALSE, range = 3)$out
        # 
        # if(length(outliers > 0)) df <- df[-which(df$ratio %in% outliers),]
        
        
        mod <-  lmer(ratio ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.linear <- lmer(ratio ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
        
        if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$ratio)

        if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = T)
        
        ########## include this section for analysis of cooks distance and subsetting ##########

        # cooksd <- cooks.distance(mod.full)
        # 
        # plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
        # abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
        # text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
        # 
        # influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
        # rows.to.keep<-which(rownames(df) %in% influential)
        # ifelse(length(influential > 0), dfsubset <- df[-rows.to.keep,], dfsubset <- df)
        
        mod <-  lmer(ratio ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.linear <- lmer(ratio ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
        
        if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df)
        
        if (best.model == "mod.linear") mod.full <- lmer(ratio ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = T)
        
        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100), masl = c(0.5))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        png(file = paste0("C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/ratio_regressions/Effect_of_", fixed.v, "_MATURE_only_", set1[[i]], "_", set2[[i]], ".png"), width = 2255, height = 2000, units = "px", res = 300)
        
        par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(5,5,2,0))
        
        ylim <- range(df$ratio)
        ylim[1] <- ylim[1] - 0.25
        ylim[2] <- ylim[2] + 0.25
        
        
        plot(ratio ~ fixed, data = df, xlab = "", ylab = "", ylim = ylim)
        
        for(masl in unique(newDat$masl)){
          k <- which(unique(newDat$masl) %in% masl)
          lines(fit ~ fixed, data = newDat[newDat$masl %in% masl,], lty = ifelse(significant.effect, 1, 2), lwd = k)}
        
        mod.linear <- lmer(ratio ~ poly(fixed, 1, raw = T) + masl + (1|geographic.area/plot.name), data = df, REML = T)
        
        r <- round(fixef(mod.linear), 5)
        
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        significance <- signif(significance, digits=4)
        
        Rsq <- as.data.frame(r.squaredGLMM(mod.full))
        Rsq <- signif(Rsq, digits=4)
        legend2 <- paste("r-squared = ", Rsq[1], "p-value = ", significance)
        mtext(side = 3, text = legend2, adj = 0.9, cex = 0.5)
        
        title (paste("Effect of", fixed.v), outer = T, line = 1)
        mtext(side = 1, line = 3, text = fixed.v, outer = T)
        mtext(side = 2, line = 3,  text = paste("Ratio", set1[[i]], "to", set2[[i]]), outer = T)
        
        
        dev.off()
      }
      
}#}}

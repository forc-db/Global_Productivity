results <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/best_model_outputs/best_model_scaled_unweighted.csv", stringsAsFactors = FALSE)
response <- c("GPP", "NPP", "ANPP", "ANPP_woody_stem", "ANPP_foliage", "BNPP_root", "BNPP_root_fine", "R_auto", "R_auto_root")

fixed.v <- c("lat", "mat", "map", "TempSeasonality", "PreSeasonality", "TempRangeAnnual", "SolarRadiation", "Aridity", "CloudCover", "AnnualFrostDays", "AnnualWetDays", "PotentialEvapotranspiration", "VapourPressureDeficit", "MaxVPD", "WaterStressMonths", "length_growing_season")

table <- as.data.frame(response)

all.results <- NULL

for(v in fixed.v){
rs <- results[results$fixed %in% v,]

test <- merge(rs[, c("response", "fixed", "best.model", "Rsq.R2m", "delta.aic", "significant")], table, by = "response")

all.results <- rbind(all.results, test)
}
  
all.results$best.model[all.results$best.model %in% "mod.clim"] <- "Lin"
all.results$best.model[all.results$best.model %in% "mod"] <- "Null"
all.results$best.model[all.results$best.model %in% "mod.clim.poly"] <- "Poly"
all.results$best.model[all.results$best.model %in% "mod.clim.log"] <- "Log"

write.csv(all.results, "C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/tables/best_model_outputs/prep_table_s2.csv")

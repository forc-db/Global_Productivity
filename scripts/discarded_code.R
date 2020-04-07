### for graph with two plots, latitude only, saved to supporting information mature forests only ####

fixed.variables <- "lat"

response.variables.groups <- list(c("GPP", "NPP", "ANPP", "BNPP_root", "R_auto"),
                                  c("ANPP_foliage", "ANPP_woody_stem", "BNPP_root_fine", "R_auto_root"))

all.results = NULL

for (age in ages){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  for(fixed.v in fixed.variables){
    
    png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/supporting_information/latitude unscaled.png"), width = 5000, height = 2000, units = "px", res = 300)
    
    layout(matrix(1:3, nrow = 1), widths = c(1,1,0.25))
    par(mar = c(3,3,3,0), oma = c(3,3,0,7))
    
    print(fixed.v)
    
    fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
    
    xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
    
    for(response.variables in response.variables.groups){
      
      if(response.variables[1] == "GPP") n <- 1
      if(response.variables[1] == "ANPP_foliage") n <- 2
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
        
        col <- col.sym$col[which(col.sym$variable %in% response.v)]
        sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
        if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
        
        rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
        
        fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
        
        df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
        
        # df$masl <- df$masl/1000
        
        df$fixed <- df[, fixed.v]
        
        if(response.v %in% c("GPP", "ANPP_foliage")){
          a <- ForC_simplified[ages.to.keep, ]
          a <- a[a$variable.name %in% unlist(response.variables),]
          ylim <- range(a$mean)
          ylim[1] <- ylim[1] - 0.25
          ylim[2] <- ylim[2] + 0.25
        }
        
        if(response.v == "GPP"){
          b <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables.groups),]
          no.na <- !is.na(b[, fixed.v])
          b <- b[no.na,]
          xlim <- range(b[, fixed.v])
        }
        
        
        mod <-  lmer(mean ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.linear <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        mod.poly <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        if (best.model == "mod.poly") mod.full <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.linear") mod.full <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod") mod.full <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        
        ###in this case only linear model used - override AIC results here
        mod.full <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.poly") mod.full <- lmer(mean ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.linear") mod.full <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = T)
        
        ###in this case only linear model used - override AIC results here
        mod.full <- lmer(mean ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = T)
        
        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        if(first.plot) plot(mean ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], pch = sym, ylim = ylim, xlim = xlim)
        if(!first.plot) points(mean ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
        
        lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
        
        first.plot <- FALSE
        
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        significance <- signif(significance, digits=4)
        
        Rsq <- as.data.frame(r.squaredGLMM(mod.full))
        Rsq <- signif(Rsq, digits=4)
        legend1 = "R-squared values"
        legend2 <- paste(response.v, " = ", Rsq[1])
        # legend3 <- paste(response.v, "p-value = ", significance)
        
        
        
        results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
        
        all.results <- rbind(all.results, results)
        
      }
      
      
      if(n == 1) title(paste("Major fluxes"), outer = F, line = 1)
      if(n == 2) title(paste("Subsidiary fluxes"), outer = F, line = 1)
      mtext(side = 1, line = 2, text = eval(parse(text = xaxis)), outer = T)
      mtext(side = 2, line = 1,  text = expression("Productivity (scaled values)"), outer = T) 
      
      
      
    }
    plot(1:1, type="n", axes = F)
    
    legend("topleft", legend = c("GPP", "NPP", "ANPP", "BNPP_root", "R_auto"), col = plasma(10)[c(1, 3, 5, 8, 4)], pch = c(1, 3, 5, 8, 4), xpd = T, text.col = plasma(10)[c(1, 3, 5, 8, 4)], bty = "n", title = "Major fluxes", title.col = "black")
    legend("left", legend = c("ANPP_woody_stem", "ANPP_foliage", "BNPP_root_fine", "R_auto_root"), col = plasma(10)[c(7, 9, 6, 2)], pch = c(7, 9, 6, 2), xpd = T, text.col = plasma(10)[c(7, 9, 6, 2)], bty = "n", title = "Subsidiary fluxes", title.col = "black")
    # mtext(side = 3, line = -(which(col.sym$variable %in% response.v)), text = legend2, adj = 1, col = plasma(10)[col], cex = 0.5, outer = T)
    # if(n==1) mtext(side = 3, line = 0, text = legend1, adj = 1, col = "black", cex = 0.5, outer = T)
    # mtext(side = 3, line = -7 - which(response.variables %in% response.v), text = legend3, adj = 0.95, col = plasma(8)[response.v.color], cex = 0.5, outer = T)
    
    dev.off()
  }
}


################################

response.variables.groups <- list(c("GPP", "NPP", "ANPP", "BNPP_root", "R_auto"),
                                  c("ANPP_foliage", "ANPP_woody_stem", "BNPP_root_fine", "R_auto_root"))

all.results = NULL


### mature forests only ####
for (age in ages){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  for(fixed.v in fixed.variables){
    
    print(fixed.v)
    
    fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
    
    xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
    
    for(response.variables in response.variables.groups){
      
      if(response.variables[1] == "GPP") n <- 1
      if(response.variables[1] == "ANPP_foliage") n <- 2
      
      png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/Effect_of_", fixed.v, n, "_MATURE_only_poly_all.png"), width = 2255, height = 2000, units = "px", res = 300)
      
      par(mfrow = c(1,1), mar = c(3,3,3,3))
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
        
        col <- col.sym$col[which(col.sym$variable %in% response.v)]
        sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
        if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
        
        
        ##response.v.color <- response.variables.col[which(response.variables %in% response.v)]
        
        rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
        
        fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
        
        df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
        
        # df$masl <- df$masl/1000
        
        df$fixed <- df[, fixed.v]
        
        a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
        ylim <- range(tapply(a$mean, a$variable.name, scale))
        ylim[1] <- ylim[1] - 0.25
        ylim[2] <- ylim[2] + 0.25
        
        
        mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.linear <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        mod.poly <- lmer(scale(mean) ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod.linear = mod.linear, mod.poly = mod.poly), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.linear = mod.linear, mod.poly = mod.poly), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.poly") mod.full <- lmer(scale(mean) ~ poly(fixed, 2) + (1|geographic.area/plot.name), data = df, REML = T)
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = T)
        
        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10)[col], pch = sym, yaxt = "n", ylim = ylim)
        if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10)[col], pch = sym) 
        
        lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = ifelse(significant.effect, 1, 2))
        
        first.plot <- FALSE
        
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        significance <- signif(significance, digits=4)
        
        Rsq <- as.data.frame(r.squaredGLMM(mod.full))
        Rsq <- signif(Rsq, digits=4)
        legend1 = "R-squared values"
        legend2 <- paste(response.v, " = ", Rsq[1])
        # legend3 <- paste(response.v, "p-value = ", significance)
        legend1 <- paste(response.v)
        mtext(side = 3, line = -(which(response.variables %in% response.v)), text = legend2, adj = 0.95, col = plasma(10)[col], cex = 0.5, outer = F)
        
        
        
        results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
        
        all.results <- rbind(all.results, results)
        
      }
      
      
      if(n == 1) title(paste("Major fluxes"), outer = F, line = 1)
      if(n == 2) title(paste("Subsidiary fluxes"), outer = F, line = 1)
      mtext(side = 1, line = 2, text = eval(parse(text = xaxis)), outer = F)
      mtext(side = 2, line = 1,  text = expression("Productivity (scaled values)"), outer = F) 
      
      
      dev.off()
      
      
      
    }
    
    
    # mtext(side = 3, line = -(which(col.sym$variable %in% response.v)), text = legend2, adj = 1, col = plasma(10)[col], cex = 0.5, outer = T)
    # if(n==1) mtext(side = 3, line = 0, text = legend1, adj = 1, col = "black", cex = 0.5, outer = T)
    # mtext(side = 3, line = -7 - which(response.variables %in% response.v), text = legend3, adj = 0.95, col = plasma(8)[response.v.color], cex = 0.5, outer = T)
    
  }
}



################################ graph plots no points

response.variables.groups <- list(c("GPP", "NPP", "ANPP", "BNPP_root", "ANPP_foliage", "ANPP_woody_stem", "BNPP_root_fine", "R_auto", "R_auto_root"))
fixed.variables <- "lat"

all.results = NULL
all.legends = NULL


### mature forests only ####
for (age in ages){
  
  if (age %in% "age.greater.than.100") ages.to.keep <- ForC_simplified$stand.age >= 100 & !is.na(ForC_simplified$stand.age)
  if (age %in% "age.greater.than.200") ages.to.keep <- ForC_simplified$stand.age >= 200 & !is.na(ForC_simplified$stand.age)
  
  for(fixed.v in fixed.variables){
    
    print(fixed.v)
    
    fixed.v.info <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/fixedv_data.csv", stringsAsFactors = F)
    
    xaxis <- fixed.v.info$xaxis[which(fixed.v.info$fixed.v %in% fixed.v)]
    
    for(response.variables in response.variables.groups){
      
      if(response.variables[1] == "GPP") n <- 1
      if(response.variables[1] == "ANPP_foliage") n <- 2
      
      png(file = paste0("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/unweighted_model/effect_of_", fixed.v, "_transparent.png"), width = 2255, height = 2000, units = "px", res = 300)
      
      par(mfrow = c(1,1), mar = c(3,3,3,3))
      
      first.plot <- TRUE
      
      for (response.v in response.variables){
        
        col.sym <- read.csv("C:/Users/becky/Dropbox (Smithsonian)/GitHub/Global_Productivity/raw.data/colsym.csv", stringsAsFactors = F)
        
        col <- col.sym$col[which(col.sym$variable %in% response.v)]
        sym <- col.sym$sym[which(col.sym$variable %in% response.v)]
        lty <- col.sym$lty[which(col.sym$variable %in% response.v)]
        
        if(response.v %in% "NPP") responses.to.keep  <- c("NPP_1", "NPP_2", "NPP_3", "NPP_4", "NPP_5", "NPP_0")
        if(response.v %in% "ANPP") responses.to.keep  <- c("ANPP_1", "ANPP_2", "ANPP_0")
        if(!response.v %in% c("NPP", "ANPP")) responses.to.keep  <- response.v
        
        
        # response.v.color <- response.variables.col[which(response.variables %in% response.v)]
        
        rows.with.response <- ForC_simplified$variable.name %in% responses.to.keep
        
        fixed.no.na <- !is.na(ForC_simplified[, fixed.v]) & !is.na(ForC_simplified[, "masl"])
        
        df <- ForC_simplified[rows.with.response & ages.to.keep & fixed.no.na, ]
        
        # df$masl <- df$masl/1000
        
        df$fixed <- df[, fixed.v]
        
        a <- ForC_simplified[ForC_simplified$variable.name %in% unlist(response.variables),]
        ylim <- range(tapply(a$mean, a$variable.name, scale))
        ylim[1] <- ylim[1] - 0.25
        ylim[2] <- ylim[2] + 0.25
        
        
        mod <-  lmer(scale(mean) ~ 1 + (1|geographic.area/plot.name), data = df, REML = F)
        mod.linear <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        
        aictab <- aictab(list(mod = mod, mod.linear = mod.linear), sort = T)
        
        best.model <- as.character(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Modname[1])
        delta.aic <- as.numeric(aictab(list(mod = mod, mod.linear = mod.linear), sort = T)$Delta_AICc[2])
        delta.aic <- signif(delta.aic, digits=4)
        
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        if (best.model == "mod") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = F)
        
        significant.effect <- anova(mod, mod.full)$"Pr(>Chisq)"[2] < 0.05
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        sample.size <- length(df$mean)
        
        if (best.model == "mod.linear") mod.full <- lmer(scale(mean) ~ poly(fixed, 1) + (1|geographic.area/plot.name), data = df, REML = T)
        
        newDat <- expand.grid(fixed = seq(min(df$fixed), max(df$fixed), length.out = 100))
        newDat$fit <- predict(mod.full, newDat, re.form = NA)
        
        # if(first.plot) plot((mean) ~ fixed, data = df, xlab = "", ylab = "", col = "white", pch = sym, yaxt = "n", ylim = ylim)
        if(first.plot) plot(scale(mean) ~ fixed, data = df, xlab = "", ylab = "", col = plasma(10, alpha = 0.3)[col], pch = sym, ylim = ylim)
        if(!first.plot) points(scale(mean) ~ fixed, data = df, ylab = "", col = plasma(10, alpha = 0.3)[col], pch = sym)
        
        lines(fit ~ fixed, data = newDat, col = plasma(10)[col], lty = lty)
        
        first.plot <- FALSE
        
        significance <- anova(mod, mod.full)$"Pr(>Chisq)"[2]
        significance <- signif(significance, digits=4)
        
        Rsq <- as.data.frame(r.squaredGLMM(mod.full))
        Rsq <- signif(Rsq, digits=4)
        # legend1 = "R-squared values"
        # legend2 <- paste(response.v, " = ", Rsq[1])
        # # legend3 <- paste(response.v, "p-value = ", significance)
        # legend1 <- paste(response.v)
        # mtext(side = 3, line = -(which(response.variables %in% response.v)), text = legend2, adj = 0.95, col = plasma(10)[col], cex = 0.5, outer = F)
        # 
        
        legend <- paste0(response.v, "; Rsq = ", Rsq[1])
        
        results <- data.frame(response = response.v, fixed = fixed.v, random = "geographic.area/plot.name", Age.filter = age, significant = significant.effect, p.value = significance, sample.size = sample.size, Rsq = Rsq)
        
        all.results <- rbind(all.results, results)
        all.legends <- rbind(all.legends, legend)
        
      }
      all.legends <- as.character(all.legends)
      
      # if(n == 1) title(paste("Major fluxes"), outer = F, line = 1)
      # if(n == 2) title(paste("Subsidiary fluxes"), outer = F, line = 1)
      mtext(side = 1, line = 2, text = "Latitude", outer = F)
      mtext(side = 2, line = 2,  text = expression("Productivity (scaled values)"), outer = F) 
      
      legend("topright", legend = all.legends, col = plasma(10)[c(1, 3, 5, 8, 9, 7, 6, 4, 2)], pch = c(1, 3, 5, 8, 9, 7, 6, 4, 2), xpd = T, lty = c(1, 6, 5, 1, 6, 5, 6, 1, 5), text.col = plasma(10)[c(1, 3, 5, 8, 9, 7, 6, 4, 2)], bty = "n", title.col = "black", cex = 0.75)
      
      
      dev.off()
      
      
      
    }
    
    
    # mtext(side = 3, line = -(which(col.sym$variable %in% response.v)), text = legend2, adj = 1, col = plasma(10)[col], cex = 0.5, outer = T)
    # if(n==1) mtext(side = 3, line = 0, text = legend1, adj = 1, col = "black", cex = 0.5, outer = T)
    # mtext(side = 3, line = -7 - which(response.variables %in% response.v), text = legend3, adj = 0.95, col = plasma(8)[response.v.color], cex = 0.5, outer = T)
    
  }
}


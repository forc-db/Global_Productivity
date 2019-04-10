# Global-scale variation in forest productivity with respect to climate
(a simple declarative statement would make a better title, if we can think of something appropriate)

Authors:
- Becky
- Valentine
- Nobby
- Helene
- Krista

## Abstract

## Introduction

[We know, broadly, that forest productivity decreases with increasing latidude; however, we don't know how this relationship compares across the various productivity variables.]

*Hypotheses* (These are the null hypotheses. Consider order.):

H1. C fluxes vary in constant proportion to one another across major climate gradients (i.e., C allocation climate-invariant)

H2. C fluxes respond in the same way to the same set of climate variables. 

H3. Climate explains the same proportion of variability in C fluxes (i.e., the relative importance of climate is invariant)

## Methods (summary?)
**Fig. 1** - Map showing all data used in the anlysis, coded by variable.
![Distribution of carbon data](C:/Users/banburymorganr/Dropbox (Smithsonian)/GitHub/Global_Productivity/results/figures/final_figures/maps/distribution_all_variables.png)

- specify main model
- mention other versions triend, with logic behind each

## Results
*Setting the foundation:*

**All major C fluxes increase with decreasing latitude (and altitude) and associated climate variables.** (see preliminary [figures](https://github.com/forc-db/Global_Productivity/tree/master/results/figures/final_figures/scaled_best_model_with_alt) and [results](https://github.com/forc-db/Global_Productivity/blob/master/results/tables/best_model_outputs/best_model_scaled_with_ci.csv))
  - best fit is often polynomial, shapes vary (in potentially interesting ways - covered under H1)  

**Fig. 2** - multi-panel plots similar to [these](https://github.com/forc-db/Global_Productivity/tree/master/results/figures/archive/test/ratio_plots) showing (a) GPP = NPP + R_auto, (b) NPP = ANPP + BNPP, (c) ANPP = ANPP_foliage+ ANPP_woody_stem (+ ANPP_woody_branch + ANPP_repro) (d) BNPP, BNPP_root_fine, hopefully R_root
  
**Variables sum (approximately) as expected.**
  - Sum of statistical models for component models falls within confidence intervals for larger flux. 
  - Fig. 2

*Testing H1:*

**There are some differences in allocation across climate gradients (but how strong/signficant?)**
  - tendencies, based on [these plots](https://github.com/forc-db/Global_Productivity/tree/master/results/figures/test/ratio_plots):
    - allocation to woody productivity increases with latitude. (Agrees with [ratio tests](https://github.com/forc-db/Global_Productivity/tree/master/results/figures/foliage_woody).)
    - allocation to roots declines with latitude. (Partially agrees with [ratio tests](https://github.com/forc-db/Global_Productivity/tree/master/results/figures/foliage_woody), which show lowest fraction to BNPP in temperate.)
    
*Testing H2:*

[THere is some variation as to which are the best predictor variables. is any of it very interesting?]

*Testing H3:*

**Climate / latitude explains a larger portion of variation in major fluxes (GPP, NPP, and Rauto) than in subsidiary components.**
  - R2  ~ 0.7 for GPP, NPP, less for subsidiary components 
  - in model where mean and variance are scaled, both R2 and slope are highest for major fluxes, lower in subsidiary components

**Fig. 3** - bar plot or table showing R2 and slope (preferably with CIs) by variable

## Discussion

H1-

H3- 
- factors other than climate are important for determining allocation to subsidiary components

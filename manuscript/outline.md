# Global-scale variation in forest productivity with respect to climate
(a simple declarative statement would make a better title, if we can think of something appropriate)
target journal: GCB

Authors:
- Becky
- Valentine
- Nobby
- Helene
- Krista

## Abstract

## Introduction

[We know, broadly, that forest productivity decreases with increasing latidude; however, we don't know which climate variables drive this pattern or how the relationship compares across the various productivity variables.]

*Hypotheses* (These are the null hypotheses. Consider order.):

H1. The latitudinal pattern is driven by [interactive effects of energy, moisture, and solar radiation]

H2. C fluxes vary in constant proportion to one another across major climate gradients (i.e., C allocation climate-invariant)

H3. Climate explains the same proportion of variability in C fluxes (i.e., the relative importance of climate is invariant)

## Methods (summary?)
**Fig. 1** - Map showing all data used in the anlysis, coded by variable.
![Distribution of carbon data](https://github.com/forc-db/Global_Productivity/blob/master/results/figures/final_figures/maps/distribution_all_variables.png)

- specify main model
- mention other versions triend, with logic behind each

## Results
*Setting the foundation:*

**All major C fluxes increase with decreasing latitude (and altitude).** (see preliminary [figures](https://github.com/forc-db/Global_Productivity/tree/master/results/figures/final_figures/scaled_best_model_with_alt) and [results](https://github.com/forc-db/Global_Productivity/blob/master/results/tables/best_model_outputs/best_model_scaled_with_ci.csv))
  - best fit is often polynomial, shapes vary (in potentially interesting ways - covered under H1)  
  
**Variables sum (approximately) as expected.**
  - Sum of statistical models for component models falls within confidence intervals for larger flux. 
  - Fig. 2
  
**Fig. 2** - multi-panel plots similar to [these](https://github.com/forc-db/Global_Productivity/tree/master/results/figures/archive/test/ratio_plots) showing (a) GPP = NPP + R_auto, (b) NPP = ANPP + BNPP, (c) ANPP = ANPP_foliage+ ANPP_woody_stem (+ ANPP_woody_branch + ANPP_repro) (d) BNPP, BNPP_root_fine, hopefully R_root  
![(a) GPP = NPP + R_auto](https://github.com/forc-db/Global_Productivity/blob/master/results/figures/final_figures/stacked_plots/NPP_to_R_auto_lat_stacked.png)
![(b) NPP = ANPP + BNPP](https://github.com/forc-db/Global_Productivity/blob/master/results/figures/final_figures/stacked_plots/ANPP_2_to_BNPP_root_lat_stacked.png)
![(c)](https://github.com/forc-db/Global_Productivity/blob/master/results/figures/final_figures/stacked_plots/ANPP_foliage_to_ANPP_woody_stem_lat_stacked.png)
    
*Testing H1:*

  - C variables respond fairly similarly to climate variables (so I don't think we want a hypothesis testing for differences)
  - Generally TempSeasonality, mat, lat, TempRangeAnnual, VapourPressure and AnnualMeanTemp come out as the best predictors
  - For ANPP_woody and ANPP_woody_stem, VapourPressureDeficit and PotentialEvapotranspiration and mat are the best predictors
  
*Testing H2:*

**There are some differences in allocation across climate gradients (but how strong/signficant?)**
  - tendencies, based on [these plots](https://github.com/forc-db/Global_Productivity/tree/master/results/figures/archive/test/ratio_plots):
    - allocation to woody productivity increases with latitude. (Agrees with [ratio tests](https://github.com/forc-db/Global_Productivity/tree/master/results/figures/archive/foliage_woody).)
    - allocation to roots declines with latitude. (Partially agrees with [ratio tests](https://github.com/forc-db/Global_Productivity/tree/master/results/figures/archive/foliage_woody), which show lowest fraction to BNPP in temperate.)

*Testing H3:*

**Climate / latitude explains a larger portion of variation in major fluxes (GPP, NPP, and Rauto) than in subsidiary components.**
  - R2  ~ 0.7 for GPP, NPP, less for subsidiary components 
  - in model where mean and variance are scaled, both R2 and slope are highest for major fluxes, lower in subsidiary components

**Fig. 3** - bar plot or table showing R2 and slope (preferably with CIs) by variable

## Discussion

H1-

H3- 
- effects of climate on woody growth and other subsidiary components of NPP are complex: ([Friend et al. 2019](https://link.springer.com/content/pdf/10.1007%2Fs13595-019-0819-x.pdf); Helcoski et al. in press)
- factors other than climate are important for determining allocation to subsidiary components

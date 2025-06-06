# Mulchatna Caribou Calf Survival Analysis

This repository contains the complete analysis pipeline for studying calf survival in the Mulchatna Caribou Herd (MCH) in Alaska. The code examines trends in calf weight, survival, and post-capture movement from 2001 to 2021, with a focus on spatial and temporal variation across calving grounds, sexes, and years.

Calf survival and the effects of capture in a migratory barren-ground caribou herd following a population decline
Renae Sattler1, Dominic Demma1, Janna R. Willoughby2 

1 Alaska Department of Fish and Game, Palmer, Alaska, United States of America 
2 College of Forestry, Wildlife, and Environment, Auburn University, Auburn, Alabama, United States of America


## Overview

1. **Calf Weight Trends**: Have calf weights changed over time, and do they differ between eastern and western calving grounds or between sexes?
2. **Survival Trends**: Has calf survival changed over the study period in each calving area?
3. **Survival Predictors**: What are the strongest predictors of calf survival to six months?
4. **Sex Differences**: Is survival probability different between male and female calves?
5. **Winter Survival**: (Planned analysis) What are calf survival rates over the winter period?
6. **Post-Capture Movement and Mortality**: Do captured calves show different movement patterns or survival probabilities in the days following handling?

## Requirements

This project is implemented in R and uses the following R packages:

* `survival`, `survminer` – for survival analysis
* `geosphere` – for geographic distance calculations
* `tidyverse`, `dplyr`, `ggplot2`, `reshape2`, `ggdist`, `broom`, `scales`, `vioplot`, `see`, `gghalves` – for data wrangling and visualization

## Key Analyses

* **Linear models** examine trends in calf weight by year, sex, and calving ground.
* **Kaplan-Meier curves** estimate survival functions by year, sex, and capture status.
* **Cox proportional hazards models** identify key predictors of calf survival.
* **AIC-based model selection** is used to determine the most parsimonious survival model.
* **Movement analyses** compare distances moved post-capture with uncaptured calves using geospatial data.

## Outputs

* Survival plots by sex, year, and calving ground
* Hazard ratio plots showing covariate effects on survival
* Post-capture movement summaries by time interval and age at capture
* AIC-weighted model rankings for predictor importance


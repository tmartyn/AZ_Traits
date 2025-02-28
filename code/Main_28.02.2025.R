## Main file

#clear workspace
rm(list=ls())

#### read in libraries ####
library(lme4)
library(lmerTest)
library(MuMIn)
library(partR2)
library(emmeans)
library(tidyverse)
library(Cairo)
library(ggsci)
library(AlleleShift)
library(spatstat)
library(sp)
library(rgdal)
library(ads)
library(maptools)
library(chron)
library(ggpubr)
library(grid)
library(plotrix)
library(BAT)

####read in data ####
plants<-read.csv("output/Traits.GPS.Comp.26.04.2023c.csv")
comp.all<-read.csv("data/composition.csv")
weath<-read.csv("Weather.Data.NOAA/Jan.1990.to.Dec.2022.csv")

####source functions ####
source("code/population.shift.function_28.02.2025.R")

####source code ####
# boxplot margin plots (supps)
source("code/Margin_plots_28.02.2025.R")
# Density figures for each trait (supps)
source("code/Trait.density.figs_28.02.2025.R")
# composition ordination figure (supps)
source("code/comp_ordination_28.02.2025.R")
# plot map (supps)
source("code/plot_block_map_28.02.2025.R")
# plot weather (supps)
source("code/weather.30.yr_28.02.2025.R")
# plot dot shift figures for mean and CV
source("code/dot.shift_28.02.2025.R")
# trait ordination shift fig
source("code/ordination.shift.fig_28.02.2025.R")
# glms and ANOVAs
source("code/ANOVAs_28.02.2025.R")

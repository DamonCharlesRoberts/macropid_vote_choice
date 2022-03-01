# Title: Constructing macropartisanship ----

# Notes: ----
    #* Description: script for loading anes cumulative time series and then converting it into macropartisan and macro-vote choice series ----
    #* Updated: 2022 - 03 - 01 ----
    #* Updated by: dcr

# Setup: ----
    #* Modularly load functions from packages ----
#install.packages('box')#package used for modularly loading functions
library(anesr)
box::use(
    
)
    #* Load ANES time series cumulative file ----
anescum = data(timeseries_cum)

# Constructing series from cumulative file ----
    #* vote choice ----
    #* pid ----
    #* ideology ----
    #* gender ----
    #* race ----

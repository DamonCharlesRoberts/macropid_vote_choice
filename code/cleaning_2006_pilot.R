# Title: Cleaning from 2006 pilot study ----

# Notes: ----
    #* Description: script for loading 2006 ANES panel and then converting it into macropartisan and macro-vote choice series ----
    #* Updated: 2022 - 03 - 01 ----
    #* Updated by: dcr

# Setup: ----
    #* Modularly load functions from packages ----
#install.packages('box')#package used for modularly loading functions
library(anesr)
box::use(
    dplyr = dplyr[rename, select, mutate, case_when, group_by, summarize]
)
    #* Load ANES 2006 pilot file ----
data(pilot_2006)
pilot_2006 = pilot_2006 |>
    mutate(year = 2006)
# Constructing series from 2006 pilot file ----
    #* vote choice ----
voteDem = pilot_2006 |>
    select(V06P785) |>
    mutate(V06P785 = case_when(V06P785 == 1 ~ 1,
                                V06P785 == 2 ~ 0)) |>
    summarize(voteDem = mean(V06P785, na.rm = TRUE))
    #* pid ----
pid = pilot_2006 |>
    select(V06P680) |>
    mutate(V06P680 = V06P680 + 1,
            V06P680 = ifelse(V06P680 <= 7, V06P680, NA)) |>
    summarize(pid = mean(V06P680, na.rm = TRUE))
    #* ideology ----

    #* gender ----
    
    #* race ----

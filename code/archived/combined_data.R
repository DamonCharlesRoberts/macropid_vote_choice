# Title: Combining the data ----

# Notes: ----
    #* Description: script for sourcing the cleaning scripts, merging the data, then saving it in a time series format ----
    #* Updated: 2022 - 03 - 01 ----
    #* Updated by: dcr

# Setup: ----
    #* Modularly load functions from packages ----
#install.packages('box')#package used for modularly loading functions
box::use(
    haven = haven[read_dta],
    dplyr = dplyr[rename, select, mutate, case_when, group_by, summarize, full_join, bind_rows, arrange],
    purrr = purrr[reduce]
)
    #* Sourcing cleaning scripts ----
source('code/clean_cumulative_file.R')
source('code/cleaning_2006_cces.R')
source('code/cleaning_2010_cces.R')
source('code/cleaning_2018_pilot.R')

    #* Append the datasets ----
combined = bind_rows(combinedCumulative, combined06, combined10, combined18) |>
    arrange(year)

print('ALL merging and cleaning complete!')
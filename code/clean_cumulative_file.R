# Title: Cleaning the ANES Cumulative file 1948 - 2020 ----

# Notes: ----
    #* Description: script for loading anes cumulative time series and then converting it into macropartisan and macro-vote choice series ----
    #* Updated: 2022 - 03 - 01 ----
    #* Updated by: dcr

# Setup: ----
    #* Load ANES time series cumulative file ----
timeseries_cum = read.csv('data/anes_cumulative.csv')
timeseries_cum = timeseries_cum |>
    rename(year = VCF0004)
# Constructing series from cumulative file ----
    #* vote choice ----
voteDem = timeseries_cum |>
    select(VCF0707, year) |>
    mutate(VCF0707 = case_when(VCF0707 == 1 ~ 1,
                                VCF0707 == 2 ~ 0)) |>
    group_by(year) |>
    summarize(voteDem = mean(VCF0707, na.rm = TRUE))
    #* pid ----
pid = timeseries_cum |>
    select(VCF0301, year) |>
    group_by(year) |>
    summarize(pid = mean(VCF0301, na.rm = TRUE))
    #* ideology ----
ideo = timeseries_cum |>
    select(VCF0803, year)|>
    group_by(year) |>
    summarize(ideo = mean(VCF0803, na.rm = TRUE))
    #* gender ----
female = timeseries_cum |>
    select(VCF0104, year) |>
    mutate(VCF0104 = case_when(VCF0104 == 1 ~ 0,
                                VCF0104 == 2 ~ 1)) |>
    group_by(year) |>
    summarize(female = mean(VCF0104, na.rm = TRUE))
    #* race ----
white = timeseries_cum |>
    select(VCF0105b, year) |>
    mutate(VCF0105b = ifelse(VCF0105b == 1, 1, 0)) |>
    group_by(year) |>
    summarize(white = mean(VCF0105b, na.rm = TRUE))
black = timeseries_cum |>
    select(VCF0105b, year) |>
    mutate(VCF0105b = ifelse(VCF0105b == 2, 1, 0)) |>
    group_by(year) |>
    summarize(black = mean(VCF0105b, na.rm = TRUE))
hispanic = timeseries_cum |>
    select(VCF0105b, year) |>
    mutate(VCF0105b = ifelse(VCF0105b == 3, 1, 0)) |>
    group_by(year) |>
    summarize(hispanic = mean(VCF0105b, na.rm = TRUE))

# Merging the series ----
tsList = list(voteDem, pid, ideo, female, white, black, hispanic)
combinedCumulative = tsList |>
    reduce(full_join, by = 'year')
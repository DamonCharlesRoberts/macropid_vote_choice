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
        #** Coded as: 1 vote for democratic house candidate; 0 vote for republican house candidate
voteDem = timeseries_cum |>
    select(VCF0707, year) |>
    mutate(VCF0707 = case_when(VCF0707 == 1 ~ 1,
                                VCF0707 == 2 ~ 0)) |>
    group_by(year) |>
    summarize(voteDem = mean(VCF0707, na.rm = TRUE))
    #* pid ----
        #** Coded as: 1 strong democrat; 7 strong republican
pid = timeseries_cum |>
    select(VCF0301, year) |>
    group_by(year) |>
    summarize(pid = mean(VCF0301, na.rm = TRUE))
    #* ideology ----
        #** Coded as: 1 extremely liberal; 7 extremely conservative
ideo = timeseries_cum |>
    select(VCF0803, year)|>
    group_by(year) |>
    summarize(ideo = mean(VCF0803, na.rm = TRUE))
    #* gender ----
        #** Coded as: 0 male; 1 female
female = timeseries_cum |>
    select(VCF0104, year) |>
    mutate(VCF0104 = case_when(VCF0104 == 1 ~ 0,
                                VCF0104 == 2 ~ 1)) |>
    group_by(year) |>
    summarize(female = mean(VCF0104, na.rm = TRUE))
    #* race ----
        #** Coded as: 1 member of racial group; 0 not member of racial group
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
    #* approval ----
        #** Coded as: 1 approval of presidential performance, 0 disapprove of presidential performance
approval = timeseries_cum |>
    select(VCF0450, year) |>
    mutate(VCF0450 = case_when(VCF0450 == 1 ~ 1,
                                VCF0450 == 2 ~ 0)) |>
    group_by(year) |>
    summarize(approval = mean(VCF0450, na.rm = TRUE))
    #* education ----
        #** Coded as: 1 less than 8th grade; 6 bachelors or higher
education = timeseries_cum |>
    select(VCF0140, year) |>
    mutate(VCF0140 = ifelse(VCF0140 > 6, NA, VCF0140)) |>
    group_by(year) |>
    summarize(education = mean(VCF0140, na.rm = TRUE))
# Merging the series ----
tsList = list(voteDem, pid, ideo, female, white, black, hispanic, approval, education)
combinedCumulative = tsList |>
    reduce(full_join, by = 'year')

print('ANES Cumulative File Cleaning Complete') # let me know that this code has finished running
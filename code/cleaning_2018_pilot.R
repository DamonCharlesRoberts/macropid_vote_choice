# Title: Cleaning from 2018 pilot study ----

# Notes: ----
    #* Description: script for loading 2018 ANES panel and then converting it into macropartisan and macro-vote choice series ----
    #* Updated: 2022 - 03 - 01 ----
    #* Updated by: dcr

# Setup: ----
    #* Load ANES 2018 pilot file ----
data(pilot_2018)
# Constructing series from 2018 pilot file ----
    #* Vote choice ----
voteDem = pilot_2018 |>
    select(house18p) |>
    mutate(house18p = case_when(house18p == 1 ~ 1,
                                house18p == 2 ~ 0)) |>
    summarize(voteDem = mean(house18p, na.rm = TRUE)) |>
    mutate(year = 2018)
    #* pid ----
pid = pilot_2018 |>
    select(pid1r, pid1d, pidstr, pidlean) |>
    mutate(pid = case_when(pid1d == 3 | pid1d == 4 | pid1r == 3 | pid1d == 4 & pidlean == 3 ~ 0, #report indpendent or something else and report not close to either; true independent
                          pid1d == 3 | pid1d == 4 | pid1r == 3 | pid1d == 4 & pidlean == 2 ~ 1, #report independent or something else and report closer to democrats; lean democrat
                          pid1d == 3 | pid1d == 4 | pid1r == 3 | pid1d == 4 & pidlean == 1 ~ -1, #report independent or something else and report closer to republicans; lean republican
                          pid1d == 1 | pid1r == 1 & pidstr == 2 ~ 1, #report democrat and report not very strong
                          pid1d == 1 | pid1r == 1 & pidstr == 1 ~ 2, #report democrat and report strong
                          pid1d == 2 | pid1r == 2 & pidstr == 2 ~ -1, #report republican and report not very strong
                          pid1d == 2 | pid1r == 2 & pidstr == 1 ~ -2 #report republican and report strong
                          )) |>
    summarize(pid = mean(pid, na.rm = TRUE)) |>
    mutate(year = 2018)
    #* ideology ----
ideo = pilot_2018 |>
    select(lcself) |>
    mutate(lcself = ifelse(lcself < 0, NA, lcself)) |>
    summarize(ideo = mean(lcself, na.rm = TRUE)) |>
    mutate(year = 2018)
    #* gender ----
female = pilot_2018 |>
    select(gender) |>
    mutate(female = case_when(gender == 1 ~ 0,
                            gender == 2 ~ 1)) |>
    summarize(female = mean(female, na.rm = TRUE)) |>
    mutate(year = 2018)
    #* race ----
white = pilot_2018 |>
    select(race) |>
    mutate(white = ifelse(race == 1, 1, 0)) |>
    summarize(white = mean(white, na.rm = TRUE)) |>
    mutate(year = 2018)
black = pilot_2018 |>
    select(race) |>
    mutate(black = ifelse(race == 2, 1, 0)) |>
    summarize(black = mean(black, na.rm = TRUE)) |>
    mutate(year = 2018)
hispanic = pilot_2018 |>
    select(race) |>
    mutate(hispanic = ifelse(race == 3, 1, 0)) |>
    summarize(hispanic = mean(hispanic, na.rm = TRUE)) |>
    mutate(year = 2018)

# Save the data ----
tsList = list(voteDem, pid, ideo, female, white, black, hispanic)
combined18 = tsList |>
    reduce(full_join, by = 'year')
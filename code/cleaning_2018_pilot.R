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
        #** Coded as: 1 - voted for democratic house candidate; 0 voted fro republican house candidate
voteDem = pilot_2018 |>
    select(house18p) |>
    mutate(house18p = case_when(house18p == 1 ~ 1,
                                house18p == 2 ~ 0)) |>
    summarize(voteDem = mean(house18p, na.rm = TRUE)) |>
    mutate(year = 2018)
    #* pid ----
        #** Coded as: 1 strong Democrat; 7 strong Republican
pid = pilot_2018 |>
    select(pid1r, pid1d, pidstr, pidlean) |>
    mutate(pid = case_when(pid1d == 3 | pid1d == 4 | pid1r == 3 | pid1d == 4 & pidlean == 3 ~ 4, #report indpendent or something else and report not close to either; true independent
                          pid1d == 3 | pid1d == 4 | pid1r == 3 | pid1d == 4 & pidlean == 2 ~ 3, #report independent or something else and report closer to democrats; lean democrat
                          pid1d == 3 | pid1d == 4 | pid1r == 3 | pid1d == 4 & pidlean == 1 ~ 5, #report independent or something else and report closer to republicans; lean republican
                          pid1d == 1 | pid1r == 1 & pidstr == 2 ~ 2, #report democrat and report not very strong
                          pid1d == 1 | pid1r == 1 & pidstr == 1 ~ 1, #report democrat and report strong
                          pid1d == 2 | pid1r == 2 & pidstr == 2 ~ 6, #report republican and report not very strong
                          pid1d == 2 | pid1r == 2 & pidstr == 1 ~ 7 #report republican and report strong
                          )) |>
    summarize(pid = mean(pid, na.rm = TRUE)) |>
    mutate(year = 2018)
    #* ideology ----
        #** Coded as: 1 very liberal; 7 very conservative
ideo = pilot_2018 |>
    select(lcself) |>
    mutate(lcself = ifelse(lcself < 0, NA, lcself)) |>
    summarize(ideo = mean(lcself, na.rm = TRUE)) |>
    mutate(year = 2018)
    #* gender ----
        #** coded as: 0 male; 1 female
female = pilot_2018 |>
    select(gender) |>
    mutate(female = case_when(gender == 1 ~ 0,
                            gender == 2 ~ 1)) |>
    summarize(female = mean(female, na.rm = TRUE)) |>
    mutate(year = 2018)
    #* race ----
        #** Coded as: 1 member of racial group; 0 not member of racial group
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
    #* approval ----
        #** Coded as: 1 approve of Donald Trump; 0 do not approve
approval = pilot_2018 |>
    select(apppres) |>
    mutate(approval = case_when(apppres <= 3 ~ 1,
                                apppres >= 5 ~ 0)) |>
    summarize(approval = mean(approval, na.rm = TRUE)) |>
    mutate(year = 2018)
    #* education ----
        #** Coded as: 1 no high school; 6 post graduate
education = pilot_2018 |>
    select(educ) |>
    summarize(education = mean(educ, na.rm = TRUE)) |>
    mutate(year = 2018)
# Save the data ----
tsList = list(voteDem, pid, ideo, female, white, black, hispanic, approval, education)
combined18 = tsList |>
    reduce(full_join, by = 'year')

print('2018 ANES Pilot Cleaning Complete')
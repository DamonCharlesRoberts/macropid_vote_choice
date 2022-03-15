# Title: Cleaning the 2006 CCES ----

# Notes: ----
    #* Description: script for loading the 2006 CCES and converting it into a macropartisan and macro-vote choice series ----
    #* Updated: 2022 - 03 - 02 ----
    #* Updated by: dcr

# Setup: ----
    #* Load 2006 CCES file ----
cces06 = read_dta('data/cces_2006.dta')
# Constructing series from 2006 CCES file ----
    #* vote choice ----
        #** Coded as: 1 - Voted for Democratic candidate, 0 voted for Republican candidate ----
voteDem = cces06 |>
    select(v4015) |>
    mutate(v4015 = case_when(v4015 == 1 ~ 1,
                            v4015 == 2 ~ 0)) |>
    summarize(voteDem = mean(v4015, na.rm = TRUE)) |>
    mutate(year = 2006)
    #* pid ----
        #** Coded as: 1 - Strong democrat; 7 Strong Republican
pid = cces06 |>
    select(v3007) |>
    mutate(v3007 = ifelse(v3007 == 8, NA, v3007)) |>
    summarize(pid = mean(v3007, na.rm = TRUE)) |>
    mutate(year = 2006)
    #* ideology ----
        #** Coded as: 1 - Very Liberal; 5 Very conservative
ideo = cces06 |>
    select(v2021) |>
    mutate(v2021 = ifelse(v2021 == 6, NA, v2021)) |>
    summarize(ideo = mean(v2021, na.rm = TRUE)) |>
    mutate(year = 2006)
    #* gender ----
        #** Coded as: 1 - Female; 0 male
female = cces06 |>
    select(v2004) |>
    mutate(female = case_when(v2004 == 1 ~ 0,
                            v2004 == 2 ~ 1)) |>
    summarize(female = mean(female, na.rm = TRUE)) |>
    mutate(year = 2006)
    #* race ----
        #** Coded as: 1 racial group; 0 not-racial group
white = cces06 |>
    select(v2005) |>
    mutate(white = ifelse(v2005 == 1, 1, 0)) |>
    summarize(white = mean(white, na.rm = TRUE)) |>
    mutate(year = 2006)
black = cces06 |>
    select(v2005) |>
    mutate(black = ifelse(v2005 == 2, 1, 0)) |>
    summarize(black = mean(black, na.rm = TRUE)) |>
    mutate(year = 2006)
hispanic = cces06 |>
    select(v2005) |>
    mutate(hispanic = ifelse(v2005 == 3, 1, 0)) |>
    summarize(hispanic = mean(hispanic, na.rm = TRUE)) |>
    mutate(year = 2006)
    #* approval ----
        #** Coded as: 1 approve, 0 disapprove
approval = cces06 |>
    select(v3003) |>
    mutate(approval = case_when(v3003 <= 2 ~ 1,
                                v3003 == 3 | v3003 == 4 ~ 0)) |>
    summarize(approval = mean(approval, na.rm = TRUE)) |>
    mutate(year = 2006)
    #* education ----
        #** Coded as: 1 did not graduate highschool; 6 postgraduate college degree
education = cces06 |>
    select(v2018) |>
    summarize(education = mean(v2018, na.rm = TRUE)) |>
    mutate(year = 2006)

# Merging the series ----
tsList = list(voteDem, pid, ideo, female, white, black, hispanic, approval, education)
combined06 = tsList |>
    reduce(full_join, by = 'year')

print('2006 CCES Cleaning Complete')
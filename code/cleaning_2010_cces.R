# Title: Cleaning the 2010 CCES ----

# Notes: ----
    #* Description: script for loading the 2010 CCES and converting it into a macropartisan and macro-vote choice series ----
    #* Updated: 2022 - 03 - 02 ----
    #* Updated by: dcr

# Setup: ----
    #* Load 2010 CCES file ----
cces10 = read_dta('data/cces_2010.dta')
# Constructing series from 2010 CCES file ----
    #* vote choice ----
        #** Coded as: 1 - voted for democratic candidate in House; 0 - voted for Republican candidate in House
    voteDem = cces10 |>
        select(CC412) |>
        mutate(CC412 = case_when(CC412 == 1 ~ 1,
                                CC412 == 2 ~ 0)) |>
        summarize(voteDem = mean(CC412, na.rm = TRUE)) |>
        mutate(year = 2010)
    #* pid ----
        #** Coded as: 1 - strong Democrat; 7 - strong Republican
    pid = cces10 |>
        select(V212d) |>
        mutate(V212d = ifelse(V212d == 98 | V212d == 99 | V212d == 8, NA, V212d)) |>
        summarize(pid = mean(V212d, na.rm = TRUE)) |>
        mutate(year = 2010)
    #* ideology ----
        #** Coded as: 1 - very liberal; 7 - very conservative
    ideo = cces10 |>
        select(CC334A) |>
        mutate(CC334A = ifelse(CC334A >= 8, NA, CC334A)) |>
        summarize(ideo = mean(CC334A, na.rm = TRUE)) |>
        mutate(year = 2010)
    #* gender ----
        #** Coded as: 0 male; 1 female
    female = cces10 |>
        select(V208) |>
        mutate(V208 = case_when(V208 == 2 ~ 1,
                                V208 == 1 ~ 0)) |>
        summarize(female = mean(V208, na.rm = TRUE)) |>
        mutate(year = 2010)
    #* race ----
        #** Coded as: 1 member of racial group; 0 not member of racial group
    white = cces10 |>
        select(V211) |>
        mutate(white = ifelse(V211 == 1, 1, 0)) |>
        summarize(white = mean(white, na.rm = TRUE)) |>
        mutate(year = 2010)
    black = cces10 |>
        select(V211) |>
        mutate(black = ifelse(V211 == 2, 1, 0)) |>
        summarize(black = mean(black, na.rm = TRUE)) |>
        mutate(year = 2010)
    hispanic = cces10 |>
        select(V211) |>
        mutate(hispanic = ifelse(V211 == 3, 1, 0)) |>
        summarize(hispanic = mean(hispanic, na.rm = TRUE)) |>
        mutate(year = 2010)
    #* approval ----
        #** Coded as: 1 approval of president; 0 disapprove of president
    approval = cces10 |>
        select(CC308a) |>
        mutate(approval = case_when(CC308a <= 2 ~ 1,
                                    CC308a == 3 | CC308a == 4 ~ 0)) |>
        summarize(approval = mean(approval, na.rm = TRUE)) |>
        mutate(year = 2010)
    #* education ----
        #** Coded as: 1 did not graduate from highschool; 5 post-graduate degree
    education = cces10 |>
        select(V213) |>
        summarize(education = mean(V213, na.rm = TRUE)) |>
        mutate(year = 2010)
# Merging the series ----
tsList = list(voteDem, pid, ideo, female, white, black, hispanic, approval, education)
combined10 = tsList |>
    reduce(full_join, by = 'year')

print('2010 CCES Cleaning Complete') # let me know this code has finished runnning
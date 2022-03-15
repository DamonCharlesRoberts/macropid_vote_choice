# Title: Superiority of PID Primary Analysis File -----

# Notes: ----
    #* Description: Script for the primary analyses to examine the predictive superiority of partisan identification on vote choice
    #* Updated: 2022 - 03 - 18
    #* Updated by: dcr 

# Setup: ----
    #* Modularly load functions from packages
#install.packages('box')#package used for modularly loading functions from packages
box::use(
    ggplot2 = ggplot2[ggplot, geom_line, aes, labs, theme_bw],
    dplyr = dplyr[filter]
)
    #* Source script that cleans all of the datasets and will load the series
source('code/combined_data.R')
ts = combined #rename the resulting dataframe and call it ts

# Hunting NA's ----
voteDemNaN = subset(ts, ts$voteDem == 'NaN') # missing 1948 and 1954
pidNaN = subset(ts, ts$pid == 'NaN') #missing 1948
ideoNaN = subset(ts, ts$ideo == 'NaN') #missing prior to 1972
femaleNaN = subset(ts, ts$female == 'NaN')#no missing
whiteNaN = subset(ts, ts$white == 'NaN')#missing in 1954
blackNaN = subset(ts, ts$black == 'NaN')#missing in 1954
hispanicNaN = subset(ts, ts$hispanic == 'NaN')#missing in 1954
approvalNaN = subset(ts, ts$approval == 'NaN')
educationNaN = subset(ts, ts$education == 'NaN')
    #* Note: Most NA's come before 1972... filter those out ----
ts = ts |>
    filter(year >= 1972)

# Plot the series ---
seriesPlot = ggplot(data = ts) +
    geom_line(aes(x = year, y = voteDem), linetype = 1) +
    geom_point(aes(x = year, y = voteDem), shape = 1) +
    geom_line(aes(x = year, y = pid), linetype = 2) +
    geom_point(aes(x = year, y = pid), shape = 1) +
    geom_line(aes(x = year, y = ideo), linetype = 3) +
    geom_point(aes(x = year, y = ideo), shape = 1) + 
    geom_line(aes(x = year, y = female), linetype = 1) +
    geom_point(aes(x = year, y = female), shape = 2) +
    geom_line(aes(x = year, y = white), linetype = 2) +
    geom_point(aes(x = year, y = white), shape = 2) +
    geom_line(aes(x = year, y = black), linetype = 3) +
    geom_point(aes(x = year, y = black), shape = 2) +
    geom_line(aes(x = year, y = hispanic), linetype = 1) +
    geom_point(aes(x = year, y = hispanic), shape = 3) +
    geom_line(aes(x = year, y = approval), linetype = 2) +
    geom_point(aes(x = year, y = approval), shape = 3) +
    geom_line(aes(x = year, y = education), linetype = 3) +
    geom_point(aes(x = year, y = education), shape = 3) +
    theme_bw() +
    labs(y = '', x = 'Year', caption = 'Data Source: American National Election Studies and Cooperative Congressional Election Studies')
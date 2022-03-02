# Title: Cleaning the 2010 CCES ----

# Notes: ----
    #* Description: script for loading the 2010 CCES and converting it into a macropartisan and macro-vote choice series ----
    #* Updated: 2022 - 03 - 02 ----
    #* Updated by: dcr

# Setup: ----
    #* Load 2010 CCES file ----
cces06 = read_dta('data/cces_2010.dta')
# Constructing series from 2010 CCES file ----
    #* vote choice ----
    #* pid ----
    #* ideology ----
    #* gender ----
    #* race ----
# Merging the series ----
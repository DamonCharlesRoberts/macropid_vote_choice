# Title: Cleaning the 2014 CCES ----

# Notes: ----
    #* Description: script for loading the 2014 CCES and converting it into a macropartisan and macro-vote choice series ----
    #* Updated: 2022 - 03 - 15 ----
    #* Updated by: dcr

# Setup: ----
    #* Load 2014 CCES file ----
cces14 = read_dta('data/cces_2014.dta')
# Constructing series from 2010 CCES file ----
    #* vote choice ----
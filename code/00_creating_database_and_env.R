# Title: Creating the database and conda environment

# Notes:
    #* Description: R script to create duckDB database with all the datasets for the project
    #* Updated: 2022-11-05
    #* Updated by: dcr

# Bash commands to create env
## conda search -c conda-forge r-base
## conda create --prefix=~/pid_moneyballing_env r-base=4.2.2

# Install packages
## install.packages(c('box', 'DBI','duckdb'))

# Modularly load functions
box::use(
    haven = haven[read_dta], # to load dta files
    DBI = DBI[dbConnect,dbExecute], # to query the database
    duckdb = duckdb[duckdb], # to connect to the database
    dplyr = dplyr[mutate, case_when, select] # to clean datasets

)

# Create empty list object to store data
data <- list()

# Create database

db = dbConnect(duckdb("data/pid_moneyballing"))

# Create tables
    #* Load data
data[["anes_cumulative"]] <- read_dta("data/anes_2020_cumulative.dta")
arrow::write_parquet(data[["anes_cumulative"]], "data/anes_cumulative.parquet")

data[["anes_2018"]] <- read_dta("data/anes_pilot_2018.dta")
arrow::write_parquet(data[["anes_2018"]], "data/anes_2018.parquet")

data[['cces_2006']] <- read_dta("data/cces_2006.dta")
arrow::write_parquet(data[["cces_2006"]], "data/cces_2006.parquet")

data[["cces_2010"]] <- read_dta("data/cces_2010.dta")
arrow::write_parquet(data[["cces_2010"]], "data/cces_2010.parquet")

data[["cces_2014"]] <- read_dta("data/cces_2014.dta")
arrow::write_parquet(data[["cces_2014"]], "data/cces_2014.parquet")

    #* Create tables

dbExecute(db, "CREATE OR REPLACE TABLE anes_cumulative AS SELECT * FROM 'data/anes_cumulative.parquet'")

dbExecute(db, "CREATE OR REPLACE TABLE anes_2018 AS SELECT * FROM 'data/anes_2018.parquet'")

dbExecute(db, "CREATE OR REPLACE TABLE cces_2006 AS SELECT * FROM 'data/cces_2006.parquet'")

dbExecute(db, "CREATE OR REPLACE TABLE cces_2010 AS SELECT * FROM 'data/cces_2010.parquet'")

dbExecute(db, "CREATE OR REPLACE TABLE cces_2014 AS SELECT * FROM 'data/cces_2014.parquet'")

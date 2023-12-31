## This script runs all code related to gymnastics case study. 
## It reads data and generates all outputs. 
## main.R expects get.data.R, prep.data.R, fit.model.R and get.best.teams.R to be in the same directory level. It will import these files to create outputs.

## this script expects the current working directory to be the folder in which this file is located

# get.data.R data in the same directory level, inside which contains "data_2017_2021.csv" and "data_2022_2023.csv".
source('get.data.R')

## prep.data.R will output the cleaned data, "cleaned_combined_data.csv," into the data/ folder
source('prep.data.R')

## fit.model.R will output the per-athlete per-apparatus means and stddevs of scores into "means_per_app.csv" and "stddevs_per_app.csv" respectively into the data/ folder
source('fit.model.R')

## final script to get best teams and the US team simulations for all reasonable combinations 
source('get.best.teams.R')# outputs 2 lists (men & women) of 2 data frames each,
                          # one is optimized top 12 teams, other is all
                          # simulation results of country of interest (default: USA)
                          # with apparatus-athlete pairs and descending order of preference
                          # i.e. team on top is best

print("finished")


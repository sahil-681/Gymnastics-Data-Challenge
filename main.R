## This script runs all code related to gymnastics case study. 
## It reads data and generates all outputs. 
## main.R expects get.data.R, prep.data.R, and fit.model.R to be in the same directory level. It will import these files to create outputs.

## this script expects the current working directory to be the folder in which this file is located
 
# get.data.R data in the same directory level, inside which contains "data_2017_2021.csv" and "data_2022_2023.csv".
source('get.data.R')

## prep.data.R will output the cleaned data, "cleaned_combined_data.csv," into the data/ folder
source('prep.data.R')

## fit.model.R will output the per-athlete per-apparatus means and stddevs of scores into "means_per_app.csv" and "stddevs_per_app.csv" respectively into the data/ folder
source('fit.model.R')

print("finished")

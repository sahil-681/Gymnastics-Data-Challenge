
<!-- README.md is generated from README.Rmd. Please edit that file -->

# README

## Important Links:

- Github Repo: <https://github.com/sahil-681/Gymnastics-Data-Challenge>

- R Shiny App: <https://sahil-singh-6.shinyapps.io/gymnastics>

## Introduction:

The goal of this analysis and Shiny pap is to navigate the multifaceted
challenges of selecting the most optimum team to pick for the 2024 Paris
Olympics Gymnastics Event. Our tool is built on the comprehensive
dataset provided, and takes the help of a plethora of R scripts written
by us to analyze and utilize past competition statistics and current
performance trends in the end goal of building the interactive
application which can be used by the end user easily.

This file outlines the purpose of each script present in our submission.

## Key components of the repository:

- app.R: Integrates various components of the application, including
  user interface elements and sourcing other scripts, to create the main
  application for gymnastics team selection.

- fit.model.R: Computes mean scores and standard deviations for athletes
  across different apparatuses, likely as a preliminary step for model
  fitting or data analysis.

- get_default_assignments.R: Defines a function to assign default
  athlete selections for teams based on top performances, segregated by
  gender and apparatus.

- get_reasonable_set.R: Implements a function to select a reasonable set
  of competitors for each country and gender, considering performance
  data and a specified number of top performers.

- get.best.teams.R: Processes data to identify and recommend the best
  gymnastics teams, taking into account factors like team performance
  and athlete compatibility. This script runs all of the simulations

- get.data.R: Loads gymnastics data from CSV files for the years
  2017-2021 and 2022-2023.

- main.R: A master script that orchestrates the entire gymnastics case
  study. It sources and executes other scripts like get.data.R,
  prep.data.R, fit.model.R, and get.best.teams.R to generate all
  outputs.

- prep.data.R: Contains functions for data preprocessing, including date
  processing and data cleaning, to prepare gymnastics datasets for
  analysis and modeling.

- qual_names_aa.R: Reads and combines athlete data from various sources
  to create a consolidated list of athletes eligible for all-around
  events, including individual and team qualifiers.

- qual_names_e.R: Prepares a dataset of athletes eligible for event
  qualifications, including detailed scores across different
  apparatuses, used for further analysis in event-specific
  qualifications.

- qual_names_t.R: Processes and filters athlete data to determine top
  performers for team qualifications, focusing on aggregate scores
  across different apparatuses.

- query.existing.database.R: Implements functions to modify data weights
  based on medal standings and to exclude specific individuals from
  datasets for analysis purposes.

- run_sims.R: Defines a function to tally medals based on simulation
  results, integrating data from various sources to produce a
  comprehensive medal count.

- simulate_medals.R: Simulates medal outcomes for gymnastics events
  using specified teams and athlete data, integrating default
  assignments and simulation logic.

- styles.css: A Cascading Style Sheets (CSS) file that defines the
  styling rules for the application’s user interface. It includes font
  size adjustments for various screen sizes to ensure responsive and
  visually appealing design across different devices.

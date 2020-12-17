## Table of contents
* [General info](#general-info)
* [Setup](#setup)
* [Data Information](#Data-Information)

## General info: MC_EnsembleCompleteness
In this study we compare the three algorithms for the generation of conformer ensembles Biovia BEST, Schrödinger Prime macrocycle sampling (PMM) and Conformator (CONF) form the University of Hamburg, with ensembles derived from exhaustive molecular dynamics simulations applied to a dataset of 7 small macrocycles in two charge states and three solvents.

## Setup: Start Shiny App
To start the Shiny App you need to open R. The shiny App needs the loaded input to be named PCA_results_all_data
The R object holds the standard information after an PCA as well as the complete data table of data projection onto this space coming from conformer generators etc.
```
setwd('program/') #go to the folder where MC_server.R and MC_ui.R are located
source('MC_server.R')
source('MC_ui.R')
setwd('data/')
PCA_results_all_data=readRDS('PCA_results_all_data.rds')
shiny::shinyApp(ui,server)
```
## Data Information
We have completed long MD runs which were done over 10ns which are not included within the uploaded data set, as well as all Simulated Annelaing Runs, as well as the 500K MD run. Morever only PC1 to PC8 are given due to size restrictions. In case you have interest in such data email to leaseep@gmail.com and the full data will be provided.

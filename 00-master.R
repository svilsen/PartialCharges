#### Libraries ----
# 
library("caret")
library("ranger")
library("kernlab")
library("keras")

# 
library("ggcorrplot")
library("minerva")
library("vip")

#
library("moments")
library("MASS")
library("Rcpp")
library("tidyverse")

#### Paths ---- 
data_dir <- "Data"
files_dir <- "Files"

#### Cleaning data ----
#
extract_capacity <- FALSE

#
source("01-extractcapacity.R")

#### Extracting features ----
#
extract_features <- FALSE

#
source("Functions/feature_functions.R")
source("02-extractfeatures.R")

#### Create and train models ----
#
create_trainingset <- FALSE

#
train_mlr_models <- FALSE
train_svr_models <- FALSE
train_rf_models <- FALSE
train_nn_models <- FALSE

#
source("Functions/training_functions.R")
source("03-trainmodels.R")

#### Validate models ----
#
theme_set(theme_bw(base_size = 15))

make_figures <- FALSE
create_vi <- FALSE

#
source("04-validatemodels.R")

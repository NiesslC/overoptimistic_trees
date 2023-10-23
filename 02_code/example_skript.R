
setwd("/nfsmb/shares/cmmgrp/Companion/luzia_hanssum/code")
load("../data/data_ba_thesis.RData")

# Load packages
library(tidyverse) # many useful packages such as ggplot, dplyr etc
library(mlr3) # package for machine learning, see https://mlr3book.mlr-org.com/
library(mlr3viz) # for mlr3 plots
library(mlr3tuning) # for mlr3tuning
library(stringr)
library(ggparty) # (habe sonst fehler dafür bekommen) (Luzia)

# added 2023-03-16:
library(mlr3pipelines)
library(rpart.plot)


# Load the three CART functions 
source("functions.R")
# Try functions (with example arguments) -----------------------------------------------------------

# A) CART without hyperparameter tuning (= no resampling) ----
cart_no_resampling_fct(data_train = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "train"),
                       data_validation = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "validation"),
                       features = c("AKPS", "alter"),
                       eval_criterion = "regr.rsq",
                       cp = 0.01, 
                       minbucket = 20,
                       plot = TRUE)
# no plot, only values
cart_no_resampling_fct(data_train = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "train"),
                       data_validation = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "validation"),
                       features = c("AKPS", "alter"),
                       eval_criterion = "regr.rsq",
                       cp = 0.01, 
                       minbucket = 20,
                       maxdepth = 4,
                       plot = FALSE)

# no validation set 
cart_no_resampling_fct(data_train = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "train"),
                      # data_validation = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "validation"),
                       features = c("AKPS", "alter"),
                       eval_criterion = "regr.rsq",
                       cp = 0.01, 
                       minbucket = 20,
                       maxdepth = 4,
                       plot = FALSE)

# B) CART with hyperparameter tuning (= resampling) ----
set.seed(123) # set this exact seed before every call of the function!!! (ensures same splits)
cart_resampling_fct(data_train = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "train") ,
                    data_validation = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "validation"),
                    features = c("AKPS", "alter", "IPOS_schmerzen", "IPOS_atemnot"),
                    eval_criterion = "regr.rsq",
                    plot = FALSE)
# same result but show plot 
set.seed(123) # set this exact seed before every call of the function!!! (ensures same splits)
cart_resampling_fct(data_train = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "train") ,
                    data_validation = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "validation"),
                    features = c("AKPS", "alter", "IPOS_schmerzen", "IPOS_atemnot"),
                    eval_criterion = "regr.rsq",
                    plot = TRUE)

# C) CART with nested resampling (note: rather long runtime) ---- 
set.seed(123) # set this exact seed before every call of the function!!! (ensures same splits)
cart_nested_resampling_fct(data_train = data_tree %>% filter(setting == "SAPV" & allocation_thesis == "train") ,
                                  features = c("AKPS", "alter", "IPOS_schmerzen", "IPOS_atemnot"),
                                  eval_criterion = "regr.rsq")


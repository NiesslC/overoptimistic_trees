# Load packages ------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(mlr3)
library(mlr3pipelines)
library(mlr3misc)
library(mlr3tuning)
library(paradox)
library(R6)
library(party) # for reemctree function
library(nlme) # for reemctree function
#library(rpart.plot)

# Load data and functions --------------------------------------------------------------------------
load("./01_data/data_phaselevel.RData")
source("./02_code/_fcts_preproc.R")
source("./02_code/_fcts_resampling.R")
source("./02_code/_fcts_optim.R")
source("./02_code/_fcts_mlrlearners_helpers.R")

source("./02_code/_src_add_mlrpipeops.R")
source("./02_code/_src_add_mlrlearners.R")

# Set up learners (= algorithms) with corresponding hps --------------------------------------------
source("./02_code/_src_setup_learners.R")
# -> returned objects: learners_default and learners_hp_searchspace_default
learner_names = names(learners_default)

# Set up preprocessing pipeline with corresponding hps ---------------------------------------------

# Preprocessing pipeline
# (Note: in the pipeline %>>%, preproc.target need to be before preproc.drop.targetout and
#  preproc.drop.iposca before preproc.feature.ipos)
preproc_default = po("preproc.target") %>>%
  po("preproc.drop.targetout") %>>%
  po("preproc.drop.iposca") %>>%
  po("preproc.feature.ipos") %>>% 
  po("preproc.feature.age") %>>% 
  po("preproc.feature.akps")%>>% 
  po("fixfactors.overwhelmingly") # if overwhelmingly was not present in train for ipos_pain or ipos_shortness_breath,
                                  # it is set to "severly" for prediction (only occurs for nested resampling)                                  

#examplegraph =preproc_default %>>% lrn("regr.rpart", id = "learner")
#examplegraph$plot(html = TRUE)

# Search space preprocessing hps
preproc_hp_searchspace_default = ps(
  preproc.target.option = p_fct(c("A", "B", "C")),
  preproc.drop.targetout.option = p_fct(c("A", "B", "C", "D")),
  preproc.drop.iposca.option = p_fct(c("A", "B", "C", "D")),
  preproc.feature.ipos.option = p_fct(c("A", "B", "C", "D")),
  preproc.feature.age.option = p_fct(c("A", "B")),
  preproc.feature.akps.option = p_fct(c("A", "B"))
) 

# Order for stepwise optimization 
preproc_hp_stepopt_order = c("preproc.target", "preproc.feature.ipos", "preproc.feature.age", "preproc.feature.akps",
                             "preproc.drop.targetout", "preproc.drop.iposca")
preproc_hp_stepopt_order = paste0(preproc_hp_stepopt_order, ".option") 
# (add ".option" because will access preproc_hp_searchspace with this vector)

# Simulation parameters  ---------------------------------------------------------------------------
# Number of simulated train/test datasets
nrep = 50

# Possible settings
setting_names = c("sapv", "pmd", "station")

# Tuning parameters 
resampling_parameters = list(
  eval_criterion = "regr.rmse", # "regr.rsq", 
  n_evals_learn_hp = 50,
  n_evals_learnandpreproc_hp = 500,
  folds_cv = 5, 
  inner_folds_nestedcv = 2, 
  outer_folds_nestedcv = 5,
  outer_repeats = 1, 
  seed_resampling = 1705410730,
  seed_nestedresampling = 1705419930
)

# Optimization procedures 
procedure_list = list(
  ## learner.hyperparam: manually select value most prone to overfitting, preproc: try all combinations, error: apparent
  "p0" = "learner.hp.maxoverfit_preproc.hp.allcombinations_error.apparent",
  ## learner.hyperparam: default, preproc.hyperparam: resampling, error: apparent+resampling+nested
  "p1" = "learner.hp.tune_preproc.hp.default",
  ## learner.hyperparam: resampling, preproc.hyperparam: stepwise optimization, error: apparent
  "p2a" = "learner.hp.tune_preproc.hp.steopt_error.apparent",
  ## learner.hyperparam: resampling, preproc.hyperparam: stepwise optimization, error: resampling
  "p2b" = "learner.hp.tune_preproc.hp.steopt_error.resampling",
  ## learner.hyperparam: resampling, preproc.hyperparam: stepwise optimization, error: nested resampling
  "p2c" = "learner.hp.tune_preproc.hp.steopt_error.nested_resampling",
  ## learner.hyperparam: resampling, preproc.hyperparam: resampling, error: apparent+resampling+nested
  "p3" = "learner.hp.tune_preproc.hp.tune"
)


# Simulate random allocation -----------------------------------------------------------------------
set.seed(1698072152)
id_train_list = 1:nrep %>% map(function(x) data_phaselevel %>% distinct(companion_id,.keep_all = TRUE) %>%  
                                 group_by(team_id) %>% # split within each team_id
                                 slice_sample(prop = 0.5) %>% .$companion_id)
save(id_train_list, file = "./03_results/rdata/id_train_list.RData")

# Run optimization ---------------------------------------------------------------------------------

## setting: sapv, leaner: cart, procedure = p0 ---- 
setting_name = setting_names[1]
learner_name = learner_names[1]
procedure = procedure_list$p0

1:nrep %>% purrr::walk(.f = function(x) {
  optim_fct(rep = x,
            data = data_phaselevel,
            id_train_list = id_train_list, 
            setting_name = setting_name,
            learner_name = learner_name, 
            learners_default = learners_default, 
            learners_hp_searchspace_default = learners_hp_searchspace_default,
            preproc_default = preproc_default, 
            preproc_hp_searchspace_default = preproc_hp_searchspace_default, 
            preproc_hp_stepopt_order = preproc_hp_stepopt_order,
            procedure = procedure, 
            procedure_list = procedure_list,
            resampling_parameters = resampling_parameters)
})

## setting: sapv, leaner: cart, procedure = p1 ---- 
setting_name = setting_names[1]
learner_name = learner_names[1]
procedure = procedure_list$p1

1:nrep %>% purrr::walk(.f = function(x) {
  optim_fct(rep = x,
            data = data_phaselevel,
            id_train_list = id_train_list, 
            setting_name = setting_name,
            learner_name = learner_name, 
            learners_default = learners_default, 
            learners_hp_searchspace_default = learners_hp_searchspace_default,
            preproc_default = preproc_default, 
            preproc_hp_searchspace_default = preproc_hp_searchspace_default, 
            preproc_hp_stepopt_order = preproc_hp_stepopt_order,
            procedure = procedure, 
            procedure_list = procedure_list,
            resampling_parameters = resampling_parameters)
})


## setting: sapv, leaner: cart, procedure = p2a ---- 
setting_name = setting_names[1]
learner_name = learner_names[1]
procedure = procedure_list$p2a

1:nrep %>% purrr::walk(.f = function(x) {
  optim_fct(rep = x,
            data = data_phaselevel,
            id_train_list = id_train_list, 
            setting_name = setting_name,
            learner_name = learner_name, 
            learners_default = learners_default, 
            learners_hp_searchspace_default = learners_hp_searchspace_default,
            preproc_default = preproc_default, 
            preproc_hp_searchspace_default = preproc_hp_searchspace_default, 
            preproc_hp_stepopt_order = preproc_hp_stepopt_order,
            procedure = procedure, 
            procedure_list = procedure_list,
            resampling_parameters = resampling_parameters)
})



## setting: sapv, leaner: cart, procedure = p2b ---- 
setting_name = setting_names[1]
learner_name = learner_names[1]
procedure = procedure_list$p2b

1:nrep %>% purrr::walk(.f = function(x) {
  optim_fct(rep = x,
            data = data_phaselevel,
            id_train_list = id_train_list, 
            setting_name = setting_name,
            learner_name = learner_name, 
            learners_default = learners_default, 
            learners_hp_searchspace_default = learners_hp_searchspace_default,
            preproc_default = preproc_default, 
            preproc_hp_searchspace_default = preproc_hp_searchspace_default, 
            preproc_hp_stepopt_order = preproc_hp_stepopt_order,
            procedure = procedure, 
            procedure_list = procedure_list,
            resampling_parameters = resampling_parameters)
})

## setting: sapv, leaner: cart, procedure = p2c ---- 
setting_name = setting_names[1]
learner_name = learner_names[1]
procedure = procedure_list$p2c

1:nrep %>% purrr::walk(.f = function(x) {
  optim_fct(rep = x,
            data = data_phaselevel,
            id_train_list = id_train_list, 
            setting_name = setting_name,
            learner_name = learner_name, 
            learners_default = learners_default, 
            learners_hp_searchspace_default = learners_hp_searchspace_default,
            preproc_default = preproc_default, 
            preproc_hp_searchspace_default = preproc_hp_searchspace_default, 
            preproc_hp_stepopt_order = preproc_hp_stepopt_order,
            procedure = procedure, 
            procedure_list = procedure_list,
            resampling_parameters = resampling_parameters)
})

## setting: sapv, leaner: cart, procedure = p3 ---- 
setting_name = setting_names[1]
learner_name = learner_names[1]
procedure = procedure_list$p3

1:nrep %>% purrr::walk(.f = function(x) {
  optim_fct(rep = x,
            data = data_phaselevel,
            id_train_list = id_train_list, 
            setting_name = setting_name,
            learner_name = learner_name, 
            learners_default = learners_default, 
            learners_hp_searchspace_default = learners_hp_searchspace_default,
            preproc_default = preproc_default, 
            preproc_hp_searchspace_default = preproc_hp_searchspace_default, 
            preproc_hp_stepopt_order = preproc_hp_stepopt_order,
            procedure = procedure, 
            procedure_list = procedure_list,
            resampling_parameters = resampling_parameters)
})

# Add featureless learner results for each repetition ----------------------------------------------
setting_name = setting_names[1]
1:nrep %>% purrr::walk(.f = function(x) {
  procedure_featureless_fct(rep = x,
                            data = data_phaselevel,
                            id_train_list = id_train_list, 
                            setting_name = setting_name,
                            preproc_default = preproc_default,
                            resampling_parameters = resampling_parameters)
})

# To Do: -------------------------------------------------------------------------------------------
# - Extract errors [(i) reported error and (ii) test set error] (results aufbereiten als datensatz)
# - check that correct vars have been used via graph_learner_tuned$model$lrn_glmertree_if$model$formula
# t =res_sapv_p1[[1]]$graph_learner_tuned
# t$graph_learner_tuned$param_set
# t$model$lrn_rpart$train_task

# again check that same id train test splits were used 

# - Function descriptions
# - Implement procedure where learner choice is also tunable 
# - Do we need all functions in learner_helpers benötigt?
# - Add info on R package versions (also for those called when using invoke()?)
# - glmertree_if singular boundary warning, other algorithms also affected? store warnings somewhere?
# - Check behavior of learners when new (ordered) factors in test data 
#   -> need fixfactors pipeop? (but currently throws error)
#   -> behavior of learners when missing values (resulting from fixfactors)


# Note on po("fixfactors")
# "Fixes factors of type factor, ordered: Makes sure the factor levels during prediction are the same
#  as during training; possibly dropping empty training factor levels before. 
#  Note this may introduce missing values during prediction if unseen factor levels are found."
# -> in case of rpart, this does not lead to missing values during prediction; instead, rpart
#    basically interpolates or extrapolates; it seems as if the surrogate splits are not used (?)

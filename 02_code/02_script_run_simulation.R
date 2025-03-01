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
toy = TRUE
if(toy == TRUE){
  # run with toy data (only for setting SAPV)
  load("./01_data/data_phaselevel_toy.RData")
  data_phaselevel = data_phaselevel_toy
  rm(data_phaselevel_toy)
} else{
  # run with real data
  load("./01_data/data_phaselevel.RData")
}
source("./02_code/_fcts_preproc.R")
source("./02_code/_fcts_resampling.R")
source("./02_code/_fcts_optim.R")
source("./02_code/_src_add_mlrpipeops.R")
source("./02_code/_src_add_mlrlearners.R")

# Set up learners (= algorithms) with corresponding hps --------------------------------------------
source("./02_code/_src_setup_learners.R")
# -> returned objects: learners_default and learners_hp_searchspace_default

# Set up preprocessing pipeline with corresponding hps ---------------------------------------------

# Preprocessing pipeline
# (Note: in the pipeline %>>%, preproc.target need to be before preproc.drop.targetout and
#  preproc.drop.iposca before preproc.feature.ipos)
preproc_default = po("preproc.target", option = "A") %>>%
  po("preproc.drop.targetout") %>>%
  po("preproc.drop.iposca") %>>%
  po("preproc.feature.ipos") %>>% 
  po("preproc.feature.age") %>>% 
  po("preproc.feature.akps") %>>% 
  po("fixfactors.overwhelmingly") %>>% # if overwhelmingly is not present in train for ipos_pain or ipos_shortness_breath, it is set to "severly" for prediction (only occurs for nested resampling) 
  po("fixfactors.age", affect_columns = selector_name("age")) %>>% # if age = [21,50]  is not present in predict, it is collapsed with (50,60] (only occurs for some resampling iterations)                              
  po("fixfactors.akps", affect_columns = selector_name("akps")) %>>% # if akps = 70_80_90  is not present in predict, it is collapsed with 60 (only occurs for some resampling iterations)                              
  po("fixfactors.cogn_agitation", affect_columns = selector_name("cogn_agitation")) # if cogn_agitation = severe  is not present in predict, it is collapsed with moderate (only occurs for 4 resampling iterations in p3) 

#examplegraph =preproc_default %>>% lrn("regr.rpart", id = "learner")
#examplegraph$plot(html = TRUE)

# Search space preprocessing hps
preproc_hp_searchspace_default = ps(
  preproc.drop.targetout.option = p_fct(c("A", "B", "C", "D")),
  preproc.drop.iposca.option = p_fct(c("A", "B", "C", "D")),
  preproc.feature.ipos.option = p_fct(c("A", "B", "C", "D")),
  preproc.feature.age.option = p_fct(c("A", "B")),
  preproc.feature.akps.option = p_fct(c("A", "B"))
) 

# Order for stepwise optimization 
preproc_hp_stepopt_order = c("preproc.feature.ipos", "preproc.feature.age", "preproc.feature.akps",
                             "preproc.drop.targetout", "preproc.drop.iposca")
preproc_hp_stepopt_order = paste0(preproc_hp_stepopt_order, ".option") 
# (add ".option" because will access preproc_hp_searchspace with this vector)

# Simulation parameters  ---------------------------------------------------------------------------
# 1) Setting
setting_name  = "sapv" # others possible settings: "pmd", "station"

# 2) Tuning parameters 
resampling_parameters = list(
  terminator_k = 30,
  folds_cv = 10, 
  inner_folds_nestedcv = 2, 
  outer_folds_nestedcv = 10,
  seed_resampling = 1705410730,
  seed_nestedresampling = 1705419930
)

# 3) Full factorial design based on rep, sample_size, split_type, eval_criterion, procedure, learner

# Number of simulated train/test datasets

nrep = 50

# Optimization procedures 
procedure_list = list(
  ## learner.hyperparam: manually select value most prone to overfitting, preproc: try all combinations, error: apparent
  ##"px" = "learner.hp.maxoverfit_preproc.hp.allcombinations_error.apparent",
  ## learner.hyperparam: default, preproc.hyperparam: default, error: apparent+resampling
  "p0" = "learner.hp.default_preproc.hp.default",
  ## learner.hyperparam: resampling, preproc.hyperparam: default, error: apparent+resampling+nested
  "p1" = "learner.hp.tune_preproc.hp.default",
  ## learner.hyperparam: resampling, preproc.hyperparam: stepwise optimization, error: apparent
  "p2a" = "learner.hp.tune_preproc.hp.steopt_error.apparent",
  ## learner.hyperparam: resampling, preproc.hyperparam: stepwise optimization, error: resampling
  "p2b" = "learner.hp.tune_preproc.hp.steopt_error.resampling",
  ## learner.hyperparam: resampling, preproc.hyperparam: resampling, error: apparent+resampling+nested
  "p3" = "learner.hp.tune_preproc.hp.tune",
  ## learner.hyperparam: resampling, preproc.hyperparam: stepwise optimization, error: apparent
  "p4a" = "learner.hp.default_preproc.hp.steopt_error.apparent"
)

# -> Full factorial design
## naive splits
fullfac_naive = expand.grid(split_type = "naive", # split type
                      rep = 1:nrep, # repetition 
                      sample_size = c("sample50","sample25"), # sample size (sample size of train dataset, 50 or 25 percent of original dataset)
                      eval_criterion = c("regr.rmse","regr.rsq"), # evaluation criterion
                      procedure = unname(unlist(procedure_list)), # procedures
                      learner_name = names(learners_default)) # learners 
## team splits (only with default HPs)
fullfac_teams = expand.grid(split_type = "teams", # split type
                      rep = 1:nrep, # repetition 
                      sample_size = c("sample50","sample25"), # sample size (sample size of train dataset, 50 or 25 percent of original dataset)
                      eval_criterion = c("regr.rmse","regr.rsq"), # evaluation criterion
                      procedure = "learner.hp.default_preproc.hp.default", # procedures
                      learner_name = names(learners_default)) # learners 
fullfac = bind_rows(fullfac_naive, fullfac_teams)                  
fullfac = fullfac %>% mutate_if(is.factor, as.character)

# Simulate random allocation -----------------------------------------------------------------------
# generate unique row identifier
stopifnot(data_phaselevel %>% select(companion_id, grp) %>% n_distinct() == nrow(data_phaselevel)) # make sure this identifier will really be unique
data_phaselevel = data_phaselevel %>% mutate(companion_id_grp = factor(paste(companion_id, grp, sep = "_")), .after = companion_id)

# SAPV ids (could also be done for the two other palliative care settings PMD and Station)

set.seed(1698072152)

## Ignore teams ----
# Generate train ids (for 50% of data)
train_50_sapv_naive =  1:nrep %>% map(function(x) data_phaselevel %>%
                                        filter(setting == "sapv") %>%
                                        slice_sample(prop = 0.5) %>%
                                        .$companion_id_grp)
# Get test ids (for both 50 and 25% train)
test_sapv_naive = train_50_sapv_naive %>% map(function(x) data_phaselevel %>%
                                                filter((setting == setting_name) &!(companion_id_grp %in% x)) %>%
                                                .$companion_id_grp)
# Generate train ids (for 25% of data)
train_25_sapv_naive = train_50_sapv_naive %>% map(function(x) sample(x, size = round(length(x)/2), replace = FALSE))

## Include teams ----

# Sample number of teams being allocated to train for each repetition for uneven numbers of teams (could be relevant if number is rather small)
no_teams = length(unique(data_phaselevel$team_id[data_phaselevel$setting=="sapv"]))
sample_no_teams = sample(floor(no_teams/2):ceiling(no_teams/2), size = nrep, replace = TRUE)

# Sample teams
train_teams = sample_no_teams %>% map(function(x) data_phaselevel %>% filter(setting == "sapv") %>%
                 distinct(team_id, .keep_all = TRUE) %>%
                 slice_sample(n = x) %>% .$team_id)
stopifnot(any(duplicated(train_teams)) == FALSE) # check whether all splits are distinct (but would also be ok if they weren't)
rm(no_teams, sample_no_teams)

# Generate train ids (for 50% of data)
train_50_sapv_teams = train_teams %>% map(function(x) data_phaselevel %>% filter(setting == "sapv" & team_id %in% x) %>%
                      .$companion_id_grp)

# Get test ids (for both 50 and 25% train)
test_sapv_teams = train_teams %>% map(function(x) data_phaselevel %>% filter(setting == "sapv" & !(team_id %in% x)) %>%
                                            .$companion_id_grp)
# Generate train ids (for 25% of data)
train_25_sapv_teams = train_teams %>% map(function(x) data_phaselevel %>% filter(setting == "sapv" & team_id %in% x) %>%
                      group_by(team_id) %>%
                      slice_sample(prop = 0.5) %>%
                      .$companion_id_grp)
id_split_sapv_list = list(test_sapv_naive, train_50_sapv_naive, train_25_sapv_naive,
                          test_sapv_teams, train_50_sapv_teams, train_25_sapv_teams)
names(id_split_sapv_list) = c("test_naive", "train_50_naive", "train_25_naive",
                              "test_teams", "train_50_teams", "train_25_teams")

# Check that no intersection between train and test
stopifnot(sum(map2(train_50_sapv_naive, test_sapv_naive, intersect) %>% map_dbl(length))==0)
stopifnot(sum(map2(train_25_sapv_naive, test_sapv_naive, intersect) %>% map_dbl(length))==0)
stopifnot(sum(map2(train_50_sapv_teams, test_sapv_teams, intersect) %>% map_dbl(length))==0)
stopifnot(sum(map2(train_25_sapv_teams, test_sapv_teams, intersect) %>% map_dbl(length))==0)

save(id_split_sapv_list, file = "./03_results/rdata/id_split_sapv_list.RData")
rm(train_25_sapv_naive, train_25_sapv_teams, train_50_sapv_naive, train_50_sapv_teams,
   train_teams, test_sapv_naive, test_sapv_teams)
## load("./03_results/rdata/id_split_sapv_list.RData")

# Check
# test=data_phaselevel %>% mutate(t1 = ifelse(companion_id_grp %in% train_50_sapv_naive[[1]], 0,1),
#                            t2 = ifelse(companion_id_grp %in% train_25_sapv_naive[[1]], 0,1),
#                            t3 = ifelse(companion_id_grp %in% train_50_sapv_teams[[1]], 0,1),
#                            t4 = ifelse(companion_id_grp %in% train_25_sapv_teams[[1]], 0,1),
#                            t5 = ifelse(companion_id_grp %in% test_sapv_naive[[1]], 0,1),
#                            t6 = ifelse(companion_id_grp %in% test_sapv_teams[[1]], 0,1))
# table(test$t4,test$team_id, test$setting)


# Run optimization ---------------------------------------------------------------------------------


1:nrow(fullfac) %>% purrr::walk(.f = function(i) {
    optim_fct(rep = fullfac$rep[i],
            data = data_phaselevel,
            sample_size = fullfac$sample_size[i],
            id_split_list = id_split_sapv_list,
            setting_name = setting_name,
            split_type = fullfac$split_type[i],
            eval_criterion = fullfac$eval_criterion[i],
            learner_name = fullfac$learner_name[i],
            learners_default = learners_default,
            learners_hp_searchspace_default = learners_hp_searchspace_default,
            preproc_default = preproc_default,
            preproc_hp_searchspace_default = preproc_hp_searchspace_default,
            preproc_hp_stepopt_order = preproc_hp_stepopt_order,
            procedure = fullfac$procedure[i],
            procedure_list = procedure_list,
            resampling_parameters = resampling_parameters)
})








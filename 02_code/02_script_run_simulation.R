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
source("./02_code/_fcts_trees.R")
source("./02_code/_fcts_optim.R")

source("./02_code/_src_add_mlrpipeops.R")
source("./02_code/_src_add_mlrlearners.R")
source("./02_code/_src_add_mlrlearners_helperfcts.R")

# Set up learners (= algorithms) with corresponding hps --------------------------------------------
source("./02_code/_src_setup_learners.R")
# -> learners and learners_hp_searchspace

# Set up preprocessing pipeline with corresponding hps ---------------------------------------------

# Preprocessing pipeline
# (Note: in the pipeline %>>%, preproc.target need to be before preproc.drop.targetout and
#  preproc.drop.iposca before preproc.feature.ipos)
preproc_pipeline = po("preproc.target") %>>%
  po("preproc.drop.targetout") %>>%
  po("preproc.drop.iposca") %>>%
  po("preproc.feature.ipos") %>>% 
  po("preproc.feature.age") %>>% 
  po("preproc.feature.akps") %>>% 
  po("select", selector = selector_help, id = "select.features") %>>%
  po("fixfactors") 

# Search space preprocessing hps
preproc_hp_searchspace = list("preproc.target" = c("A", "B", "C"),
                      "preproc.drop.targetout" = c("A", "B", "C", "D"),
                      "preproc.drop.iposca" = c("A", "B", "C", "D", "E", "F", "G", "H"),
                      "preproc.feature.ipos" = c("A", "B", "C", "D", "E"),
                      "preproc.feature.age" = c("A", "B"),
                      "preproc.feature.akps" = c("A", "B"))

# Order for stepwise optimization 
preproc_hp_stepopt_order = c("preproc.target", "preproc.feature.ipos", "preproc.feature.age", "preproc.feature.akps",
                             "preproc.drop.targetout", "preproc.drop.iposca")


# Simulation parameters  ---------------------------------------------------------------------------
# Number of simulated train/test datasets
nrep = 3 # 50? 100? 

# Considered setting
param_setting = "sapv"

# Evaluation criterion
eval_criterion = "regr.rsq" 

# Raw features names (as they are in the loaded dataset)
feature_names_raw = c("age","ipos_pain","ipos_shortness_breath","ipos_weakness","ipos_nausea",          
                     "ipos_vomiting","ipos_poor_appetite","ipos_constipation","ipos_sore_dry_mouth",
                     "ipos_drowsiness","ipos_poor_mobility","ipos_patient_anxiety","ipos_family_anxiety",
                     "ipos_depression","ipos_peace","ipos_sharing_feelings","ipos_information",
                     "ipos_practical_matters","cogn_confusion","cogn_agitation","akps" )

# Tuning parameters 
tuning_parameters = list(
  n_evals = 5, # 100 
  inner_folds = 3, # 5 
  outer_folds = 3, # 5?
  outer_repeats = 1, #3 ? 
  resolution = 10 #  50
)

# Optimization procedures 

#TO DOOOOO


# Simulate random allocation -----------------------------------------------------------------------
set.seed(1698072152)
id_train_list = 1:nrep %>% map(function(x) data_phaselevel %>% distinct(companion_id,.keep_all = TRUE) %>%  
  group_by(team_id) %>% # split within each team_id
  slice_sample(prop = 0.5) %>% .$companion_id)

id_train = id_train_list[[1]] ###todooo
# Run optimization ---------------------------------------------------------------------------------
# (for all nrep train/test allocations)

# 1. Train/test data
data_train = data_phaselevel %>% filter((setting == param_setting) &
                                       (companion_id %in% id_train))
data_test = data_phaselevel %>% filter((setting == param_setting) &
                                          !(companion_id %in% id_train))
stopifnot(length(intersect(data_train$companion_id, data_test$companion_id)) ==0) # make sure no ids are in both datasets

# 2. Specify task
task = as_task_regr(data_train, target = "targetvar")
# specify companion_id as grouping variable bc observations from same id should not be split when resampling
# = group by id 
task$col_roles$group = "companion_id"
# = remove id from features
task$col_roles$feature = setdiff(task$col_roles$feature, "companion_id")
rm(data_train)

# only select possible targets, features, and id because per default the learner uses all variables in data set as features 
# TODOO!!!!!############

# 3. Generate trees and get associated errors 

# 
## learner.hyperparam: resampling, preproc.hyperparam: stepwise optimization, error: resampling
procedure = "preproc.hp.steopt_learner.hp.tune_error.resampling"

## learner.hyperparam: resampling, preproc.hyperparam: stepwise optimization, error: apparent
procedure = "preproc.hp.steopt_learner.hp.tune_error.apparent"

## learner.hyperparam: resampling, preproc.hyperparam: nested resampling
procedure = "preproc.hp.steopt_learner.hp.tune_error.nested_resampling"

## learner.hyperparam: resampling, preproc.hyperparam: resampling
procedure = "preproc.hp.tune_learner.hp.tune"

## learner.hyperparam: default, preproc.hyperparam: resampling
procedure = "preproc.hp.default_learner.hp.tune"
Sys.setenv(OMP_NUM_THREADS="1")
t1 = get_tree_and_error_fct(procedure = procedure,
                            preproc_hp_searchspace = preproc_hp_searchspace,
                           task = task,
                           data_test = data_test, 
                           eval_criterion = eval_criterion, 
                           rpart_hp = rpart_hp,
                           tuning_parameters = tuning_parameters)

# 4. Extract errors [(i) reported error and (ii) test set error]

# next 
####mlr3 aussage bzgl. optimistic algorithm (now really needed) nochmal checken

# al procedure
# evtl. unnötige errors löschen

# schlimm dass nicht gleiche splits bei a)tunen (falls mehrere algos) und b) bei nested resampling stepwise

###########################################################################

#### pipelines kurzer test mit informeller mini-simulation!!!


###########################
# add learner_graph$model$regr.rpart$train_task info -> Fehler: The backend of Task 'data_train' has been removed. Set `store_backends`
#                                                      to `TRUE` during model fitting to conserve it.
#formula checken rpart!
# falls mehrere learner: schlimm wenn auf unterschiedlichen splits getuned?
# seeeeed!!!!!!!!!!!
# - verhalten der algorithmen bei missing values (auch bzgl. fixfactors)
# - schauen dass bei target wirklich nicht benötigte target vars weggelassen in task
# pipelines final entscheiden
# feature names explizit bei task angeben, müssen noch variablen ausgeschlossen werden?
#   evlt preproc die alle removed außer feature target und grouping
# evtl. invoke packages checkn
# alle functionen in learner_helpers benötigt?
###
# - irgendwo sicherstellen dass in data_train und data_test dieselben levels vorhanden sind
# ordinale vars nicht vergessen
# -fct descriptions
# - document id_train irgendwo
# - welche variables noch entfernt außer dry mouth
# - setting auswahl parameter
# - check dass alle die sein sollen ordinal 
# - evtl auch different sample size for simulation
# - falls extern kann keine correctur verwendet werden
# - man könnte manche hyperparameter auch stetig machen (zB outlier removal)
# - reihenfolge der pipelines beachten (zb select ipos_ kann probleme machen wenn nach transf. angewendet)
#####################################################

# Note on po("fixfactors")
# "Fixes factors of type factor, ordered: Makes sure the factor levels during prediction are the same
#  as during training; possibly dropping empty training factor levels before. 
#  Note this may introduce missing values during prediction if unseen factor levels are found."
# -> in case of rpart, this does not lead to missing values during prediction; instead, rpart
#    basically interpolates or extrapolates; it seems as if the surrogate splits are not used (?)



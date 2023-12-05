library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
# mlr3
library(mlr3)
library(mlr3pipelines)
library(mlr3misc)
library(mlr3tuning)
library(paradox)
library(rpart.plot)


load("./01_data/data_all.RData")
source("./02_code/_fcts_preprocessing.R")
source("./02_code/_fcts_pipeops.R")
source("./02_code/_fcts_trees.R")
source("./02_code/_fcts_optim.R")

# Simulation parameters ----------------------------------------------------------------------------
preproc_hp_set = list("preproc.target" = c("A", "B", "C"),
                               "preproc.feature.ipos" = c("A", "B", "C", "D", "E"),
                               "preproc.feature.age" = c("A", "B"))
preproc_hp_stepopt_order = c("preproc.target", "preproc.feature.ipos", "preproc.feature.age")
setting_param = "sapv"
eval_criterion = "regr.rsq" 
feature_names_raw = c("age","ipos_pain","ipos_shortness_breath","ipos_weakness","ipos_nausea",          
                     "ipos_vomiting","ipos_poor_appetite","ipos_constipation","ipos_sore_dry_mouth",
                     "ipos_drowsiness","ipos_poor_mobility","ipos_patient_anxiety","ipos_family_anxiety",
                     "ipos_depression","ipos_peace","ipos_sharing_feelings","ipos_information",
                     "ipos_practical_matters","cogn_confusion","cogn_agitation","akps" )
target_names = c("costpd", "costpd_exclsys", "minutespd")

# Algorithm hyperparameters
rpart_hp = list(
  fixed = list( 
    maxdepth = 4,
    xval = 0,  
    keep_model = TRUE),
  tuning = list(
    cp_lower = 0.001,
    cp_upper = 0.1, 
    minbucket_lower = 10,
    minbucket_upper = 20
  )
)

# Tuning parameters 
tuning_parameters = list(
  n_evals = 5, # 100 
  inner_folds = 3, # 5 
  outer_folds = 3, # 5?
  outer_repeats = 1, #3 ? 
  resolution = 10 #  50
)


# Start simulation ---------------------------------------------------------------------------------
#set.seed(1698072152) # set seed for simulation

##### Repeat the following code n_rep times ####

# 1. Select specified setting ----
data = data %>% filter(setting == setting_param)
# 1. Allocation of train/test ----
id_train = data %>% distinct(companion_id,.keep_all = TRUE) %>%  
  group_by(team_id) %>% # to ensure sampling will not change when adding new datasets
  slice_sample(prop = 0.5) %>% .$companion_id
datasim_train = data %>% filter(companion_id %in% id_train) %>% mutate(allocation="train")
datasim_test = data %>% filter(!companion_id %in% id_train) %>% mutate(allocation="test")
datasim = bind_rows(datasim_train, datasim_test)
rm(id_train, datasim_train, datasim_test)

# 2. Correction of first day of first phase using first day of next phases ----
# (apply on whole data set but only use TRAIN for calculation)
# (separately for all three possible targets)

cf1 = preprocess_get_firstdaycorrect_fct(data = datasim %>% filter(allocation == "train"), target_name = target_names[1])
datasim = preprocess_apply_firstdaycorrect_fct(data = datasim, target_name = target_names[1], correction_factors = cf1)

cf2 = preprocess_get_firstdaycorrect_fct(data = datasim %>% filter(allocation == "train"), target_name = target_names[2])
datasim = preprocess_apply_firstdaycorrect_fct(data = datasim, target_name = target_names[2], correction_factors = cf2)

cf3 = preprocess_get_firstdaycorrect_fct(data = datasim %>% filter(allocation == "train"), target_name = target_names[3])
datasim = preprocess_apply_firstdaycorrect_fct(data = datasim, target_name = target_names[3], correction_factors = cf3)
rm(cf1, cf2, cf3)

# 3. Get data on phase level ----
# (remove variables varying within phase and only keep distinct values and make sure there is 
# exactly one row per patient and phase)
datasim = datasim %>% select(-date, -time, -datetime, -minutes, -cost, -cost_exclsys) %>% distinct()
stopifnot(nrow(datasim %>% distinct(companion_id, grp)) == nrow(datasim))

# 4. Removal of missing values (fixed) ----
# - ipos_ variables: remove phases with NAs in ipos variables
datasim = datasim %>% filter_at(vars(starts_with("ipos_")), all_vars(!is.na(.)))
# - cogn_ variables: remove phases with NA in cogn variables
datasim = datasim %>% filter_at(vars(starts_with("cogn_")), all_vars(!is.na(.)))
# - akps: remove phases with NA or 'cannot assess' 
datasim = datasim %>% filter(!is.na(akps) & akps != "cannot assess")

# 5. Drop unused levels and convert team and companion id to factor ----
datasim = datasim %>% droplevels()
datasim$id = factor(datasim$companion_id)
datasim$team_name = factor(datasim$team_id) 

# 6. Start optimization ----
## Split train and test data ----
data_train = datasim %>% filter(allocation == "train")
data_test = datasim %>% filter(allocation == "test")
## Specify target ----
# only select possible targets, features, and id because per default the learner uses all variables in data set as features 
data_train = data_train %>% select(targetvar, all_of(target_names), all_of(feature_names_raw), companion_id) 
# specify companion_id as grouping variable bc observations from same id should not be split when resampling
task = as_task_regr(data_train, target = "targetvar")
# = group by id 
task$col_roles$group = "companion_id"
# = remove id from features
task$col_roles$feature = setdiff(task$col_roles$feature, "companion_id")
rm(data_train)

## algorithm.hyperparam: resampling, preproc.hyperparam: stepwise optimization, error: resampling
t1 = generate_and_eval_tree_fct(procedure = "preproc.hp.steopt_algo.hp.tune_error.resampling",
                           preproc_hp_set = preproc_hp_set,
                           task = task,
                           data_test = data_test, 
                           eval_criterion = eval_criterion, 
                           rpart_hp = rpart_hp,
                           tuning_parameters = tuning_parameters)
## algorithm.hyperparam: resampling, preproc.hyperparam: stepwise optimization, error: apparent
t2 = generate_and_eval_tree_fct(procedure = "preproc.hp.steopt_algo.hp.tune_error.apparent",
                                preproc_hp_set = preproc_hp_set,
                                task = task,
                                data_test = data_test, 
                                eval_criterion = eval_criterion, 
                                rpart_hp = rpart_hp,
                                tuning_parameters = tuning_parameters)

## algorithm.hyperparam: resampling, preproc.hyperparam: nested resampling
t3 = generate_and_eval_tree_fct(procedure = "preproc.hp.steopt_algo.hp.tune_error.nested_resampling",
                                preproc_hp_set = preproc_hp_set,
                                task = task,
                                data_test = data_test, 
                                eval_criterion = eval_criterion, 
                                rpart_hp = rpart_hp,
                                tuning_parameters = tuning_parameters)


## algorithm.hyperparam: resampling, preproc.hyperparam: resampling
t4 = generate_and_eval_tree_fct(procedure = "preproc.hp.tune_algo.hp.tune",
                                preproc_hp_set = preproc_hp_set,
                                task = task,
                                data_test = data_test, 
                                eval_criterion = eval_criterion, 
                                rpart_hp = rpart_hp,
                                tuning_parameters = tuning_parameters)





# next 
####commit!
####mlr3 aussage bzgl. optimistic algorithm (now really needed) nochmal checken

# nested resampling +
# default hp
# evtl. unnötige fehler löschen


# schlimm dass nicht gleiche splits bei a)tunen (falls mehrere algos) und b) bei nested resampling stepwise

###########################################################################

#### pipelines kurzer test mit informeller mini-simulation!!!


###########################
#formula checken rpart!
# falls mehrere learner: schlimm wenn auf unterschiedlichen splits getuned?
# seeeeed!!!
#mlr_pipeops_mutate
# target :mlr_pipeops_updatetarget or mlr_pipeops_targetmutate ?
# ipos NA/"missing entfernen" + für akps auch cannot assess entferenen
###
# -for input in mlr3 prüfen dass keine NAs
# - irgendwo sicherstellen dass in data_train und data_test dieselben levels vorhanden sind
# ordinale vars nicht vergessen
# -outlier removal könnte man auch stetig machen
# ganz am schluss droplevels?
# -fct descriptions
# - document id_train irgendwo
# - welche variables noch entfernt außer dry mouth
# - setting auswahl parameter
# - check dass alle die sein sollen ordinal 
# - check dass kein NA + kein cannot assess in den daten 
# - evtl. schnellere alternative für sysvar rlang 
# - man könnte manche hyperparameter auch stetig machen
# - evtl cutoff werte für ipos cannot assess noch ändern (damit nicht zu viel entfernt)
# - reihenfolge der pipelines beachten (zb select ipos_ kann probleme machen wenn nach transf. angewendet)
#####################################################

# Note on po("fixfactors")
# "Fixes factors of type factor, ordered: Makes sure the factor levels during prediction are the same
#  as during training; possibly dropping empty training factor levels before. 
#  Note this may introduce missing values during prediction if unseen factor levels are found."
# -> in case of rpart, this does not lead to missing values during prediction; instead, rpart
#    basically interpolates or extrapolates; it seems as if the surrogate splits are not used (?)



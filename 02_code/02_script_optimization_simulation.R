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


load("./01_data/data_phaselevel.RData")
source("./02_code/_fcts_preproc.R")
source("./02_code/_fcts_preproc2pipeop.R")
source("./02_code/_fcts_trees.R")
source("./02_code/_fcts_optim.R")

# Simulation parameters ----------------------------------------------------------------------------
# Set of preprocessing operations with hyperparameters 
# (Note: in the pipeline %>>%, preproc.target need to be before preproc.drop.targetout and
#  preproc.drop.iposca before preproc.feature.ipos)
preproc_hp_set = list("preproc.target" = c("A", "B", "C"),
                      "preproc.drop.targetout" = c("A", "B", "C", "D"),
                      "preproc.drop.iposca" = c("A", "B", "C", "D", "E", "F", "G", "H"),
                      "preproc.feature.ipos" = c("A", "B", "C", "D", "E"),
                      "preproc.feature.age" = c("A", "B"),
                      "preproc.feature.akps" = c("A", "B"))
# Order for stepwise optimization 
preproc_hp_stepopt_order = c("preproc.target", "preproc.feature.ipos", "preproc.feature.age", "preproc.feature.akps",
                             "preproc.drop.targetout", "preproc.drop.iposca")
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
data_phaselevel = data_phaselevel %>% filter(setting == setting_param)
# 1. Allocation of train/test ----
id_train = data_phaselevel %>% distinct(companion_id,.keep_all = TRUE) %>%  
  group_by(team_id) %>% # to ensure sampling will not change when adding new datasets
  slice_sample(prop = 0.5) %>% .$companion_id
datasim_train = data_phaselevel %>% filter(companion_id %in% id_train) %>% mutate(allocation="train")
datasim_test = data_phaselevel %>% filter(!companion_id %in% id_train) %>% mutate(allocation="test")
datasim = bind_rows(datasim_train, datasim_test)
rm(id_train, datasim_train, datasim_test)



# 6. Start optimization ----
## Split train and test data ----
data_train = datasim %>% filter(allocation == "train")
data_test = datasim %>% filter(allocation == "test")
## Specify target ----
# only select possible targets, features, and id because per default the learner uses all variables in data set as features 
#data_train = data_train %>% select(targetvar, contains(c("sum_cost", "sum_minutes")), all_of(feature_names_raw), companion_id) 
data_train = data_train %>% select(-allocation) 
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
####mlr3 aussage bzgl. optimistic algorithm (now really needed) nochmal checken

# nested resampling +
# default hp
# evtl. unnötige errors löschen

# schlimm dass nicht gleiche splits bei a)tunen (falls mehrere algos) und b) bei nested resampling stepwise

###########################################################################

#### pipelines kurzer test mit informeller mini-simulation!!!


###########################
# evtl summarise info loswerden
# add learner_graph$model$regr.rpart$train_task info
#formula checken rpart!
# falls mehrere learner: schlimm wenn auf unterschiedlichen splits getuned?
# seeeeed!!!
# - verhalten der algorithmen bei missing values (auch bzgl. fixfactors)
# - schauen dass bei target wirklich nicht benötigte target vars weggelassen in task
# pipelines final entscheiden
# feature names explizit bei task angeben, müssen noch variablen ausgeschlossen werden?
#   evlt preproc die alle removed außer feature target und grouping
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



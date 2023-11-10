library(dplyr)
library(tidyr)

load("./01_data/data_all.RData")
source("./02_code/_fcts_preprocessing.R")

#set.seed(1698072152) # set seed for simulation

##### Repeat the following code n_rep times ####

# 1. Allocation of train/validation ----
id_train = data %>% distinct(companion_id,.keep_all = TRUE) %>%  
  group_by(team_id) %>% # to ensure sampling will not change when adding new datasets
  slice_sample(prop = 0.5) %>% .$companion_id
datasim_train = data %>% filter(companion_id %in% id_train) %>% mutate(allocation="train")
datasim_validation = data %>% filter(!companion_id %in% id_train) %>% mutate(allocation="validation")
datasim = bind_rows(datasim_train, datasim_validation)
rm(id_train, datasim_train, datasim_validation)

# 2. Correction of first day of first phase using first day of next phases ----
# (apply on whole data set but only use TRAIN for calculation)
# (separately for all three possible targets)
target_names = c("costpd", "costpd_exclsys", "minutespd")

cf1 = preprocess_get_firstdaycorrect_fct(data = datasim %>% filter(allocation == "train"), target_name = target_names[1])
datasim = preprocess_apply_firstdaycorrect_fct(data = datasim, target_name = target_names[1], correction_factors = cf1)

cf2 = preprocess_get_firstdaycorrect_fct(data = datasim %>% filter(allocation == "train"), target_name = target_names[2])
datasim = preprocess_apply_firstdaycorrect_fct(data = datasim, target_name = target_names[2], correction_factors = cf2)

cf3 = preprocess_get_firstdaycorrect_fct(data = datasim %>% filter(allocation == "train"), target_name = target_names[3])
datasim = preprocess_apply_firstdaycorrect_fct(data = datasim, target_name = target_names[3], correction_factors = cf3)
rm(cf1, cf2, cf3)


# 3. Get data on phase level ----
# (for variables potentially varying within phase, value on first day is used)
datasim = preprocess_contact2phaselevel_fct(data = datasim, 
                                         vars_vary = colnames(data %>% select(contains("ipos_"),  contains("cogn_"), akps)))

# 4. Missing values handling (fixed) ----
# - ipos_ variables: remove phases with NAs in ipos variables
datasim = datasim %>% filter_at(vars(starts_with("ipos_")), all_vars(!is.na(.)))
# - cogn_ variables: remove phases with NA in cogn variables
datasim = datasim %>% filter_at(vars(starts_with("cogn_")), all_vars(!is.na(.)))
# - cogn_ variables: remove phases with NA in cogn variables
datasim = datasim %>% mutate(across(starts_with("cogn_"), ~ forcats::fct_recode(.,  "absent" = "cannot assess")))
# - akps: remove phases with NA or 'cannot assess' 
datasim = datasim %>% filter(!is.na(akps) & akps != "cannot assess")


# Drop unused levels ----
datasim = datasim %>% droplevels()


### berechne first day werte von vars_vary schon in 01_script_preprocessing_fixed!!!




###########################
#mlr_pipeops_mutate
# target :mlr_pipeops_updatetarget or mlr_pipeops_targetmutate ?
# ipos NA/"missing entfernen" + für akps auch cannot assess entferenen
###
# -for input in mlr3 prüfen dass keine NAs
# ordinale vars nicht vergessen
# -outlier removal könnte man auch stetig machen
# ganz am schluss droplevels?
# -fct descriptions
# - document id_train irgendwo
# - welche variables noch entfernt außer dry mouth
# - setting auswahl parameter
# - evtl. schnellere alternative für sysvar rlang 
#####################################################


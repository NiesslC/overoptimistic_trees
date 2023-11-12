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


# Drop unused levels ----
datasim = datasim %>% droplevels()


# next: removal of missing values ipos


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
# - check dass alle die sein sollen ordinal 
# - check dass kein NA + kein cannot assess in den daten 
# - evtl. schnellere alternative für sysvar rlang 
#####################################################


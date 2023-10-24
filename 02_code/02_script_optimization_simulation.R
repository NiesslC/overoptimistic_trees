library(dplyr)
library(tidyr)

load("./01_data/data_all.RData")
source("./02_code/_fcts_preprocessing.R")

set.seed(1698072152) # set seed for simulation

##### Repeat the following code n_rep times ####

# Allocation of train/validation ----
id_train = data %>% distinct(companion_id,.keep_all = TRUE) %>%  
  group_by(team_id) %>% # to ensure sampling will not change when adding new datasets
  slice_sample(prop = 0.5) %>% .$companion_id
datasim_train = data %>% filter(companion_id %in% id_train) %>% mutate(allocation="train")
datasim_validation = data %>% filter(!companion_id %in% id_train) %>% mutate(allocation="validation")
datasim = bind_rows(datasim_train, datasim_validation)
rm(id_train, datasim_train, datasim_validation)

# Correction of first day of first phase using first day of next phases ----
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

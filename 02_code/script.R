# script 
library(dplyr)
library(purrr)
library(forcats) #fct_drop
library(tidyr)

### change names!

load("./data/data_ba_thesis.RData")
data = data_tree_large_n %>% filter(setting == "SAPV" & allocation_large == "train") %>% 
  dplyr::rename(phase = palliativphase,
                cogn_confusion = kognitiv_verwirrtheit,
                cogn_agitation = kognitiv_unruhe,
                age = alter) %>%
  mutate(age_cat = cut(age, breaks = c(20,seq(50,90,10),110)) )
############################################################

data_choices = list("outliers" = c("keep", "remove"),
                    "feature_ipos" = c(list(c("IPOS_atemnot", "IPOS_schmerzen"), c("IPOS_erbrechen"))),
                    "feature_age" = c("age", "age_cat"),
                    "target" = "kost_per_diem_all")
current_option = list("outliers" = data_choices$outliers[[1]],
                      "feature_ipos" = data_choices$feature_ipos[[1]],
                      "feature_age" = data_choices$feature_age[[1]],
                      "target" = data_choices$target[[1]])



#---------------------------------------------------------------------------------------


features = unname(unlist(feature_list))

# optimization w.r.t: feature_age 


  
cart_resampling_fct(data_train = data_tree,
                    features = c("AKPS", "age", "IPOS_schmerzen", "IPOS_atemnot"),
                    eval_criterion = "regr.rsq",
                    plot = FALSE)

setting = options_to_evaluate[[1]]
###########################################################

# function that returns a nested list containing the options_to_evaluate where across the lists, only one data choice varies
generate_options_to_evaluate_fct = function(current_option, data_choices, choice_of_interest){
  options_to_evaluate = replicate(length(data_choices[[choice_of_interest]]), current_option, simplify = FALSE)
  
  # dynamically insert value of choice of interest into options to evaluate
  for(i in 1:length(data_choices[[choice_of_interest]])){
    options_to_evaluate[[i]][[choice_of_interest]] = data_choices[[choice_of_interest]][i]
  }
  return(options_to_evaluate)
}

# function that returns a list of feature names, target name, and data set based on 
# a list specifying the options to evaluate (with differences only in one choice)
prepare_input_fct = function(data, setting){
  
  ## features ----
  feature_list = list("phase" = "phase",
                      "cogn_confusion" = "cogn_confusion",
                      "cogn_agitation" = "cogn_agitation",
                      "ipos" = setting$feature_ipos,
                      "age" = setting$feature_age,
                      "akps" = "AKPS") 
  
  ## target ----
  target = setting$target
  
  ## data set ----
  if (setting$outliers == "keep"){
    data = data 
  }
  
  # ### insert more options ###
  
  input = list(features_list = feature_list, target = target, data = data)
  return(data)
}


get_performance_fct = function(evaluation_type, input){
  if(evaluation_type == "II"){
    set.seed(123) # set this exact seed before every call of the function!!! (ensures same splits)
    cart_resampling_fct(data_train = input$data,
                        features = unname(unlist(input$features_list)),
                        eval_criterion = "regr.rsq",
                        plot = TRUE)
  }
}





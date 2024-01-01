selector_help = function(task) {
  task$feature_names[grepl("age|ipos|age|cogn|akps|companion_id|team_id", task$feature_names)]
}


preprocess_drop_iposca_fct = function(option){
  ## Option A-H ----
  list_options = as.list(17:10) 
  names(list_options) = LETTERS[1:length(list_options)]
  threshold = list_options[[option]]
  return(threshold)
}


preprocess_drop_targetout_fct = function(target_values, option){
  if(option == "A"){
    # Option A ----
    # 100th percentile (do not remove outliers)
    percentile = 1.00
  } else if(option == "B"){
    # Option B ----
    # 99th percentile 
    percentile = 0.99
  } else if(option == "C"){
    # Option C ----
    # 95th percentile 
    percentile = 0.95
  } else if(option == "D"){
    # Option D ----
    # 90th percentile 
    percentile = 0.90
  }
  
  outlier_threshold = quantile(unlist(target_values),percentile)
  return(outlier_threshold)
  }




preprocess_target_fct = function(data, option, correction_factors){
  
  # Select target variable
  if(option == "A"){
    # Option A ---- 
    # cost variable as basis for target
    sum_target_day_1 = "sum_cost_day_1"
    sum_target_day_geq2 = "sum_cost_day_geq2"
  } else if(option == "B"){
    # Option B ---- 
    # cost_exclsys variable as basis for target   
    sum_target_day_1 = "sum_cost_exclsys_day_1"
    sum_target_day_geq2 = "sum_cost_exclsys_day_geq2"
  } else if(option == "C"){
    # Option C ---- 
    # minutes variable as basis for target
    sum_target_day_1 = "sum_minutes_day_1"
    sum_target_day_geq2 = "sum_minutes_day_geq2"
    
  }
  # Add correction factors to data (if there is no correction factor for a team id in data, set cf to 1)
  data = left_join(data, correction_factors, by = c("setting", "team_id")) %>%
    mutate(correction_factor = ifelse(is.na(correction_factor), 1, correction_factor))
  
  # Calculate target variable 
  # (note that sum_target_day_geq2 can be NA if phase only lasts one day)
  data = data %>%
    rowwise() %>% 
    mutate(targetvar = case_when(
      grp == 0  ~ sum(c(correction_factor * .data[[sum_target_day_1]],
                        .data[[sum_target_day_geq2]]), na.rm = TRUE)/ 
        phase_days,
      grp != 0 ~ sum(c(.data[[sum_target_day_1]],
                       .data[[sum_target_day_geq2]]), na.rm = TRUE)/
        phase_days
    )) %>%
    ungroup() %>% 
    select(-correction_factor, # remove variables that are not needed anymore
           -contains("sum_cost"),
           -contains("sum_minutes"),
           -phase_days,
           -grp)
  stopifnot(sum(is.na(data$targetvar)) == 0) # check that no NAs
  stopifnot(all(data$targetvar >= 0)) # check that target variable is > 0 
  return(data)
}

preprocess_target_getcorr_fct = function(data, option){ 
  
  if(option == "A"){
    # Option A ---- 
    # cost variable as basis for target
    sum_target_day_1 = "sum_cost_day_1"
      } else if(option == "B"){
    # Option B ---- 
    # cost_exclsys variable as basis for target   
    sum_target_day_1 = "sum_cost_exclsys_day_1"
  } else if(option == "C"){
    # Option C ---- 
    # minutes variable as basis for target
    sum_target_day_1 = "sum_minutes_day_1"
  }
  
  # Calculate correction factors
  means_firstday = data %>%
    mutate(number_phase = ifelse(grp == 0, "phase_1", "phase_geq2")) %>%
    group_by(setting, team_id, number_phase) %>% # calculate mean separately for phase no. = 1 vs. >= 2
    summarise(mean_sum_target_day_1 = mean(.data[[sum_target_day_1]]), .groups = "drop_last") %>% 
    ungroup()
  correction_factors = means_firstday %>% 
    spread(number_phase, mean_sum_target_day_1) %>%
    mutate(correction_factor = ifelse(phase_geq2/phase_1 < 1,
                                           phase_geq2/phase_1,1)) %>%
    select(-phase_1, -phase_geq2)
  
 
  return(correction_factors)
}


preprocess_feature_age_fct = function(data, option){ 
  if(option == "A"){
    # Option A ---- 
    # Leave age as continuous variable
    data = data
  } else if(option == "B"){
    # Option B ---- 
    # Categorize age (= factor variable, but not ordered factor!)
    data = data %>% 
      mutate(age =  cut(age, breaks = c(21,seq(50,90,10),104), include.lowest = TRUE ))
  }
  stopifnot(sum(is.na(data$age))==0) # check that no NAs
  return(data)
  }


preprocess_feature_akps_fct = function(data, option){
  if(option == "A"){
    # Option A ---- 
    # Only collapse the AKPS categories 70,80,90, leave remaining categories as they are
    data = data %>% 
      mutate(akps = case_when(
        akps %in% c("70", "80", "90") ~ "70_80_90",
        .default = akps)) %>% 
      mutate(akps = factor(akps, ordered = TRUE))
  } else if(option == "B"){
    # Option B ---- 
    # Only collapse several AKPS categories (note: values 0 and 100 do not exist in the data)
    data = data %>% 
      mutate(akps = case_when(
        akps %in% c("10", "20") ~  "10_20",
        akps %in% c("30", "40", "50") ~  "30_40_50",
        akps %in% c("60", "70", "80", "90") ~ "60_70_80_90",
        .default = akps)) %>% 
      mutate(akps = factor(akps, ordered = TRUE))
  }
  stopifnot(sum(is.na(data$akps))==0) # check that no NAs
  return(data)
}


preprocess_feature_ipos_fct = function(data, option){
  if(option == "A"){
    # Option A ----
    # IPOS score defined as sum of all 17 IPOS variables with range [0,68]
    # (for each variable, 0 = least extreme value, 4 = most extreme value)
    # Handling of "cannot assess": "cannot assess" is set to NA which is equal to setting
    # these values to the least extreme value
    data = data %>%   
      mutate(across(starts_with("ipos"), ~ na_if(., "cannot assess"))) %>%
      mutate(across(starts_with("ipos"), ~ fct_drop(., only = c("cannot assess")))) %>%
      mutate(across(starts_with("ipos"), ~ as.integer(.)-1)) %>% 
      mutate(ipos_score = rowSums(across(starts_with("ipos")), na.rm =TRUE)) %>% 
      select(-setdiff(starts_with("ipos"), contains("ipos_score"))) # remove unused ipos vars
    feature_names = "ipos_score"
  } else if(option == "B"){
    # Option B ----
    # IPOS score similar to option A except that two separate scores are created, one for the 10
    # physical IPOS variables (range=[0,40]) and one for the remaining 7 non-physical IPOS 
    # variables (range=[0,28])
    # Handling of "cannot assess": "cannot assess" is set to NA which is equal to setting
    # these values to the least extreme value
    data = data %>% 
      mutate(across(starts_with("ipos"), ~ na_if(., "cannot assess"))) %>%
      mutate(across(starts_with("ipos"),~  fct_drop(., only = c("cannot assess")))) %>%
      mutate(across(starts_with("ipos"), ~ as.integer(.)-1)) %>% 
      mutate(ipos_score_phys = rowSums(across(c("ipos_pain",
                                                "ipos_shortness_breath","ipos_weakness","ipos_nausea","ipos_vomiting",
                                                "ipos_poor_appetite","ipos_constipation","ipos_sore_dry_mouth",
                                                "ipos_drowsiness", "ipos_poor_mobility")), na.rm =TRUE)) %>% 
      mutate(ipos_score_nonphys = rowSums(across(c("ipos_patient_anxiety","ipos_family_anxiety",
                                                   "ipos_depression","ipos_peace",
                                                   "ipos_sharing_feelings","ipos_information",
                                                   "ipos_practical_matters")), na.rm =TRUE)) %>% 
      select(-setdiff(starts_with("ipos"), contains("ipos_score"))) # remove unused ipos vars
    feature_names = c("ipos_score_phys", "ipos_score_nonphys")
  } else if(option == "C"){
    # Option C ----
    # IPOS score counting how many IPOS variables take the most extreme or second most extreme 
    # value, range = [0,17]
    # Handling of "cannot assess": By only accounting for the two most extreme values, 
    # "cannot assess" values are implicitly assumed to be at most equal to the third most extreme value
    data=data %>% mutate(across(starts_with("ipos"), 
                                ~ . %in% tail(levels(.)[!grepl("cannot assess",levels(.))], n=2))) %>%
      mutate(ipos_score_extreme = rowSums(across(starts_with("ipos")))) %>% 
      select(-setdiff(starts_with("ipos"), contains("ipos_score_extreme"))) # remove unused ipos vars
    
    feature_names = "ipos_score_extreme"
    
  } else if(option == "D"){
    # Option D ----
    # IPOS score similar to option C except that ipos_pain and ipos_shortness_breath are not 
    # included in the score but considered individually as ordinal variables.
    # Handling of "cannot assess": By only accounting for the two most extreme values, 
    # "cannot assess" values are implicitly assumed to be at most equal to the third most extreme value;
    # for ipos_pain and ipos_shortness_breath "cannot assess" is set to "not at all"
    data=data %>% mutate(across(setdiff(starts_with("ipos"), ends_with(c('pain','shortness_breath'))), 
                                ~ . %in% tail(levels(.)[!grepl("cannot assess",levels(.))], n=2))) %>%
      mutate(ipos_score_extreme_exclpb = rowSums(across(setdiff(starts_with("ipos"), ends_with(c('pain','shortness_breath')))))) %>% 
      mutate(across(c("ipos_pain", "ipos_shortness_breath"), ~ na_if(., "cannot assess"))) %>%
      mutate(across(c("ipos_pain", "ipos_shortness_breath"), ~  fct_drop(., only = c("cannot assess")))) %>%
      mutate(across(c("ipos_pain", "ipos_shortness_breath"), ~ replace_na(.,"not at all"))) %>%
      mutate(across(c("ipos_pain","ipos_shortness_breath"), ~ factor(., ordered = TRUE))) %>%
      select(-setdiff(starts_with("ipos"), ends_with(c("ipos_score_extreme_exclpb","ipos_pain","ipos_shortness_breath")))) # remove unused ipos vars
    
    feature_names = c("ipos_score_extreme_exclpb","ipos_pain","ipos_shortness_breath")
  } else if(option == "E"){
    
    # Option E ----
    # IPOS variables considered separately as ordinal variables, excluding some variables that 
    # likely do not have an effect on resources on their own 
    # Handling of "cannot assess": "cannot assess" is set to the corresponding least extreme value
    data = data %>% select(-ipos_sore_dry_mouth) %>%
      mutate(across(starts_with("ipos"), ~ na_if(., "cannot assess"))) %>%
      mutate(across(starts_with("ipos"), ~ fct_drop(., only = c("cannot assess")))) %>%
      mutate(across(starts_with("ipos"), ~ replace_na(.,levels(.)[1]))) %>%
      mutate(across(starts_with("ipos"), ~ factor(., ordered = TRUE)))
    ## Note: In case only the three most extreme values should be differentiated, use sth like below 
    # mutate(across(starts_with("ipos"), ~na_if(., "cannot assess"))) %>%
    # (fct_drop makes sure that only cannot assess is removed and not other unused factor levels to keep original scale)
    # mutate(across(starts_with("ipos"),~ fct_drop(., only = c("cannot assess")))) %>%
    # mutate(across(starts_with("ipos"), ~as.integer(.))) %>%
    # mutate(across(starts_with("ipos"), ~ -1*(. -(1+max(., na.rm =TRUE))))) %>%
    # mutate(across(starts_with("ipos"), ~ case_when(
    #   . == 1 ~ "overwhelmingly",
    #   . == 2 ~ "severely",
    #   . == 3 ~ "moderately",
    #   . %in% c(4,5) |is.na(.)~ "slightlyorless" )))%>%
    #   mutate(across(starts_with("ipos"),  ~ factor(., levels = c("slightlyorless","moderately","severely","overwhelmingly"),ordered = TRUE)))
    #feature_names = colnames(data %>% select(starts_with("ipos")))
  }
  
  #return(list("data" = data, "feature_names" = feature_names))
  
  stopifnot(all(colSums(is.na(data%>% select(starts_with("ipos")) ))==0)) # check that no NAs
  return(data)
}










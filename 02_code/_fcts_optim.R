is_ok_resampling_fct = function(task,
                                data_test,
                                split_type,
                                resampling_parameters){
  
  
  # Set seed
  set.seed(resampling_parameters$seed_resampling)
  
  # Specify tuner and terminator ----
  tuner = tnr("random_search") # for efficiency, batch_size = number of evaluations
  
  terminator = trm("evals", n_evals = 1) # stop searching after (k*number of HPs) evaluations
  
  # Specify resampling strategy ----
  if(split_type == "naive"){
    resampling = rsmp("cv", folds = resampling_parameters$folds_cv)
  } else if(split_type == "teams"){
    resampling = rsmp("loo")
  }
  
  resampling$instantiate(task)
  
  # seltene ausprägungen
  # within resampling
  df = task$data()
  df$row_id = task$row_ids
  inst = as.data.frame(resampling$instance)
  df=full_join(df, inst, by = "row_id")
  df = df %>% select(row_id, fold, age, ipos_shortness_breath, ipos_pain, akps)
  df = df %>% mutate(age =  cut(age, breaks = c(21,seq(50,90,10),104), include.lowest = TRUE )) %>%
    mutate(akps = case_when(
      akps %in% c("70", "80", "90") ~ "70_80_90",
      .default = akps)) %>%
    mutate(ipos_pain =  fct_collapse(ipos_pain, `not at all` = c("not at all", "cannot assess")),
           ipos_shortness_breath  =  fct_collapse(ipos_shortness_breath, `not at all` = c("not at all", "cannot assess")))
  df_sum = df %>% group_by(fold) %>% summarise_all(list(n_distinct))
  df_sum = df_sum %>% summarise(age_c = sum(age == max(age)),
                                ipos_shortness_breath_c = sum(ipos_shortness_breath == max(ipos_shortness_breath)),
                                ipos_pain_c = sum(ipos_pain == max(ipos_pain)),
                                akps_c = sum(akps == max(akps)))
  is_ok1 = rowSums(df_sum==1)==0
  
  # test data
  data_test_ok = data_test  %>%
    select(age, ipos_shortness_breath, ipos_pain, akps) %>%
    mutate(age =  cut(age, breaks = c(21,seq(50,90,10),104), include.lowest = TRUE )) %>%
    mutate(akps = case_when(
      akps %in% c("70", "80", "90") ~ "70_80_90",
      .default = akps)) %>%
    mutate(ipos_pain =  fct_collapse(ipos_pain, `not at all` = c("not at all", "cannot assess")),
           ipos_shortness_breath  =  fct_collapse(ipos_shortness_breath, `not at all` = c("not at all", "cannot assess")))  %>% 
    summarise_all(list(n_distinct)) %>% mutate(which = "test")
  df_compare = df %>% select(age, ipos_shortness_breath, ipos_pain, akps) %>% summarise_all(list(n_distinct)) %>%
    mutate(which = "train")
  data_test_ok = bind_rows(data_test_ok, df_compare)
  is_ok2 = any(data_test_ok %>% filter(which =="test") %>% select(-which) >   data_test_ok %>% filter(which =="train" )%>% select(-which)) == FALSE
  
  is_ok = is_ok1 & is_ok2
  return(is_ok)
}

######################


optim_fct = function(rep, data, id_split_list, 
                     sample_size,# = c("sample50", "sample25"), 
                     setting_name,# = c("sapv", "pmd", "station"), 
                     split_type,# = c("naive", "teams"),
                     eval_criterion, 
                     learner_name, learners_default, learners_hp_searchspace_default,
                     preproc_default, preproc_hp_searchspace_default, preproc_hp_stepopt_order,
                     procedure, procedure_list,
                     resampling_parameters){
  
  # Check whether folder already exists
  foldername = file.path("./03_results/rdata", names(which(procedure_list == procedure)))
  if(!dir.exists(foldername)){
    dir.create(foldername)
  }
  
  # Check whether file already exists, if no -> start calculation
  filename = paste0(foldername, "/res_", paste(setting_name, split_type, sample_size, eval_criterion,
                                                     names(which(procedure_list == procedure)), learner_name, rep, sep = "_"),".RData")
  

  if(!file.exists(filename)){
  
  # 1. Train/test data 
  if(split_type == "naive"){
    data_test = data %>% filter((setting == setting_name) & companion_id_grp %in% id_split_list$test_naive[[rep]])
    if(sample_size == "sample50"){
      data_train = data %>% filter((setting == setting_name) & companion_id_grp %in% id_split_list$train_50_naive[[rep]])
    } else if(sample_size == "sample25"){
      data_train = data %>% filter((setting == setting_name) & companion_id_grp %in% id_split_list$train_25_naive[[rep]])
    }
  } else if(split_type == "teams"){
    data_test = data %>% filter((setting == setting_name) & companion_id_grp %in% id_split_list$test_teams[[rep]])
    if(sample_size == "sample50"){
      data_train = data %>% filter((setting == setting_name) & companion_id_grp %in% id_split_list$train_50_teams[[rep]])
    } else if(sample_size == "sample25"){
      data_train = data %>% filter((setting == setting_name) & companion_id_grp %in% id_split_list$train_25_teams[[rep]])
    }
  }
  stopifnot(length(intersect(data_train$companion_id_grp, data_test$companion_id_grp)) ==0) # check that no phases (=id_grps) are in both datasets
  stopifnot((length(unique(data_train$setting)) == 1) & (length(unique(data_test$setting)) == 1)) # check that only one palliative care setting is considered
  # 2. Specify task
  task = as_task_regr(data_train, target = "targetvar")
  if(split_type == "teams"){   # if specified, set team_id as grouping variable -> not split when resampling
    task$col_roles$group = "team_id"
  }
  rm(data_train)
  
  ########################
 # is_ok = is_ok_resampling_fct(task = task, data_test = data_test,split_type = split_type, resampling_parameters = resampling_parameters)
 # if(is_ok){
  #######################
  
  
  # 3. Specify graph_learner (combination of preprocessing pipeline and learner)
  
  # clone all necessary objects (must not alter the input objects!)
  learner = learners_default[[learner_name]]$clone(deep = TRUE) # learner
  learner_hp_searchspace = learners_hp_searchspace_default[[learner_name]]$clone(deep = TRUE) # learner hp search space
  preproc = preproc_default$clone(deep = TRUE) # preprocessing 
  preproc_hp_searchspace = preproc_hp_searchspace_default$clone(deep = TRUE) # preprocessing hp search space
  
  # graph learner
  graph_learner = as_learner(preproc %>>%  # preprocessing pipeline
                               learner) # currently selected learner
  # also generate a featureless learner for comparison
  graph_learner_featureless = as_learner(preproc_default$clone(deep = TRUE) %>>%  # preprocessing pipeline
                                           lrn("regr.featureless")) # featureless learner
  
  
  # 4. Add eval criterion to resampling parameters
  resampling_parameters = list_modify(resampling_parameters, eval_criterion = eval_criterion)
  
  # 5. Optimize hps and get corresponding tree and error values
  results = get_tree_and_error_fct(procedure = procedure, 
                                   split_type = split_type,
                                   graph_learner = graph_learner,
                                   graph_learner_featureless = graph_learner_featureless,
                                   learner_hp_searchspace = learner_hp_searchspace,
                                   preproc_hp_searchspace = preproc_hp_searchspace,
                                   preproc_hp_stepopt_order = preproc_hp_stepopt_order,
                                   task = task, 
                                   data_test = data_test, 
                                   resampling_parameters = resampling_parameters)
  
  # Add information on procedure, learner_name and repetition
  results$procedure = procedure
  results$learner_name = as.character(learner_name)
  results$rep = rep
  results$setting = setting_name
  results$sample_size = sample_size
  results$split_type = split_type
  results$eval_criterion = eval_criterion
  
  # Save result
  save(results, file = filename)
  #return(results)
 ###isok }
  }
}



get_tree_and_error_fct = function(procedure,
                                  split_type,
                                  graph_learner,
                                  graph_learner_featureless,
                                  learner_hp_searchspace,
                                  preproc_hp_searchspace,
                                  preproc_hp_stepopt_order,
                                  task,
                                  data_test, 
                                  resampling_parameters){
  ## 0) Default procedures ------------------------------------------------------------------------
  if(procedure == "learner.hp.default_preproc.hp.default") {
   
    set.seed(resampling_parameters$seed_resampling)
    # Specify resampling strategy ----
    if(split_type == "naive"){
      resampling = rsmp("cv", folds = resampling_parameters$folds_cv)
    } else if(split_type == "teams"){
      resampling = rsmp("loo")
    }
    
    # Train learner
    graph_learner$train(task)
    
    # Predict + Calculate apparent and resampling error on train data set ----
    apparent_error = graph_learner$predict(task)$score(msr(resampling_parameters$eval_criterion))
    rr = resample(task, graph_learner, resampling)
    resampling_error = rr$aggregate(msr(resampling_parameters$eval_criterion))
    
    # Predict + Calculate error on test data set ----
    test_error = graph_learner$predict_newdata(data_test)$score(msr(resampling_parameters$eval_criterion))
    
    # Also train featureless learner and get errors ----
    graph_learner_featureless$train(task)
    apparent_error_featureless = graph_learner_featureless$predict(task)$score(msr(resampling_parameters$eval_criterion))
    test_error_featureless = graph_learner_featureless$predict_newdata(data_test)$score(msr(resampling_parameters$eval_criterion))
    
    
    final_tree = list("graph_learner" = graph_learner,
                      "apparent_error" = apparent_error,
                      "resampling_error" = resampling_error,
                      "test_error" = test_error,
                      "apparent_error_featureless" = apparent_error_featureless,
                      "test_error_featureless" = test_error_featureless)
    return(final_tree)
    
  }
  
  ## I) Procedures involving tuning but no stepwise optimization) ---------------------------------

  else if(procedure %in% c("learner.hp.tune_preproc.hp.default", "learner.hp.tune_preproc.hp.tune")) {

    if(procedure == "learner.hp.tune_preproc.hp.default"){
      # Set parameter space only for learner hps (preprocessing hp = default) 
      search_space = learner_hp_searchspace$clone(deep = TRUE)
    } else if(procedure == "learner.hp.tune_preproc.hp.tune"){
      # Set parameter space for learner and preprocessing hps
      search_space = learner_hp_searchspace$clone(deep = TRUE)
      search_space$add(preproc_hp_searchspace)
      
    }
    # Get final tree incl. i) apparent error ii) resampling error and iii) test set error 
    final_tree = resampling_fct(task = task,
                                data_test = data_test,
                                split_type = split_type,
                                graph_learner = graph_learner, 
                                search_space = search_space,
                                resampling_parameters = resampling_parameters)
    
    # Add nested resampling error 
    final_tree$nested_resampling_error = nested_resampling_fct(task = task, 
                                                               split_type = split_type,
                                                               graph_learner = graph_learner, 
                                                               search_space = search_space,
                                                               resampling_parameters = resampling_parameters)
  
    
    return(final_tree)
    
  }
  
  
  ## II) Procedures involving stepwise optimization ------------------------------------------------
  
  else if(procedure %in% c("learner.hp.tune_preproc.hp.steopt_error.apparent",
                      "learner.hp.tune_preproc.hp.steopt_error.resampling",
                      "learner.hp.default_preproc.hp.steopt_error.apparent")){
    
    # Get error of interest
    if(procedure %in% c("learner.hp.tune_preproc.hp.steopt_error.apparent",
                        "learner.hp.default_preproc.hp.steopt_error.apparent")){
      error_of_interest = "apparent_error"
      
    } else if(procedure %in% c("learner.hp.tune_preproc.hp.steopt_error.resampling")){
      error_of_interest = "resampling_error"
      
    }
    
    # Get information on learner HP tuning
    if(procedure %in% c("learner.hp.tune_preproc.hp.steopt_error.apparent",
                        "learner.hp.tune_preproc.hp.steopt_error.resampling")){
      tune_learner = TRUE
      
    } else if(procedure %in% c("learner.hp.default_preproc.hp.steopt_error.apparent")){
      tune_learner = FALSE
      
    }
    
    
    # Initialize preprocessing hyperparameter setting 
    # (= use first option [A], which was also specified as the default, so actually this step is actually not necessary)
    graph_learner$param_set$values[preproc_hp_searchspace$ids()]  =  lapply(preproc_hp_searchspace$levels,"[[",1) 

    # For each preprocessing step (in the order given by preproc_hp_stepopt_order): 
    # -> For each hp option, generate tree with tuned or default learner hps and evaluate best option w.r.t. error of interest
    tree_results_list = preproc_hp_stepopt_order %>%
      purrr::map(.f = function(x) {
        get_stepopt_preproc_hp_fct(preproc_of_interest = x,
                                   error_of_interest = error_of_interest,
                                   graph_learner = graph_learner,
                                   tune_learner = tune_learner, 
                                   learner_hp_searchspace = learner_hp_searchspace,
                                   preproc_hp_searchspace = preproc_hp_searchspace,
                                   task = task,
                                   data_test = data_test, 
                                   split_type = split_type,
                                   resampling_parameters = resampling_parameters)
      })
    names(tree_results_list) = preproc_hp_stepopt_order
    
  
    # Get final tree (incl. error according to which preproc hps were optimized)
    if(error_of_interest %in% c("apparent_error", "resampling_error")){
      # get last list element
      last_list_element =  tree_results_list[[length(tree_results_list)]] 
      # in last list element, get tree with best preproc hp
      final_tree = last_list_element[[which(sapply(last_list_element, '[[', "best_preproc_hp"))]] 
    } 
    if(tune_learner == FALSE){
      # Add final graph_learner
      final_tree = list_modify(final_tree, graph_learner = graph_learner)
      
      # Additionally calculate resampling error for final HP configuration
      set.seed(resampling_parameters$seed_resampling)
      if(split_type == "naive"){
        resampling = rsmp("cv", folds = resampling_parameters$folds_cv)
      } else if(split_type == "teams"){
        resampling = rsmp("loo")
      }
      rr = resample(task, graph_learner, resampling)
      resampling_error = rr$aggregate(msr(resampling_parameters$eval_criterion))
      final_tree = list_modify(final_tree, resampling_error = resampling_error)
    }
    
   
    return(list("tree_results_list" = tree_results_list, "final_tree" = final_tree))
  }
}
  

# function that returns a nested list containing the values_to_evaluate where across the lists, only one preprocessing hp varies
get_toeval_preproc_hp_fct = function(graph_learner, 
                                          preproc_hp_searchspace, 
                                          preproc_of_interest){
  # Get possible options for current preprocessing hp of interest
  preproc_of_interest_options = preproc_hp_searchspace$levels[[preproc_of_interest]]
  
  # Replicate current hp values
  current_preproc_hp = graph_learner$param_set$values[preproc_hp_searchspace$ids()]
  toeval_preproc_hp = replicate(length(preproc_of_interest_options),
                                current_preproc_hp, simplify = FALSE)
  
  # Dynamically insert value of preprocessing hp of interest into options to evaluate
  for(i in 1:length(preproc_of_interest_options)){
    toeval_preproc_hp[[i]][[preproc_of_interest]] = preproc_of_interest_options[i]
  }
  return(toeval_preproc_hp)
}




get_stepopt_preproc_hp_fct = function(preproc_of_interest,
                                      error_of_interest,
                                      graph_learner,
                                      tune_learner,
                                      learner_hp_searchspace,
                                      preproc_hp_searchspace,
                                      task,
                                      data_test, 
                                      split_type,
                                      resampling_parameters){
  
  
  # For each preprocessing operation, generate list of possible hp values that will be evaluated 
  toeval_preproc_hp = get_toeval_preproc_hp_fct(graph_learner = graph_learner,
                                                     preproc_hp_searchspace = preproc_hp_searchspace,
                                                     preproc_of_interest = preproc_of_interest)
  
  if(tune_learner == TRUE){
    # Generate search space for tuning (only include learner hps)
    search_space = learner_hp_searchspace$clone(deep = TRUE)
    
    # Generate learner incl. preprocessing pipeline with set preprocessing HPs (fixed for learner hp tuning) 
      names_preproc = preproc_hp_searchspace$ids()
      tree_results = toeval_preproc_hp %>%
        purrr::map(.f = function(x) {
          graph_learner$param_set$values[names_preproc] = x 
          results = resampling_fct(task = task,
                                   data_test = data_test,
                                   split_type = split_type,
                                   graph_learner = graph_learner, 
                                   search_space = search_space,
                                   resampling_parameters = resampling_parameters)
          return(results)
        })
      
      if(resampling_parameters$eval_criterion == "regr.rsq"){
        which.best_preproc_hp = which.max(sapply(tree_results, '[[', error_of_interest))
      } else if(resampling_parameters$eval_criterion == "regr.rmse"){
        which.best_preproc_hp = which.min(sapply(tree_results, '[[', error_of_interest))
      }
      
      ind.best_preproc_hp = logical(length(tree_results))
      ind.best_preproc_hp[which.best_preproc_hp] = TRUE
      tree_results = Map(c, tree_results, "best_preproc_hp" = ind.best_preproc_hp)
      
  } else if(tune_learner == FALSE & error_of_interest == "apparent_error"){
    # Generate learner incl. preprocessing pipeline with set preprocessing HPs (and default learner HPs)
    names_preproc = preproc_hp_searchspace$ids()
    tree_results = toeval_preproc_hp %>%
      purrr::map(.f = function(x) {
        graph_learner$param_set$values[names_preproc] = x 
      
        # Train learner
        graph_learner$train(task)
        # Predict + Calculate apparent error on train data set 
        apparent_error = graph_learner$predict(task)$score(msr(resampling_parameters$eval_criterion))
        # Predict + Calculate error on test data set 
        test_error = graph_learner$predict_newdata(data_test)$score(msr(resampling_parameters$eval_criterion))
        
        results = list("apparent_error" = apparent_error,
                       "test_error"= test_error)
        return(results)
      })
    if(resampling_parameters$eval_criterion == "regr.rsq"){
      which.best_preproc_hp = which.max(sapply(tree_results, '[[', error_of_interest))
    } else if(resampling_parameters$eval_criterion == "regr.rmse"){
      which.best_preproc_hp = which.min(sapply(tree_results, '[[', error_of_interest))
    }
    ind.best_preproc_hp = logical(length(tree_results))
    ind.best_preproc_hp[which.best_preproc_hp] = TRUE
    tree_results = Map(c, tree_results, "best_preproc_hp" = ind.best_preproc_hp)
    
  } 
  
  
  # Update learner hps using stepwise optimized hps
  names_preproc = preproc_hp_searchspace$ids()
  graph_learner$param_set$values[names_preproc] = toeval_preproc_hp[[which.best_preproc_hp]]
  
  return(tree_results)
}











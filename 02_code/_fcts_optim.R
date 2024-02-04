optim_fct = function(rep, data, id_train_list, setting,
                     learner_name, learners_default, learners_hp_searchspace_default,
                     preproc_default, preproc_hp_searchspace_default, preproc_hp_stepopt_order,
                     procedure, procedure_list,
                     resampling_parameters){
  # Check whether file already exists, if no -> start calculation
  filename = paste0("./03_results/rdata/res_", paste(setting, names(which(procedure_list == procedure)), learner_name, rep, sep = "_"),".RData")
  if(!file.exists(filename)){
  
  # 1. Train/test data
  id_train = id_train_list[[rep]]
  data_train = data %>% filter((setting == setting) &
                                            (companion_id %in% id_train))
  data_test = data %>% filter((setting == setting) &
                                           !(companion_id %in% id_train))
  stopifnot(length(intersect(data_train$companion_id, data_test$companion_id)) ==0) # make sure no ids are in both datasets
  
  # 2. Specify task
  task = as_task_regr(data_train, target = "targetvar")
  # specify companion_id as grouping variable bc observations from same id should not be split when resampling
  # = group by id 
  task$col_roles$group = "companion_id"
  rm(data_train)
  
  # 3. Specify graph_learner (combination of preprocessing pipeline and learner)
  
  # clone all necessary objects (must not alter the input objects!)
  learner = learners_default[[learner_name]]$clone(deep = TRUE) # learner
  learner_hp_searchspace = learners_hp_searchspace_default[[learner_name]]$clone(deep = TRUE) # learner hp search space
  preproc = preproc_default$clone(deep = TRUE) # preprocessing 
  preproc_hp_searchspace = preproc_hp_searchspace_default$clone(deep = TRUE) # preprocessing hp search space
  
  # graph learner
  graph_learner = as_learner(preproc %>>%  # preprocessing pipeline
                               learner) # currently selected learner
  
  # 4. Optimize hps and get corresponding tree and error values
  results = get_tree_and_error_fct(procedure = procedure, 
                                   graph_learner = graph_learner,
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
  results$setting = setting
  
  # Save result
  save(results, file = filename)
  #return(results)
  }
}



get_tree_and_error_fct = function(procedure,
                                  graph_learner,
                                  learner_hp_searchspace,
                                  preproc_hp_searchspace,
                                  preproc_hp_stepopt_order,
                                  task,
                                  data_test, 
                                  resampling_parameters){
  
  ## I) Procedures  involving tuning but no stepwise optimization) ---------------------------------

  if(procedure %in% c("learner.hp.tune_preproc.hp.default", "learner.hp.tune_preproc.hp.tune")) {

    if(procedure == "learner.hp.tune_preproc.hp.default"){
      # Set parameter space only for learner hps (preprocessing hp = default) 
      search_space = learner_hp_searchspace$clone(deep = TRUE)
    } else if(procedure == "preproc.hp.tune_learner.hp.tune"){
      # Set parameter space for learner and preprocessing hps
      search_space = learner_hp_searchspace$clone(deep = TRUE)
      search_space$add(preproc_hp_searchspace)
      
    }
    # Get final tree incl. i) apparent error ii) resampling error and iii) test set error 
    final_tree = resampling_fct(task = task,
                                  data_test = data_test,
                                  graph_learner = graph_learner, 
                                  search_space = search_space,
                                  resampling_parameters = resampling_parameters)
    
    # Add nested resampling error 
    final_tree$nested_resampling_error = nested_resampling_fct(task = task, 
                                                                         graph_learner = graph_learner, 
                                                                         search_space = search_space,
                                                                         resampling_parameters = resampling_parameters)
  
    
    return(final_tree)
    
  }
  
  
  ## II) Procedures involving stepwise optimization ------------------------------------------------
  
  else if(procedure %in% c("learner.hp.tune_preproc.hp.steopt_error.apparent",
                      "learner.hp.tune_preproc.hp.steopt_error.resampling",
                      "learner.hp.tune_preproc.hp.steopt_error.nested_resampling")){
    
    if(procedure == "learner.hp.tune_preproc.hp.steopt_error.apparent"){
      error_of_interest = "apparent_error"
      
    } else if(procedure == "learner.hp.tune_preproc.hp.steopt_error.resampling"){
      error_of_interest = "resampling_error"
      
    } else if(procedure == "learner.hp.tune_preproc.hp.steopt_error.nested_resampling"){
      error_of_interest = "nested_resampling_error"
    }
    
    # Initialize preprocessing hyperparameter setting 
    # (= use first option [A], which was also specified as the default, so actually this step is not necessary)
    graph_learner$param_set$values[preproc_hp_searchspace$ids()]  =  lapply(preproc_hp_searchspace$levels,"[[",1) 

    # For each preprocessing step (in the order given by preproc_hp_stepopt_order): 
    # -> For each hp option, generate tree with tuned learner hps and evaluate best option w.r.t. error of interest
    tree_results_list = preproc_hp_stepopt_order %>%
      purrr::map(.f = function(x) {
        get_stepopt_preproc_hp_fct(preproc_of_interest = x,
                                   error_of_interest = error_of_interest,
                                   graph_learner = graph_learner,
                                   learner_hp_searchspace = learner_hp_searchspace,
                                   preproc_hp_searchspace = preproc_hp_searchspace,
                                   task = task,
                                   data_test = data_test, 
                                   resampling_parameters = resampling_parameters)
      })
    names(tree_results_list) = preproc_hp_stepopt_order
    
    # Get final tree (incl. error according to which preproc hps were optimized)
    if(error_of_interest %in% c("apparent_error", "resampling_error")){
      # get last list element
      last_list_element =  tree_results_list[[length(tree_results_list)]] 
      # in last list element, get tree with best preproc hp
      final_tree = last_list_element[[which(sapply(last_list_element, '[[', "best_preproc_hp"))]] 
    } else if(error_of_interest == "nested_resampling_error"){
      
      # param_set of graph_learner was updated and now has the step-wise optimized preproc hp values
      # only need to tune learner hps one last time and evaluate on test data
      search_space = learner_hp_searchspace$clone(deep = TRUE)

      final_tree = resampling_fct(task = task,
                                    data_test = data_test,
                                    graph_learner = graph_learner, 
                                    search_space = search_space,
                                    resampling_parameters = resampling_parameters)
      # Add nested resampling error 
      # get last list element
      last_list_element =  tree_results_list[[length(tree_results_list)]] 
      # in last list element, get tree with best preproc hp
      final_tree_nested_resampling_error = unlist(last_list_element[[which(sapply(last_list_element,
                                                                                  '[[', "best_preproc_hp"))]][resampling_parameters$eval_criterion])
      
      final_tree$nested_resampling_error = final_tree_nested_resampling_error
    }

    return(list("tree_results_list" = tree_results_list, "final_tree" = final_tree))
  }
  
  ## III) Reference procedure ----------------------------------------------------------------------
  # (learner hps: value most prone to overfitting; preprocessing: try all combinations) 
  else if(procedure == "learner.hp.maxoverfit_preproc.hp.allcombinations_error.apparent"){

    # Set learner hps to values most prone to overfitting 
    param_names = graph_learner$param_set$ids()
    # -> Set minbucket to value most prone to overfitting (=lowest possible value)
    param_name_minbucket = param_names[grep(".minbucket", param_names)]
    stopifnot(length(param_name_minbucket)==1) # make sure that only one hp was selected
    overfitvalue_minbucket = learner_hp_searchspace$params[[param_name_minbucket]]$lower
    graph_learner$param_set$values[[param_name_minbucket]] = overfitvalue_minbucket
    
    # -> Set complexity parameters to value most prone to overfitting 
    # (cp: lowest possible; alpha: highest possible; mincriterion: lowest possible)
    param_name_complexity = param_names[grep(".cp|.alpha|.mincriterion", param_names)]
    stopifnot(length(param_name_complexity)==1) # make sure that only one hp was selected
    if(grepl(".alpha", param_name_complexity)){
      overfitvalue_complexity = learner_hp_searchspace$params[[param_name_complexity]]$upper
    } else{
      overfitvalue_complexity = learner_hp_searchspace$params[[param_name_complexity]]$lower
    }
    graph_learner$param_set$values[[param_name_complexity]] = overfitvalue_complexity
    
    # Try all combinations of preprocessing hp values
    toeval_preproc_hp=expand.grid(preproc_hp_searchspace$levels)
    toeval_preproc_hp_list =apply(toeval_preproc_hp, 1, FUN = function(i) lapply(split(unlist(i), # expand.grid yields df but need list
                                                                                       names(toeval_preproc_hp)), 
                                                                                 as.character), 
                                  simplify = FALSE)
    names_preproc = preproc_hp_searchspace$ids()
    apparent_error_results = toeval_preproc_hp_list %>%
      purrr::map_dbl(.f = function(x) {
        graph_learner$param_set$values[names_preproc] = x[names_preproc]
        graph_learner$train(task)
        apparent_error = graph_learner$predict(task)$score(msr(resampling_parameters$eval_criterion))
        return(apparent_error)
      })
    ind.best_preproc_hp = which.max(unname(apparent_error_results))
    
    # Train graph_learner one last time with best combination and evaluate on test data 
    graph_learner$param_set$values[names_preproc] = toeval_preproc_hp_list[[ind.best_preproc_hp]][names_preproc]
    graph_learner$train(task)
    apparent_error = graph_learner$predict(task)$score(msr(resampling_parameters$eval_criterion))
    test_error = graph_learner$predict_newdata(data_test)$score(msr(resampling_parameters$eval_criterion))
    
    final_tree = list("graph_learner" = graph_learner,
                      "apparent_error" = apparent_error,
                      "test_error"= test_error,
                      "procedure" = procedure,
                      "rep" = rep)
    return(final_tree)
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
                                      learner_hp_searchspace,
                                      preproc_hp_searchspace,
                                      task,
                                      data_test, 
                                      resampling_parameters){
  
  
  # For each preprocessing operation, generate list of possible hp values that will be evaluated 
  toeval_preproc_hp = get_toeval_preproc_hp_fct(graph_learner = graph_learner,
                                                     preproc_hp_searchspace = preproc_hp_searchspace,
                                                     preproc_of_interest = preproc_of_interest)
  
  # Generate search space for tuning (only include learner hps)
  search_space = learner_hp_searchspace$clone(deep = TRUE)
  
  # Generate learner incl. preprocessing pipeline with set preprocessing parameters (fixed for learner hp tuning) 
  if(error_of_interest %in% c("apparent_error", "resampling_error")){
    names_preproc = preproc_hp_searchspace$ids()
    tree_results = toeval_preproc_hp %>%
      purrr::map(.f = function(x) {
        graph_learner$param_set$values[names_preproc] = x 
        results = resampling_fct(task = task,
                                   data_test = data_test,
                                   graph_learner = graph_learner, 
                                   search_space = search_space,
                                   resampling_parameters = resampling_parameters)
        return(results)
      })
     
    which.best_preproc_hp = which.max(sapply(tree_results, '[[', error_of_interest))
    ind.best_preproc_hp = logical(length(tree_results))
    ind.best_preproc_hp[which.best_preproc_hp] = TRUE
    tree_results = Map(c, tree_results, "best_preproc_hp" = ind.best_preproc_hp)
    
  } else if(error_of_interest == "nested_resampling_error"){
    names_preproc = preproc_hp_searchspace$ids()
    tree_results = toeval_preproc_hp %>%
      purrr::map(.f = function(x) {
        graph_learner$param_set$values[names_preproc] = x 
        results = nested_resampling_fct(task = task,
                                                  graph_learner = graph_learner, 
                                                  search_space = search_space,
                                                  resampling_parameters = resampling_parameters)
        return(results)
      })
    
    which.best_preproc_hp = which.max(unname(unlist(tree_results)))
    ind.best_preproc_hp = logical(length(tree_results))
    ind.best_preproc_hp[which.best_preproc_hp] = TRUE
    
    tree_results = lapply(tree_results, as.list)
    tree_results = Map(c, tree_results, "best_preproc_hp" = ind.best_preproc_hp)
  }
  
  # Update learner hps using stepwise optimized hps
  names_preproc = preproc_hp_searchspace$ids()
  graph_learner$param_set$values[names_preproc] = toeval_preproc_hp[[which.best_preproc_hp]]
  
  return(tree_results)
}











get_tree_and_error_fct = function(procedure,
                                      preproc_hp_searchspace,
                                      task,
                                      data_test, 
                                      eval_criterion, 
                                      rpart_hp,
                                      tuning_parameters){
  
  if(procedure %in% c("preproc.hp.steopt_algo.hp.tune_error.resampling",
                      "preproc.hp.steopt_algo.hp.tune_error.apparent",
                      "preproc.hp.steopt_algo.hp.tune_error.nested_resampling")){
    
    if(procedure == "preproc.hp.steopt_algo.hp.tune_error.resampling"){
      error_of_interest = "resampling_error"
    } else if(procedure == "preproc.hp.steopt_algo.hp.tune_error.apparent"){
      error_of_interest = "apparent_error"
    } else if(procedure == "preproc.hp.steopt_algo.hp.tune_error.nested_resampling"){
      error_of_interest = "nested_resampling_error"
    }
    
    # Initialize preprocessing hyperparameter setting (= defaults = all options set to "A")
    current_preproc_hp = list("preproc.target" = preproc_hp_searchspace$preproc.target[[1]],
                              "preproc.drop.targetout" = preproc_hp_searchspace$preproc.drop.targetout[[1]],
                              "preproc.drop.iposca" = preproc_hp_searchspace$preproc.drop.iposca[[1]],
                              "preproc.feature.ipos" = preproc_hp_searchspace$preproc.feature.ipos[[1]],
                              "preproc.feature.age" = preproc_hp_searchspace$preproc.feature.age[[1]],
                              "preproc.feature.akps" = preproc_hp_searchspace$preproc.feature.akps[[1]])
    
    # For each preprocessing operation, generate a tree with each option where the algorithm hyperparameters 
    # are tuned using resampling    
    tree_results_list = vector("list", length = length(preproc_hp_stepopt_order))
    for(i in 1:length(preproc_hp_stepopt_order)){
      tree_results_and_hp = get_stepopt_preproc_hp_fct(current_preproc_hp = current_preproc_hp,
                                                       preproc_hp_searchspace = preproc_hp_searchspace,
                                                       preproc_of_interest = preproc_hp_stepopt_order[i],
                                                       error_of_interest = error_of_interest,
                                                       task = task,
                                                       data_test = data_test, 
                                                       eval_criterion = eval_criterion, 
                                                       rpart_hp = rpart_hp,
                                                       tuning_parameters = tuning_parameters)
      current_preproc_hp = tree_results_and_hp$updated_preproc_hp
      tree_results_list[[i]] = tree_results_and_hp$tree_results
    }
    
    # Get final tree ----
    if(error_of_interest %in% c("apparent_error", "resampling_error")){
      # get last list element
      last_list_element =  tree_results_list[[length(tree_results_list)]] 
      # in last list element, get tree with best preproc hp
      final_tree = last_list_element[[which(sapply(last_list_element, '[[', "best_preproc_hp"))]] 
    } else if(error_of_interest == "nested_resampling_error"){
      # Get stepwise optimized preproc hp
      final_preproc_hp = current_preproc_hp 
      
      # Generate final tree by tuning algorithm hp with step-optimized preprocessing hp ----
      learner = lrn("regr.rpart", 
                    maxdepth = rpart_hp$fixed$maxdepth, 
                    xval = rpart_hp$fixed$xval, 
                    keep_model = rpart_hp$fixed$keep_model) 
      graph_learner = as_learner(po("preproc.target", option = final_preproc_hp$preproc.target) %>>%
                                   po("preproc.drop.targetout", option = final_preproc_hp$preproc.drop.targetout) %>>%
                                   po("preproc.drop.iposca", option = final_preproc_hp$preproc.drop.iposca) %>>%
                                   po("preproc.feature.ipos", option = final_preproc_hp$preproc.feature.ipos) %>>% 
                                   po("preproc.feature.age", option = final_preproc_hp$preproc.feature.age) %>>% 
                                   po("preproc.feature.akps", option = final_preproc_hp$preproc.feature.akps) %>>% 
                                   po("fixfactors") %>>%
                                   learner)
      search_space = ps(
        regr.rpart.cp = p_dbl(lower = rpart_hp$tuning$cp_lower, 
                              upper = rpart_hp$tuning$cp_upper),
        regr.rpart.minbucket = p_int(lower = rpart_hp$tuning$minbucket_lower, 
                                     upper = rpart_hp$tuning$minbucket_upper)
      ) 
      final_tree = get_tuned_hp_fct(task = task,
                                    data_test = data_test,
                                    graph_learner = graph_learner, 
                                    search_space = search_space,
                                    eval_criterion = eval_criterion, 
                                    tuning_parameters = tuning_parameters)
      # Add nested resampling error 
      # get last list element
      last_list_element =  tree_results_list[[length(tree_results_list)]] 
      # in last list element, get tree with best preproc hp
      final_tree_nested_resampling_error = unlist(last_list_element[[which(sapply(last_list_element,
                                                                                  '[[', "best_preproc_hp"))]][eval_criterion])
      
      final_tree$nested_resampling_error = final_tree_nested_resampling_error
    }


    return(list("tree_results_list" = tree_results_list, "final_tree" = final_tree))
  }
  
  else if(procedure %in% c("preproc.hp.default_algo.hp.tune", "preproc.hp.tune_algo.hp.tune")) {
    # Generate learner + preprocessing pipeline ----
    learner = lrn("regr.rpart", 
                  maxdepth = rpart_hp$fixed$maxdepth, 
                  xval = rpart_hp$fixed$xval, 
                  keep_model = rpart_hp$fixed$keep_model) 
    graph_learner = as_learner(po("preproc.target") %>>%
                                 po("preproc.drop.targetout") %>>%
                                 po("preproc.drop.iposca") %>>%
                                 po("preproc.feature.ipos") %>>% 
                                 po("preproc.feature.age") %>>% 
                                 po("preproc.feature.akps") %>>% 
                                 po("fixfactors") %>>%
                                learner)
    if(procedure == "preproc.hp.default_algo.hp.tune"){
      # Set parameter space only for algorithm (preprocessing hp = default) ----
      search_space = ps(
        regr.rpart.cp = p_dbl(lower = rpart_hp$tuning$cp_lower, 
                              upper = rpart_hp$tuning$cp_upper),
        regr.rpart.minbucket = p_int(lower = rpart_hp$tuning$minbucket_lower, 
                                     upper = rpart_hp$tuning$minbucket_upper)
      )
    } else if(procedure == "preproc.hp.tune_algo.hp.tune"){
      # Set parameter space for algorithm and preprocessing hp ----
      search_space = ps(
        regr.rpart.cp = p_dbl(lower = rpart_hp$tuning$cp_lower, 
                              upper = rpart_hp$tuning$cp_upper),
        regr.rpart.minbucket = p_int(lower = rpart_hp$tuning$minbucket_lower, 
                                     upper = rpart_hp$tuning$minbucket_upper),
        preproc.target.option = p_fct(preproc_hp_searchspace$preproc.target),
        preproc.drop.targetout.option = p_fct(preproc_hp_searchspace$preproc.drop.targetout),
        preproc.drop.iposca.option = p_fct(preproc_hp_searchspace$preproc.drop.iposca),
        preproc.feature.ipos.option = p_fct(preproc_hp_searchspace$preproc.feature.ipos),
        preproc.feature.age.option = p_fct(preproc_hp_searchspace$preproc.feature.age),
        preproc.feature.akps.option = p_fct(preproc_hp_searchspace$preproc.feature.akps)
      ) 
    }

    # Get tree incl. apparent and resampling and test set error ----
   final_tree = get_tuned_hp_fct(task = task,
                     data_test = data_test,
                     graph_learner = graph_learner, 
                     search_space = search_space,
                     eval_criterion = eval_criterion, 
                     tuning_parameters = tuning_parameters)
    
   # Add nested resampling error ----
   final_tree$nested_resampling_error = get_nested_resampling_error_fct(task = task, 
                                                                               graph_learner = graph_learner, 
                                                                               search_space = search_space,
                                                                               eval_criterion = eval_criterion, 
                                                                               tuning_parameters = tuning_parameters)
   return(final_tree)
    
  }

}
  



# function that returns a nested list containing the values_to_evaluate where across the lists, only one data choice varies
generate_toeval_preproc_hp_fct = function(current_preproc_hp, preproc_hp_searchspace, preproc_of_interest){
  toeval_preproc_hp = replicate(length(preproc_hp_searchspace[[preproc_of_interest]]), current_preproc_hp, simplify = FALSE)
  
  # dynamically insert value of choice of interest into options to evaluate
  for(i in 1:length(preproc_hp_searchspace[[preproc_of_interest]])){
    toeval_preproc_hp[[i]][[preproc_of_interest]] = preproc_hp_searchspace[[preproc_of_interest]][i]
  }
  return(toeval_preproc_hp)
}


get_stepopt_preproc_hp_fct = function(current_preproc_hp,
                                             preproc_hp_searchspace,
                                             preproc_of_interest,
                                             error_of_interest,
                                             task,
                                             data_test, 
                                             eval_criterion, 
                                             rpart_hp,
                                             tuning_parameters){
  # For each preprocessing operation, generate list of possible hp values that will be evaluated ----
  toeval_preproc_hp = generate_toeval_preproc_hp_fct(current_preproc_hp = current_preproc_hp,
                                                     preproc_hp_searchspace = preproc_hp_searchspace,
                                                     preproc_of_interest = preproc_of_interest)
  
  # Generate learner incl. preprocessing pipeline with set preprocessing parameters (fixed for algorithm hp tuning) ----
  learner = lrn("regr.rpart", 
                maxdepth = rpart_hp$fixed$maxdepth, 
                xval = rpart_hp$fixed$xval, 
                keep_model = rpart_hp$fixed$keep_model) 
  graph_list = toeval_preproc_hp %>%
    purrr::map(~ as_learner(po("preproc.target", option = .x$preproc.target) %>>%
                              po("preproc.drop.targetout", option = .x$preproc.drop.targetout) %>>%
                              po("preproc.drop.iposca", option = .x$preproc.drop.iposca) %>>%
                              po("preproc.feature.ipos", option = .x$preproc.feature.ipos) %>>% 
                              po("preproc.feature.age", option = .x$preproc.feature.age) %>>% 
                              po("preproc.feature.akps", option = .x$preproc.feature.akps) %>>% 
                              po("fixfactors") %>>%
                 learner))
  # #graph$plot()
  
  # Set parameter space for algorithm ----
  search_space = ps(
    regr.rpart.cp = p_dbl(lower = rpart_hp$tuning$cp_lower, 
                          upper = rpart_hp$tuning$cp_upper),
    regr.rpart.minbucket = p_int(lower = rpart_hp$tuning$minbucket_lower, 
                                 upper = rpart_hp$tuning$minbucket_upper)
  ) 
  
  # Get best hyperparameter setting using resampling or nested resampling
  if(error_of_interest %in% c("apparent_error", "resampling_error")){
    tree_results = graph_list %>%
      purrr::map(~ get_tuned_hp_fct(task = task,
                                    data_test = data_test,
                                    graph_learner = .x, 
                                    search_space = search_space,
                                    eval_criterion = eval_criterion, 
                                    tuning_parameters = tuning_parameters))
    
    which.best_preproc_hp = which.max(sapply(tree_results, '[[', error_of_interest))
    ind.best_preproc_hp = logical(length(tree_results))
    ind.best_preproc_hp[which.best_preproc_hp] = TRUE
    tree_results = Map(c, tree_results, "best_preproc_hp" = ind.best_preproc_hp)
    
  } else if(error_of_interest == "nested_resampling_error"){
    tree_results =  graph_list %>%
      purrr::map(~ get_nested_resampling_error_fct(task = task,
                                                   graph_learner = .x, 
                                                   search_space = search_space,
                                                   eval_criterion = eval_criterion, 
                                                   tuning_parameters = tuning_parameters))
    
    
    which.best_preproc_hp = which.max(unname(unlist(tree_results)))
    ind.best_preproc_hp = logical(length(tree_results))
    ind.best_preproc_hp[which.best_preproc_hp] = TRUE
    
    tree_results = lapply(tree_results, as.list)
    tree_results = Map(c, tree_results, "best_preproc_hp" = ind.best_preproc_hp)
  }

  # Update preprocessing hp setting
  updated_preproc_hp = toeval_preproc_hp[[which.best_preproc_hp]]
  
  return(list("updated_preproc_hp" = updated_preproc_hp, "tree_results" = tree_results))
}









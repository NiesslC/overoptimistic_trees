resampling_fct = function(task,
                        data_test,
                        graph_learner, 
                        search_space,
                        resampling_parameters){

  # Set seed
  set.seed(resampling_parameters$seed_resampling)
  
  # Specify tuner and terminator ----
  tuner = tnr("grid_search", resolution = resampling_parameters$resolution) # grid search, resolution of the grid 
  terminator = trm("evals", n_evals = resampling_parameters$n_evals) # stop searching after n_evals evaluations
  
  # Tune parameters using cross-validation ---
  instance = TuningInstanceSingleCrit$new(
    task = task,
    learner = graph_learner,
    resampling =  rsmp("cv", folds = resampling_parameters$folds_cv), 
    measure = msr(resampling_parameters$eval_criterion),
    search_space = search_space,
    terminator = terminator
  )
  tuner$optimize(instance)
  
  # Get best parameters and grow tree on train_data ----
  graph_learner_tuned = graph_learner$clone(deep = TRUE) # clone because otherwise we cannot track the graph_learners tuned during stepwise optimization
  graph_learner_tuned$param_set$values = instance$result_learner_param_vals
  graph_learner_tuned$train(task)
  
  # Predict + Calculate apparent and resampling error on train data set ----
  apparent_error = graph_learner_tuned$predict(task)$score(msr(resampling_parameters$eval_criterion))
  resampling_error = unlist(instance$result %>% select(all_of(resampling_parameters$eval_criterion)))
  
  # Predict + Calculate error on test data set ----
  test_error = graph_learner_tuned$predict_newdata(data_test)$score(msr(resampling_parameters$eval_criterion))
  
  # Return results ----
    result = list("graph_learner_tuned" = graph_learner_tuned,
                  #"hp_values" = instance$result_learner_param_vals,
                  # "train_task" = graph_learner_tuned$model[[tail(names(graph_learner$model),n=1)]]$train_task, # task that was eventually used to train the model
                  "apparent_error" = apparent_error,
                  "resampling_error" = resampling_error,
                  "test_error"= test_error)
    return(result)
}



nested_resampling_fct = function(task,
                                           graph_learner, 
                                           search_space,
                                           resampling_parameters){
  # Set seed
  set.seed(resampling_parameters$seed_nestedresampling)
  
  # Specify nested resampling scheme (tuner, terminator, inner and outer resampling etc.)
  tuner = tnr("grid_search", resolution = resampling_parameters$resolution) # grid search, resolution of the grid = 50
  terminator = trm("evals", n_evals = resampling_parameters$n_evals) # stop searching after n_evals evaluations
  inner_resampling = rsmp("cv", folds = resampling_parameters$inner_folds_nestedcv) # number of inner folds
  outer_resampling = rsmps("repeated_cv", repeats = resampling_parameters$outer_repeats, folds = resampling_parameters$outer_folds_nestedcv)
  
  graph_learner_tune = auto_tuner(
    tuner = tuner,
    learner = graph_learner,
    search_space = search_space,
    resampling = inner_resampling,
    measure = msr(resampling_parameters$eval_criterion),
    terminator = terminator)
  
  # Get nested resampling error
  design = benchmark_grid(
    tasks = task,
    learners = graph_learner_tune,
    resamplings = outer_resampling
  )
  bmr = benchmark(design, store_models = TRUE)
  tab = bmr$aggregate(msr(resampling_parameters$eval_criterion))
  
  # Result contains some useful information about the nested resampling result, but will only extract nested resampling error
  eval_criterion = resampling_parameters$eval_criterion
  nested_resampling_error = unlist(tab[,..eval_criterion])
  return(nested_resampling_error)
  
}






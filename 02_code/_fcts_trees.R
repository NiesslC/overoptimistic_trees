get_tuned_hp_fct = function(task,
                        data_test,
                        graph_learner, 
                        search_space,
                        eval_criterion, 
                        tuning_parameters){

  
  # Specify tuner and terminator ----
  tuner = tnr("grid_search", resolution = tuning_parameters$resolution) # grid search, resolution of the grid 
  terminator = trm("evals", n_evals = tuning_parameters$n_evals) # stop searching after n_evals evaluations
  
  # Tune parameters using cross-validation ---
  instance = TuningInstanceSingleCrit$new(
    task = task,
    learner = graph_learner,
    resampling =  rsmp("cv", folds = tuning_parameters$inner_folds), # 5 fold cross-test
    measure = msr(eval_criterion),
    search_space = search_space,
    terminator = terminator
  )
  tuner$optimize(instance)
  
  # Get best parameters and grow tree on train_data ----
  graph_learner_tuned = graph_learner
  graph_learner_tuned$param_set$values = instance$result_learner_param_vals
  graph_learner_tuned$train(task)
  
  # Predict + Calculate apparent and resampling error on train data set ----
  apparent_error = graph_learner_tuned$predict(task)$score(msr(eval_criterion))
  resampling_error = unlist(instance$result %>% select(all_of(eval_criterion)))
  
  # Predict + Calculate error on test data set ----
  test_error = graph_learner_tuned$predict_newdata(data_test)$score(msr(eval_criterion))
  
  # Return results ----
    result = list("graph_learner_tuned" = graph_learner_tuned,
                  "hp_values" = instance$result_learner_param_vals,
                  "train_task" = graph_learner_tuned$model$regr.rpart$train_task, # task that was eventually used to train the model
                  "apparent_error" = apparent_error,
                  "resampling_error" = resampling_error,
                  "test_error"= test_error)
    return(result)
}



get_nested_resampling_error_fct = function(task,
                                           graph_learner, 
                                           search_space,
                                           eval_criterion, 
                                           tuning_parameters){
  
  # Specify nested resampling scheme (tuner, terminator, inner and outer resampling etc.)
  tuner = tnr("grid_search", resolution = tuning_parameters$resolution) # grid search, resolution of the grid = 50
  terminator = trm("evals", n_evals = tuning_parameters$n_evals) # stop searching after n_evals evaluations
  inner_resampling = rsmp("cv", folds = tuning_parameters$inner_folds) # number of inner folds
  outer_resampling = rsmps("repeated_cv", repeats = tuning_parameters$outer_repeats, folds = tuning_parameters$outer_folds)
  
  graph_learner_tune = AutoTuner$new(graph_learner, 
                               inner_resampling, 
                               msr(eval_criterion), 
                               terminator, 
                               tuner,
                               search_space)
  
  # Get nested resampling error
  design = benchmark_grid(
    tasks = task,
    learners = graph_learner_tune,
    resamplings = outer_resampling
  )
  bmr = benchmark(design, store_models = TRUE)
  tab = bmr$aggregate(msr(eval_criterion))
  
  # Result contains some useful information about the nested resampling result, but will only extract nested resampling error
  nested_resampling_error = unlist(tab[,..eval_criterion])
  return(nested_resampling_error)
  
}






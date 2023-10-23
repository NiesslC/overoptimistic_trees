# FUNCTION create_pipeops 
# = function that transforms target in learner with log-transformation
# and backtransforms target when evaluating the prediction
# INPUT
# - learner: learner that will be applied to task where target should be log-transformed
# OUTPUT
# - gl2: learner that will no log-transform (and back-tranform) target when applied
create_pipeops <- function(learner) {
  g_ppl = ppl("targettrafo", graph = learner)
  g_ppl$param_set$values$targetmutate.trafo = function(x) log(x)
  g_ppl$param_set$values$targetmutate.inverter = function(x) list(response = exp(x$response))
  gl = GraphLearner$new(g_ppl)
  gl2 = as_learner(gl)
  return(gl2)
}



# FUNCTION cart_no_resampling_fct 
# = function generates tree using the CART algorithm (from package rpart). 
# Returns apparent error and (optional) error on validation data. 
# INPUT
# - data_train: train data
# - data_validation: validation data
# - features: which features should be considered for the tree
# - eval_criterion: which evaluation criterion (e.g. rsquared)
# - cp: complexity 
# - minbucket: minimum number of observations in any terminal nde
# - maxdepth: maximum depth of any node of the final tree
# - plot: should result be a list or a plot
# OUTPUT: 
# - list with errors (plot = FALSE) or plot incl. errors (plot = TRUE)
  cart_no_resampling_fct = function(data_train, data_validation = NULL, features,
                                    eval_criterion,
                                    cp = 0.01,
                                    minbucket = round(20/3), 
                                    maxdepth = 4,
                                    logtarget = FALSE,
                                    plot = FALSE){
  
  # specify algorithm and parameters 
  learner = lrn("regr.rpart", # = CART, should not be varied!
                cp = cp,
                minbucket = minbucket,
                maxdepth = maxdepth,
                xval = 0,  # otherwise rpart would do internal cross validation, do not change
                keep_model = TRUE) 
  
  # set log-tansformation of target if necessary
  if(logtarget == TRUE){
  learner = create_pipeops(learner)
  }
  
  # only select target and  features  because the learner uses all variables in data set as features
  data_train = data_train %>% select(kost_per_diem_all, all_of(features)) 
  
  # specify target
  task = as_task_regr(data_train, target = "kost_per_diem_all")
  
  # train learner on data_train
  learner$train(task, row_ids = 1:task$nrow)

  # predict target and calculate apparent error on train data
  apparent_error = learner$predict(task)$score(msr(eval_criterion))
  
  # if there is a validation set specified, evaluate tree on this data set
  if(!is.null(data_validation)){ 
    # predict target and calculate error on validation set on validation data
    validation_error = learner$predict_newdata(data_validation)$score(msr(eval_criterion))
    result = list("apparent_error" = apparent_error, "validation_error"= validation_error)
  } else{
    result = list("apparent_error" = apparent_error)
  }
  
  # if plot = TRUE, return plot instead of only numbers
  if(plot == TRUE){  
    # use rpart.plot() or autoplot() depending on whether log-transformation is used or not
    # (results in different plot appearance but this is not so important)
    
    if(logtarget == TRUE){ 
      rpart.plot(learner$model$regr.rpart$model,
                 main = str_flatten(paste(names(result), round(unlist(result),4), sep = " = "), ",\n"))
    } else{
      # note: autoplot yields an error if the tree is only a root -> only plot tree if tree is not a root
      if(length(unique(learner$model$where))== 1){
        p = ggplot() + 
          annotate("text", x = 4, y = 25, size=8, label = "No tree, only root") + 
          theme_void()
      } else{
        p = autoplot(learner, dropped_aes = c("fill"))
      }
      p = p + labs(title = str_flatten(paste(names(result), round(unlist(result),4), sep = " = "), ",\n"))
      return(p)
    }
    
  } else{
    return(result)
  }
  }
  
# FUNCTION cart_resampling_fct 
# = function generates tree using the CART algorithm (from package rpart) with
# hyperparameter tuning by resampling. Returns apparent error + resampling error on 
# training data, and (optional) error on validation data.
# INPUT
# - data_train: train data
# - data_validation: validation data
# - features: which features should be considered for the tree
# - eval_criterion: which evaluation criterion (e.g. rsquared)
# - cp_lower: cp lower limit
# - cp_upper: cp upper limit 
# - minbucket: minbucket lower limit
# - minbucket: minbucket upper limit  
# - maxdepth: maximum depth of any node of the final tree
# - resolution: resolution of the grid when doing grid search
# - n_evals: stop grid_search after n_evals evaluations
# - folds: number of folds in cross-validation
# - plot: should result be a list or a plot
# OUTPUT: 
# - list with errors (plot = FALSE) or plot incl. errors (plot = TRUE)
cart_resampling_fct = function(data_train, data_validation = NULL, features,
                               eval_criterion, 
                               cp_lower = 0, 
                               cp_upper = 1, 
                               minbucket_lower = 1, 
                               minbucket_upper = 100,
                               maxdepth = 4,
                               resolution = 50,
                               n_evals = 100, 
                               folds = 5,
                               logtarget = FALSE,
                               plot = FALSE){
  
  
  # specify algorithm
  learner = lrn("regr.rpart", maxdepth = maxdepth, keep_model = TRUE) # = CART, should not be varied!
  
  # set log-transformation of target if necessary
  if(logtarget == TRUE){
    learner = create_pipeops(learner)
  }
  
  # only select target, features, and id because per default the learner uses all variables in data set as features 
  data_train = data_train %>% select(kost_per_diem_all, id, all_of(features)) 
  
  # specify target and features
  # (here we also specify id as grouping variable bc observations from same id should not be split when resampling)
  task = as_task_regr(data_train, target = "kost_per_diem_all")
  # = group by id 
  task$col_roles$group = "id"
  # = remove id from features
  task$col_roles$feature = setdiff(task$col_roles$feature, "id")
  
  # set parameter space (the name of the parameters unfortunately depends on whether we apply log-transformation or not)
  if(logtarget == TRUE){
   search_space_rpart = ps(
      regr.rpart.cp = p_dbl(lower = cp_lower, upper = cp_upper),
      regr.rpart.minbucket = p_int(lower = minbucket_lower, 
                                      upper = minbucket_upper)
  ) 
  } else{
    search_space_rpart = ps(
      cp = p_dbl(lower = cp_lower, upper = cp_upper),
      minbucket = p_int(lower = minbucket_lower, upper = minbucket_upper)
    )
  }

  
  # tune parameters using resampling (and specify tuner and terminator)
  tuner = tnr("grid_search", resolution = resolution) # grid search, resolution of the grid 
  terminator = trm("evals", n_evals = n_evals) # stop searching after n_evals evaluations
  
  instance = TuningInstanceSingleCrit$new(
    task = task,
    learner = learner,
    resampling =  rsmp("cv", folds = folds), # 5 fold cross-validation
    measure = msr(eval_criterion),
    search_space = search_space_rpart,
    terminator = terminator
  )
  tuner$optimize(instance)
  
  
  # get best parameters and grow tree on train_data
  learner_tuned = learner
  learner_tuned$param_set$values = instance$result_learner_param_vals
  learner_tuned$train(task)
  
  # calculate resampling error and apparent error on train data set
  apparent_error = learner_tuned$predict(task)$score(msr(eval_criterion))
  resampling_error = unlist(instance$result %>% select(all_of(eval_criterion)))
  
  
  # if there is a validation set specified, also evaluate tree on this data set
  if(!is.null(data_validation)){ 
    # predict target and calculate error on validation set on validation data
    validation_error = learner_tuned$predict_newdata(data_validation)$score(msr(eval_criterion))
    result = list("apparent_error" = apparent_error,
                  "resampling_error" = resampling_error,
                  "validation_error"= validation_error)
  } else{
    result = list("apparent_error" = apparent_error,
                  "resampling_error" = resampling_error)
  }
  
  
  # if plot = TRUE, return plot instead of only numbers
  if(plot == TRUE){  
    # use rpart.plot() or autoplot() depending on whether log-transformation is used or not
    # (results in different plot appearance but this is not so important)
    
    if(logtarget == TRUE){ 
      rpart.plot(learner_tuned$model$regr.rpart$model,
                 main = str_flatten(paste(names(result), round(unlist(result),4), sep = " = "), ",\n"))
    } else{
      # note: autoplot yields an error if the tree is only a root -> only plot tree if tree is not a root
      if(length(unique(learner_tuned$model$where))== 1){
        p = ggplot() + 
          annotate("text", x = 4, y = 25, size=8, label = "No tree, only root") + 
          theme_void()
      } else{
        p = autoplot(learner_tuned) 
      }
      p = p + labs(title = str_flatten(paste(names(result), round(unlist(result),4), sep = " = "), ",\n"))
      return(p)
    }
    
  } else{
    return(result)
  }

}



# FUNCTION cart_resampling_fct 
# = function generates tree using CART algorithm (from package rpart) with
# nested cross-validation. Returns the nested cross-validation error. 
# Note that this function does not return a plot because this would require 
# a final set of hyperparameters, which is not an output of nested cv.
# INPUT
# - data_train: train data
# - features: which features should be considered for the tree
# - eval_criterion: which evaluation criterion (e.g. rsquared) 
# - cp_lower: cp lower limit
# - cp_upper: cp upper limit 
# - minbucket: minbucket lower limit
# - minbucket: minbucket upper limit  
# - maxdepth: maximum depth of any node of the final tree
# - resolution: resolution of the grid when doing grid search
# - n_evals: stop grid_search after n_evals evaluations
# - inner_folds: number of inner folds in nested cross-validation
# - outer_folds: number of outer folds in nested cross-validation
# - outer_repeats: how often should the outer loop of nested cv be repeated
# OUTPUT: 
# - nested resampling error based on training data set
cart_nested_resampling_fct = function(data_train, features,
                                      eval_criterion, 
                                      cp_lower = 0,
                                      cp_upper = 1, 
                                      minbucket_lower = 1, 
                                      minbucket_upper = 100, 
                                      maxdepth = 4,
                                      resolution = 50,
                                      n_evals = 100,
                                      inner_folds = 5,
                                      outer_folds = 5,
                                      outer_repeats = 3,
                                      logtarget = FALSE){
  
  
  # specify algorithm
  learner = lrn("regr.rpart", maxdepth = maxdepth, keep_model = TRUE) # = CART, should not be varied!
  
  # set log-transformation of target if necessary
  if(logtarget == TRUE){
    learner = create_pipeops(learner)
  }
  
  # only select target, features, and id because per default the learner uses all variables in data set as features 
  data_train = data_train %>% select(kost_per_diem_all, id, all_of(features)) 
  
  # specify target and features
  # (here we also specify id as grouping variable bc observations from same id should not be split when resampling)
  task = as_task_regr(data_train, target = "kost_per_diem_all")
  # = group by id 
  task$col_roles$group = "id"
  # = remove id from features
  task$col_roles$feature = setdiff(task$col_roles$feature, "id")
  
  # set parameter space (the name of the parameters unfortunately depends on whether we apply log-transformation or not)
  if(logtarget == TRUE){
    search_space_rpart = ps(
      regr.rpart.cp = p_dbl(lower = cp_lower, upper = cp_upper),
      regr.rpart.minbucket = p_int(lower = minbucket_lower, 
                                   upper = minbucket_upper)
    ) 
  } else{
    search_space_rpart = ps(
      cp = p_dbl(lower = cp_lower, upper = cp_upper),
      minbucket = p_int(lower = minbucket_lower, upper = minbucket_upper)
    )
  }
  
  
  # specify nested resampling scheme (tuner, terminator, inner and outer resampling etc.)
  tuner = tnr("grid_search", resolution = resolution) # grid search, resolution of the grid = 50
  terminator = trm("evals", n_evals = n_evals) # stop searching after n_evals evaluations
  inner_resampling = rsmp("cv", folds = inner_folds) # number of inner folds
  outer_resampling = rsmps("repeated_cv", repeats = outer_repeats, folds = outer_folds)
  
  learner_tune = AutoTuner$new(learner, 
                               inner_resampling, 
                               msr(eval_criterion), 
                               terminator, 
                               tuner,
                               search_space_rpart)
  
  design = benchmark_grid(
    tasks = task,
    learners = learner_tune,
    resamplings = outer_resampling
  )
  
  # run nested resampling
  bmr = benchmark(design, store_models = TRUE)
  tab = bmr$aggregate(msr(eval_criterion))
  
  # tab contains some useful information about the nested CV result, but will only extract nested cv error
  result = tab[,..eval_criterion]
  return(result)
}














# regr.rpartMod ----
# (standard rpart but can take formula as argument)
LearnerRegrRpart = R6Class("LearnerRegrRpart",
                           inherit = LearnerRegr,
                           
                           public = list(
                             #' @description
                             #' Creates a new instance of this [R6][R6::R6Class] class.
                             initialize = function() {
                               ps = ps(
                                 #formula = p_uty(tags = "train"),
                                 include.partitioning.vars_expr = p_uty(tags = "train"), # csauer
                                 minsplit = p_int(lower = 1L, default = 20L, tags = "train"),
                                 minbucket = p_int(lower = 1L, tags = "train"),
                                 cp = p_dbl(lower = 0, upper = 1, default = 0.01, tags = "train"),
                                 maxcompete = p_int(lower = 0L, default = 4L, tags = "train"),
                                 maxsurrogate = p_int(lower = 0L, default = 5L, tags = "train"),
                                 maxdepth = p_int(lower = 1L, upper = 30L, default = 30L, tags = "train"),
                                 usesurrogate = p_int(lower = 0L, upper = 2L, default = 2L, tags = "train"),
                                 surrogatestyle = p_int(lower = 0L, upper = 1L, default = 0L, tags = "train"),
                                 xval = p_int(lower = 0L, default = 0L, tags = "train")#,
                                # keep_model = p_lgl(default = FALSE, tags = "train") (rpart needs "model" and not "keep_)
                               )
                               ps$values = list(xval = 0L)
                               
                               super$initialize(
                                 id = "regr.rpartMod",
                                 packages = "rpart",
                                 feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
                                 predict_types = c("response"),
                                 param_set = ps,
                                 properties = c("weights", "selected_features"),
                                 man = "mlr3extralearners::mlr_learners_regr.rpartMod"
                               )
                             }
                             
                             
                           ),
                           
                           private = list(
                             
                             .train = function(task) {
                               pars = self$param_set$get_values(tags = "train")
                               if ("weights" %in% task$properties) {
                                 pars$weights = task$weights$weight
                               }
                             
                               ## specify formula 
                               #formula = task$formula()
                               partitioning.vars = task$feature_names[grepl(pars$include.partitioning.vars_expr, task$feature_names)]
                               formula = as.formula(paste0(task$target_names," ~ ", 
                                                           paste0(partitioning.vars, collapse = " + ")))
                               pars = insert_named(pars, list(formula = formula))
                               # remove formula.random and include.partitioning.vars_expr from pars
                               pars = remove_named(pars, "include.partitioning.vars_expr")
                               
                               
                               
                               data = task$data()
                               invoke(
                                 rpart::rpart,
                                 #    formula = formula,
                                 data = data,
                                 .args = pars
                               )
                             },
                             
                             .predict = function(task) {
                               pars = self$param_set$get_values(tags = "predict")
                               # get newdata and ensure same ordering in train and predict
                               newdata = ordered_features(task, self)
                               
                               response = invoke(
                                 predict,
                                 self$model,
                                 newdata = newdata,
                                 .args = pars
                               )
                               
                               return(list(response = response))
                             }
                           )
)
mlr_learners$add("regr.rpartMod", LearnerRegrRpart)

# regr.ctreeMod ----
# (standard ctree but can take formula as argument)
LearnerRegrCTree = R6Class("LearnerRegrCTree",
                           inherit = LearnerRegr,
                           public = list(
                             
                             #' @description
                             #' Creates a new instance of this [R6][R6::R6Class] class.
                             initialize = function() {
                               ps = ps(
                                 #formula = p_uty(tags = "train"),
                                 include.partitioning.vars_expr = p_uty(tags = "train"), # csauer
                                 teststat = p_fct(levels = c("quadratic", "maximum"),
                                                  default = "quadratic", tags = "train"),
                                 splitstat = p_fct(levels = c("quadratic", "maximum"),
                                                   default = "quadratic", tags = "train"),
                                 splittest = p_lgl(default = FALSE, tags = "train"),
                                 testtype = p_fct(levels = c("Bonferroni", "MonteCarlo",
                                                             "Univariate", "Teststatistic"), default = "Bonferroni",
                                                  tags = "train"),
                                 nmax = p_uty(tags = "train"),
                                 alpha = p_dbl(lower = 0, upper = 1, default = 0.05,
                                               tags = "train"),
                                 mincriterion = p_dbl(lower = 0, upper = 1, default = 0.95,
                                                      tags = "train"),
                                 logmincriterion = p_dbl(tags = "train"),
                                 minsplit = p_int(lower = 1L, default = 20L, tags = "train"),
                                 minbucket = p_int(lower = 1L, default = 7L, tags = "train"),
                                 minprob = p_dbl(lower = 0, default = 0.01, tags = "train"),
                                 stump = p_lgl(default = FALSE, tags = "train"),
                                 lookahead = p_lgl(default = FALSE, tags = "train"),
                                 MIA = p_lgl(default = FALSE, tags = "train"),
                                 maxvar = p_int(lower = 1L, tags = "train"),
                                 nresample = p_int(lower = 1L, default = 9999L,
                                                   tags = "train"),
                                 tol = p_dbl(lower = 0, tags = "train"),
                                 maxsurrogate = p_int(lower = 0L, default = 0L,
                                                      tags = "train"),
                                 numsurrogate = p_lgl(default = FALSE, tags = "train"),
                                 mtry = p_int(lower = 0L, special_vals = list(Inf),
                                              default = Inf, tags = "train"),
                                 maxdepth = p_int(lower = 0L, special_vals = list(Inf),
                                                  default = Inf, tags = "train"),
                                 multiway = p_lgl(default = FALSE, tags = "train"),
                                 splittry = p_int(lower = 0L, default = 2L, tags = "train"),
                                 intersplit = p_lgl(default = FALSE, tags = "train"),
                                 majority = p_lgl(default = FALSE, tags = "train"),
                                 caseweights = p_lgl(default = FALSE, tags = "train"),
                                 applyfun = p_uty(tags = "train"),
                                 cores = p_int(special_vals = list(NULL), default = NULL,
                                               tags = c("train", "threads")),
                                 saveinfo = p_lgl(default = TRUE, tags = "train"),
                                 update = p_lgl(default = FALSE, tags = "train"),
                                 splitflavour = p_fct(default = "ctree", # goes into control
                                                      levels = c("ctree", "exhaustive"), tags = "train"),
                                 offset = p_uty(tags = "train"),
                                 cluster = p_uty(tags = "train"),
                                 scores = p_uty(tags = "train"),
                                 doFit = p_lgl(default = TRUE, tags = "train"),
                                 maxpts = p_int(default = 25000L, tags = "train"),
                                 abseps = p_dbl(default = 0.001, lower = 0, tags = "train"),
                                 releps = p_dbl(default = 0, lower = 0, tags = "train")
                               )
                               ps$add_dep("nresample", "testtype", CondEqual$new("MonteCarlo"))
                               
                               super$initialize(
                                 id = "regr.ctreeMod",
                                 packages = c("mlr3extralearners", "partykit", "sandwich", "coin"),
                                 feature_types = c("integer", "numeric", "factor", "ordered"),
                                 predict_types = "response",
                                 param_set = ps,
                                 properties = "weights",
                                 man = "mlr3extralearners::mlr_learners_regr.ctreeMod"
                               )
                             }
                           ),
                           
                           private = list(
                             .train = function(task) {
                               pars = self$param_set$get_values(tags = "train")
                               pars_pargs = pars[names(pars) %in% formalArgs(mvtnorm::GenzBretz)]
                               pars = pars[names(pars) %nin% formalArgs(mvtnorm::GenzBretz)]
                               
                               if ("weights" %in% task$properties) {
                                 pars$weights = task$weights$weight
                               }
                               pars$pargs = invoke(mvtnorm::GenzBretz, pars_pargs)
                               
                               ## specify formula 
                               partitioning.vars = task$feature_names[grepl(pars$include.partitioning.vars_expr, task$feature_names)]
                               formula = as.formula(paste0(task$target_names," ~ ", 
                                                           paste0(partitioning.vars, collapse = " + ")))
                               pars = insert_named(pars, list(formula = formula))
                               # remove formula.random and include.partitioning.vars_expr from pars
                               pars = remove_named(pars, "include.partitioning.vars_expr")
                               
                               invoke(partykit::ctree, #formula = task$formula(),
                                      data = task$data(), .args = pars)
                             },
                             
                             .predict = function(task) {
                               newdata = task$data(cols = task$feature_names)
                               pars = self$param_set$get_values(tags = "predict")
                               
                               p = invoke(predict, self$model, newdata = newdata, .args = pars)
                               list(response = p)
                             }
                           )
)

mlr_learners$add("regr.ctreeMod", LearnerRegrCTree)

# regr.glmertree ----
# Reference:
# 'Fokkema M, Smits N, Zeileis A, Hothorn T, Kelderman H (2018). “Detecting Treatment-Subgroup Interactions
# 'in clustered Data With Generalized Linear Mixed-Effects Model Trees.” Behavior Research Methods,
# '50, 2016-2034. http://link.springer.com/article/10.3758/s13428-017-0971-x.
LearnerRegrGlmertree = R6Class("LearnerRegrGlmertree",
                               inherit = LearnerRegr,
                               
                               public = list(
                                 #' @description
                                 #' Creates a new instance of this [R6][R6::R6Class] class.
                                 initialize = function() {
                                   
                                   ps = ps(
                                     #formula = p_uty(tags = "train"),
                                     formula.random = p_uty(tags = "train"), # csauer
                                     include.partitioning.vars_expr = p_uty(tags = "train"), # csauer
                                     maxit = p_int(lower = 1L, default = 100L, tags = "train"),
                                     maxdepth = p_int(lower = 1L, tags = "train"),
                                     minsize = p_int(lower = 1L, tags = "train"),
                                     minsplit = p_int(lower = 1L, tags = "train"),
                                     minbucket = p_int(lower = 1L, tags = "train"),
                                     alpha = p_dbl(lower = 0, upper = 1, default = 0.05, tags = "train"),
                                     re.form = p_uty(tags = "predict"),
                                     allow.new.levels = p_lgl(default = FALSE, tags = "predict")
                                   )
                                   
                                   super$initialize(
                                     id = "regr.glmertree",
                                     packages = "glmertree",
                                     feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
                                     predict_types = c("response"),
                                     param_set = ps,
                                     properties = c("weights", "selected_features"),
                                     man = "mlr3extralearners::mlr_learners_regr.glmertree"
                                   )
                                 }
                                 
                                 
                               ),
                               
                               private = list(
                                 
                                 .train = function(task) {
                                   ## get parameters for training
                                   pars = self$param_set$get_values(tags = "train")
                                   
                                   ## set column names to ensure consistency in fit and predict
                                   self$state$feature_names = task$feature_names
                                   
                                   if ("weights" %in% task$properties) {
                                     pars = insert_named(pars, list(weights = task$weights$weight))
                                   }
                                   
                                   ## specify data 
                                   data = task$data()
                                   
                                   ## specify formula 
                                   # (needs formula specifying the response variable and a three-part 
                                   # right-hand-side describing the regressors, random effects, 
                                   # and partitioning variables, respectively)
                                   
                                   # formula = task$formula()
                                   partitioning.vars = task$feature_names[grepl(pars$include.partitioning.vars_expr, task$feature_names)]
                                   formula = as.formula(paste0(task$target_names, " ~ 1 | ",
                                                               pars$formula.random, " | ", 
                                                               paste0(partitioning.vars, collapse = " + ")))
                                   
                                   pars = insert_named(pars, list(formula = formula))
                                   # remove formula.random and include.partitioning.vars_expr from pars
                                   pars = remove_named(pars, c("formula.random", "include.partitioning.vars_expr"))
                                   
                                   
                                   ## use the mlr3misc::invoke function (it's similar to do.call()) 
                                   invoke(glmertree::lmertree,
                                          #       formula = formula,
                                          data = data,
                                          .args = pars)
                                 },
                                 
                                 .predict = function(task) {
                                   ## get parameters with tag "predict"
                                   pars = self$param_set$get_values(tags = "predict")
                                   
                                   ## get newdata and ensure same ordering in train and predict
                                   newdata = task$data(cols = self$state$feature_names)
                                   
                                   response = invoke(predict, self$model, newdata = newdata, type = "response", .args = pars)
                                   
                                   list(response = response)
                                 }
                               )
)
mlr_learners$add("regr.glmertree", LearnerRegrGlmertree)

# regr.reemtree ----
# Reference:
# 'Sela, R.J. and Simonoff, J.S. (2012) RE-EM trees: a data mining approach for longitudinal
#  and clustered data <doi:10.1007/s10994-011-5258-3>.

LearnerRegrReemtree = R6Class("LearnerRegrReemtree",
                              inherit = LearnerRegr,
                              
                              public = list(
                                #' @description
                                #' Creates a new instance of this [R6][R6::R6Class] class.
                                initialize = function() {
                                  ps = ps(
                                   #formula = p_uty(tags = "train"),
                                    include.partitioning.vars_expr = p_uty(tags = "train"), # csauer
                                    random = p_uty(tags = "train"),
                                    MaxIterations = p_int(lower = 1L, default = 1000L, tags = "train"),
                                    cv = p_lgl(default = TRUE, tags = "train"),
                                    minsplit = p_int(lower = 1L, default = 20L, tags = "train"),
                                    minbucket = p_int(lower = 1L, tags = "train"),
                                    cp = p_dbl(lower = 0, upper = 1, default = 0.01, tags = "train"),
                                    maxdepth = p_int(lower = 1L, upper = 30L, default = 30L, tags = "train"),
                                    grouping = p_uty(default = "", tags = "predict"),
                                    EstimateRandomEffects = p_lgl(default = TRUE, tags = "predict")
                                    
                                  )
                                  
                                  super$initialize(
                                    id = "regr.reemtree",
                                    packages = "REEMtree",
                                    feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
                                    predict_types = c("response"),
                                    param_set = ps,
                                    properties = c("selected_features"),
                                    man = "mlr3extralearners::mlr_learners_regr.reemtree"
                                  )
                                }
                                
                                
                              ),
                              
                              private = list(
                                
                                .train = function(task) {
                                  ## get parameters for training
                                  pars = self$param_set$get_values(tags = "train")
                                  
                                  tree_control_names = c("minsplit", "minbucket", "cp", "maxdepth")
                                  
                                  tree_control_pars = pars[names(pars) %in% tree_control_names]
                                  pars = pars[names(pars) %nin% tree_control_names]
                                  
                                  ## set column names to ensure consistency in fit and predict
                                  self$state$feature_names = task$feature_names
                                  
                                  ## specify data
                                  data = task$data()
                                  
                                  ## specify formula 
                                  # (formula in the style of "target ~ partitionig.var1 + partitionig.var2 + ..";
                                  #  random formula is in a separate argument)
                                  #formula = task$formula()
                                  partitioning.vars = task$feature_names[grepl(pars$include.partitioning.vars_expr, task$feature_names)]
                                  formula = as.formula(paste0(task$target_names," ~ ", 
                                                               paste0(partitioning.vars, collapse = " + ")))
                                  pars = insert_named(pars, list(formula = formula))
                                  # remove formula.random and include.partitioning.vars_expr from pars
                                  pars = remove_named(pars, "include.partitioning.vars_expr")
                                 
                                  
                                  ## use the mlr3misc::invoke function (it's similar to do.call())
                                  invoke(REEMtree::REEMtree,
                                         #       formula = formula,
                                         data = data,
                                         tree.control = tree_control_pars,
                                         .args = pars)
                                },
                                
                                .predict = function(task) {
                                  ## get parameters with tag "predict"
                                  pars = self$param_set$get_values(tags = "predict")
                                  
                                  ## get newdata and ensure same ordering in train and predict
                                  newdata = task$data(cols = self$state$feature_names)
                                  
                                  newdata = cbind(data.frame(A = NA), newdata)
                                  colnames(newdata)[1] = task$target_names[1]
                                  
                                  if (pars$grouping != "") {
                                    grouping = as.vector(newdata[,pars$grouping])
                                  } else {
                                    grouping = NULL
                                  }
                                  pars$grouping = NULL
                                  
                                  response = invoke(predict, self$model, newdata = newdata, id = grouping, .args = pars)
                                  
                                  list(response = response)
                                }
                              )
)

mlr_learners$add("regr.reemtree", LearnerRegrReemtree)

# regr.reemctree ----
# Reference:
# 'Fu W. and Simonoff, J.S. (2015) Unbiased regression trees for longitudinal and clustered data 
#  <doi:10.1016/j.csda.2015.02.004>.

LearnerRegrReemctree = R6Class("LearnerRegrReemctree",
                               inherit = LearnerRegr,
                               
                               public = list(
                                 #' @description
                                 #' Creates a new instance of this [R6][R6::R6Class] class.
                                 initialize = function() {
                                   param_set = ps(
                                     #formula = p_uty(tags = "train"),
                                     include.partitioning.vars_expr = p_uty(tags = "train"), # csauer
                                     random = p_uty(tags = "train"),
                                     MaxIterations = p_int(lower = 1L, default = 1000L, tags = "train"),
                                     minsplit = p_int(lower = 1L, default = 20L, tags = "train"),
                                     minbucket = p_int(lower = 1L, default = 7L, tags = "train"),
                                     maxdepth = p_int(lower = 1L, upper = 30L, default = 30L, tags = "train"),
                                     mincriterion = p_dbl(lower = 0, upper = 1, default = 0.95, tags = "train")
                                   )
                                   
                                   super$initialize(
                                     id = "regr.reemctree",
                                     packages = "party",
                                     feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
                                     predict_types = c("response"),
                                     param_set = param_set,
                                     properties = c("selected_features"),
                                     man = "mlr3extralearners::mlr_learners_regr.reemctree",
                                     label = "Unbiased RE-EM tree"
                                   )
                                 }
                               ),
                               private = list(
                                 .train = function(task) {
                                   ## get parameters for training
                                   pars = self$param_set$get_values(tags = "train")
                                   
                                   ctree_control_names = c("minsplit", "minbucket", "maxdepth", "mincriterion")
                                   
                                   ctree_control_pars = pars[names(pars) %in% ctree_control_names]
                                   pars = pars[names(pars) %nin% ctree_control_names]
                                   
                                   ## specify data
                                   data = task$data()
                                   
                                   ## specify formula 
                                   # (formula in the style of "target ~ partitionig.var1 + partitionig.var2 + ..";
                                   #  random formula is in a separate argument)
                                   #formula = task$formula()
                                   partitioning.vars = task$feature_names[grepl(pars$include.partitioning.vars_expr, task$feature_names)]
                                   formula = as.formula(paste0(task$target_names," ~ ", 
                                                               paste0(partitioning.vars, collapse = " + ")))
                                   pars = insert_named(pars, list(formula = formula))
                                   # remove formula.random and include.partitioning.vars_expr from pars
                                   pars = remove_named(pars, "include.partitioning.vars_expr")
                                   
                                   ## use the mlr3misc::invoke function (it's similar to do.call())
                                   invoke(
                                     REEMctree,
                                     data = data,
                                     ctree.control = do.call(party::ctree_control, ctree_control_pars),
                                     .args = pars)
                                 },
                                 .predict = function(task) {
                                   ## get parameters with tag "predict"
                                   pars = self$param_set$get_values(tags = "predict")
                                   
                                   ## get newdata and ensure same ordering in train and predict
                                   newdata = task$data(cols = self$state$feature_names)
                                   
                                   #response = invoke(predict, self$model, newdata = newdata, .args = pars)
                                   #response = as.vector(predict(self$model$Tree, newdata = newdata))
                                   
                                   ## get the predicted costs for each node of the tree, based on the training data
                                   predicted_costs_nodes = unique(cbind(where(self$model$Tree), predict(self$model$EffectModel, level = 0)))
                                   predicted_costs_nodes = as.data.frame(predicted_costs_nodes)
                                   colnames(predicted_costs_nodes) = c("nodes", "costs")
                                   
                                   ## predict nodes of instances in the test data
                                   nodes_testdata = data.frame(nodes = where(self$model$Tree, newdata = newdata))
                                   
                                   ## predict costs of instances in the test data
                                   predicted_costs_testdata = left_join(nodes_testdata, predicted_costs_nodes, by = "nodes")
                                   
                                   response = as.vector(predicted_costs_testdata$costs)
                                   names(response) = 1:nrow(newdata)
                                   
                                   list(response = response)
                                 }
                               )
)

mlr_learners$add("regr.reemctree", LearnerRegrReemctree)


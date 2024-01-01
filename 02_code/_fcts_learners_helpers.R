# regr.reemctree implementation 
# Source: https://pages.stern.nyu.edu/~jsimonof/unbiasedREEM/REEMctree.r

#-------------------------start of main function----------------------------------------------------
REEMctree <- function (formula, data, random, subset = NULL, initialRandomEffects = rep(0, 
                                                                                        TotalObs), ErrorTolerance = 0.001, MaxIterations = 1000, 
                       verbose = FALSE, lme.control = lmeControl(returnObject = TRUE), ctree.control = party::ctree_control(mincriterion = 0.95),
                       method = "REML", correlation = NULL) 
{
  TotalObs <- dim(data)[1]
  if (identical(subset, NULL)) {
    subs <- rep(TRUE, dim(data)[1])
  }
  else {
    subs <- subset
  }
  Predictors <- paste(attr(terms(formula), "term.labels"), 
                      collapse = "+")
  TargetName <- formula[[2]]
  if (length(TargetName) > 1) 
    TargetName <- TargetName[3]
  if (verbose) 
    print(paste("Target variable: ", TargetName))
  tm = toString(TargetName) 
  Target <- data[, ..tm] # modified by Ullmann and Niessl to ensure compatibility with data.table
   #Target <- data[, tm] 
  
  ContinueCondition <- TRUE
  iterations <- 0
  AdjustedTarget <- Target - initialRandomEffects
  oldlik <- -Inf
  newdata <- data
  newdata[, "SubsetVector"] <- subs
  while (ContinueCondition) {
    newdata[, "AdjustedTarget"] <- AdjustedTarget
    iterations <- iterations + 1
    
    tree <- party::ctree(formula(paste(c("AdjustedTarget", Predictors), collapse = "~")), data = newdata, subset = subs, controls = ctree.control)  
    
    if (verbose) 
      print(tree)
    newdata[, "nodeInd"] <- 0
    newdata[subs, "nodeInd"] <- where(tree)
    if (min(where(tree)) == max(where(tree))) { #it doesn't split on root
      lmefit <- lme(formula(paste(c(toString(TargetName), 
                                    1), collapse = "~")), data = newdata, random = random, 
                    subset = SubsetVector, method = method, control = lme.control, 
                    correlation = correlation)
    }
    else {
      lmefit <- lme(formula(paste(c(toString(TargetName), 
                                    "as.factor(nodeInd)"), collapse = "~")), data = newdata, 
                    random = random, subset = SubsetVector, method = method, 
                    control = lme.control, correlation = correlation)
    }
    if (verbose) {
      print(lmefit)
      print(paste("Estimated Error Variance = ", lmefit$sigma))
      print("Estimated Random Effects Variance = ")
      print(as.matrix(lmefit$modelStruct$reStruct[[1]]) * 
              lmefit$sigma^2)
    }
    newlik <- logLik(lmefit)
    if (verbose) 
      print(paste("Log likelihood: ", newlik))
    ContinueCondition <- (newlik - oldlik > ErrorTolerance & 
                            iterations < MaxIterations)
    oldlik <- newlik
    AllEffects <- lmefit$residuals[, 1] - lmefit$residuals[, 
                                                           dim(lmefit$residuals)[2]]
    AdjustedTarget[subs] <- Target[subs] - AllEffects
  }
  residuals <- rep(NA, length = length(Target))
  residuals[subs] <- Target[subs] - predict(lmefit)
  attr(residuals, "label") <- NULL 
  
  result <- list(Tree = tree, EffectModel = lmefit, RandomEffects = ranef(lmefit), 
                 BetweenMatrix = as.matrix(lmefit$modelStruct$reStruct[[1]]) * 
                   lmefit$sigma^2, ErrorVariance = lmefit$sigma^2, data = data, 
                 logLik = newlik, IterationsUsed = iterations, Formula = formula, 
                 Random = random, Subset = subs, ErrorTolerance = ErrorTolerance, 
                 correlation = correlation, residuals = residuals, method = method, 
                 lme.control = lme.control, ctree.control = ctree.control)
  class(result) <- "REEMctree"
  return(result)
}
#-------------------------end of main function----------------------------------------------------
# Example to use unbiased RE-EM tree
# library(REEMtree) # Only needed to access data set
# library(party)
# data(simpleREEMdata)
# REEM.ctree.result<-REEMctree(Y~D+t+X, data=simpleREEMdata, random=~1|ID)
# plot(REEM.ctree.result$Tree)

### Some functions in the original RE-EM tree may be lost or need different treatment such as using the predict() function
### to predict the fixed effect of testing data from a fitted tree. In particular, if a correlation structure other than
### independence is assumed for errors within individuals, the predicted response values at the terminal nodes of the tree
### will not be correct, and need to be obtained from the associated mixed model fit; that is,
###          unique(cbind(where(REEM.ctree.result$Tree), predict(REEM.ctree.result$Tree)))
### will NOT give the correct responses at the terminal nodes, but
###          unique(cbind(where(REEM.ctree.result$Tree), predict(REEM.ctree.result$EffectModel, level = 0)))
### will. Note that this means that the estimated responses at the terminal nodes when the tree is plotted will be (slightly)
### incorrect as well.


# More helper functions ----
toproper = function(str, split = " ", fixed = TRUE) {
  str = strsplit(str, split, fixed)
  str = lapply(str,
               function(x) {
                 paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, 1000)), collapse = split)
               }
  )
  return(unlist(str))
}

# a less robust but faster version of devtools::as.package
pkg_root = function(path = ".") {
  path = normalizePath(path)
  
  if (!grepl("mlr3extralearners", path)) {
    stopf("Path (%s) is not in mlr3extralearners directory.", path)
  }
  
  while (TRUE) {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    } else {
      path = dirname(path)
    }
  }
}


pprob_to_matrix = function(pp, task) {
  y = matrix(c(pp, 1 - pp), ncol = 2L, nrow = length(pp))
  colnames(y) = task$class_names
  y
}

#' @title Convert a Ratio Hyperparameter
#'
#' @description
#' Given the named list `pv` (values of a [ParamSet]), converts a possibly provided hyperparameter
#' called `ratio` to an integer hyperparameter `target`.
#' If both are found in `pv`, an exception is thrown.
#'
#' @param pv (named `list()`).
#' @param target (`character(1)`)\cr
#'   Name of the integer hyperparameter.
#' @param ratio (`character(1)`)\cr
#'   Name of the ratio hyperparameter.
#' @param n (`integer(1)`)\cr
#'   Ratio of what?
#'
#' @return (named `list()`) with new hyperparameter settings.
#' @noRd
convert_ratio = function(pv, target, ratio, n) {
  switch(to_decimal(c(target, ratio) %in% names(pv)) + 1L,
         # !mtry && !mtry.ratio
         pv,
         
         # !mtry && mtry.ratio
         { # nolint
           pv[[target]] = max(ceiling(pv[[ratio]] * n), 1)
           remove_named(pv, ratio)
         },
         
         
         # mtry && !mtry.ratio
         pv,
         
         # mtry && mtry.ratio
         stopf("Hyperparameters '%s' and '%s' are mutually exclusive", target, ratio)
  )
}
# Helper functions for RWeka

# Formats an RWeka argument into the mlr3 naming style
format_rweka_once = function(x) {
  first_char = substr(x, 1, 1)
  if (first_char == "-") {
    x = substr(x, 2, nchar(x))
  }
  last_char = substr(x, nchar(x), nchar(x))
  if (last_char == "-") x = substr(x, 1, nchar(x) - 1)
  x = gsub("-", "_", x)
  return(x)
}

# Vectorized formatting
format_rweka = function(x) {
  x = x[(nchar(x) > 0)]
  x = map_chr(x, format_rweka_once)
  return(x)
}

# Get the RWeka control arguments for function f and translate them into mlr3 style
weka_control_args = function(f) {
  arg_desc = RWeka::WOW(f)
  arg_names = arg_desc$Name
  exclude = format_rweka(arg_names)
  exclude = unique(exclude)
  return(exclude)
}

ordered_features = function(task, learner) {
  task$data(cols = intersect(names(learner$state$task_prototype), task$feature_names))
}

as_numeric_matrix = function(x) { # for svm / #181
  x = as.matrix(x)
  if (is.logical(x)) {
    storage.mode(x) = "double"
  }
  x
}

rename = function(x, old, new) {
  if (length(x)) {
    ii = match(names(x), old, nomatch = 0L)
    names(x)[ii > 0L] = new[ii]
  }
  x
}


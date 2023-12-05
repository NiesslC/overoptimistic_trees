source("./02_code/_fcts_preprocessing.R")


PipeOpPreprocFeatureAge = R6::R6Class("PipeOpPreprocFeatureAge",
                                      inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
                                      public = list(
                                        initialize = function(id = "preproc.feature.age", param_vals = list()) {
                                          ps = ParamSet$new(params = list(
                                            ParamFct$new("option", levels = c("A", "B"))
                                          ))
                                          ps$values = list(option = "A")
                                          super$initialize(id = id, param_set = ps, 
                                                           param_vals = param_vals)
                                        }
                                      ),
                                      private = list(
                                        .transform_dt = function(dt, levels) {
                                          invoke(
                                            preprocess_feature_age_fct,
                                            data = dt,
                                            option=self$param_set$values$option)
                                        }
                                      )
)
mlr_pipeops$add("preproc.feature.age", PipeOpPreprocFeatureAge)


PipeOpPreprocFeatureIPOS = R6::R6Class("PipeOpPreprocFeatureIPOS",
                                      inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
                                      public = list(
                                        initialize = function(id = "preproc.feature.ipos", param_vals = list()) {
                                          ps = ParamSet$new(params = list(
                                            ParamFct$new("option", levels = c("A", "B", "C", "D", "E"))
                                          ))
                                          ps$values = list(option = "A")
                                          super$initialize(id = id, param_set = ps, 
                                                           param_vals = param_vals)
                                        }
                                      ),
                                      private = list(
                                        .transform_dt = function(dt, levels) {
                                          invoke(
                                            preprocess_feature_ipos_fct,
                                            data = dt,
                                            option=self$param_set$values$option)
                                        }
                                      )
)
mlr_pipeops$add("preproc.feature.ipos", PipeOpPreprocFeatureIPOS)


PipeOpPreprocTarget = R6::R6Class("PipeOpPreprocTarget",
                                       inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
                                       public = list(
                                         initialize = function(id = "preproc.target", param_vals = list()) {
                                           ps = ParamSet$new(params = list(
                                             ParamFct$new("option", levels = c("A", "B", "C"))
                                           ))
                                           ps$values = list(option = "A")
                                           super$initialize(id = id, param_set = ps, 
                                                            param_vals = param_vals)
                                         }
                                       ),
                                       private = list(
                                         .transform_dt = function(dt, levels) {
                                           invoke(
                                             preprocess_target_fct,
                                             data = dt,
                                             option=self$param_set$values$option)
                                         }
                                       )
)
mlr_pipeops$add("preproc.target", PipeOpPreprocTarget)

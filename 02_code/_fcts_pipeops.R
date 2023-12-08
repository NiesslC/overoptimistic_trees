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



PipeOpDropTargetOut = R6::R6Class("PipeOpDropTargetOut",
                           inherit = mlr3pipelines::PipeOpTaskPreproc,
                           public = list(
                             initialize = function(id = "preproc.drop.targetout", param_vals = list()) {
                               ps = ParamSet$new(params = list(
                                 ParamFct$new("option", levels = c("A", "B", "C", "D"))
                               ))
                               ps$values = list(option = "A")
                               super$initialize(id = id, param_set = ps, 
                                                param_vals = param_vals)
                             }
                           ),
                           
                           private = list(
                             .train_task = function(task) {
                               self$state = list(
                                 threshold = preprocess_drop_targetout_fct(target_values = task$data(cols = task$target_names),
                                                                    option=self$param_set$values$option)
                               )
                               exclude =  task$data(cols = task$target_names) > self$state$threshold
                               task$filter(task$row_ids[!exclude])
                             },
                             
                             .predict_task = function(task) {
                               exclude =  task$data(cols = task$target_names) > self$state$threshold
                               task$filter(task$row_ids[!exclude])
                               
                             }
                           )
)
mlr_pipeops$add("preproc.drop.targetout", PipeOpDropTargetOut)


PipeOpDropIposCa = R6::R6Class("PipeOpDropIposCa",
                           inherit = mlr3pipelines::PipeOpTaskPreproc,
                           public = list(
                             initialize = function(id = "preproc.drop.iposca", param_vals = list()) {
                               ps = ParamSet$new(params = list(
                                 ParamFct$new("option", levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
                               ))
                               ps$values = list(option = "A")
                               super$initialize(id = id, param_set = ps, 
                                                param_vals = param_vals)
                             }
                           ),
                           
                           private = list(
                             .train_task = function(task) {
                               self$state = list(
                                 threshold = preprocess_drop_iposca_fct(option=self$param_set$values$option)
                               )
                               exclude =  (task$data() %>% select(contains("ipos")) %>% 
                                             mutate(sum_ca = rowSums(. == "cannot assess")) %>% .$sum_ca) >= self$state$threshold
                               task$filter(task$row_ids[!exclude])
                             },
                             .predict_task = function(task) {
                               exclude =  (task$data() %>% select(contains("ipos")) %>% 
                                             mutate(sum_ca = rowSums(. == "cannot assess")) %>% .$sum_ca) >= self$state$threshold
                               task$filter(task$row_ids[!exclude])
                             }
                           )
)

mlr_pipeops$add("preproc.drop.iposca", PipeOpDropIposCa)

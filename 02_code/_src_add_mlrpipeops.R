

# preproc.target ----
PipeOpPreprocTarget = R6::R6Class("PipeOpPreprocTarget",
                                  inherit = mlr3pipelines::PipeOpTaskPreproc,
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
                                    
                                    .train_dt = function(dt, levels, target) {
                                      correction_factor = preprocess_target_getcorr_fct(data = dt, 
                                                                                         option = self$param_set$values$option)
                                      self$state = list(
                                        correction_factor = correction_factor
                                      )
                                      preprocess_target_fct(data = dt,
                                                            option=self$param_set$values$option, 
                                                            correction_factor = correction_factor)                                  
                                    },
                                    
                                    .predict_dt = function(dt, levels) {
                                      preprocess_target_fct(data = dt,
                                                            option=self$param_set$values$option, 
                                                            correction_factor = self$state$correction_factor)   
                                    }
                                  )
)
mlr_pipeops$add("preproc.target", PipeOpPreprocTarget)

# preproc.feature.age ----
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

# preproc.feature.akps ----
PipeOpPreprocFeatureAkps = R6::R6Class("PipeOpPreprocFeatureAkps",
                                      inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
                                      public = list(
                                        initialize = function(id = "preproc.feature.akps", param_vals = list()) {
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
                                            preprocess_feature_akps_fct,
                                            data = dt,
                                            option=self$param_set$values$option)
                                        }
                                      )
)
mlr_pipeops$add("preproc.feature.akps", PipeOpPreprocFeatureAkps)



# preproc.feature.ipos ----
PipeOpPreprocFeatureIpos = R6::R6Class("PipeOpPreprocFeatureIpos",
                                      inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
                                      public = list(
                                        initialize = function(id = "preproc.feature.ipos", param_vals = list()) {
                                          ps = ParamSet$new(params = list(
                                            ParamFct$new("option", levels = c("A", "B", "C", "D"))
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
mlr_pipeops$add("preproc.feature.ipos", PipeOpPreprocFeatureIpos)


# preproc.drop.targetout ----
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
                               
                               #exclude =  task$data(cols = task$target_names) > self$state$threshold
                               #task$filter(task$row_ids[!exclude])
                               
                               # do not remove any observations
                               task
                               
                             }
                           )
)
mlr_pipeops$add("preproc.drop.targetout", PipeOpDropTargetOut)

# preproc.drop.iposca ----
PipeOpDropIposCa = R6::R6Class("PipeOpDropIposCa",
                           inherit = mlr3pipelines::PipeOpTaskPreproc,
                           public = list(
                             initialize = function(id = "preproc.drop.iposca", param_vals = list()) {
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


# fixfactors.overwhelmingly ----
PipeOpFixFactorsOverwhelmingly   = R6Class("PipeOpFixFactorsOverwhelmingly",
                           inherit = PipeOpTaskPreprocSimple,
                           public = list(
                             initialize = function(id = "fixfactors.overwhelmingly", param_vals = list()) {
                               ps = ParamSet$new(params = list(
                                 ParamLgl$new("droplevels", tags = c("train", "predict"))
                               ))
                               ps$values = list(droplevels = TRUE)
                               super$initialize(id, param_set = ps, param_vals = param_vals, tags = "robustify", feature_types = c("factor", "ordered"))
                             }
                           ),
                           private = list(
                             .get_state = function(task) {
                               # get the ipos_shortness_breath and ipos_pain levels of the training task if feature is present
                               if(any(grepl("ipos_shortness_breath|ipos_pain", task$feature_names))){
                                 dt = task$data(cols = task$feature_names[grep("ipos_shortness_breath|ipos_pain", task$feature_names )])
                                 
                                 if (self$param_set$values$droplevels && nrow(dt)) {  # nrow(dt): workaround for https://github.com/Rdatatable/data.table/issues/5184
                                   dt = droplevels(dt)
                                 }
                                 list(levels = lapply(dt, function(x) levels(x)))  # explicitly access the "levels" function
                               } #else{
                               #list(NULL)
                               # }
                               
                             },
                             
                             .transform = function(task) {
                               if(any(grepl("ipos_shortness_breath|ipos_pain", task$feature_names))){
                                 dt = task$data(cols = task$feature_names[grep("ipos_shortness_breath|ipos_pain", task$feature_names )])
                                 
                                 # check which levels are actually different during training and prediction
                                 needs_adjustment = as.logical(imap(self$state$levels, function(lvx, id) {
                                   
                                   all.equal(setdiff(levels(dt[[id]]), lvx), "overwhelmingly") == TRUE
                                   
                                   
                                 }))
                                 
                                 if (!any(needs_adjustment)) {
                                   return(task)
                                 }
                                 
                                 changed_cols = as.data.table(imap(self$state$levels[needs_adjustment], function(lvx, id) {
                                   x = dt[[id]] 
                                   fct_collapse(x, severely = c("severely", "overwhelmingly"))
                                   
                                 }))
                                 task$select(setdiff(task$feature_names, colnames(changed_cols)))$cbind(changed_cols)
                               } else{
                                 return(task)
                               }
                               
                             }
                           )
)

mlr_pipeops$add("fixfactors.overwhelmingly", PipeOpFixFactorsOverwhelmingly)


# fixfactors.age ----
PipeOpFixFactorsAge   = R6Class("PipeOpFixFactorsAge",
                                inherit = PipeOpTaskPreprocSimple,
                                public = list(
                                  initialize = function(id = "fixfactors.age", param_vals = list()) {
                                    ps = ParamSet$new(params = list(
                                      ParamLgl$new("droplevels", tags = c("train", "predict"))
                                    ))
                                    ps$values = list(droplevels = TRUE)
                                    super$initialize(id, param_set = ps, param_vals = param_vals, tags = "robustify", feature_types = c("factor", "ordered"))
                                  }
                                ),
                                private = list(
                                  .get_state = function(task) {
                                    # get age levels of train task if age is a factor
                                    if(is.factor(task$data()$age)){
                                      dt = task$data(cols = "age")
                                      
                                      if (self$param_set$values$droplevels && nrow(dt)) {  # nrow(dt): workaround for https://github.com/Rdatatable/data.table/issues/5184
                                        dt = droplevels(dt)
                                      }
                                      list(levels = lapply(dt, function(x) levels(x)))  # explicitly access the "levels" function
                                    } #else{
                                    #list(NULL)
                                    # }
                                    
                                  },
                                  
                                  .transform = function(task) {
                                    if(is.factor(task$data()$age)){
                                      dt = task$data(cols = "age")
                                      
                                      # check which levels are actually different during training and prediction 
                                      # (the only problem should be the lowest category, "[21,50]" which will be transformed to "(50,60]")
                                      needs_adjustment = as.logical(imap(self$state$levels, function(lvx, id) {
                                        
                                        all.equal(setdiff(levels(dt[[id]]), lvx), "[21,50]") == TRUE
                                        
                                        
                                      }))
                                      
                                      if (!any(needs_adjustment)) {
                                        return(task)
                                      }
                                      
                                      changed_cols = as.data.table(imap(self$state$levels[needs_adjustment], function(lvx, id) {
                                        x = dt[[id]] 
                                        fct_collapse(x, `(50,60]`= c("[21,50]" , "(50,60]" ))
                                        
                                      }))
                                      task$select(setdiff(task$feature_names, colnames(changed_cols)))$cbind(changed_cols)
                                    } else{
                                      return(task)
                                    }
                                    
                                  }
                                )
)

mlr_pipeops$add("fixfactors.age", PipeOpFixFactorsAge)

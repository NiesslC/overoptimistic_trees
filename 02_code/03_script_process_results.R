library(purrr)
library(dplyr)
library(reshape2)
library(stringr)
library(janitor)


# Setting info -------------------------------------------------------------------------------------
setting_info = c("procedure", "learner_name", "rep", "setting", "sample_size",
                "split_type", "eval_criterion")

# p0 (default, default & featureless) --------------------------------------------------------------
res_filenames_p0 = list.files("./03_results/rdata/p0", full.names = TRUE)
resdf_p0_featureless = 1:length(res_filenames_p0) %>% purrr::map(.f = function(i) {
  res_p0 = mget(load(res_filenames_p0[i]))
  
  
  resdf_p0 = cbind(map_df(res_p0 , ~.[c("apparent_error", "resampling_error", "test_error",
                                        setting_info)]),
                   map_df(res_p0 , ~.$graph_learner$param_set$values[!grepl(".random|.affect_column",names(.$graph_learner$param_set$values))]))
  resdf_featureless = map_df(res_p0 , ~.[c("apparent_error_featureless", "test_error_featureless",
                                           setting_info)]) %>%
    mutate(procedure = "featureless") %>%
    rename(apparent_error = apparent_error_featureless,
           test_error = test_error_featureless)
  #resdf_p0_featureless = bind_rows(resdf_p0, resdf_featureless)
  
  rm(res_p0)
  gc()
  return(list(resdf_p0, resdf_featureless))
})
# default 
resdf_p0 = resdf_p0_featureless %>% map_df(~.[[1]])
resdf_p0 = melt(resdf_p0, measure.vars = c("apparent_error", "resampling_error"),
                value.name = "eval_error", variable.name = "procedure_eval")
resdf_p0 = resdf_p0 %>% mutate(procedure_gen_eval = case_when(
  procedure_eval == "apparent_error" ~ "p0_apparent",
  procedure_eval == "resampling_error" ~ "p0_resampling",
))
save(resdf_p0, file = "./03_results/rdata/p0.RData")

# featureless
resdf_featureless = resdf_p0_featureless %>% map_df(~.[[2]])
resdf_featureless = resdf_featureless %>% mutate(procedure_gen_eval = "featureless_apparent",
                                                 procedure_eval = "apparent_error") %>%
  rename(eval_error = apparent_error)
save(resdf_featureless, file = "./03_results/rdata/p_featureless.RData")



# p1 (tune, default) -------------------------------------------------------------------------------
res_filenames_p1 = list.files("./03_results/rdata/p1", full.names = TRUE)
resdf_p1 = 1:length(res_filenames_p1) %>% purrr::map_df(.f = function(i) {
  res_p1 = mget(load(res_filenames_p1[i]))
  resdf_p1 = cbind(map_df(res_p1 , ~.[c("apparent_error", "resampling_error", "nested_resampling_error", "test_error",
                                        setting_info)]),
                   # grepl! necessary some HPs in some learners give error because formula and not a character:
                   map_df(res_p1 , ~.$graph_learner_tuned$param_set$values[!grepl(".random|.affect_column",
                                                                                  names(.$graph_learner_tuned$param_set$values))]))
  rm(res_p1)
  gc()
  return(resdf_p1)
})

resdf_p1 = melt(resdf_p1, measure.vars = c("apparent_error", "resampling_error", "nested_resampling_error"),
     value.name = "eval_error", variable.name = "procedure_eval")
resdf_p1 = resdf_p1 %>% mutate(procedure_gen_eval = case_when(
  procedure_eval == "apparent_error" ~ "p1_apparrent",
  procedure_eval == "resampling_error" ~ "p1_resampling",
  procedure_eval == "nested_resampling_error" ~ "p1_nested"
  ))
save(resdf_p1, file = "./03_results/rdata/p1.RData")



# p2a (tune, stepwise apparent) --------------------------------------------------------------------
res_filenames_p2a = list.files("./03_results/rdata/p2a", full.names = TRUE)
resdf_p2a = 1:length(res_filenames_p2a) %>% purrr::map_df(.f = function(i) {
  res_p2a = mget(load(res_filenames_p2a[i]))
  resdf_p2a = cbind(map_df(res_p2a , ~.[setting_info]),
                    map_df(res_p2a , ~.[["final_tree"]][c("apparent_error", "resampling_error", "test_error")]),
                    # grepl! necessary some HPs in some learners give error because formula and not a character:
                    map_df(res_p2a , ~.$final_tree$graph_learner_tuned$param_set$values[!grepl(".random|affect_column",
                                                                                               names(.$final_tree$graph_learner_tuned$param_set$values))]))
  rm(res_p2a)
  gc()
  return(resdf_p2a)
})
resdf_p2a = melt(resdf_p2a, measure.vars = c("apparent_error", "resampling_error"),
                value.name = "eval_error", variable.name = "procedure_eval")
resdf_p2a = resdf_p2a %>% mutate(procedure_gen_eval = case_when(
  procedure_eval == "apparent_error" ~ "p2a_apparent",
  procedure_eval == "resampling_error" ~ "p2a_resampling"
))
save(resdf_p2a, file = "./03_results/rdata/p2a.RData")


# p2b (tune, stepwise resampling) --------------------------------------------------------------------
res_filenames_p2b = list.files("./03_results/rdata/p2b", full.names = TRUE)
resdf_p2b = 1:length(res_filenames_p2b) %>% purrr::map_df(.f = function(i) {
  res_p2b = mget(load(res_filenames_p2b[i]))
  resdf_p2b = cbind(map_df(res_p2b , ~.[setting_info]),
                    map_df(res_p2b , ~.[["final_tree"]][c("apparent_error", "resampling_error", "test_error")]),
                    # grepl! necessary some HPs in some learners give error because formula and not a character:
                    map_df(res_p2b , ~.$final_tree$graph_learner_tuned$param_set$values[!grepl(".random",
                                                                                               names(.$final_tree$graph_learner_tuned$param_set$values))]))
  rm(res_p2b)
  gc()
  return(resdf_p2b)
})
resdf_p2b = melt(resdf_p2b, measure.vars = c("apparent_error", "resampling_error"),
                 value.name = "eval_error", variable.name = "procedure_eval")
resdf_p2b = resdf_p2b %>% mutate(procedure_gen_eval = case_when(
  procedure_eval == "apparent_error" ~ "p2b_apparent",
  procedure_eval == "resampling_error" ~ "p2b_resampling"
))
save(resdf_p2b, file = "./03_results/rdata/p2b.RData")


# p3 (tune, tune) ----------------------------------------------------------------------------------
res_filenames_p3 = list.files("./03_results/rdata/p3", full.names = TRUE)
resdf_p3 = 1:length(res_filenames_p3) %>% purrr::map_df(.f = function(i) {
  res_p3 = mget(load(res_filenames_p3[i]))
  resdf_p3 = cbind(map_df(res_p3 , ~.[c("apparent_error", "resampling_error", "nested_resampling_error", "test_error",
                                        setting_info)]),
                   # grepl! necessary some HPs in some learners give error because formula and not a character:
                   map_df(res_p3 , ~.$graph_learner_tuned$param_set$values[!grepl(".random|.affect_column",names(.$graph_learner_tuned$param_set$values))]))
  rm(res_p3)
  gc()
  return(resdf_p3)
})

resdf_p3 = melt(resdf_p3, measure.vars = c("apparent_error", "resampling_error", "nested_resampling_error"),
                value.name = "eval_error", variable.name = "procedure_eval")
resdf_p3 = resdf_p3 %>% mutate(procedure_gen_eval = case_when(
  procedure_eval == "apparent_error" ~ "p3_apparent",
  procedure_eval == "resampling_error" ~ "p3_resampling",
  procedure_eval == "nested_resampling_error" ~ "p3_nested"
  ))
save(resdf_p3, file = "./03_results/rdata/p3.RData")

# p4a (default, stepwise apparent) -----------------------------------------------------------------
res_filenames_p4a = list.files("./03_results/rdata/p4a", full.names = TRUE)

resdf_p4a = 1:length(res_filenames_p4a) %>% purrr::map_df(.f = function(i) { 
  res_p4a = mget(load(res_filenames_p4a[i]))
  resdf_p4a = cbind(map_df(res_p4a , ~.[setting_info]),
                    map_df(res_p4a , ~.[["final_tree"]][c("apparent_error", "resampling_error", "test_error")]),
                    # grepl! necessary some HPs in some learners give error because formula and not a character:
                    map_df(res_p4a , ~.$final_tree$graph_learner$param_set$values[!grepl(".random|affect_column",
                                                                                           names(.$final_tree$graph_learner$param_set$values))]))
  rm(res_p4a)
  gc()
  return(resdf_p4a)
})
resdf_p4a = melt(resdf_p4a, measure.vars = c("apparent_error", "resampling_error"),
                 value.name = "eval_error", variable.name = "procedure_eval")
resdf_p4a = resdf_p4a %>% mutate(procedure_gen_eval = case_when(
  procedure_eval == "apparent_error" ~ "p4a_apparent",
  procedure_eval == "resampling_error" ~ "p4a_resampling"
))
save(resdf_p4a, file = "./03_results/rdata/p4a.RData")




# Summarise results in one df and save -------------------------------------------------------------
load("./03_results/rdata/p_featureless.RData") # ok & complete (7.8.)
load("./03_results/rdata/p0.RData") # ok & complete (7.8.)
load("./03_results/rdata/p1.RData") # ok & complete (7.8.)
load("./03_results/rdata/p2a.RData") # ok & complete (9.8.)
load("./03_results/rdata/p2b.RData") # ok & complete (7.8.)
load("./03_results/rdata/p3.RData") # ok & complete (9.8.)
load("./03_results/rdata/p4a.RData") # ok & complete (31.7.)


resdf = bind_rows(resdf_featureless, 
  resdf_p0, resdf_p1, resdf_p2a, resdf_p2b, resdf_p3, resdf_p4a)
rm(setting_info, resdf_featureless,
   resdf_p0, resdf_p1, resdf_p2a, resdf_p2b, resdf_p3, resdf_p4a,
   res_p0, res_p1, res_p2a, res_p2b, res_p3, res_p4a,
   res_filenames_p0, res_filenames_p1, res_filenames_p2a, res_filenames_p2b, res_filenames_p3, res_filenames_p4a)

# Correct and remove variables
resdf = resdf %>% 
  mutate(procedure_gen_eval = factor(procedure_gen_eval, # order levels 
                                  levels = c("featureless_apparent", 
                                             "p0_apparent", "p0_resampling", 
                                             "p1_apparrent", "p1_resampling", "p1_nested",
                                             "p2a_apparent", "p2a_resampling",
                                             "p2b_apparent", "p2b_resampling",
                                             "p3_apparent", "p3_resampling", "p3_nested",
                                             "p4a_apparent", "p4a_resampling")),
         procedure_gen = procedure, # correct name
         procedure_gen_short = case_when( # abbreviations
           procedure_gen == "featureless" ~ "featureless",
           procedure_gen == "learner.hp.default_preproc.hp.default" ~ "p0",
           procedure_gen == "learner.hp.tune_preproc.hp.default" ~ "p1",
           procedure_gen == "learner.hp.tune_preproc.hp.steopt_error.apparent" ~ "p2a",
           procedure_gen == "learner.hp.tune_preproc.hp.steopt_error.resampling" ~ "p2b",
           procedure_gen == "learner.hp.tune_preproc.hp.tune" ~ "p3",
           procedure_gen == "learner.hp.default_preproc.hp.steopt_error.apparent" ~ "p4a"
           )) %>% 
  select(-procedure)
resdf = resdf %>% select(-contains(c("xval","include.partitioning.vars_expr","EstimateRandomEffects",
                                     "fixfactors", "re.form", ".cv", "grouping")))

# Unify HP names
res_cart = resdf %>% filter(learner_name == "lrn_cart") %>% remove_empty(which = "cols") %>%
  rename_with(~ sub("^lrn_cart\\.", "", .))
res_ctree = resdf %>% filter(learner_name == "lrn_ctree") %>% remove_empty(which = "cols") %>%
  rename_with(~ sub("^lrn_ctree\\.", "", .))
res_lmertree_t = resdf %>% filter(learner_name == "lrn_lmertree_t") %>% remove_empty(which = "cols") %>%
  rename_with(~ sub("^lrn_lmertree_t\\.", "", .))
res_reemtree_t = resdf %>% filter(learner_name == "lrn_reemtree_t") %>% remove_empty(which = "cols") %>%
  rename_with(~ sub("^lrn_reemtree_t\\.", "", .))

resdf = bind_rows(res_cart, res_ctree, res_lmertree_t, res_reemtree_t)
rm(res_cart, res_ctree, res_lmertree_t, res_reemtree_t)
colnames(resdf)[grepl("preproc.|maxdepth|minsplit|minbucket|cp|alpha",colnames(resdf))] = 
  paste0("hp_",colnames(resdf)[grepl("preproc.|maxdepth|minsplit|minbucket|cp|alpha",colnames(resdf))] )

resdf = resdf %>% select(-hp_preproc.target.option) # remove this variable because HP is never tuned

# Relocate variables
resdf = resdf %>% relocate(procedure_gen_eval, procedure_gen, procedure_gen_short, procedure_eval, 
                           eval_error, test_error, .after = eval_criterion ) %>% 
  relocate(setting, split_type)
# Save
resdf = resdf %>% arrange(procedure_gen_eval,rep)
save(resdf, file = "./03_results/rdata/_resdf.RData") 
#---------------------------------------------------------------------------------------------------

# Trees
# restree_p0 = map(res_p0, ~.$graph_learner$model[grepl("lrn",names(.$graph_learner$model))][[1]]$model) 
# names(restree_p0) = setting_names

# Trees
# restree_p1 = map(res_p1, ~.$graph_learner$model[grepl("lrn",names(.$graph_learner_tuned$model))][[1]]$model) 
# names(restree_p1) = setting_info_names
# test = restree_p1 %>% map_at(grep("lmertree", names(restree_p1)), ~ .$tree)
# test = test %>% map_at(grep("reemtree", names(restree_p1)[745:750]), ~ .$Tree)
# cart = $x
# ctree as is
# reemtree = $Tree
# lmertree $tree
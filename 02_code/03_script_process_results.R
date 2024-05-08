library(purrr)
library(dplyr)
library(reshape2)
library(stringr)
# Load results -------------------------------------------------------------------------------------
res_filenames = list.files("./03_results/rdata", full.names = TRUE)
res_filenames = res_filenames[!grepl("pilot|id_split", res_filenames)]

# Extract relevant results -------------------------------------------------------------------------
setting_info = c("procedure", "learner_name", "rep", "setting", "sample_size",
                "split_type", "eval_criterion")
# featureless
# res_filenames_featureless = res_filenames[grepl("featureless", res_filenames)]
# res_featureless = sapply(res_filenames_featureless, function(x) mget(load(x)))
# resdf_featureless = cbind(map_df(res_featureless , ~.[c("apparent_error", "test_error", 
#                                       "procedure", "rep", "learner_name", "setting")]),
#                  map_df(res_featureless , ~.$graph_learner$param_set$values))
# resdf_featureless = resdf_featureless %>% 
#   rename(eoi_value = apparent_error) %>% 
#   mutate(procedure_short = "featureless",
#          eoi_type = "apparent_error",
#          eval_criterion = stringr::word(names(res_featureless), 4, sep = '\\_')) 

# p0 (default, default)
res_filenames_p0 = res_filenames[grepl("p0", res_filenames)]
res_p0 =sapply(res_filenames_p0, function(x) mget(load(x)))
resdf_p0 = cbind(map_df(res_p0 , ~.[c("apparent_error", "resampling_error", "test_error", setting_info)]),
                 # grepl! necessary bc otherwise the ".random" HP in some learners gives an error because it is a formula and not a character:
                 map_df(res_p0, ~.$graph_learner$param_set$values[!grepl(".random",names(.$graph_learner$param_set$values))])) 

resdf_p0 = melt(resdf_p0, measure.vars = c("apparent_error", "resampling_error"),
                value.name = "eoi_value", variable.name = "eoi_type")
resdf_p0 = resdf_p0 %>% mutate(procedure_short = case_when(
  eoi_type == "apparent_error" ~ "p0a",
  eoi_type == "resampling_error" ~ "p0b",
))

# p1 
res_filenames_p1 = res_filenames[grepl("p1", res_filenames)]
res_p1 =sapply(res_filenames_p1, function(x) mget(load(x)))
resdf_p1 = cbind(map_df(res_p1 , ~.[c("apparent_error", "resampling_error", "nested_resampling_error", "test_error", setting_info)]),
                 # grepl! necessary bc otherwise the ".random" HP in some learners gives an error because it is a formula and not a character:
                 map_df(res_p1, ~.$graph_learner$param_set$values[!grepl(".random",names(.$graph_learner$param_set$values))])) 
resdf_p1 = melt(resdf_p1, measure.vars = c("apparent_error", "resampling_error", "nested_resampling_error"),
     value.name = "eoi_value", variable.name = "eoi_type")
resdf_p1 = resdf_p1 %>% mutate(procedure_short = case_when(
  eoi_type == "apparent_error" ~ "p1a",
  eoi_type == "resampling_error" ~ "p1b",
  eoi_type == "nested_resampling_error" ~ "p1c"
  ))

# p2a 
res_filenames_p2a = res_filenames[grepl("p2a", res_filenames)] 
res_p2a =sapply(res_filenames_p2a, function(x) mget(load(x)))
resdf_p2a = cbind(map_df(res_p2a , ~.[setting_info]),
                  map_df(res_p2a , ~.[["final_tree"]][c("apparent_error", "test_error")]),
                  map_df(res_p2a , ~.$final_tree$graph_learner_tuned$param_set$values[!grepl(".random",names(.$final_tree$graph_learner_tuned$param_set$values))]))
resdf_p2a = resdf_p2a %>% 
  rename(eoi_value = apparent_error) %>% 
  mutate(procedure_short = "p2a",
         eoi_type = "apparent_error")

# p2b 
res_filenames_p2b = res_filenames[grepl("p2b", res_filenames)] 
res_p2b =sapply(res_filenames_p2b, function(x) mget(load(x)))
resdf_p2b = cbind(map_df(res_p2b , ~.[setting_info]),
                  map_df(res_p2b , ~.[["final_tree"]][c("resampling_error", "test_error")]),
                  map_df(res_p2b , ~.$final_tree$graph_learner_tuned$param_set$values[!grepl(".random",names(.$final_tree$graph_learner_tuned$param_set$values))]))
resdf_p2b = resdf_p2b %>% 
  rename(eoi_value = resampling_error) %>% 
  mutate(procedure_short = "p2b",
         eoi_type = "resampling_error")


# p3
res_filenames_p3 = res_filenames[grepl("p3", res_filenames)]
res_p3 = sapply(res_filenames_p3, function(x) mget(load(x)))
resdf_p3 = cbind(map_df(res_p3 , ~.[c("apparent_error", "resampling_error", "nested_resampling_error", "test_error",
                                      setting_info)]),
                 map_df(res_p3 , ~.$graph_learner_tuned$param_set$values[!grepl(".random",names(.$graph_learner$param_set$values))]))

resdf_p3 = melt(resdf_p3, measure.vars = c("apparent_error", "resampling_error", "nested_resampling_error"),
                value.name = "eoi_value", variable.name = "eoi_type")
resdf_p3 = resdf_p3 %>% mutate(procedure_short = case_when(
  eoi_type == "apparent_error" ~ "p3a",
  eoi_type == "resampling_error" ~ "p3b",
  eoi_type == "nested_resampling_error" ~ "p3c"
  ))

# Summarise results in one df and save
resdf = bind_rows(#resdf_featureless, 
  resdf_p0, resdf_p1, resdf_p2a, resdf_p2b, resdf_p3)
resdf = resdf %>% 
  mutate(procedure_short = factor(procedure_short,
                                  levels = c(#"featureless", 
                                             "p0a", "p0b", 
                                             "p1a", "p1b", "p1c",
                                             "p2a", "p2b",
                                             "p3a", "p3b", "p3c")))
resdf = resdf %>% arrange(procedure_short,rep)
save(resdf, file = "./03_results/rdata/_resdf.RData")




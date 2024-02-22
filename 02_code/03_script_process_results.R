library(purrr)
library(dplyr)
library(reshape2)
library(stringr)
# Load results -------------------------------------------------------------------------------------
res_filenames = list.files("./03_results/rdata", full.names = TRUE)

# Extract relevant results -------------------------------------------------------------------------
# featureless
res_filenames_featureless = res_filenames[grepl("featureless", res_filenames)]
res_featureless = sapply(res_filenames_featureless, function(x) mget(load(x)))
resdf_featureless = cbind(map_df(res_featureless , ~.[c("apparent_error", "test_error", 
                                      "procedure", "rep", "learner_name", "setting")]),
                 map_df(res_featureless , ~.$graph_learner$param_set$values))
resdf_featureless = resdf_featureless %>% 
  rename(eoi_value = apparent_error) %>% 
  mutate(procedure_short = "featureless",
         eoi_type = "apparent_error",
         eval_criterion = stringr::word(names(res_featureless), 4, sep = '\\_')) 

# p0
res_filenames_p0 = res_filenames[grepl("p0", res_filenames)]
res_p0 =sapply(res_filenames_p0, function(x) mget(load(x)))
resdf_p0 = cbind(map_df(res_p0 , ~.[c("apparent_error", "test_error", 
                              "procedure", "rep", "learner_name", "setting")]),
                 map_df(res_p0 , ~.$graph_learner$param_set$values))
resdf_p0 = resdf_p0 %>% 
  rename(eoi_value = apparent_error) %>% 
  mutate(procedure_short = "p0",
         eoi_type = "apparent_error",
         eval_criterion = stringr::word(names(res_p0), 4, sep = '\\_')) 

# p1 
res_filenames_p1 = res_filenames[grepl("p1", res_filenames)]
res_p1 =sapply(res_filenames_p1, function(x) mget(load(x)))
resdf_p1 = cbind(map_df(res_p1 , ~.[c("apparent_error", "resampling_error", "nested_resampling_error", "test_error",
                              "procedure", "rep", "learner_name", "setting")]),
                 map_df(res_p1 , ~.$graph_learner_tuned$param_set$values))
resdf_p1 = resdf_p1 %>% 
  mutate(eval_criterion = stringr::word(names(res_p1), 4, sep = '\\_'))
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
resdf_p2a = cbind(map_df(res_p2a , ~.[c("procedure", "rep", "learner_name", "setting")]),
  map_df(res_p2a , ~.[["final_tree"]][c("apparent_error", "test_error")]),
                 map_df(res_p2a , ~.$final_tree$graph_learner_tuned$param_set$values))
resdf_p2a = resdf_p2a %>% 
  rename(eoi_value = apparent_error) %>% 
  mutate(procedure_short = "p2a",
         eoi_type = "apparent_error",
         eval_criterion = stringr::word(names(res_p2a), 4, sep = '\\_'))

# p2b 
res_filenames_p2b = res_filenames[grepl("p2b", res_filenames)] 
res_p2b =sapply(res_filenames_p2b, function(x) mget(load(x)))
resdf_p2b = cbind(map_df(res_p2b , ~.[c("procedure", "rep", "learner_name", "setting")]),
                  map_df(res_p2b , ~.[["final_tree"]][c("resampling_error", "test_error")]),
                  map_df(res_p2b , ~.$final_tree$graph_learner_tuned$param_set$values))
resdf_p2b = resdf_p2b %>% 
  rename(eoi_value = resampling_error) %>% 
  mutate(procedure_short = "p2b",
         eoi_type = "resampling_error",
         eval_criterion = stringr::word(names(res_p2b), 4, sep = '\\_'))


# p3
res_filenames_p3 = res_filenames[grepl("p3", res_filenames)]
res_p3 = sapply(res_filenames_p3, function(x) mget(load(x)))
resdf_p3 = cbind(map_df(res_p3 , ~.[c("apparent_error", "resampling_error", "nested_resampling_error", "test_error",
                                      "procedure", "rep", "learner_name", "setting")]),
                 map_df(res_p3 , ~.$graph_learner_tuned$param_set$values))
resdf_p3 = resdf_p3 %>% 
  mutate(eval_criterion = stringr::word(names(res_p3), 4, sep = '\\_'))
resdf_p3 = melt(resdf_p3, measure.vars = c("apparent_error", "resampling_error", "nested_resampling_error"),
                value.name = "eoi_value", variable.name = "eoi_type")
resdf_p3 = resdf_p3 %>% mutate(procedure_short = case_when(
  eoi_type == "apparent_error" ~ "p3a",
  eoi_type == "resampling_error" ~ "p3b",
  eoi_type == "nested_resampling_error" ~ "p3c"
  ))

# Summarise results in one df and save
resdf = bind_rows(resdf_featureless, resdf_p0, resdf_p1, resdf_p2a, resdf_p2b, resdf_p3)
resdf = resdf %>% 
  mutate(procedure_short = factor(procedure_short,
                                  levels = c("featureless", 
                                             "p0", 
                                             "p1a", "p1b", "p1c",
                                             "p2a", "p2b",
                                             "p3a", "p3b", "p3c")))
resdf = resdf %>% arrange(procedure_short,rep)
save(resdf, file = "./03_results/rdata/_resdf.RData")




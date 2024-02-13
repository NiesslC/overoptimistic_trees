library(purrr)
library(dplyr)
library(reshape2)
library(ggplot2)
# Load results
resfiles = list.files("./03_results/rdata", full.names = TRUE)

# Extract relevant results
# p0
resfiles_p0 = resfiles[grepl("p0", resfiles)]
res_p0 =sapply(resfiles_p0, function(x) mget(load(x)))
resdf_p0 = cbind(map_df(res_p0 , ~.[c("apparent_error", "test_error", 
                              "procedure", "rep", "learner_name", "setting")]),
                 map_df(res_p0 , ~.$graph_learner$param_set$values))
resdf_p0 = resdf_p0 %>% 
  rename(eoi_value = apparent_error) %>% 
  mutate(procedure_short = "p0",
         eoi_type = "apparent_error") 

# p1 
resfiles_p1 = resfiles[grepl("p1", resfiles)]
res_p1 =sapply(resfiles_p1, function(x) mget(load(x)))
resdf_p1 = cbind(map_df(res_p1 , ~.[c("apparent_error", "resampling_error", "nested_resampling_error", "test_error",
                              "procedure", "rep", "learner_name", "setting")]),
                 map_df(res_p1 , ~.$graph_learner_tuned$param_set$values))
resdf_p1 = melt(resdf_p1, measure.vars = c("apparent_error", "resampling_error", "nested_resampling_error"),
     value.name = "eoi_value", variable.name = "eoi_type")
resdf_p1 = resdf_p1 %>% mutate(procedure_short = case_when(
  eoi_type == "apparent_error" ~ "p1a",
  eoi_type == "resampling_error" ~ "p1b",
  eoi_type == "nested_resampling_error" ~ "p1c"
))

# p2a 
resfiles_p2a = resfiles[grepl("p2a", resfiles)] 
res_p2a =sapply(resfiles_p2a, function(x) mget(load(x)))
resdf_p2a = cbind(map_df(res_p2a , ~.[c("procedure", "rep", "learner_name", "setting")]),
  map_df(res_p2a , ~.[["final_tree"]][c("apparent_error", "test_error")]),
                 map_df(res_p2a , ~.$final_tree$graph_learner_tuned$param_set$values))
resdf_p2a = resdf_p2a %>% 
  rename(eoi_value = apparent_error) %>% 
  mutate(procedure_short = "p2a",
         eoi_type = "apparent_error")

# p2b 
resfiles_p2b = resfiles[grepl("p2b", resfiles)] 
res_p2b =sapply(resfiles_p2b, function(x) mget(load(x)))
resdf_p2b = cbind(map_df(res_p2b , ~.[c("procedure", "rep", "learner_name", "setting")]),
                  map_df(res_p2b , ~.[["final_tree"]][c("resampling_error", "test_error")]),
                  map_df(res_p2b , ~.$final_tree$graph_learner_tuned$param_set$values))
resdf_p2b = resdf_p2b %>% 
  rename(eoi_value = resampling_error) %>% 
  mutate(procedure_short = "p2b",
         eoi_type = "resampling_error")


# p3
resfiles_p3 = resfiles[grepl("p3", resfiles)]
res_p3 =sapply(resfiles_p3, function(x) mget(load(x)))
resdf_p3 = cbind(map_df(res_p3 , ~.[c("apparent_error", "resampling_error", "nested_resampling_error", "test_error",
                                      "procedure", "rep", "learner_name", "setting")]),
                 map_df(res_p3 , ~.$graph_learner_tuned$param_set$values))
resdf_p3 = melt(resdf_p3, measure.vars = c("apparent_error", "resampling_error", "nested_resampling_error"),
                value.name = "eoi_value", variable.name = "eoi_type")
resdf_p3 = resdf_p3 %>% mutate(procedure_short = case_when(
  eoi_type == "apparent_error" ~ "p3a",
  eoi_type == "resampling_error" ~ "p3b",
  eoi_type == "nested_resampling_error" ~ "p3c"
))

# summarise
resdf = bind_rows(resdf_p0, resdf_p1, resdf_p2a, resdf_p2b, resdf_p3)
resdf = resdf %>% 
  mutate(procedure_short = factor(procedure_short,
                                  levels = c("p0", 
                                             "p1a", "p1b", "p1c",
                                             "p2a", "p2b",
                                             "p3a", "p3b", "p3c")))
save(resdf, file = "./03_results/rdata/resdf.RData")

# plot
ggplot(resdf, aes(y = test_error - eoi_value, x = procedure_short, col = procedure_short))+
  geom_boxplot()+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted")
ggplot(resdf, aes(y = test_error, x = procedure_short, col = procedure_short))+
  geom_boxplot()+
  theme_bw()
 # geom_abline()+
 # lims(x = c(65, 90), y = c(65,90))
#######################

# - Check consistency of estimates of optimistic bias
# - Extract errors [(i) reported error and (ii) test set error] (results aufbereiten als datensatz)
# - check that correct vars have been used via graph_learner_tuned$model$lrn_glmertree_if$model$formula
# t =res_sapv_p1[[1]]$graph_learner_tuned
# t$graph_learner_tuned$param_set
# t$model$lrn_rpart$train_task


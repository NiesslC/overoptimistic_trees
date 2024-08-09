library(ggplot2)
library(cumstats)
library(dplyr)
library(gridExtra)
library(latex2exp)
library(reshape2)
library(ggh4x)
library(wesanderson)
# Load, check and prepare results df ---------------------------------------------------------------
load("./03_results/rdata/_resdf.RData")
stopifnot(length(unique(resdf$setting))==1) # we only consider one palliative care setting (sapv)

## Set plotting colors -----------------------------------------------------------------------------
###col_error = 

## Check number of settings -------------------------------------------------------------------------
# Compare number of settings by design vs. actual number (ignoring split_type)
stopifnot(
  prod(unname(unlist(resdf %>% select(procedure_gen, learner_name,  sample_size, eval_criterion) %>% summarise_all(~ length(unique(.)))))) ==
    # -> should be 80 different trees for each repetition (but more reported performances)
    nrow(resdf %>% group_by(procedure_gen, learner_name, sample_size, eval_criterion) %>%
           summarise(n())))

## Change variable lables for plotting --------------------------------------------------------------
resdf = resdf %>% mutate(learner_name = factor(learner_name, levels = c("lrn_cart",
                                                                "lrn_ctree",
                                                                "lrn_lmertree_t",
                                                                "lrn_reemtree_t"),
                                       labels = c("CART", "CIT", "REEMT", "LMMT")),
                         procedure_gen_label = factor(procedure_gen_short, 
                                                      levels = c("featureless", "p0", "p1", "p4a", "p3", "p2b", "p2a"),
                                               labels = c("Setting I (featureless)",
                                                          "Setting I",
                                                          "Setting II - A1",
                                                          "Setting II - M1",
                                                          "Setting II - A2",
                                                          "Setting II - AM1",
                                                          "Setting II - AM2")),
                         procedure_eval = factor(procedure_eval, 
                                                 levels = c("apparent_error", "resampling_error",
                                                            "nested_resampling_error"),
                                                 labels = c("Apparent", "Resampling", "Nested resampling")),
                         # only for plot titles:
                         title_eval = "Performance measure",
                         title_gen = "Model generation",
                         eval_criterion_label = factor(eval_criterion, levels = c("regr.rmse", "regr.rsq"),
                                                       labels = c("RMSE", "R^2")))


## Check that AM1 is similar to A2 and exclude from data set ----------------------------------------
ggplot(resdf %>% filter(procedure_gen_short %in% c("p3", "p2b") & split_type == "naive"), 
       aes(y = eval_error-test_error, col = procedure_eval,
                  x = procedure_gen_short))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_boxplot()+
  # facet_grid(eval_criterion ~ procedure_gen_short, scales = "free_y")+
  theme_bw()+
  labs(x = "Learning algorithm", col = "Model evaluation", y = "PE(D) - PE(Dnew)")+
  ggh4x::facet_nested(title_eval + eval_criterion_label + sample_size ~  learner_name, scales = "free_y",
                      labeller = labeller(eval_criterion_label = label_parsed))+
  theme(legend.position = "top")
# -> p2b and p3 very similar -> exclude
resdf = resdf %>% filter(procedure_gen_short != "p2b")

## Split in naive vs. teams splitting (naive is main analysis) -------------------------------------
resdf_teams = resdf %>% filter(split_type == "teams")
resdf = resdf %>% filter(split_type == "naive")


# A) Compare test and reported errors: differences  ------------------------------------------------
stopifnot(length(unique(resdf$split_type))==1) # we only consider one split_type here

p = ggplot(resdf %>% filter(procedure_gen != "featureless"), aes(y = eval_error-test_error, col = procedure_eval,
                      x = learner_name))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Learning algorithm", col = "Model evaluation", y = "PE(D) - PE(Dnew)")+
  ggh4x::facet_nested(title_eval + eval_criterion_label ~  title_gen + procedure_gen_label, scales = "free_y",
                      labeller = labeller(eval_criterion_label = label_parsed))+
  theme(legend.position = "top")

p %+% subset(resdf, sample_size == "sample50")
ggsave("03_results/plots/diff50.pdf")
p %+% subset(resdf, sample_size == "sample25")
ggsave("03_results/plots/diff25.pdf")


# B) Compare test and reported errors: absolute values  --------------------------------------------
df_test = resdf %>% select(-procedure_gen_eval, -procedure_eval, -eval_error, -contains("hp_")) %>%
  distinct() %>% 
  rename(error_value = test_error) %>%
  mutate(error_type = "Dnew")
df_eval = resdf %>% select(-test_error, -procedure_gen_eval, -contains("hp_")) %>%
  distinct() %>% 
  rename(error_value = eval_error,
         error_type = procedure_eval)
resdf_abs = bind_rows(df_test, df_eval)
resdf_abs = resdf_abs %>% mutate(error_type = factor(error_type,
                                                     levels = c("Apparent","Resampling",
                                                                "Nested resampling","Dnew")))

ggplot(resdf_abs, 
       aes(col = error_type, y = error_value, x = learner_name))+
  geom_boxplot()+
  ggh4x::facet_nested(title_eval + eval_criterion_label ~  title_gen + procedure_gen_label, scales = "free_y",
                      labeller = labeller(eval_criterion_label = label_parsed))+
  theme_bw()+
  labs(x = "Learning algorithm", col = "Model evaluation", y = "PE")+
  theme(legend.position = "top")


## evtl. rechts bias mit skala rechts
#########sample size
#### featureless test ? 



# C) Check selected HPs -------------------------------------------------------------------------------

# Only consider generation procedures
resdf_hp = resdf %>% select(-procedure_gen_eval, -procedure_eval, -eval_error) %>%
  distinct() %>% 
  filter(!(procedure_gen_short %in% c("p0")))

# Preprocessing HPs
resdf_hp_preproc = resdf_hp %>% filter(!(procedure_gen_short %in% c("p1")))
resdf_hp_preproc = melt(resdf_hp_preproc, 
                        measure.vars = colnames(resdf %>% select(starts_with("hp_"))),
                        value.name = "hp_value", variable.name = "hp_type") %>%
  filter(grepl("preproc.", hp_type))
resdf_hp_preproc = droplevels(resdf_hp_preproc)
resdf_hp_preproc = resdf_hp_preproc %>% 
  mutate(hp_type_label = factor(hp_type,
                                # the level order corresponds to the order in which the HPs are considered during sequential tuning
                                levels = c("hp_preproc.feature.ipos.option", "hp_preproc.feature.age.option", 
                                           "hp_preproc.feature.akps.option",
                                           "hp_preproc.drop.targetout.option", 
                                           "hp_preproc.drop.iposca.option"),
                                labels = c("IPOS", "Age", "AKPS", "Drop Outlier", "Drop IPOS"))) 


p = ggplot(resdf_hp_preproc, 
           aes(x = learner_name, fill = hp_value))+
  geom_bar()+
  ggh4x::facet_nested(title_eval + eval_criterion_label ~ hp_type_label + title_gen + procedure_gen_label ,
                     # labeller = labeller(eval_criterion_label = label_parsed)
                      )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # scale_x_discrete(labels = c("IPOS" = expression(lambda[IPOSScore]),
  #                             "Age" = expression(lambda[Age]), 
  #                             "AKPS" = expression(lambda[AKPS]),
  #                             "Drop Outlier" = expression(lambda[DropOutlier]), 
  #                             "Drop IPOS" = expression(lambda[DropIPOS])))+
  scale_fill_manual(values = rev(wes_palette("GrandBudapest2", n = 4)))+
  labs()

p %+% subset(resdf_hp_preproc, sample_size == "sample50")
p %+% subset(resdf_hp_preproc, sample_size == "sample25")


# Algorithm HPs
resdf_hp_algo = melt(resdf_hp,
                     measure.vars = colnames(resdf %>% select(hp_minbucket, hp_cp, hp_alpha)),
                     value.name = "hp_value", variable.name = "hp_type") %>% 
  filter(!is.na(hp_value))
### lieber einzeln die hps
### alpha reemtree????
ggplot(resdf_hp_algo %>% 
         filter(hp_type == "hp_cp"), aes(x = learner_name, y = hp_value))+
  geom_boxplot()+
  ggh4x::facet_nested(title_eval + eval_criterion_label ~ title_gen + procedure_gen_label,
                      labeller = labeller(eval_criterion_label = label_parsed))+
  theme_bw()

ggplot(resdf_hp_algo %>% 
         filter(hp_type == "hp_alpha"), aes(x = learner_name, y = hp_value))+
  geom_boxplot()+
  ggh4x::facet_nested(title_eval + eval_criterion_label ~ title_gen + procedure_gen_label,
                      labeller = labeller(eval_criterion_label = label_parsed))+
  theme_bw()

ggplot(resdf_hp_algo %>% 
         filter(hp_type == "hp_minbucket"), aes(x = learner_name, y = hp_value))+
  geom_boxplot()+
  ggh4x::facet_nested(title_eval + eval_criterion_label ~ title_gen + procedure_gen_label,
                      labeller = labeller(eval_criterion_label = label_parsed))+
  theme_bw()













# Plot cummedian of estimates --------------------------------------------------------------------
estimatesvar= resdf %>% group_by(learner_name, eval_criterion, procedure_gen_eval, sample_size) %>%
  arrange(rep) %>%
  mutate(cummedian_test_error = cummedian(test_error), 
         cummedian_diff = cummedian(test_error-eval_error), .after=test_error)
ggplot(estimatesvar, ######## entferne duplicates test setting
       aes(x = rep, y = cummedian_test_error, col = learner_name))+
  geom_point()+
  geom_line()+
  theme_bw()+
  ggh4x::facet_nested(title_eval + eval_criterion_label +sample_size ~ procedure_gen,
                      scales = "free_y")


ggplot(estimatesvar, 
       aes(x = rep, y = cummedian_test_error, col = learner_name))+
  geom_point()+
  geom_line()+
  theme_bw()+
  ggh4x::facet_nested(title_eval + eval_criterion_label +sample_size ~ procedure_gen_eval,
                      scales = "free_y")
ggsave("./03_results/plots/cummedian_testerror.pdf", width = 10, height = 5)

ggplot(estimatesvar, aes(x = rep, y = cummedian_bias, col = procedure_short))+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~procedure_short+ eval_criterion, scales = "free")
ggsave("./03_results/plots/cummedian_bias.pdf", width = 10, height = 5)

# Plot HPs -----------------------------------------------------------------------------------------
preprochps = colnames(resdf %>% select(starts_with("preproc."), contains("minbucket"), contains("cp")))
resresdf_hp = melt(resdf, measure.vars = preprochps, value.name = "hp_value", variable.name = "hp_type")
ggplot(resresdf_hp %>% filter(grepl("preproc.", hp_type)), 
       aes(x = hp_type, fill = hp_value))+
  geom_bar()+
  facet_wrap(~procedure_short+ eval_criterion)
# Plot errors --------------------------------------------------------------------------------------
ggplot(resdf, aes(y = test_error - eoi_value, x = procedure_short, col = procedure_short))+
  geom_boxplot()+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(x = "Procedure name", col = "", y = "Test error - reported error ")+
  facet_wrap(~eval_criterion, scales = "free_y")
ggsave("./03_results/plots/cart_bias.pdf", width = 10, height = 5)
ggplot(resdf, aes(y = test_error, x = procedure_short, col = procedure_short))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Procedure name", col = "", y = "Error on test data")+
  facet_wrap(~eval_criterion, scales = "free_y")
ggsave("./03_results/plots/cart_testerror.pdf", width = 10, height = 5)


resdf_Median = resdf %>% group_by(procedure_short, eval_criterion, eoi_type) %>%
  summarise(diff_value_Median = median(test_error-eoi_value),
            test_error_Median = median(test_error),
            eoi_value_Median = median(eoi_value))
ggplot(resdf_median, aes(y = eoi_value_median, x = test_error_median, col = procedure_short))+
  geom_point()+
  theme_bw()+
  facet_wrap(~eval_criterion, scales = "free")+
  geom_abline()

ggplot(resdf, aes(x = test_error, y = eoi_value, col = procedure_short))+
  geom_point()+
  facet_wrap(~eval_criterion, scales = "free")+
  geom_abline()+
  theme_bw()

test= melt(resdf %>% filter(eval_criterion=="regr.rmse"), measure.vars = c("test_error", "eoi_value"))
ggplot(test, aes(x = variable, y = value))+
  geom_boxplot()+
  facet_wrap(~procedure_short)
# Lübeck Plots -------------------------------------------------------------------------------------
df_test = resdf %>% select(-eoi_type,-eoi_value) %>%
  mutate(procedure_short_tree = case_when(
    procedure_short %in% c("p1a", "p1b", "p1c")~ "p1",
    procedure_short %in% c("p3a", "p3b", "p3c")~ "p3",
    .default = procedure_short
  )) %>%
  select(-procedure_short) %>%
  distinct() %>% 
  rename(error_value = test_error) %>%
  mutate(error_type = "test_error",
         procedure_short = NA)
df_reported = resdf %>% select(-test_error) %>%
  rename(error_type = eoi_type, error_value = eoi_value) %>%
  mutate(procedure_short_tree = case_when(
    procedure_short %in% c("p1a", "p1b", "p1c")~ "p1",
    procedure_short %in% c("p3a", "p3b", "p3c")~ "p3",
    .default = procedure_short
  ))
df_plot = rbind(df_test, df_reported) %>%
  relocate(error_type, .after = error_value) %>%
  mutate(error_type_bin = case_when(
    error_type == "test_error" ~ "Performance \ntest data",
    error_type != "test_error" ~ "Performance \nreported"),
    error_type = factor(error_type, levels = c("apparent_error",
                                                        "resampling_error",
                                                        "nested_resampling_error",
                                                        "test_error"),
                             labels = c("Apparent", "Resamp.", 
                                        "Nested",
                                        "Test"))
  ) %>%
  mutate(error_type_bin = factor(error_type_bin, 
                                 levels = c("Performance \nreported","Performance \ntest data") )) 

df_text = df_plot %>% filter(error_type_bin == "Performance \ntest data") %>% 
  group_by(error_type_bin, error_type, procedure_short_tree, eval_criterion) %>%
  summarise(median_test_error =median(error_value)) %>%
  mutate(median_test_error = ifelse(eval_criterion == "regr.rsq",
                                 round(median_test_error,3),
                                 round(median_test_error,1)))

# Plots
plot_fct("regr.rsq")
plot_fct("regr.rmse")
plot_fct = function(eval){
  size = 15
  sizel=0.7
  sizex = 14
  sizem = 5
  col = "seagreen"
  if(eval == "regr.rsq"){
    breakspar = seq(-0.05, 0.25,0.05)
    limitspar = c(-0.05, 0.25)
    ytext = 0.23
    ylab = TeX(r'(Performance measure: $R^2$)')
  } else{
    breakspar = seq(26,46,2)
    limitspar = c(26,46)
    ytext = 45
    ylab = TeX(r'(Performance measure: $RMSE$)')
  }
  # Only tree algorithm HPs
  p1 = ggplot(df_plot %>% filter(procedure_short_tree=="p1",
                                 eval_criterion == eval), 
              aes(x = error_type, y = error_value))+
    geom_boxplot()+
    theme_bw()+
    scale_y_continuous(limits = limitspar, breaks = breakspar)+
    facet_free(~error_type_bin, scales = "free_x", space = "free")+
    geom_text(data = df_text %>% filter(procedure_short_tree=="p1",
                                        eval_criterion == eval), y = ytext, 
              aes(label = paste0(median_test_error)), col =  col, size = sizem)+
    labs(x = "", y = ylab, title ="M1")+
    geom_hline(yintercept = median(df_plot %>% filter(procedure_short_tree=="p1",
                                                      eval_criterion == eval,
                                                      error_type == "Test") %>% .$error_value),
               linetype = "dashed", col = col, size = sizel)+
    theme(panel.spacing = unit(0, "lines"),
          text = element_text(size = size),
          axis.text.x = element_text(size = sizex))#+
  # geom_text(data = df_text,label = "Text")
  
  # All HPs
  p3 = ggplot(df_plot %>% filter(procedure_short_tree %in% c("p3"),
                                 eval_criterion == eval), 
              aes(x = error_type, y = error_value))+
    geom_boxplot()+
    theme_bw()+
    scale_y_continuous(limits = limitspar, breaks = breakspar)+
    facet_grid(~error_type_bin, scales = "free_x", space = "free")+
    labs(x = "", title ="M2")+
    geom_text(data = df_text %>% filter(procedure_short_tree=="p3",
                                        eval_criterion == eval), y = ytext, 
              aes(label = paste0(median_test_error)), col =  col, size = sizem)+
    geom_hline(yintercept = median(df_plot %>% filter(procedure_short_tree=="p3",
                                                      eval_criterion == eval,
                                                      error_type == "Test") %>% .$error_value),
               linetype = "dashed", col = col, size = sizel)+
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.spacing = unit(0, "lines"),
          text = element_text(size = size),
          axis.text.x = element_text(size = sizex))
  p2a = ggplot(df_plot %>% filter(procedure_short_tree %in% c("p2a"),
                                  eval_criterion == eval), 
               aes(x = error_type, y = error_value))+
    geom_boxplot()+
    theme_bw()+
    scale_y_continuous(limits = limitspar, breaks = breakspar)+
    facet_grid(~error_type_bin, scales = "free_x", space = "free")+
    geom_text(data = df_text %>% filter(procedure_short_tree=="p2a",
                                        eval_criterion == eval), y = ytext, 
              aes(label = paste0(median_test_error)), col =  col, size = sizem)+
    geom_hline(yintercept = median(df_plot %>% filter(procedure_short_tree=="p2a",
                                                      eval_criterion == eval,
                                                      error_type == "Test") %>% .$error_value),
               linetype = "dashed", col = col, size = sizel)+
    labs(x = "", title ="M3")+
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.spacing = unit(0, "lines"),
          text = element_text(size = size),
          axis.text.x = element_text(size = sizex))
  
  p2b = ggplot(df_plot %>% filter(procedure_short_tree %in% c("p2b"),
                                  eval_criterion == eval), 
               aes(x = error_type, y = error_value))+
    geom_boxplot()+
    theme_bw()+
    scale_y_continuous(limits = limitspar, breaks = breakspar)+
    facet_grid(~error_type_bin, scales = "free_x", space = "free")+
    labs(x = "", title ="M4")+
    geom_text(data = df_text %>% filter(procedure_short_tree=="p2b",
                                        eval_criterion == eval), y = ytext, 
              aes(label = paste0(median_test_error)), col =  col, size = sizem)+
    geom_hline(yintercept = median(df_plot %>% filter(procedure_short_tree=="p2b",
                                                      eval_criterion == eval,
                                                      error_type == "Test") %>% .$error_value),
               linetype = "dashed", col = col, size = sizel)+
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.spacing = unit(0, "lines"),
          text = element_text(size = size),
          axis.text.x = element_text(size = sizex))
  prsq = grid.arrange(p1, p3, p2a, p2b, nrow = 1,
                      widths = c(1,0.88,0.5,0.5))
  ggsave(plot = prsq, filename = paste0("./03_results/plots/lueb_plot_", eval,".pdf"),
         height = 4, width = 14)
  
  
}

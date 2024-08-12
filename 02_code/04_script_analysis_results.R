library(ggplot2)
library(cumstats)
library(dplyr)
library(gridExtra)
library(latex2exp)
library(reshape2)
library(ggh4x)
library(wesanderson)
# Preparations =====================================================================================

## Load, check and prepare results df --------------------------------------------------------------
load("./03_results/rdata/_resdf.RData")
stopifnot(length(unique(resdf$setting))==1) # we only consider one palliative care setting (sapv)

# Stop if results do not include all repetitions of all settings (ignoring split_type)
stopifnot(all.equal(
  prod(unname(unlist(resdf %>% 
                     select(procedure_gen_eval, learner_name,  sample_size, eval_criterion) %>% 
                     summarise_all(~ length(unique(.)))))) * 50,
  nrow(resdf %>% filter(split_type == "naive"))))


## Set plotting colors -----------------------------------------------------------------------------
col_errors = brewer.pal(4, "Set2")[c(1,4,3,2)]
#col_errors = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
##col_errors = c("#F0E442", "#0072B2","#CC79A7", "#009E73")
#display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
 #                 colorblindFriendly=TRUE)

## Change variable lables for plotting -------------------------------------------------------------
resdf = resdf %>% mutate(learner_name = factor(learner_name, levels = c("lrn_cart","lrn_reemtree_t","lrn_lmertree_t","lrn_ctree"),
                                       labels = c("CART", "REEMT", "LMMT", "CIT")),
                         procedure_gen_label = factor(procedure_gen_short, 
                                                      levels = c("featureless", "p0", "p4a","p1", "p2a", "p2b", "p3"),
                                               labels = c("Setting I (featureless)",
                                                          "Setting I",
                                                          "Setting II - M",
                                                          "Setting II - A1",
                                                          "Setting II - AM",
                                                          "Setting II - AM (supp)",
                                                          "Setting II - A2")),
                         procedure_eval = factor(procedure_eval, 
                                                 levels = c("apparent_error", "resampling_error",
                                                            "nested_resampling_error"),
                                                 labels = c("Apparent", "Resampling", "Nested resampling")),
                         sample_size = factor(sample_size,
                                              levels = c("sample25", "sample50"),
                                              labels = c("n = 362", "n = 724")),
                         # only for plot titles:
                         title_eval = "Performance measure",
                         title_gen = "Model generation",
                         eval_criterion_label = factor(eval_criterion, levels = c("regr.rmse", "regr.rsq"),
                                                       labels = c("RMSE", "R^2")))


## Check that AM1 is similar to A2 and exclude from data set ---------------------------------------
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

# Figures: Main results ============================================================================
# A) Compare test and reported errors: differences  ------------------------------------------------
stopifnot(length(unique(resdf$split_type))==1) # we only consider one split_type here

p = ggplot(resdf %>% filter(procedure_gen != "featureless" & learner_name %in% c("CART", "CIT")), 
           aes(y = eval_error-test_error, col = procedure_eval,
               x = learner_name))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_boxplot()+
  theme_bw()+
#  scale_x_discrete(labels = c("CART \nn=362", "CART \nn=724", "CIT \nn=362", "CIT \nn=724"),
                #  guide = guide_axis(n.dodge = 2)
       #         )+
  scale_color_manual(values = col_errors[1:3])+
  labs(x = "Learning algorithm", col = "Model evaluation", y = "PE(D) - PE(Dnew)")+
  ggh4x::facet_nested(title_eval + eval_criterion_label ~  title_gen + procedure_gen_label + sample_size, 
                      scales = "free",
                      labeller = labeller(eval_criterion_label = label_parsed),
                      solo_line = FALSE)+
  theme(legend.position = "top",
        text = element_text(size = 15),
        strip.text = element_text(size = 14),
        panel.spacing = unit(0,"line"))

 ggsave(p, file = "./03_results/plots/main_difft.png", height = 8, width =11.5)
 rm(p)


# B) Compare test and reported errors: absolute values  --------------------------------------------

# absolute performance Dtest
df_test = resdf %>% select(-procedure_gen_eval, -procedure_eval, -eval_error, -contains("hp_")) %>%
  distinct() %>% 
  rename(error_value = test_error) %>%
  mutate(error_type = "Dnew")
# absolute performance reported
df_eval = resdf %>% select(-test_error, -procedure_gen_eval, -contains("hp_")) %>%
  distinct() %>% 
  rename(error_value = eval_error,
         error_type = procedure_eval)
resdf_abs = bind_rows(df_test, df_eval)
resdf_abs = resdf_abs %>% mutate(error_type = factor(error_type,
                                                     levels = c("Apparent","Resampling",
                                                                "Nested resampling","Dnew")))
# absolute performance featureless
resdf_abs = full_join(resdf_abs,
              df_test %>% filter(procedure_gen=="featureless") %>% 
                group_by(eval_criterion, sample_size) %>% 
                summarise(median_featureless_test = median(error_value)),
              by = c("eval_criterion", "sample_size"))

p = ggplot(resdf_abs %>% filter(learner_name %in% c("CART", "CIT") &
                              procedure_gen != "featureless") , 
       aes(col = error_type, y = error_value, x = learner_name))+
  geom_boxplot()+
  geom_hline(aes(yintercept = median_featureless_test), linetype = "dotted")+
  ggh4x::facet_nested(title_eval + eval_criterion_label ~  title_gen + procedure_gen_label + 
                        sample_size, scales = "free_y",
                      labeller = labeller(eval_criterion_label = label_parsed))+
  theme_bw()+
  scale_color_manual(values = col_errors)+
  labs(x = "Learning algorithm", col = "Model evaluation", y = "PE")+
  theme(legend.position = "top",
        text = element_text(size = 15),
        strip.text = element_text(size = 14),
        panel.spacing = unit(0,"line"))
ggsave(p, file = "./03_results/plots/main_abs.png", height = 8, width =11.5)

rm(p, resdf_abs, df_test, df_eval)



# C) Check selected HPs -------------------------------------------------------------------------------

# Only consider generation procedures
resdf_hp = resdf %>% select(-procedure_gen_eval, -procedure_eval, -eval_error) %>%
  distinct() %>% 
  filter(!(procedure_gen_short %in% c("featureless", "p0")) &
           learner_name %in% c("CART", "CIT"))

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
                                levels = c("hp_preproc.drop.targetout.option",
                                           "hp_preproc.drop.iposca.option",
                                           "hp_preproc.feature.ipos.option", 
                                           "hp_preproc.feature.age.option", 
                                           "hp_preproc.feature.akps.option"),
                                labels = c("lambda[outlier]", "lambda[ca]", "lambda[ipos]",
                                           "lambda[age]", "lambda[akps]" )), title_hp = "HP") 




plot_preproc_fct = function(n, learner){
  p = ggplot(resdf_hp_preproc %>% filter(sample_size == n & learner_name == learner), 
             aes(x = hp_type_label, fill = hp_value))+
    geom_bar(stat = "count")+
    ggh4x::facet_nested(title_eval + eval_criterion_label ~ title_gen + procedure_gen_label ,
                        labeller = labeller(eval_criterion_label = label_parsed))+
    theme_bw()+
    scale_x_discrete(labels = c("lambda[outlier]" = expression(lambda[outlier]),
                                "lambda[ca]" = expression(lambda[ca]), 
                                "lambda[ipos]" = expression(lambda[ipos]),
                                "lambda[age]" = expression(lambda[age]), 
                                "lambda[akps]"  = expression(lambda[akps])))+
    scale_fill_manual(values = rev(wes_palette("GrandBudapest2", n = 4)))+
    labs(x = "HP ", fill = "Selected HP value", y = "Number of repetitions")+
    theme(legend.position = "top")
  return(p)
}

# Algorithm HPs
cp_lower = 0.001
cp_upper = 0.1
minbucket_lower = 5
minbucket_upper = 20
alpha_lower = 0.01 
alpha_upper = 0.1
minbucket_default = 7 
alpha_default = 0.05
cp_default = 0.01
resdf_hp_algo = melt(resdf_hp %>% filter(procedure_gen_short != "p4a") ,
                     measure.vars = colnames(resdf %>% select(hp_minbucket, hp_cp, hp_alpha)),
                     value.name = "hp_value", variable.name = "hp_type") %>% 
  filter(!is.na(hp_value))

plot_algo_fct = function(n, hp, learner){
  if(hp == "hp_cp"){
      hp_default = cp_default
      hp_lower = cp_lower
      hp_upper = cp_upper
  } else if(hp == "hp_alpha"){
    hp_default = alpha_default
    hp_lower = alpha_lower
    hp_upper = alpha_upper
  } else if(hp == "hp_minbucket"){
    hp_default = minbucket_default
    hp_lower = minbucket_lower
    hp_upper = minbucket_upper
  }
  p = ggplot(resdf_hp_algo %>% 
           filter(hp_type == hp & sample_size == n & learner_name == learner),
         aes(x = hp_type, y = hp_value))+
    geom_hline(yintercept = hp_default, linetype = "dotted")+
    geom_hline(yintercept = c(hp_lower, hp_upper))+
    geom_boxplot()+
    scale_x_discrete(labels = c("hp_cp" = expression(lambda[cp]),
                                "hp_alpha" = expression(lambda[alpha]),
                                "hp_minbucket" = expression(lambda[minbucket])))+
    ggh4x::facet_nested(title_eval + eval_criterion_label ~ title_gen + procedure_gen_label,
                        labeller = labeller(eval_criterion_label = label_parsed))+
    theme_bw()+
    labs(x = "HP ", y = "HP value")
  return(p)
}

grid.arrange(plot_preproc_fct("n = 724", "CART"),
             plot_algo_fct("n = 724", "hp_cp", "CART"),
             plot_algo_fct("n = 724", "hp_minbucket", "CART"),
             layout_matrix = rbind(c(1,1),
                                   c(2,3)))

plot_preproc_fct("n = 362", "CART")
plot_preproc_fct("n = 724", "CIT")
plot_preproc_fct("n = 362", "CIT")


plot_preproc_fct("n = 724", "hp_alpha", "CIT")
plot_preproc_fct("n = 724", "hp_minbucket", "CIT")
















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
# supplement with lmmt und reemt##----------------------------------
p = ggplot(resdf %>% filter(procedure_gen != "featureless"), 
           aes(y = eval_error-test_error, col = procedure_eval,
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
# p %+% subset(resdf, sample_size == "sample50")
# ggsave("03_results/plots/diff50.pdf")
# p %+% subset(resdf, sample_size == "sample25")
# ggsave("03_results/plots/diff25.pdf")

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

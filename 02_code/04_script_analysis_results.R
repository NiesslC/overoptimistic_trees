library(ggplot2)
library(cumstats)
library(dplyr)
library(gridExtra)
library(latex2exp)
library(reshape2)
library(ggh4x)
library(wesanderson)
library(RColorBrewer)
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
#settings = c(12,2,2,2) # main
 
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
                                                 labels = c("Apparent", "10-fold CV", "10-2-fold nested CV")),
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

plot_diff_fct = function(learner){
  p = ggplot(resdf %>% filter(procedure_gen != "featureless" & learner_name %in% learner), 
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
  return(p)
}
plot_diff_fct(c("CART", "CIT"))
ggsave(file = "./03_results/plots/a_diff.png", height = 8, width =11.5)
plot_diff_fct(c("REEMT", "LMMT")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(file = "./03_results/plots/a_diff_re.png", height = 8, width =11.5)

# B) Compare test and reported errors: absolute values  --------------------------------------------

# absolute performance Dnew
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
                                                     levels = c("Apparent", "10-fold CV",
                                                                "10-2-fold nested CV", "Dnew")))
# absolute performance featureless
resdf_abs = full_join(resdf_abs,
              df_test %>% filter(procedure_gen=="featureless") %>% 
                group_by(eval_criterion, sample_size) %>% 
                summarise(median_featureless_test = median(error_value)),
              by = c("eval_criterion", "sample_size"))

plot_abs_fct = function(learner){
  p = ggplot(resdf_abs %>% filter(learner_name %in% learner &
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
  
  
  return(p)
}
plot_abs_fct(learner = c("CART", "CIT"))
ggsave(file = "./03_results/plots/b_abs.png", height = 8, width =11.5)
plot_abs_fct(c("REEMT", "LMMT")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(file = "./03_results/plots/b_abs_re.png", height = 8, width =11.5)

rm(df_test, df_eval)

# C) Check selected HPs -------------------------------------------------------------------------------

# Only consider generation procedures
resdf_hp = resdf %>% select(-procedure_gen_eval, -procedure_eval, -eval_error) %>%
  distinct() %>% 
  filter(!(procedure_gen_short %in% c("featureless", "p0")) &
           learner_name %in% c("CART", "CIT"))

# Preprocessing HPs
resdf_hp_preproc = resdf_hp %>% filter(!(procedure_gen_short %in% c("p1"))) %>% 
  mutate_at(vars(starts_with('hp_')), ~ as.character(.))
resdf_hp_preproc = melt(as.data.table(resdf_hp_preproc), 
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
    scale_x_discrete(labels = c("lambda[outlier]" = TeX('$\\lambda_{\\,outlier}$'),
                                "lambda[ca]" = TeX('$\\lambda_{\\,ca}$'), 
                                "lambda[ipos]" = TeX('$\\lambda_{\\,ipos}$'),
                                "lambda[age]" = TeX('$\\lambda_{\\,age}$'), 
                                "lambda[akps]"  = TeX('$\\lambda_{\\,akps}$')))+
    scale_fill_manual(values = rev(wes_palette("GrandBudapest2", n = 4)))+
    labs(x = "HP ", fill = "Selected \nHP value", y = "Number of repetitions")+
    theme(legend.position = "right")
  return(p)
}

# Algorithm HPs
hps = list()
hps$cp_lower = 0.001
hps$cp_upper = 0.1
hps$cp_default = 0.01
hps$minbucket_lower = 5
hps$minbucket_upper = 20
hps$minbucket_default = 7 
hps$alpha_lower = 0.01 
hps$alpha_upper = 0.1
hps$alpha_default = 0.05

resdf_hp_algo = melt(as.data.table(resdf_hp) %>% 
                       select(-contains("preproc.")) %>% 
                       filter(procedure_gen_short != "p4a") %>%
                       mutate_at(vars(starts_with('hp_')), ~ as.numeric(.)),
                     measure.vars = colnames(resdf %>% select(hp_minbucket, hp_cp, hp_alpha)),
                     value.name = "hp_value", variable.name = "hp_type") %>% 
  filter(!is.na(hp_value))

plot_algo_fct = function(n, hp, learner){
  if(hp == "hp_cp"){
      hp_default = hps$cp_default
      hp_lower = hps$cp_lower
      hp_upper = hps$cp_upper
  } else if(hp == "hp_alpha"){
    hp_default = hps$alpha_default
    hp_lower = hps$alpha_lower
    hp_upper = hps$alpha_upper
  } else if(hp == "hp_minbucket"){
    hp_default = hps$minbucket_default
    hp_lower = hps$minbucket_lower
    hp_upper = hps$minbucket_upper
  }
  p = ggplot(resdf_hp_algo %>% 
           filter(hp_type == hp & sample_size == n & learner_name == learner),
         aes(x = hp_type, y = hp_value))+
    geom_hline(yintercept = hp_default, linetype = "dotted")+
    geom_hline(yintercept = c(hp_lower, hp_upper))+
    geom_boxplot()+
    scale_x_discrete(labels = c("hp_cp" = TeX('$\\lambda_{\\,cp}$'),
                                "hp_alpha" = TeX('$\\lambda_{\\,\\alpha}$'),
                                "hp_minbucket" = TeX('$\\lambda_{\\,minbucket}$')))+
    ggh4x::facet_nested(title_eval + eval_criterion_label ~ title_gen + procedure_gen_label,
                        labeller = labeller(eval_criterion_label = label_parsed))+
    theme_bw()+
    labs(x = "HP ", y = "HP value")
  return(p)
}

plot_hp_fct = function(n, learner){
  if(learner %in% c("CART", "REEMT")){
    hp = "hp_cp"
  } else{
    hp = "hp_alpha"
  }
  p = ggpubr::ggarrange(plot_preproc_fct(n = n, learner = learner),                               
                    ggpubr::ggarrange(plot_algo_fct(n = n, "hp_minbucket", learner = learner), 
                                      plot_algo_fct(n = n, hp = hp, learner = learner),
                                      ncol = 2, 
                                      labels = c("b", "c")), # Second row with box and dot plots
                    nrow = 2, 
                    labels = "a") 
  return(p)
}
  
plot_hp_fct("n = 724", "CART")
ggsave(file = "./03_results/plots/c_cart_nlarge.png", height = 8, width =9)
plot_hp_fct("n = 362", "CART")
ggsave(file = "./03_results/plots/c_cart_nsmall.png", height = 8, width =9)
plot_hp_fct("n = 724", "CIT")
ggsave(file = "./03_results/plots/c_cit_nlarge.png", height = 8, width =9)
plot_hp_fct("n = 362", "CIT")
ggsave(file = "./03_results/plots/c_cit_nsmall.png", height = 8, width =9)


rm(hps, resdf_hp, resdf_hp_algo, resdf_hp_preproc)


# Plot cummedian of estimates --------------------------------------------------------------------

# Differences
ggplot(resdf %>% 
         filter(procedure_gen != "featureless") %>%
         group_by(procedure_gen, procedure_eval, learner_name, eval_criterion, sample_size) %>%
         arrange(rep) %>%
         mutate(cummedian_error_diff = cummedian(eval_error-test_error)),
       aes(x = rep, y = cummedian_error_diff, col = procedure_eval))+
  geom_point()+
  geom_line()+
  theme_bw()+
  scale_color_manual(values = col_errors[1:3])+
  ggh4x::facet_nested(title_eval + eval_criterion_label + learner_name ~  title_gen + procedure_gen_label + sample_size, 
                      scales = "free",
                      labeller = labeller(eval_criterion_label = label_parsed),
                      solo_line = FALSE)+
  labs(col = "Prediction error \nestimation", x = "Repetition", y = "Prediction error difference median")

# Absolute values
resdf_cumm_abs = resdf_abs %>% filter(procedure_gen != "featureless")
#table(resdf_cumm_abs$error_type,resdf_cumm$procedure_gen_short)
ggplot(resdf_cumm_abs %>% group_by(procedure_gen, error_type, learner_name, eval_criterion, sample_size) %>%
         arrange(rep) %>%
         mutate(cummedian_error = cummedian(error_value)),
       aes(x = rep, y = cummedian_error, col = error_type))+
  geom_point()+
  geom_line()+
  theme_bw()+
  scale_color_manual(values = col_errors)+
  ggh4x::facet_nested(title_eval + eval_criterion_label + learner_name ~  title_gen + procedure_gen_label + sample_size, 
                      scales = "free",
                      labeller = labeller(eval_criterion_label = label_parsed),
                      solo_line = FALSE)+
  labs(col = "Prediction error \nestimation", x = "Repetition", y = "Prediction error estimates median")



# supplement with lmmt und reemt##----------------------------------





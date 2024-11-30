library(ggplot2)
library(cumstats)
library(dplyr)
library(gridExtra)
library(latex2exp)
library(tidyr)
library(reshape2)
library(ggh4x)
library(ggtext)
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

## Set plotting colors -----------------------------------------------------------------------------
col_errors = brewer.pal(4, "Dark2")[c(2,1,3,4)]
col_hps = brewer.pal(6, "Set2")[c(3:6)]
#col_errors = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#col_errors = c("#F0E442", "#0072B2","#CC79A7", "#009E73")
#display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,colorblindFriendly=TRUE)

## Calculate prediction error difference -----------------------------------------------------------
# = such that positive = optimistic and negative = pessimistic
resdf = resdf %>% mutate(errordiff = case_when(
  eval_criterion == "regr.rmse" ~ test_error-eval_error,
  eval_criterion == "regr.rsq" ~ eval_error-test_error,
))

## Change variable lables for plotting -------------------------------------------------------------
resdf = resdf %>% mutate(learner_name = factor(learner_name, levels = c("lrn_cart","lrn_reemtree_t","lrn_ctree", "lrn_lmertree_t"),
                                       labels = c("CART", "REEMT", "CIT", "LMMT")),
                         procedure_gen_label = factor(procedure_gen_short, 
                                                      levels = c("featureless", "p0", "p4a","p1", "p2a", "p2b", "p3"),
                                               labels = c("I-featureless",
                                                          "I-no tuning",
                                                          "II-manual-P",
                                                          "II-automated-A",
                                                          "II-combined-PA",
                                                          "II-combined-PA(supp)",
                                                          "II-automated-PA")),
                         procedure_eval = factor(procedure_eval, 
                                                 levels = c("apparent_error", "resampling_error",
                                                            "nested_resampling_error"),
                                                 labels = c("Apparent error", "10-fold CV error", 
                                                            "10-2-fold nested CV error")),
                         sample_size = factor(sample_size,
                                              levels = c("sample25", "sample50"),
                                          #    labels = c("n[train] == 362", "n[train] == 724")),
                                              labels = c("362", "724")),
                         # only for plot titles:
                         title_eval = "Performance measure",
                         title_gen = "Model generation",
                         eval_criterion_label = factor(eval_criterion, levels = c("regr.rmse","regr.rsq"),
                                                       labels = c("RMSE","R^2")))


## Check that AM1 is similar to A2 and exclude from data set ---------------------------------------
ggplot(resdf %>% filter(procedure_gen_short %in% c("p3", "p2b") & split_type == "naive"), 
       aes(y = errordiff, col = procedure_eval,
                  x = procedure_gen_short))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_boxplot()+
  # facet_grid(eval_criterion ~ procedure_gen_short, scales = "free_y")+
  theme_bw()+
  labs(x = "Learning algorithm", col = "Model evaluation", y = "Prediction error difference")+
  ggh4x::facet_nested(title_eval + eval_criterion_label + sample_size ~  learner_name, scales = "free_y",
                      labeller = labeller(eval_criterion_label = label_parsed))+
  theme(legend.position = "top")
# -> p2b and p3 very similar -> exclude
resdf = resdf %>% filter(procedure_gen_short != "p2b")

## Split in naive vs. teams splitting (naive is main analysis) -------------------------------------
resdf_teams = resdf %>% filter(split_type == "teams")
resdf = resdf %>% filter(split_type == "naive")

# Figures: Main results ============================================================================
# A) Compare test and reported errors: differences (Figure 5, S8, S9 in manuscript) ----------------
stopifnot(length(unique(resdf$split_type))==1) # we only consider one split_type here

plot_diff_fct = function(plot_data, learner){
  p = ggplot(plot_data %>% filter(procedure_gen != "featureless" & learner_name %in% learner), 
           aes(y = errordiff, col = procedure_eval,
               x = learner_name))+
    geom_hline(yintercept = 0, linetype = "dotted")+
    geom_boxplot()+
    theme_bw()+
    scale_color_manual(values = col_errors[1:3])+
    labs(x = "Learning algorithm", col = "Model evaluation", 
         y = "Prediction error difference")+
    ggh4x::facet_nested(title_eval + eval_criterion_label ~  title_gen + procedure_gen_label + sample_size, 
                        scales = "free",
                        labeller = labeller(eval_criterion_label = label_parsed,
                                           # title_gen = label_parsed,
                                            sample_size = function(labels) {
                                              paste0("<span style='font-size:13pt'>n<sub>train</sub>  = ", 
                                                     labels, "</span>")
                                            }),
                        solo_line = FALSE)+
    theme(legend.position = "top",
          text = element_text(size = 16),
          strip.text = element_text(size = 16),
          strip.background = element_rect(fill="grey90"),
          panel.spacing = unit(0,"line"),
          strip.text.x = element_markdown())

  return(p)
}
# Main setup
plot_diff_fct(plot_data = resdf, learner = c("CART", "CIT")) 
#ggsave(file = "./03_results/plots/a_naive.png", height = 8, width =11.5)
ggsave(file = "./03_results/plots/a_naive.eps", height = 8, width =11.5, device="eps")
# Compare naive vs random effects
p_reemt = plot_diff_fct(plot_data = resdf, learner = c("CART", "REEMT"))+
  theme(axis.text.x.bottom = element_text(angle = 90, vjust = 0.5, hjust=1))
p_lmmt = plot_diff_fct(plot_data = resdf, learner = c("CIT", "LMMT")) +
  theme(axis.text.x.bottom = element_text(angle = 90, vjust = 0.5, hjust=1))
p = ggpubr::ggarrange(p_reemt, p_lmmt, ncol=1,
                      #heights = c(6,3,3,2.15), 
                      labels = c("a","b"), 
                      font.label = list(size = 15))
#ggsave(file = "./03_results/plots/a_naive_vs_re.png", height = 16, width =11.5)
ggsave(file = "./03_results/plots/a_naive_vs_re.eps", height = 16, width =11.5, device="eps")

# Compare naive vs team splits
lims = resdf_teams %>% filter(procedure_gen_short == "p0") %>%
  group_by(eval_criterion) %>%
  summarise(maxdiff = max(errordiff),
            mindiff = min(errordiff))
maxdiff_rmse = lims %>% filter(eval_criterion == "regr.rmse") %>% .$maxdiff  
mindiff_rmse = lims %>% filter(eval_criterion == "regr.rmse") %>% .$mindiff  
maxdiff_rsq = lims %>% filter(eval_criterion == "regr.rsq") %>% .$maxdiff  
mindiff_rsq = lims %>% filter(eval_criterion == "regr.rsq") %>% .$mindiff  

p_naive = plot_diff_fct(plot_data = resdf %>% filter(procedure_gen_short == "p0"), learner = c("CART", "CIT"))+ 
  facetted_pos_scales(
    y = list(eval_criterion_label == "RMSE" ~ scale_y_continuous(limits = c(mindiff_rmse,maxdiff_rmse)),
             eval_criterion_label == "R^2" ~ scale_y_continuous(limits = c(mindiff_rsq,maxdiff_rsq*1.2))))+
  guides(col=guide_legend(nrow=2,title = "Model evaluation     "))

p_teams = plot_diff_fct(plot_data = resdf_teams %>% filter(procedure_gen_short == "p0"), learner = c("CART", "CIT"))+ 
  facetted_pos_scales(
    y = list(eval_criterion_label == "RMSE" ~ scale_y_continuous(limits = c(mindiff_rmse,maxdiff_rmse)),
             eval_criterion_label == "R^2" ~ scale_y_continuous(limits = c(mindiff_rsq,maxdiff_rsq*1.2))))+
  guides(col=guide_legend(nrow=2,title = "Model evaluation     "))
p = ggpubr::ggarrange(p_naive, p_teams, ncol=2,
                      #heights = c(6,3,3,2.15), 
                      labels = c("a","b"), 
                      font.label = list(size = 15))
#ggsave(file = "./03_results/plots/a_naive_vs_teams.png", height = 8, width =11.5)
ggsave(file = "./03_results/plots/a_naive_vs_teams.eps", height = 8, width =11.5, device="eps")


# B) Compare test and reported errors: absolute values (Figure S2 in manuscript) -------------------

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
                                                     levels = c("Apparent error", "10-fold CV error",
                                                                "10-2-fold nested CV error", "Dnew")))
# absolute performance featureless
resdf_abs = full_join(resdf_abs,
              df_test %>% filter(procedure_gen=="featureless") %>% 
                group_by(eval_criterion, sample_size) %>% 
                summarise(median_featureless_test = median(error_value)),
              by = c("eval_criterion", "sample_size"))

# for teams
df_test_teams = resdf_teams %>% select(-procedure_gen_eval, -procedure_eval, -eval_error, -contains("hp_")) %>%
  distinct() %>% 
  rename(error_value = test_error) %>%
  mutate(error_type = "Dnew")
df_eval_teams = resdf_teams %>% select(-test_error, -procedure_gen_eval, -contains("hp_")) %>%
  distinct() %>% 
  rename(error_value = eval_error,
         error_type = procedure_eval)
resdf_abs_teams = bind_rows(df_test_teams, df_eval_teams)
resdf_abs_teams = resdf_abs_teams %>% mutate(error_type = factor(error_type,
                                                     levels = c("Apparent error", "10-fold CV error",
                                                                "10-2-fold nested CV error", "Dnew")))
resdf_abs_teams = full_join(resdf_abs_teams,
                      df_test_teams %>% filter(procedure_gen=="featureless") %>% 
                        group_by(eval_criterion, sample_size) %>% 
                        summarise(median_featureless_test = median(error_value)),
                      by = c("eval_criterion", "sample_size"))

plot_abs_fct = function(plot_data, learner){
  p = ggplot(plot_data %>% filter(learner_name %in% learner &
                                    procedure_gen != "featureless") , 
             aes(col = error_type, y = error_value, x = learner_name))+
    geom_boxplot()+
    geom_hline(aes(yintercept = median_featureless_test), linetype = "dotted")+
    ggh4x::facet_nested(title_eval + eval_criterion_label ~  title_gen + procedure_gen_label + 
                          sample_size, scales = "free_y",
                        labeller = labeller(eval_criterion_label = label_parsed,
                                            sample_size = function(labels) {
                                              paste0("<span style='font-size:13pt'>n<sub>train</sub>  = ", 
                                                     labels, "</span>")}))+
    theme_bw()+
    scale_color_manual(values = col_errors, labels = c(expression(widehat(PE)[train] ~": Apparent error"),
                                                       expression(widehat(PE)[train] ~": 10-fold CV error"),
                                                       expression(widehat(PE)[train] ~": 10-2-fold nested CV error"),
                                                       expression(widehat(PE)[new])))+
    labs(x = "Learning algorithm", col = "Model evaluation", 
         y = "Prediction error estimate")+
         #y = expression("Prediction error estimate"~ widehat(PE)[train]~~ "or"~~ widehat(PE)[new]))+
    theme(legend.position = "top",
          text = element_text(size = 16),
          strip.text = element_text(size = 16),
          strip.background = element_rect(fill="grey90"),
          panel.spacing = unit(0,"line"),
          strip.text.x = element_markdown())
  if(any(learner %in% c("REEMT", "LMMT"))){
    p = p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  
  return(p)
}
plot_abs_fct(plot_data = resdf_abs, learner = c("CART", "CIT"))
#ggsave(file = "./03_results/plots/b_naive.png", height = 8, width =11.5)
ggsave(file = "./03_results/plots/b_naive.eps", height = 8, width =11.5, device="eps")

# plot_abs_fct(plot_data = resdf_abs, learner = c("REEMT", "LMMT")) 
# ggsave(file = "./03_results/plots/b_naive_re.png", height = 8, width =11.5)
# plot_abs_fct(plot_data = resdf_abs_teams, learner = c("CART", "REEMT", "CIT", "LMMT")) 
# ggsave(file = "./03_results/plots/b_teams_all.png", height = 8, width =11.5)
rm(df_test, df_eval, df_test_teams, df_eval_teams)



# C) Check selected HPs (Figures S3-S6 in manuscript)-----------------------------------------------

# Only consider generation procedures
resdf_hp = resdf %>% select(-procedure_gen_eval, -procedure_eval, -eval_error, -errordiff) %>%
  distinct() %>% 
  filter(!(procedure_gen_short %in% c("featureless", "p0")) &
           learner_name %in% c("CART", "CIT"))

# Preprocessing HPs
resdf_hp_preproc = resdf_hp %>% filter(!(procedure_gen_short %in% c("p1"))) %>% 
  mutate_at(vars(starts_with('hp_')), ~ as.character(.))
resdf_hp_preproc = resdf_hp_preproc %>%
  pivot_longer(cols = starts_with("hp_"),
               names_to = "hp_type",
               values_to = "hp_value") %>%
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
    scale_fill_manual(values = col_hps)+
    labs(x = "HP ", fill = "Selected \nHP value", y = "Number of repetitions")+
    theme(legend.position = "right",
          strip.background = element_rect(fill="grey90"),
          axis.text.x = element_text(size = textsize))#, vjust = 0.5, margin = margin(t = 5)))
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

resdf_hp_algo = resdf_hp %>%
  select(-contains("preproc.")) %>% 
  filter(procedure_gen_short != "p4a") %>%
  mutate_at(vars(starts_with('hp_')), ~ as.numeric(.)) %>%
  pivot_longer(cols = c("hp_minbucket", "hp_cp", "hp_alpha"),
               names_to = "hp_type",
               values_to = "hp_value") %>%
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
    labs(x = "HP ", y = "HP value")+
    theme(axis.text.x = element_text(size = textsize),#, vjust = 0.5, margin = margin(t = 5)),
          strip.background = element_rect(fill="grey90"))
  return(p)
}

plot_hp_fct = function(n, learner){
  if(learner %in% c("CART", "REEMT")){
    hp = "hp_cp"
  } else{
    hp = "hp_alpha"
  }
  p = ggpubr::ggarrange(plot_preproc_fct(n = n, learner = learner), ####+ labs(title = paste(learner,n)),                               
                    ggpubr::ggarrange(plot_algo_fct(n = n, "hp_minbucket", learner = learner), 
                                      plot_algo_fct(n = n, hp = hp, learner = learner),
                                      ncol = 2, 
                                      labels = c("b", "c")), # Second row with box and dot plots
                    nrow = 2, 
                    labels = "a") 
  return(p)
}
  
textsize = 12
plot_hp_fct("724", "CART")
ggsave(file = "./03_results/plots/c_cart_nlarge.eps", height = 8, width =9, device="eps")
plot_hp_fct("362", "CART")
ggsave(file = "./03_results/plots/c_cart_nsmall.eps", height = 8, width =9, device="eps")
plot_hp_fct("724", "CIT")
ggsave(file = "./03_results/plots/c_cit_nlarge.eps", height = 8, width =9, device="eps")
plot_hp_fct("362", "CIT")
ggsave(file = "./03_results/plots/c_cit_nsmall.eps", height = 8, width =9, device="eps")

rm(hps, resdf_hp, resdf_hp_algo, resdf_hp_preproc)








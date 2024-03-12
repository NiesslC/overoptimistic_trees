library(ggplot2)
library(cumstats)
library(dplyr)
library(gridExtra)
library(latex2exp)
# Load results df ----------------------------------------------------------------------------------
load("./03_results/rdata/_resdf.RData")

# Plot cummean of estimates --------------------------------------------------------------------
estimatesvar= resdf %>% group_by(procedure_short, eval_criterion) %>%
  arrange(rep) %>%
  mutate(cummean_test_error = cummean(test_error), 
         cummean_bias = cummean(test_error-eoi_value), .after=test_error)
ggplot(estimatesvar %>% filter(eval_criterion=="regr.rmse"), 
       aes(x = rep, y = cummean_test_error, col = procedure_short))+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~procedure_short)
ggplot(estimatesvar %>% filter(eval_criterion=="regr.rsq"), 
       aes(x = rep, y = cummean_test_error, col = procedure_short))+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~procedure_short)
ggsave("./03_results/plots/cummean_testerror.pdf", width = 10, height = 5)

ggplot(estimatesvar, aes(x = rep, y = cummean_bias, col = procedure_short))+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~procedure_short+ eval_criterion, scales = "free")
ggsave("./03_results/plots/cummean_bias.pdf", width = 10, height = 5)

# Plot HPs -----------------------------------------------------------------------------------------
preprochps = colnames(resdf %>% select(starts_with("preproc."), contains("minbucket"), contains("cp")))
resdf_hp = melt(resdf, measure.vars = preprochps, value.name = "hp_value", variable.name = "hp_type")
ggplot(resdf_hp %>% filter(grepl("preproc.", hp_type)), 
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


resdf_Median = \nresdf %>% group_by(procedure_short, eval_criterion, eoi_type) %>%
  summarise(diff_value_Median = \nmedian(test_error-eoi_value),
            test_error_Median = \nmedian(test_error),
            eoi_value_Median = \nmedian(eoi_value))
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

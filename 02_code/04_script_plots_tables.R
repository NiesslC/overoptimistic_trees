library(ggplot2)
library(cumstats)
library(dplyr)
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


# Plot errors --------------------------------------------------------------------------------------
ggplot(resdf, aes(y = test_error - eoi_value, x = procedure_short, col = procedure_short))+
  geom_boxplot()+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted")+
  labs(x = "Procedure name", col = "", y = "Test error - reported error ")+
  facet_wrap(~eval_criterion, scales = "free_y")
ggsave("./03_results/plots/cart_bias.pdf", width = 8, height = 5)
ggplot(resdf, aes(y = test_error, x = procedure_short, col = procedure_short))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Procedure name", col = "", y = "Error on test data")+
  facet_wrap(~eval_criterion, scales = "free_y")
ggsave("./03_results/plots/cart_testerror.pdf", width = 8, height = 5)


resdf_median = resdf %>% group_by(procedure_short, eval_criterion, eoi_type) %>%
  summarise(diff_value_median = median(test_error-eoi_value),
            test_error_median = median(test_error),
            eoi_value_median = median(eoi_value))
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

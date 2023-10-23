# to do preprocessing
################################################################
# Prepare data sets for trees    ###############################
################################################################
library(tidyverse)
library(visdat)
library(reshape2)
library(writexl)
library(gridExtra)
library(tidyverse); theme_set(theme_bw())
#setwd("S:/cmmgrp/Companion/Code")
setwd("~/@Companion/Code")

load("../Data/data_final/data_all.RData") # prepare data for train *AND* test

# 1. Dependent variable ----------------------------------------------------------------------------
# add phase_tage_all and phase_tage_kontakt
# - phase_tage_all indicates the number of days (as integers) between first and last contact
# - phase_tage_kontakt indicates the number of days with contacts
data_tree = data %>% group_by(id) %>%
  arrange(datumzeit) %>% 
  mutate(grp = cumsum(palliativphase != lag(palliativphase, def =
                                              first(palliativphase)))) %>%
  # grp = k means this is the k'th phase of the patient 
  group_by(id, grp) %>%
  mutate(phase_tage_all = 1 + as.numeric(difftime(max(datum), min(datum), units = "days")),
         phase_tage_kontakt = length(unique(datum))) %>%
  # (datum instead of datumzeit because we only want to consider the date)
  relocate(grp, palliativphase, phase_tage_all, phase_tage_kontakt, .after = datumzeit) %>% 
  ungroup() %>%
  arrange(id, datumzeit)

# add variable "kost_per_diem_all" and "kost_per_diem_kontakt"
# which indicate the per diem costs (on phase level), using phase_tage_all/kontakt as denominator
data_tree = data_tree %>% group_by(id, grp) %>%
  mutate(kost_per_diem_all = sum(kost_sum)/phase_tage_all) %>%
  ungroup() %>%
  relocate(kost_per_diem_all, .after = phase_tage_all)
# Exclude _kontakt variables
data_tree  = data_tree %>% select(-contains("kontakt"))

# START CORRECTION  ----
data_tree_train = data_tree %>% filter(allocation == "train")

# Overview costs by day and phase 
data_day = data_tree %>% group_by(id, grp, datum, setting, team_name, palliativphase, kost_per_diem_all) %>%
  summarise(kost_day = sum(kost_sum)) %>% ungroup() %>%
  group_by(id, grp, setting, palliativphase) %>%
  mutate(day_nr = 1 + as.numeric(difftime(datum, min(datum), units = "days")),
         firstday = ifelse(day_nr == 1, "1. Tag", "> 1. Tag"),
         firstphase = ifelse(grp == 0, "1. Phase", "> 1. Phase")) %>% ungroup()
#View(data_day %>% group_by(setting, palliativphase, team_name, firstday, firstphase) %>% filter(firstday == "1. Tag") %>% 
#     count()) #-> not enough data
#View(data_day %>% group_by(setting, team_name, firstday, firstphase) %>% filter(firstday == "1. Tag") %>% count())
p_base =  ggplot(data_day,aes(x = paste0(palliativphase, "\n", firstday), y = kost_day, 
                              col = firstphase == "1. Phase" & firstday == "1. Tag"))  +
  geom_boxplot(varwidth = TRUE)+
  scale_x_discrete(limits = rev, guide=guide_axis(n.dodge=2))+
  facet_wrap(~setting +  fct_rev(firstphase), ncol  = 2, scales = "free_x") +
  guides(col = "none")+
  labs(x = "", y = "Summe der Kosten des \n
       jeweiligen Tags (€)")+
  geom_vline(xintercept = c(2,4,6)+ 0.5, linetype = "dashed")

p = arrangeGrob(p_base %+% subset(data_day, setting == "Palliativstation"),
                p_base %+% subset(data_day, setting == "Palliativdienst"),
                p_base %+% subset(data_day, setting == "SAPV"),
                ncol = 1)
ggsave(p, file = "../Plots/correction_problem_palliativphase.png", width = 11, height = 10, device = "png")
p_base =  ggplot(data_day,aes(x = firstday, y = kost_day, 
                              col = firstphase == "1. Phase" & firstday == "1. Tag"))  +
  geom_boxplot(varwidth = TRUE)+
  scale_x_discrete(limits = rev)+
  
  guides(col = "none")+
  labs(x = "", y = "Summe der Kosten des jeweiligen Tags (€)")

p = arrangeGrob(p_base %+% list(subset(data_day, setting == "Palliativstation"),  
                                facet_wrap(~setting +  fct_rev(firstphase), ncol  = 2, scales = "free_x")),
                p_base %+% list(subset(data_day, setting == "Palliativdienst"),  
                                facet_wrap(~setting +  fct_rev(firstphase), ncol  = 2, scales = "free_x")),
                p_base %+% list(subset(data_day, setting == "SAPV"),  
                                facet_wrap(~setting +  fct_rev(firstphase), ncol  = 2, scales = "free_x")),
                ncol = 3)
ggsave(p, file = "../Plots/correction_problem_all.png", width = 11, height = 6, device = "png")


p = ggplot(data_day %>% group_by(grp, setting, id) %>% summarise(max= max(day_nr)),
           aes(x = factor(grp+1), y = max))+
  geom_boxplot()+
  facet_wrap(~setting)+
  labs(x = "Wie vielte Phase?", y = "Dauer der Phase (Tage)")
ggsave(p, file = "../Plots/correction_daysperphase.png", width = 11, height = 6, device = "png")

rm(data_day)

# Correction by first day of next phases (ONLY USE TRAIN HERE)----

# get data only from the first days
data_first = data_tree_train  %>%
  group_by(id, grp, setting, team_name) %>% filter(datum == min(datum)) %>%
  summarise(kost_day = sum(kost_sum)) %>% ungroup()  %>% 
  mutate(firstphase = ifelse(grp == 0, "1. Phase", "> 1. Phase"))
any(data_first %>% group_by(id, grp) %>% count() %>% .$n > 1) # should be FALSE; check that only one value per phase (=first day)

rm(data_tree_train)

p_base = ggplot(data_first, aes(col = fct_rev(firstphase), y = kost_day, x = team_name ))+
  geom_boxplot()+
  labs(title = "Erster Tag 1. Phase vs. erster Tag > 1. Phase", y= "Summe der Kosten des jeweiligen Tags (€)", x = "",
       col = "Phase")+
  scale_x_discrete(guide=guide_axis(n.dodge=2))+
  scale_color_manual(values = c("#00BFC4", "#F8766D"))
p = arrangeGrob(p_base %+%  list(subset(data_first, setting == "Palliativstation"),  labs(subtitle = "Palliativstation")),
                p_base %+%  list(subset(data_first, setting == "Palliativdienst"), labs(subtitle = "Palliativdienst")),
                p_base %+%  list(subset(data_first, setting == "SAPV"), labs(subtitle = "SAPV")),
                ncol = 1)
ggsave(p, file = "../Plots/correction_firstday_a.png", width = 8, height = 12, device = "png")


data_first = data_first %>% mutate(firstphase = ifelse(grp == 0, "first", "secondorlater"))
data_first = data_first %>% group_by(setting, team_name, firstphase) %>%
  summarise(mean_kost_day = mean(kost_day),
            median_kost_day = median(kost_day)) %>% ungroup()
data_first_mean = data_first %>% select(-contains("median")) %>% spread(firstphase, mean_kost_day)
data_first_median = data_first %>% select(-contains("mean")) %>% spread(firstphase, median_kost_day)

p = arrangeGrob(ggplot(data_first_mean, aes(y  = first, x = secondorlater,col = setting))+
                  geom_point()+
                  geom_abline()+
                  labs(title = "Median",y = "Kosten erster Tag  1. Phase ",
                       x = "Kosten erster Tag > 1. Phase ", col = "Setting"),
                ggplot(data_first_median, aes(y  = first, x = secondorlater,col = setting))+
                  geom_point()+
                  geom_abline()+
                  labs(title = "Mean", 
                       y = "Kosten erster Tag  1. Phase ", x = "Kosten erster Tag > 1. Phase ", col = "Setting"))
ggsave(p, file = "../Plots/correction_firstday_b.png", width = 6, height = 5, device = "png")

data_first_mean = data_first_mean %>% mutate(correction_factor_mean = ifelse(secondorlater/first < 1,
                                                                             secondorlater/first,1)) %>%
  select(-first, -secondorlater)
data_first_median = data_first_median %>% mutate(correction_factor_median = ifelse(secondorlater/first < 1,
                                                                                   secondorlater/first,1)) %>%
  select(-first, -secondorlater)

rm(data_first)
# Correction on FULL data set ----
ggplot(full_join(data_first_mean, data_first_median, by = c("setting", "team_name")),
       aes(x = correction_factor_mean, y = correction_factor_median, label = team_name))+
  geom_text(hjust = 1, size = 3)+
  geom_point()+
  geom_abline()
data_tree = full_join(data_tree, data_first_mean, by = c("setting", "team_name"))
data_tree = full_join(data_tree, data_first_median, by = c("setting", "team_name"))
rm(data_first_mean, data_first_median)
data_tree = data_tree %>% group_by(id) %>% mutate(kost_sum_correction_mean = case_when(
  grp == 0 & datum == min(datum) ~ kost_sum * correction_factor_mean,
  !(grp == 0 & datum == min(datum)) ~ kost_sum), .after = kost_sum) %>%
  mutate(kost_sum_correction_median = case_when(
    grp == 0 & datum == min(datum) ~ kost_sum * correction_factor_median,
    !(grp == 0 & datum == min(datum)) ~ kost_sum), .after = kost_sum) %>%
  group_by(id, grp) %>% 
  mutate(kost_per_diem_all_factor_mean = sum(kost_sum_correction_mean)/phase_tage_all, .after = kost_per_diem_all) %>%
  mutate(kost_per_diem_all_factor_median = sum(kost_sum_correction_median)/phase_tage_all, .after = kost_per_diem_all) %>%
  ungroup()

# Compare options ----

# a. Compare costs per phase ----
data_compare = melt(data_tree %>%  
                      mutate(firstphase = ifelse(grp == 0, "1. Phase", "> 1. Phase")) %>% 
                      group_by(id, setting, grp) %>%
                      distinct(kost_per_diem_all,.keep_all = TRUE),
                    measure.vars = c("kost_per_diem_all","kost_per_diem_all_factor_mean", 
                                     "kost_per_diem_all_factor_median"))
p = ggplot(data_compare,
           aes(col = variable, y= value, x = fct_rev(firstphase)))+
  geom_boxplot()+
  facet_wrap(~setting, scales = "free_y")+
  labs(y = "Durschnittliche Kosten pro Tag pro Phase (€)", x = "", col = "Korrekturmethode")+
  scale_color_manual(labels = c("Keine Korrektur", "Korretur durch \nTeam-spez. Faktor (MEAN)",
                                "Korretur durch \nTeam-spez. Faktor (MEDIAN)"),
                     values=c("#009E73", "#CC79A7", "#0072B2"))

ggsave(p, file = "../Plots/correction_compare_phase.png", width = 10, height = 6, device = "png")

# b. Compare costs per day ----
data_compare_day = data_tree %>% group_by(id, grp, setting, team_name, datum) %>% 
  mutate(kost_day = sum(kost_sum),
         kost_day_factor_mean = sum(kost_sum_correction_mean),
         kost_day_factor_median = sum(kost_sum_correction_median)) %>%
  distinct(kost_day, kost_day_factor_mean, kost_day_factor_median) %>%
  ungroup() %>%
  group_by(id, grp, setting, team_name)  %>%
  mutate(firstphase = ifelse(grp == 0, "1. Phase", "> 1. Phase"),
         firstday = ifelse(datum == min(datum), "1. Tag", "> 1. Tag")) %>% ungroup()

# check if mean/median costs in groups are the same! (or if test+train is considered: similar)
data_compare_day %>% group_by(id, grp, setting, team_name) %>% filter(datum == min(datum)) %>%
  ungroup() %>%
  group_by(firstphase, team_name) %>% 
  summarise(mean  = mean(kost_day),
            mean2 = mean(kost_day_factor_mean),
            median  = median(kost_day),
            median2 = median(kost_day_factor_median)) %>% arrange(team_name)


data_compare_day = melt(data_compare_day, 
                        measure.vars = c("kost_day", "kost_day_factor_mean", "kost_day_factor_median"))

p_base = ggplot(data_compare_day,
                aes(col = variable, y= value, x = fct_rev(firstday)))+
  geom_boxplot()+
  labs(y = "Summe der Kosten des jeweiligen Tags (€)", x = "", col = "Korrekturmethode")+
  scale_color_manual(labels =  c("Keine Korrektur", "Korretur durch \nTeam-spez. Faktor (MEAN)",
                                 "Korretur durch \nTeam-spez. Faktor (MEDIAN)"),
                     values=c("#009E73", "#CC79A7", "#0072B2"))
p = arrangeGrob(p_base %+% list(subset(data_compare_day, setting == "Palliativstation"),  
                                facet_wrap(~setting + fct_rev(firstphase)),
                                guides(col = "none")),
                p_base %+% list(subset(data_compare_day, setting == "Palliativdienst"),  
                                facet_wrap(~setting + fct_rev(firstphase)),
                                guides(col = "none")),
                p_base %+% list(subset(data_compare_day, setting == "SAPV"),  
                                facet_wrap(~setting + fct_rev(firstphase))),
                ncol = 3, widths = c(1,1,1.5))  

ggsave(p, file = "../Plots/correction_compare_day.png", width = 12, height = 6, device = "png")

rm(data_compare, data_compare_day, p, p_base)
# Rename cost variables ---- 
data_tree = data_tree %>% select(-kost_per_diem_all_factor_median) %>%
  dplyr::rename(kost_per_diem_all_orig = kost_per_diem_all) %>%
  dplyr::rename(kost_per_diem_all = kost_per_diem_all_factor_mean)

# END CORRECTION ----

# REMOVE AND FLAG OUTLIERS ----

# Permanently remove one outlier with kost_per_diem_all > 1200
data_tree = data_tree %>% filter(kost_per_diem_all < 1200)

# Only indicate (not remove) outliers with highest 1% of costs (based on training data)

perc99 = data_tree %>% filter(allocation == "train") %>% 
  group_by(id, grp) %>% distinct(kost_per_diem_all, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by(setting) %>%
  summarise(perc99 = quantile(kost_per_diem_all, 0.99))
data_tree = full_join(data_tree, perc99, by = "setting")
data_tree = data_tree %>% 
  mutate(outlier_flag = ifelse(kost_per_diem_all > perc99, "outlier", "no outlier"))
rm(perc99)
# 2. Get values on first day of phase --------------------------------------------------------------
# relevant variables:
vars_vary = colnames(data %>% select(contains("IPOS"), contains("Barthel"), contains("kognitiv"), AKPS,
                                     contains("PCPSS")))

# After preparing the data, it should be a dataframe with nr of rows:
nrow(data_tree %>% distinct(grp, id)) 

# Consider every variable in vars_vary separately because we (sometimes) have to filter different 
# timepoints on the first day (depending on the variable)
# -> first separate and in the end merge the datasets
base = data_tree %>% group_by(id, grp) %>% filter(datum == min(datum)) %>% ungroup() %>%
  select(-all_of(vars_vary)) %>%
  # only select relevant variables
  select(team_name, Team_ID, allocation, datum, setting, id, grp, palliativphase, alter,
         geschlecht, onkologisch, diagnose, phase_tage_all, #phase_tage_kontakt, 
         kost_per_diem_all, outlier_flag) %>%
  # these variables are time-constant -> keep only one value per id and grp
  group_by(id, grp) %>%
  distinct()

missings = c("fehlend", "nicht beurteilbar")

for(i in 1:length(vars_vary)){
  sep = data_tree %>% group_by(id, grp) %>% filter(datum == min(datum)) %>% 
    select(id, grp, !!rlang::sym(vars_vary[i])) %>% 
    # only keep unique values occured on first day:
    distinct(!!rlang::sym(vars_vary[i]), .keep_all = TRUE) %>% 
    # only keep fehlend/nicht beurteilbar if it is the only value measured on the first day:
    filter(!!rlang::sym(vars_vary[i]) %in% setdiff(levels(!!rlang::sym(vars_vary[i])), missings) |
             (!any(!!rlang::sym(vars_vary[i]) %in% setdiff(levels(!!rlang::sym(vars_vary[i])), missings)) &
                !!rlang::sym(vars_vary[i]) %in% missings)) %>% ungroup() 
  
  # if more than one value on first day (e.g.: regensburg_station_id61 for IPOS schmerzen),
  # use min value for (Barthel & AKPS) and max value for all other variables:
  if(grepl("AKPS", vars_vary[i]) | grepl("Barthel", vars_vary[i])){
    sep = sep %>% group_by(id, grp) %>% 
      filter(as.integer(!!rlang::sym(vars_vary[i])) == min(as.integer(!!rlang::sym(vars_vary[i])))) %>%
      ungroup()
  } else{
    sep = sep %>% group_by(id, grp) %>% 
      filter(as.integer(!!rlang::sym(vars_vary[i])) == max(as.integer(!!rlang::sym(vars_vary[i])))) %>%
      ungroup()
  }
  
  # merge with base
  base = merge(base, sep, by = c("id", "grp"))
}

## ACHTUNG: Zeilenzahl von base entspricht nicht nrow(data_tree %>% distinct(grp, id)) 
# einige ids kommen (mit derselben Phase) doppelt vor!
######## Beginn Fehler Analyse
nrow(base)
nrow(data_tree %>% distinct(grp, id))
base$idgrp = paste0(base$id, "_", base$grp)
sep$idgrp = paste0(sep$id, "_", sep$grp)
which(!base$idgrp %in% sep$idgrp) # keine zusätzlichen ids, sondern Dopplungen
dopp = names(which(table(base$idgrp) > 1))
# 6 idgrp doppelt: 100388_1  10811_0  10814_0  10816_0  10818_0  10834_0
which(colSums(base %>% filter(idgrp %in% dopp) %>% group_by(idgrp) %>% 
                summarise_all(n_distinct) %>% select(-idgrp))!=length(dopp))
rm(dopp)
base = base %>% select(-idgrp)
######## Ende Fehler Analyse


data_tree = base

rm(base, sep, i, missings, vars_vary)



# 3. Remove phases if a) ALL 17 IPOS variables fehlend/nicht beurteilbar or b) any fehlend ---------
# nicht beurteilbar + fehlend
data_tree$sum_missing_notassess = data_tree %>% select(starts_with("IPOS")) %>% mutate(na_if(., "fehlend")) %>% 
  mutate(na_if(., "nicht beurteilbar")) %>% is.na %>% rowSums
# only fehlend
data_tree$sum_missing = data_tree %>% select(starts_with("IPOS")) %>% mutate(na_if(., "fehlend")) %>% 
  is.na %>% rowSums


## remove observations with all 17 fehlend/nicht beurteilbar ----
# How much observations do we lose depending on where we set the cutoff?
#table(data_tree$sum_missing_notassess)
# Plots:
# ggplot(data_tree %>% filter(setting == "SAPV" & allocation  == "train"),
#        aes(col = factor(sum_missing_notassess), y = kost_per_diem_all, x = factor(stark_iposvars_all)))+
#   geom_boxplot()+
#   theme_bw()
#1:17 %>%map_dbl(function(x) 100*round(1-(nrow(data_tree %>% filter(sum_missing_notassess < x))/nrow(data_tree)),3))
cutoff = 17 # also tried 7 and 8 once
data_tree = data_tree %>% filter(sum_missing_notassess < cutoff)
rm(cutoff)

## remove observations with any fehlend in IPOS ----
data_tree = data_tree %>% filter(sum_missing == 0 )


# 6. "kognitiv" variables: remove "fehlend" and set "nicht beurteilbar" to "nicht vorhanden" -------

data_tree = data_tree %>%
  filter((kognitiv_unruhe != "fehlend") & (kognitiv_verwirrtheit != "fehlend")) %>%
  mutate_at(vars(contains("kognitiv")), 
            ~recode(. ,"nicht beurteilbar" = "nicht vorhanden" )) %>%
  droplevels()


# 9. Remove missing values for AKPS and specify categories-----------------------------------------
data_tree = data_tree %>%
  mutate_at(vars(matches("AKPS")), ~na_if(., "nicht beurteilbar")) %>%
  mutate_at(vars(matches("AKPS")), ~na_if(., "fehlend")) 
data_tree = data_tree %>% drop_na(AKPS) %>% droplevels()
unique(data_tree$AKPS)


# 11. Ordinal variables ----------------------------------------------------------------------------
vars_vary = colnames(data_tree %>% select(#IPOS_schmerzen, IPOS_atemnot, 
  contains("kognitiv")))

data_tree = data_tree %>%  mutate(across(all_of(c(vars_vary)), ~ factor(., ordered = TRUE)))

rm(vars_vary)

# 13. Save datasets and predictors -----------------------------------------------------------------

# remove phases (id+grp) that appear more than once due to different ICD values 
data_tree = data_tree %>% group_by(id, grp)  %>% distinct() %>% ungroup() #filter(id == "100388")
any(data_tree %>% group_by(id, grp) %>% count() %>% .$n >1) # should be FALSE

# Predictors and important variables ---- 
predictors_all =  colnames(data_tree %>% select(alter, palliativphase, contains("IPOS"),
                                                # Barthel_score,
                                                contains("kognitiv"), contains("AKPS"),
                                                contains("PCPSS")))
#for comparison with old trees:
predictors_old = c("alter", "palliativphase",  "kognitiv_verwirrtheit",  "kognitiv_unruhe",
                   "IPOS_schmerzen","IPOS_atemnot", "wrong_stark_iposvars_wo_sa"  , "wrong_maessig_iposvars_wo_sa",
                   #"Barthel_score", 
                   "AKPS_orig")#, "onkologisch", "geschlecht")

data_tree = data_tree[,c("id", "team_name", "grp", "allocation", "kost_per_diem_all", "setting",
                         "outlier_flag", "geschlecht",
                         predictors_all, setdiff(predictors_old,predictors_all))]

# for sensitivity analysis: add cost per episode
data_tree = left_join(data_tree, data_episode, by = "id")
data_tree = data_tree %>% relocate(kost_diem_episode, .after = kost_per_diem_all) %>% arrange(setting,id,grp)

# correct variable format for id, team name
data_tree$id = factor(data_tree$id)
data_tree$team_name = factor(data_tree$team_name) 
data_tree = data_tree %>% droplevels()

# Save different data sets i) train/test and ii) station/sapv/pmd ----

data_train_sapv = data_tree %>% filter(setting == "SAPV" & allocation == "train") %>% 
  droplevels()
data_train_pmd = data_tree %>% filter(setting == "Palliativdienst"& allocation == "train") %>% 
  # select(-Barthel_score) %>% 
  droplevels()
data_train_station = data_tree %>% filter(setting == "Palliativstation"& allocation == "train")%>% 
  droplevels()

data_test_sapv = data_tree %>% filter(setting == "SAPV" & allocation == "test") %>% 
  droplevels()
data_test_pmd = data_tree %>% filter(setting == "Palliativdienst"& allocation == "test") %>% 
  # select(-Barthel_score) %>% 
  droplevels()
data_test_station = data_tree %>% filter(setting == "Palliativstation"& allocation == "test")%>% 
  droplevels()

save(data_train_sapv, predictors_list_sapv,  predictors_old_sapv, file = "../Data/data_final/data_train_sapv.RData")
save(data_train_pmd, predictors_list_pmd, predictors_old_pmd, file = "../Data/data_final/data_train_pmd.RData")
save(data_train_station, predictors_list_station, predictors_old_station, file = "../Data/data_final/data_train_station.RData")

save(data_test_sapv, predictors_list_sapv,  predictors_old_sapv, file = "../Data/data_final/data_test_sapv.RData")
save(data_test_pmd, predictors_list_pmd, predictors_old_pmd, file = "../Data/data_final/data_test_pmd.RData")
save(data_test_station, predictors_list_station, predictors_old_station, file = "../Data/data_final/data_test_station.RData")





library(dplyr)
library(data.table)
library(Hmisc)
library(tidyr)
library(table1)
#library(knitr)
library(kableExtra)
library(reshape2)
library(forcats)
library(RColorBrewer)
library(gridExtra)
library(purrr)
library(ggpubr)

# Load data and functions --------------------------------------------------------------------------
load("./01_data/data_phaselevel.RData")
source("./02_code/_fcts_preproc.R")
data_phaselevel = data_phaselevel %>% filter(setting == "sapv")

# Add feature labels -------------------------------------------------------------------------------
levels(data_phaselevel$setting) <- c("Station", "PMD", "SAPV")

label(data_phaselevel$palliativephase) <- "Palliative care phase"
label(data_phaselevel$age) <- "Age"
units(data_phaselevel$age) <- "years"
label(data_phaselevel$ipos_pain) <- "IPOS pain: how burdened is the patient by pain?"
label(data_phaselevel$ipos_shortness_breath) <- "IPOS shortness of breath: how burdened 
the patient was by shortness of breath"
label(data_phaselevel$ipos_weakness) <- "IPOS weakness: how burdened the patient was 
by weakness or lack of energy"
label(data_phaselevel$ipos_nausea) <- "IPOS nausea: how burdened the patient was by nausea"
label(data_phaselevel$ipos_vomiting) <- "IPOS vomiting: how burdened the patient was by vomiting"
label(data_phaselevel$ipos_poor_appetite) <- "IPOS poor appetite: how burdened the patient 
was by poor appetite"
label(data_phaselevel$ipos_constipation) <- "IPOS constipation: how burdened the patient 
was by constipation"
label(data_phaselevel$ipos_sore_dry_mouth) <- "IPOS sore or dry mouth: how burdened the 
patient was by a sore or dry mouth"
label(data_phaselevel$ipos_drowsiness) <- "IPOS drowsiness: how burdened the patient
was by drowsiness"
label(data_phaselevel$ipos_poor_mobility) <- "IPOS poor mobility: how burdened the patient 
was by poor mobility"
label(data_phaselevel$ipos_patient_anxiety) <- "IPOS patient anxiety: did the patient have anxiety?"
label(data_phaselevel$ipos_family_anxiety) <- "IPOS family anxiety: did the patients family have anxiety?"
label(data_phaselevel$ipos_depression) <- "IPOS depression: did the patient have depression?"
label(data_phaselevel$ipos_peace) <- "IPOS peace: was the patient feeling at peace with himself?"
label(data_phaselevel$ipos_sharing_feelings) <- "IPOS sharing feelings: was the patient 
able to share his feelings?"
label(data_phaselevel$ipos_information) <- "IPOS information: did the patient receive 
as much information as wanted?"
label(data_phaselevel$ipos_practical_matters) <- "IPOS practical matters: have practical matters been tackled?"
label(data_phaselevel$cogn_confusion) <- "Confusion"
label(data_phaselevel$cogn_agitation) <- "Agitation"
data_phaselevel = data_phaselevel %>% mutate(akps = factor(akps, levels = c("10","20","30","40","50","60","70","80","90"),
                                         labels = c("(10) comatose or barely rousable",
                                                           "(20) totally bedfast and requiring extensive nursing care \nby professionals and/or family",
                                                           "(30) almost completely bedfast",
                                                           "(40) in bed more than 50% of the time",
                                                           "(50) considerable assistance and frequent medical care required",
                                                           "(60) able to care for most needs; but requires occasional assistance",
                                                           "(70) cares for self; unable to carry on normal activity or \nto do active work",
                                                           "(80) normal activity with effort; some signs or symptoms of disease",
                                                           "(90) able to carry on normal activity; minor sign of symptoms \nof disease")))
label(data_phaselevel$akps) <- "AKPS"
# Preprocess outcome and IPOS ----------------------------------------------------------------------

# IPOS: total sum score
ipos_score = preprocess_feature_ipos_fct(data = data_phaselevel, option = "A") %>% 
  select(companion_id, grp, ipos_score)
data_phaselevel = full_join(data_phaselevel, ipos_score, by = c("companion_id","grp"))
label(data_phaselevel$ipos_score) <- "IPOS total score"


# Outcome: correction factor
correction_factor = preprocess_target_getcorr_fct(data = data_phaselevel, 
                                                  option = "A")
data_phaselevel = preprocess_target_fct(data = data_phaselevel,
                                        option= "A", 
                                        correction_factor = correction_factor)

label(data_phaselevel$targetvar) <- "Average cost per day per palliative care phase"
units(data_phaselevel$targetvar) <- "€"


# Descriptives data set (Table S1 in manuscript) ---------------------------------------------------

## Table level: palliative care phase
t1 <- table1(~ targetvar + palliativephase + age + cogn_confusion +
               cogn_agitation + akps  + ipos_score, data = data_phaselevel)

# Table with latex code:
t1kable(t1)
save_kable(t1kable(t1, format = "latex"), file = "./03_results/tex/companion_phase.tex")

# Clustering (Figure S7 in manuscript) -------------------------------------------------------------

# Figure: Episode level
data_episodelevel = data_phaselevel %>%
  group_by(companion_id) %>%
  dplyr::select(companion_id, team_id, setting) %>%
  mutate(number_phases = n()) %>%
  distinct() 
p_episode = ggplot(data_episodelevel, aes(x = number_phases))+
  geom_bar(fill = "grey55")+
  theme_bw()+
  coord_flip()+
  labs(x = "Number of phases per episode", y =  "Number of episodes (705 in total)" )+
  scale_x_continuous(breaks = 1:max(data_episodelevel$number_phases))

# Figure: Team level
data_teamlevel = data_phaselevel %>% 
    group_by(team_id) %>%
    dplyr::select(team_id, setting) %>%
    mutate(number_patients = n()) %>%
    distinct() 
data_teamlevel = data_teamlevel %>% 
  mutate(team_id = factor(team_id, 
                          levels =  unlist(data_teamlevel[order(-data_teamlevel$number_patients),"team_id"])))

p_team = ggplot(data_teamlevel, aes(y = number_patients, x = team_id))+
  geom_bar(stat = "identity", fill = "grey55")+
  labs(x = "Team (9 in total)", y = "Number of episodes per team")+
  theme_bw()+
  scale_x_discrete(labels = LETTERS[1:9])

p = ggpubr::ggarrange(p_episode, p_team,ncol=1,
                      #heights = c(6,3,3,2.15), 
                      labels = c("a","b"), 
                      font.label = list(size = 14))
ggsave(p, file = "./03_results/plots/cluster.eps", width = 6, height = 8, device="eps")


rm(data_teamlevel, data_episodelevel)
# IPOS cannot assess (Table S2 in manuscript) -----------------------------------------------------

iposdat_ca = data_phaselevel %>% select(contains("ipos"),-ipos_score) %>% 
  mutate(sum_ca = rowSums(. == "cannot assess"))
iposdat_ca$targetvar = data_phaselevel$targetvar
iposdat_ca = 1:17 %>% map(function(x) 
  ifelse(iposdat_ca$sum_ca >= x, NA, iposdat_ca$targetvar))
iposdat_ca = as.data.frame(do.call(cbind,iposdat_ca))
#iposdat_ca = cbind(iposdat_ca, data_phaselevel) %>% select(targetvar, starts_with("V"))

label(iposdat_ca$V1) <- "lambdaca  0"
label(iposdat_ca$V11) <- "lambdaca  10"
label(iposdat_ca$V13) <- "lambdaca  12"
label(iposdat_ca$V15) <- "lambdaca  14"
label(iposdat_ca$V17) <- "lambdaca  16"

my.render.cont <- function(x) {
  with(stats.default(x), 
       c("","Mean (SD)" = sprintf("%s (%s)",round_pad(MEAN, 2),
                                  round_pad(SD, 2)),"Median (Min, Max)" = sprintf("%s (%s, %s)",
                                       round_pad(MEDIAN, 2), 
                                       round_pad(MIN, 2), 
                                       round_pad(MAX, 2)))
  )
}

tca = table1(~V1+V11+V13+V15+V17, data = iposdat_ca,render.continuous = my.render.cont)
tca
save_kable(t1kable(tca, format = "latex"), file = "./03_results/tex/companion_ca.tex")
rm(iposdat_ca)

# IPOS individual features (Figure S1 in manuscript) -----------------------------------------------

# Plot individual features
group1 = paste0("ipos_",c("pain",
           "shortness_breath","weakness","nausea","vomiting",
           "poor_appetite","constipation","sore_dry_mouth",
           "drowsiness", "poor_mobility"))
group2 = c("ipos_patient_anxiety", "ipos_family_anxiety", "ipos_depression")
group3 = c("ipos_peace",
           "ipos_sharing_feelings",
           "ipos_information")
group4 = "ipos_practical_matters"

iposdat1 = melt(data_phaselevel, measure.vars = colnames(data_phaselevel %>% select(any_of(group1))))
iposdat2 = melt(data_phaselevel, measure.vars = colnames(data_phaselevel %>% select(any_of(group2)))) 
iposdat3 = melt(data_phaselevel, measure.vars = colnames(data_phaselevel %>% select(any_of(group3)))) 
iposdat4 = melt(data_phaselevel, measure.vars = colnames(data_phaselevel %>% select(any_of(group4))))
iposdat1 = iposdat1 %>% mutate(group = "group1",
                             value = factor(value, levels = rev(c("not at all","slightly","moderately",
                                                                  "severely","overwhelmingly","cannot assess")),
                                            labels = rev(c("not at all","slightly","moderately",
                                                           "severely","overwhelmingly","cannot \nassess"))),
                             variable = factor(variable, 
                                               levels = c("ipos_pain","ipos_shortness_breath","ipos_weakness","ipos_nausea",
                                                          "ipos_vomiting","ipos_poor_appetite",
                                                          "ipos_constipation","ipos_sore_dry_mouth","ipos_drowsiness",
                                                          "ipos_poor_mobility"),
                                               labels = c("IPOS: Pain",
                                                          "IPOS: Shortness of breath",
                                                          "IPOS: Weakness or \nlack of energy",
                                                          "IPOS: Nausea",
                                                          "IPOS: Vomiting",
                                                          "IPOS: Poor appetite",
                                                          "IPOS: Constipation",
                                                          "IPOS: Sore or dry mouth",
                                                          "IPOS: Drowsiness",
                                                          "IPOS: Poor mobility")))
iposdat2 = iposdat2 %>% mutate(group = "group2",
                               value = factor(value, levels = rev(c("not at all","occasionally","sometimes",
                                                                    "most of the time","always","cannot assess")),
                                              labels = rev(c("not at all","occasionally","sometimes",
                                                             "most of \nthe time","always","cannot \nassess"))),
                               variable = factor(variable, 
                                                 levels = c("ipos_patient_anxiety","ipos_family_anxiety","ipos_depression" ),
                                                 labels = c("IPOS: Patient anxiety",
                                                            "IPOS: Family anxiety",
                                                            "IPOS: Depression")))
iposdat3 = iposdat3 %>% mutate(group = "group3",
                               value = factor(value, levels = rev(c("always","most of the time","sometimes",
                                                                    "occasionally","not at all","cannot assess")),
                                              labels = rev(c("always","most of \nthe time","sometimes",
                                                             "occasionally","not at all","cannot \nassess"))),
                               variable = factor(variable, 
                                                 levels = c("ipos_peace","ipos_sharing_feelings","ipos_information" ),
                                                 labels = c("IPOS: Feeling at peace",
                                                            "IPOS: Sharing feelings",
                                                            "IPOS: Information")))
iposdat4 = iposdat4 %>% mutate(group = "group4",
                               value = factor(value, levels = rev(c( "addressed or no problem", "mostly addressed" , 
                                                                     "partly addressed","hardly addressed",      
                                                                     "not addressed","cannot assess")),
                                              labels = rev(c( "addressed or \nno problem", "mostly \naddressed" , 
                                                              "partly \naddressed","hardly \naddressed",      
                                                              "not \naddressed","cannot \nassess"))),
                               variable = factor(variable, 
                                                 levels = c("ipos_practical_matters" ),
                                                 labels = c("IPOS: Practical matters")))


cols = c("grey",brewer.pal(5, "RdYlGn")) #'Purples'
plotiposca_fct = function(iposdat){
  p = ggplot(iposdat, aes(x = variable, fill = value))+
    geom_bar(stat = "count", position = "fill")+
    scale_fill_manual(values = cols)+
    theme_bw()+
    coord_flip()+
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(fill = "", x = "", y = "Proportion")+
    guides(fill = guide_legend(reverse=T,nrow = 1))+
    theme(legend.position = "top",
          text = element_text(size = 13),
          legend.text=element_text(size=10))
  return(p)
}
p1 = plotiposca_fct(iposdat1)
p2 = plotiposca_fct(iposdat2)
p3 = plotiposca_fct(iposdat3)
p4 = plotiposca_fct(iposdat4)
p = ggpubr::ggarrange(p1,p2,p3,p4,ncol=1,
          heights = c(6,3,3,2.15), labels = c("a","b", "c", "d"), font.label = list(size = 15))
ggsave(p, file = "./03_results/plots/ipos_indivdual.eps", width = 9, height = 10, device="eps")
rm(p1, p2, p3, p4, group1, group2, group3, group4,
   iposdat1, iposdat2, iposdat3, iposdat4)


























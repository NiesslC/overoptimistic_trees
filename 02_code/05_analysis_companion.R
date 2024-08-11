library(dplyr)
library(data.table)
library(Hmisc)
library(tidyr)
library(table1)
library(knitr)
library(kableExtra)
library(reshape2)
library(forcats)
library(RColorBrewer)
library(gridExtra)

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
label(data_phaselevel$cogn_confusion) <- "Cognitive confusion"
label(data_phaselevel$cogn_agitation) <- "Cognitive agitation"
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


# Table: Descriptives dataset (default preprocessing) ----------------------------------------------

## Table level: palliative care phase
t1 <- table1(~ targetvar + palliativephase + age + cogn_confusion +
               cogn_agitation + akps  + ipos_score, data = data_phaselevel)

# Table with latex code:
t1kable(t1)
save_kable(t1kable(t1, format = "latex"), file = "./03_results/tex/companion_phase.tex")


## Table level: episode of care
data_patientlevel = data_phaselevel %>% 
  group_by(companion_id) %>%
  dplyr::select(companion_id, team_id, setting) %>%
  mutate(number_phases = n()) %>%
  distinct()

# levels(data_patientlevel$setting) <- c("Station", "PMD", "SAPV")
label(data_patientlevel$number_phases) <- "Number of phases per patient"

t2 <- table1(~ number_phases, 
             data = data_patientlevel)
t1kable(t2)
t1kable(t2, format = "latex")

## Table level: team
data_teamlevel = data_phaselevel %>% 
  group_by(team_id) %>%
  dplyr::select(team_id, setting) %>%
  mutate(number_patients = n()) %>%
  distinct()

label(data_teamlevel$number_patients) <- "Number of patients per team"
t3 <- table1(~ number_patients, 
             data = data_teamlevel)
t1kable(t3)
t1kable(t3, format = "latex")

# Figures: IPOS individual features ----------------------------------------------------------------

iposdat_ca = data_phaselevel %>% select(contains("ipos"),-ipos_score) %>% 
  mutate(sum_ca = rowSums(. == "cannot assess")) %>%
  count(sum_ca) %>%    # Count occurrences of each category
  mutate(proportion = n / sum(n)) 
ggplot(iposdat_ca, aes(x = "", y = proportion, fill = factor(sum_ca))) +
  geom_bar(stat = "identity", width = 1)+
  theme_bw()+
  coord_flip()+
  labs(fill = "Number of missing IPOS values", y = "Proportion", x = "")

ggplot(Ancestry, aes(x = row,y = Proportion, fill = Race)) +
  geom_bar(stat="identity")

rm(iposdat_ca)
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
                             value = factor(value, levels = rev(levels(data_phaselevel$ipos_pain))),
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
                               value = factor(value, levels = rev(levels(data_phaselevel$ipos_patient_anxiety))),
                               variable = factor(variable, 
                                                 levels = c("ipos_patient_anxiety","ipos_family_anxiety","ipos_depression" ),
                                                 labels = c("IPOS: Patient anxiety",
                                                            "IPOS: Family anxiety",
                                                            "IPOS: Depression")))
iposdat3 = iposdat3 %>% mutate(group = "group3",
                               value = factor(value, levels = rev(levels(data_phaselevel$ipos_peace))),
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
                                                              "not \naddressed","cannot assess"))),
                               variable = factor(variable, 
                                                 levels = c("ipos_practical_matters" ),
                                                 labels = c("IPOS: Practical matters")))

pos = "top"
gnrow = 1
cols = c("grey",brewer.pal(5, "RdYlGn")) #'Purples'
p1 = ggplot(iposdat1, aes(x = variable, fill = value))+
   geom_bar(stat = "count", position = "fill")+
   scale_fill_manual(values = cols)+
   theme_bw()+
  coord_flip()+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
 # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(fill = "", x = "", y = "Proportion")+
  guides(fill = guide_legend(reverse=T,nrow = gnrow))+
  theme(legend.position = pos)
p2 = ggplot(iposdat2, aes(x = variable, fill = value))+
  geom_bar(stat = "count", position = "fill")+
  scale_fill_manual(values = cols)+
  theme_bw()+
  coord_flip()+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(fill = "", x = "", y = "Proportion")+
  guides(fill = guide_legend(reverse=T,nrow = gnrow))+
  theme(legend.position = pos)
p3 = ggplot(iposdat3, aes(x = variable, fill = value))+
  geom_bar(stat = "count", position = "fill")+
  scale_fill_manual(values = cols)+
  theme_bw()+
  coord_flip()+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(fill = "", x = "", y = "Proportion")+
  guides(fill = guide_legend(reverse=T,nrow = gnrow))+
  theme(legend.position = pos)
p4 = ggplot(iposdat4, aes(x = variable, fill = value))+
  geom_bar(stat = "count", position = "fill")+
  scale_fill_manual(values = cols)+
  theme_bw()+
  coord_flip()+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(fill = "", x = "", y = "Proportion")+
  guides(fill = guide_legend(reverse=T,nrow = gnrow))+
  theme(legend.position = pos)

# p=grid.arrange(grobs=list(p1,p2,p3,p4),layout_matrix = rbind(c(1,1,1),
#                                                            c(2,3,4)),
#                widths = c(2, 2, 1),ncol =3)
p=grid.arrange(grobs=list(p1,p2,p3,p4),ncol=1,
               heights = c(7,3,3,2))
ggsave(p, file = "./03_results/plots/ipos.png", width = 220, 
       height = 297, units = "mm")








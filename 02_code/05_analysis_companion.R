library(dplyr)
library(data.table)
library(Hmisc)
library(tidyr)
library(table1)
library(knitr)
library(kableExtra)


load("./01_data/data_phaselevel.RData")
source("./02_code/_fcts_preproc.R")
data_phaselevel = data_phaselevel %>% filter(setting == "sapv")

# Table level: palliative care phase

## Outcome
correction_factor = preprocess_target_getcorr_fct(data = data_phaselevel, 
                                                  option = "A")
data_phaselevel = preprocess_target_fct(data = data_phaselevel,
                      option= "A", 
                      correction_factor = correction_factor)

label(data_phaselevel$targetvar) <- "Average cost per day per palliative care phase"


## Features
levels(data_phaselevel$setting) <- c("Station", "PMD", "SAPV")

label(data_phaselevel$palliativephase) <- "Palliativephase"
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
label(data_phaselevel$akps) <- "AKPS"
levels(data_phaselevel$akps) <- c("0 = dead",
                                  "10 = comatose or barely rousable",
                                  "20 = totally bedfast and requiring extensive
                            \nnursing care by professionals and/or family",
                                  "30 = almost completely bedfast",
                                  "40 = in bed more than 50% of the time",
                                  "50 = considerable assistance and \nfrequent medical care required",
                                  "60 = able to care for most needs; but \\newline \nrequires occasional assistance",
                                  "70 = cares for self; unable to carry on \nnormal activity or to do active work",
                                  "80 = normal activity with effort; some \nsigns or symptoms of disease",
                                  "90 = able to carry on normal activity; \nminor sign of symptoms of disease",
                                  "100 = normal; no complaints; \nno evidence of disease",
                                  "-77 = missing",
                                  "-99 = cannot assess")



# label(data_phaselevel$sum_cost_day_1) <- "Sum cost per day"
# units(data_phaselevel$sum_cost_day_1) <- "€"
# label(data_phaselevel$sum_cost_day_geq2) <- "Sum cost per day geq2"
# units(data_phaselevel$sum_cost_day_geq2) <- "€"
# label(data_phaselevel$sum_cost_exclsys_day_1) <- "Sum cost per day exclsys"
# units(data_phaselevel$sum_cost_exclsys_day_1) <- "€"
# label(data_phaselevel$sum_cost_exclsys_day_geq2) <- "Sum cost per day exclsys geq2"
# units(data_phaselevel$sum_cost_exclsys_day_geq2) <- "€"
# label(data_phaselevel$sum_minutes_day_1) <- "Sum minutes per day"
# units(data_phaselevel$sum_minutes_day_1) <- "min"
# label(data_phaselevel$sum_minutes_day_geq2) <- "Sum minutes per day geq2"
# units(data_phaselevel$sum_minutes_day_geq2) <- "min"


t1 <- table1(~ #targetvar + palliativephase + age + ipos_pain + ipos_shortness_breath +
               #ipos_weakness + ipos_nausea + ipos_vomiting + ipos_poor_appetite +
               #ipos_constipation + ipos_sore_dry_mouth + ipos_drowsiness +
               #ipos_poor_mobility + ipos_patient_anxiety + ipos_family_anxiety +
               #ipos_depression + ipos_peace + ipos_sharing_feelings +
               #ipos_information + ipos_practical_matters + cogn_confusion +
               cogn_agitation + akps 
             |setting, data = data_phaselevel, overall = F)

# Table with latex code:
render.strat.latex <- function(x, ...) gsub("\\\\newline", "\\\\\\\\", x)
t1kable(t1)
save_kable(t1kable(t1, format = "latex", render.strat = render.strat.latex), file = "./03_results/tex/companion_phase.tex")


## Second Table 
data_patientlevel = data_phaselevel %>% 
  group_by(companion_id) %>%
  dplyr::select(companion_id, team_id, setting) %>%
  mutate(number_phases = n()) %>%
  distinct()
# data_phaselevel = auf Phasenebene; data_patientlevel = auf Patientenebene

levels(data_patientlevel$setting) <- c("Station", "PMD", "SAPV")
data_patientlevel$team_id <- as.factor(data_patientlevel$team_id)
label(data_patientlevel$number_phases) <- "Number of phases per patient"
label(data_patientlevel$team_id) <- "Team ID"

t2 <- table1(~ number_phases + team_id | setting, 
             data = data_patientlevel, overall = F)
t1kable(t2)
t1kable(t2, format = "latex")



# Team Ebene
data_teamlevel = data_phaselevel %>% 
  group_by(team_id) %>%
  dplyr::select(team_id, setting) %>%
  mutate(number_patients = n()) %>%
  distinct()

levels(data_teamlevel$setting) <- c("Station", "PMD", "SAPV")
label(data_teamlevel$number_patients) <- "Number of patients per team"


t3 <- table1(~ number_patients | setting, 
             data = data_teamlevel, overall = F)
t1kable(t3)
t1kable(t3, format = "latex")

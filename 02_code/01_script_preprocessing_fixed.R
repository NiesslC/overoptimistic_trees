# Data preprocessing (fixed steps)

library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(purrr)
library(janitor)
library(stringr)
library(haven)

# 01 Read data -------------------------------------------------------------------------------------
data_pmd = read_excel(path = "./01_data/Originaldaten_PMD_IBE_final.xlsx", sheet = "Sheet1") 
data_sapv = read_excel(path = "./01_data/Originaldaten_SAPV_IBE_final.xlsx", sheet = "Sheet1") 
data_station = read_excel(path = "./01_data/Originaldaten_Station_IBE_final.xlsx", sheet = "Sheet1") 
data = bind_rows(data_pmd, data_sapv, data_station)
rm(data_pmd, data_sapv, data_station)

# 02 Get cost data without systemic minutes --------------------------------------------------------
station_sys = read_sav("./01_data/Station_ohnesys.sav")
pmd_sys = read_sav("./01_data/PD_ohnesys.sav")
sapv_sys = read_sav("./01_data/SAPV_ohnesys.sav")
data_sys = bind_rows(station_sys, pmd_sys, sapv_sys)
rm(station_sys, pmd_sys, sapv_sys)

# Note: there are cases with same id, date, time, and palliativphase, which means that "data_sys"
# cannot be merged to "data" unambiguously. However, the exact assignment of costs
# within a certain time point + palliativpahse + kost_sum for a specific id is not important since the data are 
# later aggregated at least on phases (even for day aggregation this would not be a problem)

# -> assign row ids for cases with identical COMPANION_ID, Team_ID, datum, zeit, palliativphase, kost_sum
# and use the additional row indicator for merging
data_sys = data_sys %>% select(COMPANION_ID, Team_ID, datum, zeit, palliativphase, kost_gesamt_ohnesys, kost_sum)
data_sys = data_sys %>% group_by(COMPANION_ID, Team_ID, datum, zeit, palliativphase, kost_sum) %>% 
  mutate(row_id = row_number(),
         join_kost_sum = round(kost_sum,5)) %>% # otherwise join will not work because of floating point error
  ungroup() %>%
  select(-kost_sum) # only need this variable in data, not in data_sys
data = data %>% group_by(COMPANION_ID, Team_ID, datum, zeit, palliativphase, kost_sum) %>% 
  mutate(row_id = row_number(),
         join_kost_sum = round(kost_sum,5)) %>%
  ungroup()
data = full_join(data, data_sys, by = c("row_id", "COMPANION_ID", "Team_ID", "datum", "zeit", "palliativphase", "join_kost_sum"))
data = data %>% select(-join_kost_sum, -row_id)
rm(data_sys)

# 03 Remove "mins_" and "kosts_" except kost_sum and other variables not used for analysis ---------
# except mins_gesamt
mins_rm = colnames(data %>% select(contains("mins_")))[!grepl("mins_gesamt",colnames(data %>% select(contains("mins_"))))]
data = data %>% select(-all_of(mins_rm), -kost_arzt, -kost_pflege, -kost_seelsorge, -kost_sonst, 
                       -uebersetzer_noetig, -contains("PCPSS"), -contains("Barthel"),
                       -phasenwechsel_sum,
                       -aufnahmedatum, -entlassungsdatum, -anzahl_tage, -Phasenwechsel,
                       -entlassart, -entlassart_kategorisiert, -geschlecht, -onkologisch, -diagnose)
rm(mins_rm)

# 04 Assign English variable names for the remaining variables -------------------------------------
data = data %>% dplyr::rename(date = datum,
                       time = zeit,
                       age = alter,
                       minutes_total = mins_gesamt,
                       cost_total = kost_sum,
                       cost_total_exclsys = kost_gesamt_ohnesys,
                       palliativephase = palliativphase,
                       ipos_pain = IPOS_schmerzen,
                       ipos_shortness_breath = IPOS_atemnot,
                       ipos_weakness = IPOS_schwaeche,
                       ipos_nausea = IPOS_uebelkeit,
                       ipos_vomiting = IPOS_erbrechen,
                       ipos_poor_appetite = IPOS_appetitlosigkeit,
                       ipos_constipation = IPOS_verstopfung,
                       ipos_sore_dry_mouth = IPOS_mundtrockenheit,
                       ipos_drowsiness = IPOS_schlaefrigkeit,
                       ipos_poor_mobility = IPOS_mobilitaet,
                       ipos_patient_anxiety = IPOS_patient_beunruhigt,
                       ipos_family_anxiety = IPOS_familie_beunruhigt,
                       ipos_depression = IPOS_traurig,
                       ipos_peace = IPOS_frieden,
                       ipos_sharing_feelings = IPOS_gefuehle,
                       ipos_information = IPOS_informationen,
                       ipos_practical_matters = IPOS_probleme,
                       cogn_confusion = kognitiv_verwirrtheit,
                       cogn_agitation = kognitiv_unruhe,
)


# 05 Correct time and date -------------------------------------------------------------------------
data = data %>% mutate(time = substr(data$time, 1, 8),
                       datetime = as.POSIXct(paste(date,time), format="%Y-%m-%d %H:%M:%S")) %>%
  relocate(datetime, .after = time)

# 06 Variable labels -------------------------------------------------------------------------------
data = data %>% mutate(palliativephase = factor(palliativephase, levels = c(1,2,3,4,5,-77,-99),
                                               labels = c("stable","unstable","deteriorating", "terminal",
                                                          "bereavement",
                                                          "missing",
                                                          "cannot assess")),
                       setting = factor(setting, levels = c(0,1,2),  
                                        labels = c("station","pmd",
                                                   "sapv")))

data = data %>% mutate_at(vars(paste0("ipos_", c("pain",
                                                 "shortness_breath","weakness","nausea","vomiting",
                                                 "poor_appetite","constipation","sore_dry_mouth",
                                                 "drowsiness", "poor_mobility"))), ~ factor(., levels = c(0,1,2,3,4,-77,-99),
                                                                                             labels = c("not at all","slightly","moderately","severely",
                                                                                                        "overwhelmingly","missing",
                                                                                                        "cannot assess")))
data = data %>% mutate_at(vars("ipos_patient_anxiety",
                               "ipos_family_anxiety",
                               "ipos_depression"), ~ factor(., levels = c(0,1,2,3,4,-77,-99),
                                                         labels = c("not at all","occasionally","sometimes","most of the time",
                                                                    "always","missing",
                                                                    "cannot assess")))
data = data %>% mutate_at(vars("ipos_peace",
                               "ipos_sharing_feelings",
                               "ipos_information"), ~ factor(., levels = c(0,1,2,3,4,-77,-99),
                                                               labels = c("always", "most of the time", "sometimes", "occasionally", 
                                                               "not at all", "missing", "cannot assess")))
data = data %>% mutate_at(vars("ipos_practical_matters"), ~ factor(., levels = c(0,1,2,3,4,-77,-99),
                                                          labels = c("addressed or no problem", "mostly addressed",
                                                                     "partly addressed","hardly addressed","not addressed",
                                                                     "missing",
                                                                     "cannot assess")))

data = data %>% mutate_at(vars("cogn_confusion",
                               "cogn_agitation"), ~ factor(., levels = c(0,1,2,3,-77,-99),
                                                            labels = c("absent", "mild", "moderate", "severe",
                                                                       "missing",
                                                                       "cannot assess")))
data = data %>% mutate_at(vars("AKPS"), ~ factor(., levels = c(seq(0,100,10),-77,-99),
                                                 # (essentially, we don't need the following labels)
                                                 # labels = c("dead",
                                                 #            "comatose or barely rousable",
                                                 #            "totally bedfast and requiring extensive nursing care by professionals and/or family",
                                                 #            "almost completely bedfast",
                                                 #            "in bed more than 50% of the time",
                                                 #            "considerable assistance and frequent medical care required",
                                                 #            "able to care for most needs; but requires occasional assistance",
                                                 #            "cares for self; unable to carry on normal activity or to do active work 70",
                                                 #            "normal activity with effort; some signs or symptoms of disease",
                                                 #            "able to carry on normal activity; minor sign of symptoms of disease 90",
                                                 #            "normal; no complaints; no evidence of disease",
                                                 #            "missing",
                                                 #            "cannot assess")
                                                 ))

# Note that there are no "missing" values, only NAs that refer to "missing"
apply(data, 2, FUN = function(i) sum(i == "missing", na.rm = TRUE))
# -> we leave the missings as NAs because they will later be dropped anyways
# -> we thus drop the "missing" level from all factors
data = data %>% mutate(across(where(is.factor), ~ fct_drop(., only = c("missing"))))

# 07 Exclude palliativephase = "bereavement" and contacts with AKPS = "dead"  ----------------------
length(unique(data$COMPANION_ID))
data = data %>% filter(palliativephase != "bereavement" & AKPS != "dead")
length(unique(data$COMPANION_ID)) 

# no (complete) patients are excluded by this restriction!

# 08 Exclude patients with kost_sum = 0 ------------------------------------------------------------
length(unique(data$COMPANION_ID))
data = data %>% filter(cost_total > 0 )
length(unique(data$COMPANION_ID)) 
# no (complete) patients are excluded by this restriction!

# 09 Save dataset ----------------------------------------------------------------------------------
data = data %>% clean_names() %>% relocate(cost_total_exclsys, .after = cost_total)
  
save(data, file = "./01_data/data_all.RData") 





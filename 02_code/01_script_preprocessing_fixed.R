# Data preprocessing (fixed steps)

library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(purrr)
library(janitor)
library(stringr)
library(haven)
library(forcats)


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
                       minutes = mins_gesamt,
                       cost = kost_sum,
                       cost_exclsys = kost_gesamt_ohnesys,
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

# 07 Exclude palliativephase = "bereavement" and contacts with AKPS = 0 ("dead")  ------------------
length(unique(data$COMPANION_ID))
data = data %>% filter(palliativephase != "bereavement" & AKPS != 0)
length(unique(data$COMPANION_ID)) 

# no (complete) patients are excluded by this restriction!

# 08 Exclude patients with kost_sum = 0 ------------------------------------------------------------
length(unique(data$COMPANION_ID))
data = data %>% filter(cost > 0 )
length(unique(data$COMPANION_ID)) 
# no (complete) patients are excluded by this restriction!

# 09 Calculate cost/minutes per diem for all potential outcome variables ---------------------------

# add phase_days (indicates the number of days (as integers) between first and last contact=
data = data %>% group_by(COMPANION_ID) %>%
  arrange(datetime) %>% 
  mutate(grp = cumsum(palliativephase != lag(palliativephase, def =
                                               first(palliativephase)))) %>%
  # grp = k means this is the k'th phase of the patient 
  group_by(COMPANION_ID, grp) %>%
  mutate(phase_days = 1 + as.numeric(difftime(max(date), min(date), units = "days"))) %>%
  # (date instead of datetime because we only want to consider the date)
  relocate(grp, palliativephase, phase_days, .after = datetime) %>% 
  ungroup() %>%
  arrange(COMPANION_ID, datetime)

# add variables costpd, costpd_exclsys, minutespd
# (indicate the per diem costs/minutes (on phase level), using phase_days as denominator)
data = data %>% group_by(COMPANION_ID, grp) %>%
  mutate(costpd = sum(cost)/phase_days,
         costpd_exclsys = sum(cost_exclsys)/phase_days,
         minutespd = sum(minutes)/phase_days) %>%
  ungroup() %>%
  relocate(costpd,costpd_exclsys,minutespd, .after = phase_days) %>%
  relocate(cost_exclsys, .after = cost)

# 10 Exclude one phase with costpd > 1200 ----------------------------------------------------------
data = data %>% filter(costpd < 1200)

# 11 Clean names -----------------------------------------------------------------------------------
data = data %>% clean_names() 


# 12 Extract phase levels values of time-varying features and insert them in contact level data ----

# Currently, the dataset is on contact level but the analysis will be on palliative phase level
# -> For each feature, we will need one single value per palliative care phase (= observation level)
# -> We extract this value ("A.") but will merge it back to the contact level dataset ("B.") since we still need
#    the contact level information for the target variables (for these variables, the phase level 
#    value will be extracted within the modeling process, so we cannot reduce the dataset to phase level
#    at this point.)

## A. Extract phase level value ----
vars_vary = colnames(data %>% select(contains("ipos_"),  contains("cogn_"), akps))
# We need to consider every variable in vars_vary separately because we (sometimes) have to filter different 
# time points on the first day 
# -> Separate dataset according to variables potentially varying (=sep) and not varying within phases (=data_phaselevel)
#    and merge them in the end
data_phaselevel = data %>% 
  group_by(companion_id, grp) %>% 
  filter(date == min(date)) %>% 
  ungroup() %>%
  select(-all_of(vars_vary),-time, -datetime,-minutes,-cost,-cost_exclsys) %>%
  group_by(companion_id, grp) %>%
  distinct()

missings = c(NA, "cannot assess")

for(i in 1:length(vars_vary)){
  sep = data %>% group_by(companion_id, grp) %>% filter(date == min(date)) %>% 
    select(companion_id, grp, !!rlang::sym(vars_vary[i])) %>% 
    # Only keep unique values occured on first day:
    distinct(!!rlang::sym(vars_vary[i]), .keep_all = TRUE) %>% 
    # Only keep NA/"cannot assess" if it is the only value measured on the first day:
    filter(!!rlang::sym(vars_vary[i]) %in% setdiff(levels(!!rlang::sym(vars_vary[i])), missings) |
             (!any(!!rlang::sym(vars_vary[i]) %in% setdiff(levels(!!rlang::sym(vars_vary[i])), missings)) &
                !!rlang::sym(vars_vary[i]) %in% missings)) %>% ungroup() 
  
  # If more than one value on first day, use min value for AKPS and max value for all other features:
  # (in the very rare case [< 5 phases] where both missing values appear on one day, use NA)
  if(grepl("akps", vars_vary[i])){
    sep = sep %>% group_by(companion_id, grp) %>% 
      filter(as.integer(!!rlang::sym(vars_vary[i])) == min(as.integer(!!rlang::sym(vars_vary[i])))) %>%
      ungroup()
  } else{
    sep = sep %>% group_by(companion_id, grp) %>% 
      filter(as.integer(!!rlang::sym(vars_vary[i])) == max(as.integer(!!rlang::sym(vars_vary[i])))) %>%
      ungroup()
  }
  
  # Merge with data_phaselevel
  data_phaselevel = full_join(data_phaselevel, sep, by = c("companion_id", "grp"))
} 

# Remove date variable (also varying within phase)
data_phaselevel = data_phaselevel %>% select(-date)

# After preparing the data, it should have excactly one observation per companion_id and phase (grp):
stopifnot(nrow(data %>% distinct(grp, companion_id)) == nrow(data_phaselevel))
 
## B. Insert phase level value into contact level dataset ----
data = data %>% 
  select(-all_of(vars_vary)) %>% # remove contact level values
  select(-age) # also remove age from contact level dataset because age may change within 
               # phase by one year (insert age value of first day of phase below)
     
data = full_join(data, data_phaselevel, by = setdiff(colnames(data_phaselevel), c("age", vars_vary)))

# 13 Set "cannot assess" to "absent" in cognitive variables and mutate as ordinal variable ---------
data = data %>%
  mutate(across(starts_with("cogn_"), ~ forcats::fct_recode(.,  "absent" = "cannot assess"))) %>% 
  mutate(across(starts_with("cogn_"), ~ factor(., ordered = TRUE)))
# 14 Save dataset ----------------------------------------------------------------------------------
save(data, file = "./01_data/data_all.RData") 





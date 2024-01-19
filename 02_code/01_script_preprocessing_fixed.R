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

# > READ DATA --------------------------------------------------------------------------------------
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

# > DATA CLEANING ----------------------------------------------------------------------------------
# 03 Remove "mins_" and "kosts_" except kost_sum and other variables not used for analysis ---------
# except mins_gesamt
mins_rm = colnames(data %>% select(contains("mins_")))[!grepl("mins_gesamt",colnames(data %>% select(contains("mins_"))))]
data = data %>% select(-all_of(mins_rm), -kost_arzt, -kost_pflege, -kost_seelsorge, -kost_sonst, 
                       -uebersetzer_noetig, -contains("PCPSS"), -contains("Barthel"),
                       -phasenwechsel_sum,
                       -aufnahmedatum, -entlassungsdatum, -anzahl_tage, -Phasenwechsel,
                       -entlassart, -entlassart_kategorisiert, -geschlecht, -onkologisch, -diagnose)
rm(mins_rm)

# 04 Assign English variable names for the remaining variables and clean names ---------------------
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

data = data %>% clean_names() 

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
data = data %>% mutate_at(vars("akps"), ~ factor(., levels = c(seq(0,100,10),-77,-99),
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


# REMOVE ROWS (CONTACTS) (dead patients or zero cost) ----------------------------------------------
# 07 Remove contacts with palliativephase = "bereavement" or AKPS = 0 ("dead")  --------------------
length(unique(data$companion_id))
data = data %>% filter(palliativephase != "bereavement" & (akps != 0| is.na(akps))) # (akps NAs are excluded elsewhere)
length(unique(data$companion_id)) 
# no (complete) patients are excluded by this restriction, only some contacts!

# 08 Remove contacts with cost == 0 ----------------------------------------------------------------
length(unique(data$companion_id))
data = data %>% filter(cost > 0 )
length(unique(data$companion_id)) 
# no (complete) patients are excluded by this restriction, only some contacts!

# > GET PHASE LEVEL DATA ---------------------------------------------------------------------------
# 09 Add number of days per phase and palliative phase counter -------------------------------------
# phase_days indicates the number of days (as integers) between first and last contact
# grp indicates the number of the palliative phase within each episode (grp = 0 -> first pahse)
data = data %>% group_by(companion_id) %>%
  arrange(datetime) %>% 
  mutate(grp = cumsum(palliativephase != lag(palliativephase, def =
                                               first(palliativephase)))) %>%
  # grp = k means this is the k'th phase of the patient 
  group_by(companion_id, grp) %>%
  mutate(phase_days = 1 + as.numeric(difftime(max(date), min(date), units = "days"))) %>%
  # (date instead of datetime because we only want to consider the date)
  relocate(grp, palliativephase, phase_days, .after = datetime) %>% 
  ungroup() %>%
  arrange(companion_id, datetime)

# 10 Extract target values on palliative phase level ----------------------------------------------

# Currently, the target values (cost/cost_exclsys/minutes) are on contact level but the analysis will
# be on palliative phase level
# -> For each target, we will need one single value per palliative care phase (= observation level)
# -> However, at this point, we will not calculate one single value per target but one value for the 
#    first day of the palliative phase and one for the second and later days (the values are the sum
#    of costs/minutes for the respective time period). Later, these variables will be combined to one 
#    sum (per target variable) but the information of the first day is needed to calculate correction
#    factors. This cannot be performed on the whole data set to avoid data leakage.

# Summarize costs for first day vs. second or later day within each phase
data_target_phaselevel = data %>%
  group_by(setting, team_id, companion_id, grp) %>% 
  mutate(number_day = ifelse(date == min(date), "day_1", "day_geq2")) %>%
  ungroup()
data_target_phaselevel = data_target_phaselevel %>%
  group_by(setting, team_id, companion_id, grp, number_day) %>%
  summarise(sum_cost = sum(cost),
            sum_cost_exclsys = sum(cost_exclsys),
            sum_minutes = sum(minutes)) %>%
  ungroup()

# Generate two variables (day 1 vs day >= 2) per target variable
data_target_phaselevel = data_target_phaselevel %>% 
  pivot_wider(names_from = number_day, values_from = c(sum_cost, sum_cost_exclsys, sum_minutes))

# After preparing the data, there should be  exactly one observation per companion_id and phase (grp):
stopifnot(nrow(data %>% distinct(companion_id, grp)) == nrow(data_target_phaselevel))

# Add dummy target variable (will later have the values of one of the three possible outcome variables)
data_target_phaselevel = data_target_phaselevel %>% mutate(targetvar = 0)


# 11 Extract feature values on palliative phase level ----------------------------------------------

# Currently, the feature values are on contact level but the analysis will be on palliative phase level
# -> For each feature, we will need one single value per palliative care phase (= observation level)
# -> We extract this value for each feature to later join the resulting data set with data_target_phaselevel

# Specify time-varying variables 
# (note that age can also vary during the phase by one unit [year] -> use value of first day in phase)
vars_vary = colnames(data %>% select(contains("ipos_"),  contains("cogn_"), akps)) 

# Consider every variable in vars_vary separately because we (sometimes) have to filter different 
# time points of the first day (if the value varies during the first day)
# -> Separate dataset according to variables potentially varying (=sep) and not varying within phases (=data_feature_phaselevel)
#    and join them in the end
data_feature_phaselevel = data %>% 
  group_by(companion_id, grp) %>% 
  filter(date == min(date)) %>% 
  ungroup() %>%
  select(-all_of(vars_vary),-time, -datetime, -date, -cost,-cost_exclsys,-minutes) %>%
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
  
  # Merge with data_feature_phaselevel
  data_feature_phaselevel = full_join(data_feature_phaselevel, sep, by = c("companion_id", "grp"))
} 
rm(i, sep, missings, vars_vary) 

# After preparing the data, there should be  exactly one observation per companion_id and phase (grp):
stopifnot(nrow(data %>% distinct(companion_id, grp)) == nrow(data_feature_phaselevel))
 
# 12 Join target and feature data on palliative phase level ----------------------------------------
stopifnot(nrow(data_target_phaselevel) == nrow(data_feature_phaselevel))

data_phaselevel = full_join(data_target_phaselevel, data_feature_phaselevel, 
                 by = c("setting","team_id", "companion_id", "grp") )
stopifnot(ncol(data_phaselevel) == (ncol(data_target_phaselevel)+
                                      ncol(data_feature_phaselevel) -
                                      length(c("setting","team_id", "companion_id", "grp"))))
rm(data, data_target_phaselevel, data_feature_phaselevel)


# > REMOVE ROWS (PHASES) (one extreme outlier + missing feature values) ----------------------------
# 13 Remove one extreme target outlier -------------------------------------------------------------
# Remove one outlier with (uncorrected) average cost per phase > 1200
outlier_exclude = which(data_phaselevel %>%
                          rowwise() %>%
                          mutate(sum_cost = sum(c(sum_cost_day_1,sum_cost_day_geq2), na.rm = TRUE)/phase_days) %>% . $sum_cost > 1200)
data_phaselevel = data_phaselevel[-outlier_exclude,]
rm(outlier_exclude)

# 14 Remove NAs and -99/cannot assess --------------------------------------------------------------
# - ipos_ variables: remove phases with NAs in ipos variables
data_phaselevel = data_phaselevel %>% filter_at(vars(starts_with("ipos_")), all_vars(!is.na(.)))
# - cogn_ variables: remove phases with NA in cogn variables
data_phaselevel = data_phaselevel %>% filter_at(vars(starts_with("cogn_")), all_vars(!is.na(.)))
# - akps: remove phases with NA or 'cannot assess' 
data_phaselevel = data_phaselevel %>% filter(!is.na(akps) & akps != -99)

# > REPLACE CANNOT ASSESS VALUES (cogn_ variables) -------------------------------------------------
# 15 Set "cannot assess" to "absent" in cognitive variables and mutate as ordinal variable ---------
data_phaselevel = data_phaselevel %>%
  mutate(across(starts_with("cogn_"), ~ forcats::fct_recode(.,  "absent" = "cannot assess"))) %>% 
  mutate(across(starts_with("cogn_"), ~ factor(., ordered = TRUE)))



# > DROP UNUSED LEVELS AND IDS AS FACTORS ----------------------------------------------------------
# 16 Drop unused levels and convert team and companion id to factor ----
data_phaselevel = data_phaselevel %>% droplevels()
data_phaselevel$companion_id = factor(data_phaselevel$companion_id)
data_phaselevel$team_id = factor(data_phaselevel$team_id) 

# > SAVE -------------------------------------------------------------------------------------------
# 17 Save dataset ----------------------------------------------------------------------------------
stopifnot(all(data_phaselevel %>% group_by(companion_id, grp) %>% count() %>% .$n == 1)) # one row per phase per patient
save(data_phaselevel, file = "./01_data/data_phaselevel.RData") 


# CHECKS 
# No NAs (except for cost/minutes_geq2) and no "cannot assess"/-99 values (except for ipos)
which(colSums(is.na(data_phaselevel))>0) # ok 
which(colSums(data_phaselevel == "cannot assess")>0) # ok
which(colSums(data_phaselevel == -99)>0) # ok
# Check general distribution
skimr::skim(data_phaselevel) # ok
# Check maxima of uncorrected costs/minutes per diem
summary(data_phaselevel %>%  rowwise() %>% # ok
  mutate(sum_cost_uncorr = sum(c(sum_cost_day_1,sum_cost_day_geq2), na.rm = TRUE)/phase_days,
         sum_cost_exclsys_uncorr = sum(c(sum_cost_exclsys_day_1,sum_cost_exclsys_day_geq2), na.rm = TRUE)/phase_days,
         sum_minutes_uncorr = sum(c(sum_minutes_day_1,sum_minutes_day_geq2), na.rm = TRUE)/phase_days) %>%
  select(contains("uncorr")))

# with cutoff >= 10, there are max. (for pmd) 0.8496241 -> 15% of phases removed (in total, i.e. also including exclusions above)
# data_phaselevel=data_phaselevel %>% rowwise() %>%
#   mutate(sum_ca = sum(c_across(starts_with("ipos_")) == "cannot assess", na.rm = T)) 
# 1:17 %>% map_dbl(function(x) nrow(data_phaselevel %>% filter(setting == "sapv" & sum_ca < x)) / 
#   nrow(all %>% filter(setting == "sapv")) )
# [1] 0.4825261 0.6229307 0.7081545 0.7572042 0.7933783 0.8320049 0.8522379 0.8700184 0.8749234 0.8792152 0.8828939 0.8859595 0.8865727
# [14] 0.8871858 0.8871858 0.8884120 0.8884120
# 1:17 %>% map_dbl(function(x) nrow(data_phaselevel %>% filter(setting == "pmd" & sum_ca < x)) / 
#   nrow(all %>% filter(setting == "pmd")) )
# [1] 0.3412698 0.4778613 0.5810359 0.6428571 0.6908939 0.7435255 0.7736007 0.8224728 0.8383459 0.8496241 0.8650794 0.8746867 0.8830409
# [14] 0.8897243 0.8922306 0.8955723 0.8984962
# 1:17 %>% map_dbl(function(x) nrow(data_phaselevel %>% filter(setting == "station"& sum_ca < x)) / 
#   nrow(all %>% filter(setting == "station")) )
# [1] 0.5652381 0.6880952 0.7733333 0.8414286 0.8714286 0.8909524 0.9042857 0.9161905 0.9242857 0.9290476 0.9319048 0.9357143 0.9395238
# [14] 0.9419048 0.9433333 0.9442857 0.9442857
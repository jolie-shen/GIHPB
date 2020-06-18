# File looking at GI trials
set.seed(5)
wants <- c('zip', 'svMisc', 'ggpubr', 'Hmisc', 'mice', 'glmnet', 'tidyverse','RPostgreSQL', 'europepmc', 'RefManageR', 'DT', 'lubridate', 'ggplot2', 'openxlsx', 'survminer', 'Kendall', 'coin')

# ------------------------------------- On Laptop

has <- wants %in% row.names(installed.packages())
if(any(!has)) install.packages(wants[!has])

obtained <- unlist(lapply(wants, require, character.only = TRUE))
names(obtained) <- wants

data.frame(loaded = obtained)

#setwd("~/Documents/Coding/R/gi_hpb_project") 
source('brandonfunctions.r')


# --------------------------------------------------------------------------------------------------------- #
# --------------------------           Load The Rest Of The Clinical Trials Data          -------------------
# ----------------------------------------------------------------------------------------------------------#

# ----- WHAT VERSION OF DATA DO WE WANT TO USE?? --------------------
gianalysis_data_directory <- 'all_ctgov_tables_oct_29_2019'
# gianalysis_data_directory <- 'BackupDataFiles/all_ctgov_tables_oct_29_2019'

# load most of the supporting tables
load(file = file.path(gianalysis_data_directory, 'nct_startupfiles_1b.RData')) # includes things like my_fac2 or my_studies, already processed

# Bigtbl
load(file = file.path(gianalysis_data_directory, 'Bigtbl.Rdata'))
cutoff_date <- ymd(unique(Bigtbl %>% pull(br_ctgov_download_date)))

# Load the FDAAA Tracker Data
fdaaa_tracker_data <- readRDS(file = file.path(gianalysis_data_directory, 'fdaaa_tracker_data.rds'))
colnames(fdaaa_tracker_data) <- paste0('fdaaatracker_', colnames(fdaaa_tracker_data))


# -------------------------------------------------------------------------------------------------------- #
# --------------------        load and organize data from GI Team       --------------------------
# -------------------------------------------------------------------------------------------------------- #

raw_gi_list <- 
  openxlsx::read.xlsx(xlsxFile = 'all_ctgov_jun17/gi_hpb_trials_updated_jun17.xlsx',
                      sheet = 1, startRow = 1) %>%
  as_tibble() %>%
  mutate_all(as.character)

# need to merge a few categories...
"""raw_gi_list <- 
  raw_gi_list %>%
  mutate(pnspine = case_when(
    '1' == peripheral | '1' == cranial ~ '1',
    TRUE ~ NA_character_
  )) %>%
  mutate(nmjmuscle = case_when(
    '1' == nmj | '1' == muscle ~ '1',
    TRUE ~ NA_character_
  )) %>%
  mutate(systemic = case_when(
    '1' == toxic | '1' == inherited ~ '1',
    TRUE ~ NA_character_
  ))"""


#blue columns below
cols_disease_full <- 
  c(
    'infection_any',
    'neoplasia_disease',
    'abdominal_hernia',
    'appendicitis',
    'cirrhosis',
    'diverticular_disease',
    'fecal_diversion',
    'foreign_body',
    'functional_disorder',
    'gallstones',
    'gerd',
    'hemorrhoids',
    'hypoxic',
    'ileus',
    'ibd',
    'malabsorptive',
    'motility',
    'nafld_nash',
    'nonspecific',
    'pancreatitis',
    'transplant',
    'ulcerative_disease',
    'other')

#white columns below
spec_disease <- 
  c(
    'infection_helminth',
    'infection_intestines',
    'infection_hepatitis',
    'neoplasia_primary',
    'neoplasia_metastasis'
    )

#location columns
cols_location <-
  c(
    'location_esophagus',
    'location_stomach',
    'location_small_intestine',
    'location_colon_rectum',
    'location_anus',
    'location_liver',
    'location_biliarytract',
    'location_gallbladder',
    'location_pancreas',
    'location_peritoneum',
    'location_notspecified'
  )

all_disease_cols <- c(cols_location, cols_disease_full, spec_disease)
cols_disease <- c(cols_disease_full, spec_disease)
cols_location <- c(cols_location)

raw_gi_list <- 
  raw_gi_list %>%
  select(nct_id, true_gi, one_of(all_disease_cols), coder, codecount)


name_table <- 
  openxlsx::read.xlsx(xlsxFile = 'all_ctgov_jun17/gi_hpb_trials_updated_jun17.xlsx',
                      sheet = 2, startRow = 1) %>%
  as_tibble() %>%
  mutate_all(as.character)


# -------------------------------------------------------------------------------------------------------- #
# ---------------        Do Quality Checks and Such for Duplicate Entries, Etc     -----------------------
# -------------------------------------------------------------------------------------------------------- #

# find how many nct_id are duplicated...
raw_gi_list %>% 
  add_count(nct_id) %>% 
  filter(n > 1) %>%
  select(coder, nct_id, n) %>%
  arrange(nct_id) %>% 
  print(n=Inf)

# there are no duplicates in this file. 

# how many reviews, etc
raw_gi_list %>%
  bcount(true_gi)
#19296 (91%) coded 1, 499 ((2.4%) coded r, 1396 (6.6%) coded n


# make sure there are only '1' or NA in the disease columns
raw_gi_list %>% 
  select(one_of(all_disease_cols)) %>% 
  lapply(unique) %>% 
  unlist() %>% 
  unname() %>% 
  unique()


# in my case, there are only '1' or NA, as instructed

# do some light processing
raw_gi_list <-
  raw_gi_list %>%
  mutate_at(vars(one_of(all_disease_cols)), function(x) !is.na(x)) # convert NA to FALSE for disease

# --------------------------        MERGE GI DATA WITH BIGTBL       -----------------------------

full_gi_df <- 
  left_join(raw_gi_list %>%
              filter(true_gi == '1') %>%
              select(one_of(c('nct_id','coder', all_disease_cols))),
            Bigtbl,
            by = 'nct_id') %>%
  mutate(bintime = case_when(
    year(study_first_submitted_date) <= 2013 ~ '2007_2013',
    year(study_first_submitted_date) > 2013 ~ '2014_2019',
    TRUE ~ NA_character_
  )) %>%
  mutate(nct_gi = TRUE)

# any in our set no longer in the full set? We should remove these...
setdiff(full_gi_df %>% pull(nct_id), 
        Bigtbl %>% pull(nct_id))

full_gi_df <- 
  full_gi_df %>%
  filter(nct_id %nin% setdiff(full_gi_df %>% pull(nct_id), 
                              Bigtbl %>% pull(nct_id)))


# ------ Get Max Date
gi_maxdate <- full_gi_df %>% pull(study_first_submitted_date) %>% max(na.rm = TRUE) # get the last date for trials that we used
gi_maxdate

#Oct 24 2019

# -------------------------------------------#
# filter out interventional and stuff before May 1 or after Oct 2007

full_gi_df <- 
  full_gi_df %>%
  filter(study_type == 'Interventional') %>% 
  filter(study_first_submitted_date >= ymd('20071001')) %>%
  filter(study_first_submitted_date < ymd('20200101'))



full_comparison_df <- 
  Bigtbl %>%
  filter(study_type == 'Interventional') %>% 
  filter(study_first_submitted_date >= ymd('20071001')) %>%
  filter(study_first_submitted_date < ymd('20200101')) %>%
  filter(nct_id %nin% (full_gi_df %>% pull(nct_id))) %>% 
  mutate(bintime = case_when(
    year(study_first_submitted_date) <= 2012 ~ '2007_2013',
    year(study_first_submitted_date) > 2012 ~ '2014_2019',
    TRUE ~ NA_character_
  ))

# -------------------------------------------# 
# -------- Get Size Data

my_studies %>% count(study_first_submitted_date <= gi_maxdate) # how many trials were in database at time that we downloaded stuff? 
btest0 <- my_studies %>% count(study_first_submitted_date <= gi_maxdate) %>% {colnames(.)[1] <- 'totaltrials'; .}
btest0b <- my_studies %>% count(study_first_submitted_date < ymd('20200101')) %>% {colnames(.)[1] <- 'totaltrials'; .}

btest1 <- my_studies %>% filter(study_first_submitted_date < ymd('20200101'))
btest2 <- btest1 %>% filter(study_type == 'Interventional') # how many interventional trials? 
btest3 <- btest2 %>% filter(study_first_submitted_date >= ymd('20071001')) # how many lost because submitted before Oct 2007?

# how many lost b/c of interventional status
nrow(btest1) - nrow(btest2)
#56301

# how many additional lost b/c of registration before October 1, 2007
nrow(btest2) - nrow(btest3)
#38102

# how many are we left with before specialty filter?
nrow(btest3)
#180708

# how many are lost from specialty filter?
nrow(btest3) - nrow(full_gi_df)
#166053

# how many are left after subspecialty filter
nrow(full_gi_df)
#14655

# make table of these results
flowfiguredf <- 
  c('get the last date for trials that we used' = as.character(gi_maxdate),
    'how many trials were in the database at time that we downloaded stuff' = btest0 %>% filter(totaltrials) %>% pull(n),
    'how many trials were in the database at April 30th 2019?' = btest0b %>% filter(totaltrials) %>% pull(n),
    'how many lost b/c of lack of interventional status' = nrow(btest1) - nrow(btest2),
    'how many additional lost b/c of registration before October 1, 2007?' = nrow(btest2) - nrow(btest3),
    'how many are we left with before specialty filter?' = nrow(btest3),
    'how many are left after subspecialty filter?' = nrow(full_gi_df)
  )

flowfiguredf <- 
  data.frame(titlething = names(flowfiguredf), values = unname(flowfiguredf))
flowfiguredf

rm(btest0, btest0b, btest1, btest2, btest3)

#Total patient numbers
sum(full_gi_df$enrollment, na.rm = TRUE)

# --------------------------------------------#

col_regions <- c('Africa', 'CentralAmerica', 'EastAsia', 'Europe', 
                 'MiddleEast', 'NorthAmerica', 'Oceania',
                 'Other', 'SouthAmerica', 'SouthAsia', 'SoutheastAsia')
# list(Africa, CentralAmerica, EastAsia, Europe, 
# MiddleEast, NorthAmerica, Oceania,
# Other, SouthAmerica, SouthAsia, SoutheastAsia)

# add regional data
full_gi_df <-
  full_gi_df %>%
  mutate_at(.vars = col_regions,
            .funs = rlang::list2(~case_when(is.na(all_regions) ~ NA, 
                                            is.na(.) ~ FALSE, 
                                            TRUE ~ .)))

#full_comparison_df <-
#  full_comparison_df %>%
#  mutate_at(.vars = col_regions,
#            .funs = rlang::list2(~case_when(is.na(all_regions) ~ NA, 
#                                            is.na(.) ~ FALSE, 
#                                            TRUE ~ .)))

# some notes for you on regions and countries in case you want to know what the top 20 are (in order) across ALL trials
col_reg_top <-
  c('NorthAmerica', 'Europe', 'EastAsia', 'SouthAmerica', 'Oceania', 'MiddleEast',
    'Africa', 'SouthAsia', 'SoutheastAsia', 'CentralAmerica', 'Other')

col_con_top20 <-
  c('UnitedStates', 'Germany', 'France', 'Canada', 'Japan',
    'UnitedKingdom', 'Spain', 'Italy', 'China', 'Poland',
    'RussianFederation', 'KoreaRepublicof', 'Belgium',
    'Australia', 'Netherlands', 'Brazil', 'Hungary',
    'India', 'Israel', 'Sweden')



cols_disease_in_order <-
  full_gi_df %>% 
  select(one_of(cols_disease)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(rowname %nin% c('other')) %>%
  arrange(desc(V1)) %>% 
  pull(rowname)


cols_location_in_order <-
  full_gi_df %>% 
  select(one_of(cols_location)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(rowname %nin% c('notspecified')) %>%
  arrange(desc(V1)) %>% 
  pull(rowname)

# when selecting "top diseases" we don't want "other"

cols_disease_other3 <- setdiff(cols_disease, cols_disease_in_order[1:3]) # ie if there is a check for anything *outside* the top 3
cols_disease_other4 <- setdiff(cols_disease, cols_disease_in_order[1:4]) # remember that this also combines other inside!
cols_disease_other5 <- setdiff(cols_disease, cols_disease_in_order[1:5]) #     because other is a part of cols_disease
cols_disease_other6 <- setdiff(cols_disease, cols_disease_in_order[1:6])
cols_disease_other7 <- setdiff(cols_disease, cols_disease_in_order[1:7])
cols_disease_other8 <- setdiff(cols_disease, cols_disease_in_order[1:8])
cols_disease_other9 <- setdiff(cols_disease, cols_disease_in_order[1:9])
cols_disease_other10 <- setdiff(cols_disease, cols_disease_in_order[1:10])

cols_disease_other11 <- setdiff(cols_disease, cols_disease_in_order[1:11])
cols_disease_other12 <- setdiff(cols_disease, cols_disease_in_order[1:12])
cols_disease_other13 <- setdiff(cols_disease, cols_disease_in_order[1:13])
cols_disease_other14 <- setdiff(cols_disease, cols_disease_in_order[1:14])
cols_disease_other15 <- setdiff(cols_disease, cols_disease_in_order[1:15])
cols_disease_other16 <- setdiff(cols_disease, cols_disease_in_order[1:16])
cols_disease_other17 <- setdiff(cols_disease, cols_disease_in_order[1:17])
cols_disease_other18 <- setdiff(cols_disease, cols_disease_in_order[1:18])
cols_disease_other19 <- setdiff(cols_disease, cols_disease_in_order[1:19])

# when selecting "top location" we don't want "not specified"

cols_location_other3 <- setdiff(cols_location, cols_location_in_order[1:3]) # ie if there is a check for anything *outside* the top 3
cols_location_other4 <- setdiff(cols_location, cols_location_in_order[1:4]) # remember that this also combines other inside!
cols_location_other5 <- setdiff(cols_location, cols_location_in_order[1:5]) #     because other is a part of cols_disease
cols_location_other6 <- setdiff(cols_location, cols_location_in_order[1:6])
cols_location_other7 <- setdiff(cols_location, cols_location_in_order[1:7])
cols_location_other8 <- setdiff(cols_location, cols_location_in_order[1:8])
cols_location_other9 <- setdiff(cols_location, cols_location_in_order[1:9])
cols_location_other10 <- setdiff(cols_location, cols_location_in_order[1:10])


full_gi_df <-
  full_gi_df %>%
  mutate(
    disease_other3 = pmap_lgl(list(!!! rlang::syms(cols_disease_other3)), 
                              function(...) any(sapply(list(...), function(icol) icol))),
    disease_other4 = pmap_lgl(list(!!! rlang::syms(cols_disease_other4)), 
                              function(...) any(sapply(list(...), function(icol) icol))),
    disease_other5 = pmap_lgl(list(!!! rlang::syms(cols_disease_other5)), 
                              function(...) any(sapply(list(...), function(icol) icol))),
    disease_other6 = pmap_lgl(list(!!! rlang::syms(cols_disease_other6)), 
                              function(...) any(sapply(list(...), function(icol) icol))),
    disease_other7 = pmap_lgl(list(!!! rlang::syms(cols_disease_other7)), 
                              function(...) any(sapply(list(...), function(icol) icol))),
    disease_other8 = pmap_lgl(list(!!! rlang::syms(cols_disease_other8)), 
                              function(...) any(sapply(list(...), function(icol) icol))),
    disease_other9 = pmap_lgl(list(!!! rlang::syms(cols_disease_other9)), 
                              function(...) any(sapply(list(...), function(icol) icol))),
    disease_other10 = pmap_lgl(list(!!! rlang::syms(cols_disease_other10)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    
    disease_other11 = pmap_lgl(list(!!! rlang::syms(cols_disease_other11)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    disease_other12 = pmap_lgl(list(!!! rlang::syms(cols_disease_other12)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    disease_other13 = pmap_lgl(list(!!! rlang::syms(cols_disease_other13)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    disease_other14 = pmap_lgl(list(!!! rlang::syms(cols_disease_other14)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    disease_other15 = pmap_lgl(list(!!! rlang::syms(cols_disease_other15)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    disease_other16 = pmap_lgl(list(!!! rlang::syms(cols_disease_other16)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    disease_other17 = pmap_lgl(list(!!! rlang::syms(cols_disease_other17)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    disease_other18 = pmap_lgl(list(!!! rlang::syms(cols_disease_other18)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    disease_other19 = pmap_lgl(list(!!! rlang::syms(cols_disease_other19)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    
    location_other3 = pmap_lgl(list(!!! rlang::syms(cols_location_other3)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    location_other4 = pmap_lgl(list(!!! rlang::syms(cols_location_other4)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    location_other5 = pmap_lgl(list(!!! rlang::syms(cols_location_other5)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    location_other6 = pmap_lgl(list(!!! rlang::syms(cols_location_other6)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    location_other7 = pmap_lgl(list(!!! rlang::syms(cols_location_other7)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    location_other8 = pmap_lgl(list(!!! rlang::syms(cols_location_other8)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    location_other9 = pmap_lgl(list(!!! rlang::syms(cols_location_other9)),
                               function(...) any(sapply(list(...), function(icol) icol))),
    location_other10 = pmap_lgl(list(!!! rlang::syms(cols_location_other10)),
                               function(...) any(sapply(list(...), function(icol) icol)))
    
  )

full_gi_df %>% bcount(br_gni_lmic_hic) # there are so few that have both HMIC & LMIC in the trial, we'll just convert these to NA below

# add a bunch of other useful columns
full_gi_df <-
  full_gi_df %>%
  mutate(new_arms = Hmisc::cut2(x = number_of_arms, cuts = c(1,2,3,Inf)),
         new_arms2 = Hmisc::cut2(x = number_of_arms, cuts = c(2, Inf))) %>%
  mutate(new_enroll = Hmisc::cut2(x = enrollment, cuts = c(10, 50, 100, 500, 1000, Inf))) %>% 
  mutate(new_enroll2 = Hmisc::cut2(x = enrollment, cuts = c(100, Inf))) %>%
  mutate(enroll_10 = enrollment / 10,
         enroll_20 = enrollment / 20) %>%
  mutate(new_first_submit = year(study_first_submitted_date)) %>%
  mutate(new_num_regions = Hmisc::cut2(x = num_regions, cuts = c(1,2,3, Inf)),
         new_num_regions2 = Hmisc::cut2(x = num_regions, cuts = c(2, Inf)),
         new_num_facilities = Hmisc::cut2(x = num_facilities, cuts = c(1,2,3,10,Inf)),
         new_num_facilities2 = Hmisc::cut2(x = num_facilities, cuts = c(2, Inf))) %>%
  mutate(new_br_phase2 = fct_explicit_na(br_phase2, na_level = 'Unknown Phase')) %>%
  mutate(new_br_phase2 = fct_relevel(new_br_phase2, 'Phase 2')) %>%
  mutate(primary_purpose = fct_relevel(primary_purpose, 'Treatment'),
         new_primary_purpose_treatment = fct_collapse(.f = primary_purpose, # should be able to use group_other here rather than use setdiff
                                                      Treatment = 'Treatment', Prevention = 'Prevention', `Basic Science` = 'Basic Science',  # but there is a known forcats bug right now
                                                      Other = setdiff(primary_purpose, c("Treatment", "Prevention", "Basic Science")), group_other = FALSE), # generates "Unknown levels in `f`"
         new_primary_purpose_treatment2 = fct_lump(primary_purpose, n = 3)) %>%
  mutate(new_actduration = Hmisc::cut2(actual_duration, c(0, 10, 20, 30, 40, 50, Inf))) %>%
  mutate(br_masking2 = fct_relevel(br_masking2, 'None')) %>%
  mutate(num_disease_groups = pmap_dbl(list(!!! rlang::syms(cols_disease)),
                                       function(...) sum(...))) %>%
  mutate(single_disease_group = pmap_chr(list(!!! rlang::syms(cols_disease)),
                                         function(...) paste0(cols_disease[which(x = c(...))], collapse = ','))) %>%
  mutate(single_disease_group = case_when(
    num_disease_groups > 1 ~ 'multi_disease',
    TRUE ~ single_disease_group)) %>%
  mutate(num_location_group = pmap_dbl(list(!!! rlang::syms(cols_location)),
                                        function(...) sum(...))) %>%
  mutate(single_location_group = pmap_chr(list(!!! rlang::syms(cols_location)),
                                         function(...) paste0(cols_location[which(x = c(...))], collapse = ','))) %>%
  mutate(single_location_group = case_when(
    num_location_group > 1 ~ 'multi_location',
    TRUE ~ single_location_group)) %>%
  # mutate(br_singleregion4 = fct_lump(br_singleregion, n = 4)) %>% # I don't like this one, I want more reproducibility for which regions
  mutate(br_singleregion4 = fct_collapse(.f = br_singleregion,
                                         NorthAmerica = 'NorthAmerica', Europe = 'Europe', EastAsia = 'EastAsia', 
                                         OtherAndMultiRegion = c('MultiRegion','MiddleEast','SouthAmerica','SoutheastAsia',
                                                                 'SouthAsia','Africa','Oceania','CentralAmerica'))) %>% 
  mutate(early_discontinuation = ifelse(br_studystatus == 'Stopped early', TRUE, FALSE),
         early_discontinuation_completed_vs_stoppedearly = case_when(
           br_studystatus == 'Completed' ~ FALSE,
           br_studystatus == 'Stopped early' ~ TRUE,
           TRUE ~ NA
         )) %>%
  mutate(br_time_until_resultsreport_or_present_inmonths = case_when(
    br_studystatus != 'Completed' ~ NA_real_,
    were_results_reported ~ as.period(results_first_submitted_date - primary_completion_date) / months(1),
    TRUE ~ as.period(ymd('20200101') - primary_completion_date) / months(1)
  )) %>%
  mutate(br_censor_were_results_reported = as.numeric(were_results_reported)) %>%
  mutate(br_were_results_reported_within_2year = case_when(
    br_studystatus != 'Completed' ~ NA,
    primary_completion_date >= ymd('20170101') ~ NA, # we only consider trials completed >=2 years ago (we should later change this to not be hard coded)
    were_results_reported & (br_time_until_resultsreport_or_present_inmonths <= 24) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(br_were_results_reported_within_1year = case_when(
    br_studystatus != 'Completed' ~ NA,
    primary_completion_date >= ymd('20180101') ~ NA, # we only consider trials completed >=1 year ago
    were_results_reported & (br_time_until_resultsreport_or_present_inmonths <= 12) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(
    USA_only_facilities = case_when(
      all_countries == 'UnitedStates' ~ TRUE,
      is.na(all_countries) ~ NA,
      TRUE ~ FALSE),
    USA_any_facilities = case_when(
      is.na(all_countries) ~ NA,
      grepl(pattern = 'UnitedStates', x = all_countries) ~ TRUE,
      TRUE ~ FALSE),
    NorthAmerica_only_facilities = case_when(
      is.na(all_regions) ~ NA,
      all_regions == 'NorthAmerica' ~ TRUE,
      TRUE ~ FALSE),
    NorthAmerica_any_facilities = case_when(
      is.na(all_regions) ~ NA,
      grepl(pattern = 'NorthAmerica', x = all_regions) ~ TRUE,
      TRUE ~ FALSE)) %>%
  mutate(neither3regions = pmap_lgl(list(!!! rlang::syms(c("NorthAmerica","Europe","EastAsia"))),
                                    function(...) ! any(sapply(list(...), function(i) i)))) %>% 
  mutate(new_industry_any3_ref_nih = fct_relevel(industry_any3, 'NIH'),
         new_industry_any3_ref_other = fct_relevel(industry_any3, 'Other')) %>%
  mutate(new_industry_any2b_ref_usgovt = fct_relevel(industry_any2b, 'US.Govt'),
         new_industry_any2b_ref_other = fct_relevel(industry_any2b, 'Other')) %>%
  mutate(br_gni_lmic_hic_only = ifelse(br_gni_lmic_hic == 'LMIC and HIC', NA_character_, br_gni_lmic_hic)) %>%
  mutate(br_gni_hic_text = case_when(
    is.na(br_gni_hic) ~ NA_character_,
    br_gni_hic ~ 'IncludesHIC',
    ! br_gni_hic ~ 'OnlyLMIC'
  )) %>%
  left_join(fdaaa_tracker_data,
            by = c('nct_id' = 'fdaaatracker_registry_id')) %>%
  mutate(br_phase4_ref_ph3 = fct_relevel(br_phase4, 'Phase 2/3-3'),
         br_phase4_ref_ph1 = fct_relevel(br_phase4, 'Phase 1'))



full_comparison_df <- 
  full_comparison_df %>%
  mutate(new_arms = Hmisc::cut2(x = number_of_arms, cuts = c(1,2,3,Inf)),
         new_arms2 = Hmisc::cut2(x = number_of_arms, cuts = c(2, Inf))) %>%
  mutate(new_enroll = Hmisc::cut2(x = enrollment, cuts = c(10, 50, 100, 500, 1000, Inf))) %>% 
  mutate(new_enroll2 = Hmisc::cut2(x = enrollment, cuts = c(100, Inf))) %>%
  mutate(enroll_10 = enrollment / 10,
         enroll_20 = enrollment / 20) %>%
  mutate(new_first_submit = year(study_first_submitted_date)) %>%
  mutate(new_num_regions = Hmisc::cut2(x = num_regions, cuts = c(1,2,3, Inf)),
         new_num_regions2 = Hmisc::cut2(x = num_regions, cuts = c(2, Inf)),
         new_num_facilities = Hmisc::cut2(x = num_facilities, cuts = c(1,2,3,10,Inf)),
         new_num_facilities2 = Hmisc::cut2(x = num_facilities, cuts = c(2, Inf))) %>%
  mutate(new_br_phase2 = fct_explicit_na(br_phase2, na_level = 'Unknown Phase')) %>%
  mutate(new_br_phase2 = fct_relevel(new_br_phase2, 'Phase 2')) %>%
  mutate(primary_purpose = fct_relevel(primary_purpose, 'Treatment'),
         new_primary_purpose_treatment = fct_collapse(.f = primary_purpose, # should be able to use group_other here rather than use setdiff
                                                      Treatment = 'Treatment', Prevention = 'Prevention', `Basic Science` = 'Basic Science',  # but there is a known forcats bug right now
                                                      Other = setdiff(primary_purpose, c("Treatment", "Prevention", "Basic Science")), group_other = FALSE), # generates "Unknown levels in `f`"
         new_primary_purpose_treatment2 = fct_lump(primary_purpose, n = 3)) %>%
  mutate(new_actduration = Hmisc::cut2(actual_duration, c(0, 10, 20, 30, 40, 50, Inf))) %>%
  mutate(br_masking2 = fct_relevel(br_masking2, 'None')) %>%
  # mutate(num_disease_groups = pmap_dbl(list(!!! rlang::syms(cols_disease)),
  #                                      function(...) sum(...))) %>%
  # mutate(single_disease_group = pmap_chr(list(!!! rlang::syms(cols_disease)),
  #                                        function(...) paste0(cols_disease[which(x = c(...))], collapse = ','))) %>%
  # mutate(single_disease_group = case_when(
  #   num_disease_groups > 1 ~ 'multi_disease',
  #   TRUE ~ single_disease_group)) %>%
  # mutate(br_singleregion4 = fct_lump(br_singleregion, n = 4)) %>% 
  mutate(br_singleregion4 = fct_collapse(.f = br_singleregion,
                                         NorthAmerica = 'NorthAmerica', Europe = 'Europe', EastAsia = 'EastAsia', 
                                         OtherAndMultiRegion = c('MultiRegion','MiddleEast','SouthAmerica','SoutheastAsia',
                                                                 'SouthAsia','Africa','Oceania','CentralAmerica'))) %>% 
  mutate(early_discontinuation = ifelse(br_studystatus == 'Stopped early', TRUE, FALSE),
         early_discontinuation_completed_vs_stoppedearly = case_when(
           br_studystatus == 'Completed' ~ FALSE,
           br_studystatus == 'Stopped early' ~ TRUE,
           TRUE ~ NA
         )) %>%
  mutate(br_time_until_resultsreport_or_present_inmonths = case_when(
    br_studystatus != 'Completed' ~ NA_real_,
    were_results_reported ~ as.period(results_first_submitted_date - primary_completion_date) / months(1),
    TRUE ~ as.period(ymd('20200101') - primary_completion_date) / months(1)
  )) %>%
  mutate(br_censor_were_results_reported = as.numeric(were_results_reported)) %>%
  mutate(br_were_results_reported_within_2year = case_when(
    br_studystatus != 'Completed' ~ NA,
    primary_completion_date >= ymd('20180101') ~ NA, # we only consider trials completed >=2 years ago (we should later change this to not be hard coded)
    were_results_reported & (br_time_until_resultsreport_or_present_inmonths <= 24) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(br_were_results_reported_within_1year = case_when(
    br_studystatus != 'Completed' ~ NA,
    primary_completion_date >= ymd('20190101') ~ NA, # we only consider trials completed >=1 year ago
    were_results_reported & (br_time_until_resultsreport_or_present_inmonths <= 12) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(
    USA_only_facilities = case_when(
      all_countries == 'UnitedStates' ~ TRUE,
      is.na(all_countries) ~ NA,
      TRUE ~ FALSE),
    USA_any_facilities = case_when(
      is.na(all_countries) ~ NA,
      grepl(pattern = 'UnitedStates', x = all_countries) ~ TRUE,
      TRUE ~ FALSE),
    NorthAmerica_only_facilities = case_when(
      is.na(all_regions) ~ NA,
      all_regions == 'NorthAmerica' ~ TRUE,
      TRUE ~ FALSE),
    NorthAmerica_any_facilities = case_when(
      is.na(all_regions) ~ NA,
      grepl(pattern = 'NorthAmerica', x = all_regions) ~ TRUE,
      TRUE ~ FALSE)
  ) %>%
  mutate(neither3regions = pmap_lgl(list(!!! rlang::syms(c("NorthAmerica","Europe","EastAsia"))),
                                    function(...) ! any(sapply(list(...), function(i) i)))) %>% 
  mutate(new_industry_any3_ref_nih = fct_relevel(industry_any3, 'NIH'),
         new_industry_any3_ref_other = fct_relevel(industry_any3, 'Other')) %>%
  mutate(new_industry_any2b_ref_usgovt = fct_relevel(industry_any2b, 'US.Govt'),
         new_industry_any2b_ref_other = fct_relevel(industry_any2b, 'Other')) %>%
  mutate(br_gni_lmic_hic_only = ifelse(br_gni_lmic_hic == 'LMIC and HIC', NA_character_, br_gni_lmic_hic)) %>%
  mutate(br_gni_hic_text = case_when(
    is.na(br_gni_hic) ~ NA_character_,
    br_gni_hic ~ 'IncludesHIC',
    ! br_gni_hic ~ 'OnlyLMIC'
  )) %>%
  left_join(fdaaa_tracker_data,
            by = c('nct_id' = 'fdaaatracker_registry_id')) %>% 
  mutate(br_phase4_ref_ph3 = fct_relevel(br_phase4, 'Phase 2/3-3'),
         br_phase4_ref_ph1 = fct_relevel(br_phase4, 'Phase 1'))



# For a variety of purposes...
full_spec_combined_df <- 
  bind_rows(gi = full_gi_df,
            comparison = full_comparison_df,
            .id = 'specialty_source')

# ** Special thing for gi**
"""full_gi_df <- 
  full_gi_df %>%
  mutate(neither_cvd_mvt_dem_mal = pmap_lgl(list(cvd, malignancy, movement, dementia), 
                                            function(a, b, c, d) ! any(a, b, c, d, na.rm=TRUE)))"""


# -------------------------------------------------------------------------#
# ---------                 CLEANING UP STOPS HERE                 -------------
# -------------------------------------------------------------------------#

# -------------------------------------------------------------------------#
# ---------                 ACTUAL FIGURES                    -------------
# -------------------------------------------------------------------------#

# for table1
nct_gi <- full_gi_df %>% pull(nct_id)
nct_gi_global <- full_gi_df %>% pull(nct_id)
nct_gi_USA <- full_gi_df %>% pull(nct_id)
nct_gi_early_global <- full_gi_df %>% filter(bintime == '2007_2013') %>% pull(nct_id)
nct_gi_late_global <- full_gi_df %>% filter(bintime == '2014_2019') %>% pull(nct_id)
nct_gi_early_USA <- full_gi_df %>% filter(bintime == '2007_2013') %>% filter(USA_only_facilities) %>% pull(nct_id)
nct_gi_late_USA <- full_gi_df %>% filter(bintime == '2014_2019') %>% filter(USA_only_facilities) %>% pull(nct_id)

# for table2
nct_gi_other_global <- full_gi_df %>% filter(industry_any2b == 'Other') %>% pull(nct_id)
nct_gi_industry_global <- full_gi_df %>% filter(industry_any2b == 'Industry') %>% pull(nct_id)
nct_gi_usgovt_global <- full_gi_df %>% filter(industry_any2b == 'US.Govt') %>% pull(nct_id)
nct_gi_other_USA <- full_gi_df %>% filter(industry_any2b == 'Other') %>% filter(USA_only_facilities) %>% pull(nct_id)
nct_gi_industry_USA <- full_gi_df %>% filter(industry_any2b == 'Industry') %>% filter(USA_only_facilities) %>% pull(nct_id)
nct_gi_usgovt_USA <- full_gi_df %>% filter(industry_any2b == 'US.Govt') %>% filter(USA_only_facilities) %>% pull(nct_id)

# for table4 (these need to be updated to reflect global/USA at some point in the future when you get to that)
nct_gi_good_3c_good <- full_gi_df %>% filter(br_good3c_single_random) %>% pull(nct_id)
nctgi_good_3c_poor <- full_gi_df %>% filter(!br_good3c_single_random) %>% pull(nct_id)

nct_gi_good_4c_good <- full_gi_df %>% filter(br_good4c_double_random) %>% pull(nct_id)
nct_gi_good_4c_poor <- full_gi_df %>% filter(!br_good4c_double_random) %>% pull(nct_id)


# -------------------------------------------------------------------------#
# ---------           THIS SECTION IS A MESS OF GRAPHS JUST FYI        -------------
# -------------------------------------------------------------------------#

#trying to plot anatomic locations
gilocations_test <- full_gi_df %>%
  select(one_of(cols_location))
summarize(gilocations_test, count=n())

gicountslocation_test <- gilocations_test %>%
  gather() %>%
  group_by(key) %>%
  summarise(value = sum(value=="TRUE"))

#using name table
name_locations <- name_table[8:18,]
name_locations <- name_locations[
  with(name_locations, order(code_name)),
  ]

name_loc_original <- name_locations$original_name

#LOCATIONS GRAPH
ggplot(gicountslocation_test, aes(x=reorder(name_loc_original, -value), y=value)) + 
  geom_bar(stat="identity", fill = "firebrick4") + labs(x = "anatomical location", y = "count") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

#then overall disease process

gioverall_test <- full_gi_df %>%
  select(one_of(cols_disease_full))

gioverall_test <- gioverall_test %>%
  gather() %>%
  group_by(key) %>%
  summarise(value = sum(value=="TRUE"))

gioverall_test_in <- filter(gioverall_test, value > 258)
gioverall_test_out <- filter(gioverall_test, value <= 258)

gioverall_in_other <- gioverall_test_in[gioverall_test_in$key %in% 'other', 'value']
gioverall_add <- as.integer(sum(gioverall_test_out$value) + gioverall_in_count$value)
gioverall_test_in$value[gioverall_test_in$key == "other"] <- gioverall_add

#PLOT

name_disease <- name_table %>%
  filter(blue_vs_yellow == 'b')
name_disease <- name_disease[
  with(name_disease, order(code_name)),
  ]

gioverall_test_in <- gioverall_test_in %>% 
  rename(
    code_name = key
  )

gioverall_test_in <- gioverall_test_in %>%
  left_join(name_disease, by = "code_name")

gioverall_test_out <- gioverall_test_out %>% 
  rename(
    code_name = key
  )

gioverall_test_out <- gioverall_test_out %>%
  left_join(name_disease, by = "code_name")

gioverall_test <- gioverall_test %>% 
  rename(
    code_name = key
  )

gioverall_test <- gioverall_test %>%
  left_join(name_disease, by = "code_name")

ggplot(gioverall_test_in, aes(x=reorder(longname, -value), y=value)) + 
  geom_bar(stat="identity", fill = "firebrick4") + labs(x = "disease process", y = "count") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

ggplot(gioverall_test_out, aes(x=reorder(longname, -value), y=value)) + 
  geom_bar(stat="identity", fill = "firebrick4") + labs(x = "disease process", y = "count") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))


#Caring about neoplasm, versus infection, versus other


#separate into major groups
gioverall_other <- gioverall_test %>%
  filter(code_name != "infection_any" | code_name != "neoplasia_disease")

gioverall_neoplasm <- gioverall_test %>%
  filter(code_name == "infection_any" | code_name == "neoplasia_disease") %>%
  add_row(code_name = "other", value = sum(gioverall_other$value))

# Compute percentages
gioverall_neoplasm <- gioverall_neoplasm %>%
  mutate(fraction = value / sum(value)) %>%
  mutate(ymax = cumsum(fraction)) %>%
  mutate(ymin = c(0, head(ymax, n = -1))) %>%
  mutate(labelPosition = (ymax + ymin) / 2) %>%
  mutate(label = paste0(longname, ":\n ", round(fraction * 100, digits = 1), "%")) %>%
  mutate(longname = c("Infection", "Neoplasia", "Other"))




# Make the plot

ggplot(gioverall_neoplasm, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=longname)) +
  geom_rect() +
  geom_label(x=1.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette="YlOrRd") +
  scale_color_brewer(palette="YlOrRd") +
  coord_polar(theta="y") +
  xlim(c(-2, 4)) +
  theme_void() +
  theme(legend.position = "none")


#LASTLY, anatomy x neoplasm versus other

data_long <- full_gi_df %>%
  select(nct_id, location_esophagus:neoplasia_disease)

data_others <- full_gi_df %>%
  select(abdominal_hernia:other) %>%
  transmute(others = rowSums(data_others) > 0)

data_long <- cbind(data_long, data_others)

data_long2 <- tidyr::gather(data_long, key = location, value = loc_cat, -nct_id, -infection_any, -neoplasia_disease, -others)

data_long2$location[data_long2$location == "location_esophagus"] <- "Esophagus"
data_long2$location[data_long2$location == "location_stomach"] <- "Stomach"
data_long2$location[data_long2$location == "location_small_intestine"] <- "Small Intestine"
data_long2$location[data_long2$location == "location_colon_rectum"] <- "Colon & Rectum"
data_long2$location[data_long2$location == "location_anus"] <- "Anus"
data_long2$location[data_long2$location == "location_liver"] <- "Liver"
data_long2$location[data_long2$location == "location_biliarytract"] <- "Biliary Tract"
data_long2$location[data_long2$location == "location_gallbladder"] <- "Gallbladder"
data_long2$location[data_long2$location == "location_pancreas"] <- "Pancreas"
data_long2$location[data_long2$location == "location_peritoneum"] <- "Peritoneum"
data_long2$location[data_long2$location == "location_notspecified"] <- "Not Specified"


data_long3 <- tidyr::gather(data_long2, key = disease, value = dis_cat, -nct_id, -location, -loc_cat)

data_long3$disease[data_long3$disease == "infection_any"] <- "Infectious Process"
data_long3$disease[data_long3$disease == "neoplasia_disease"] <- "Neoplasia"
data_long3$disease[data_long3$disease == "others"] <- "Other"

#unique(data_long3$disease)

data_long_true <- data_long3 %>%
  filter(loc_cat == 'TRUE' & dis_cat == "TRUE")

ggplot(data = data_long_true) +
  geom_bar(mapping = aes(x = fct_infreq(location), fill = disease)) +
  scale_fill_brewer(palette="YlOrRd") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 45, hjust = 1)) +
  labs(
    y = "Count",
    x = "Location"
  )

#inverse

ggplot(data = data_long_true) +
  geom_bar(mapping = aes(x = fct_infreq(disease), fill = location)) +
  scale_fill_brewer(palette="Spectral") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 45, hjust = 1)) +
  labs(
    y = "Count",
    x = "Disease"
  )


#by proportion
data_long_true$location <- factor(data_long_true$location, levels = c("Pancreas", "Peritoneum", "Colon & Rectum", "Biliary Tract", "Esophagus", "Stomach", "Liver", "Anus", "Gallbladder", "Small Intestine", "Not Specified"))
data_long_true$disease <- factor(data_long_true$disease, levels = c("Other", "Infectious Process", "Neoplasia"))
ggplot(data = data_long_true) +
  geom_bar(mapping = aes(x = location, fill = disease),
           position = "fill") +
  scale_fill_brewer(palette="YlOrRd") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 45, hjust = 1)) +
  labs(
    y = "Proportion",
    x = "Location"
  )

#the inverse
ggplot(data = data_long_true) +
  geom_bar(mapping = aes(x = disease, fill = location),
           position = "fill") +
  scale_fill_brewer(palette="Spectral") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 45, hjust = 1)) +
  labs(
    y = "Proportion",
    x = "Disease Process"
  )


#all_disease_cols <- c(cols_location, cols_disease_full, spec_disease)
#cols_disease <- c(cols_disease_full, spec_disease)

# -------------------------------------------------------------------------#
# ---------           BELOW: BRANDON'S CODES FOR REGRESSION       -------------
# -------------------------------------------------------------------------#

# -------------------------------------------------------------------------#
# ---------           Do the Imputations Up Front        -------------
# -------------------------------------------------------------------------#

br_mice_methods_generation <- function(dataset, binary = 'logreg', multi = 'polyreg', continuous = 'norm', exclude_variables = NULL) {
  
  # use this function to generate an input methods vector for use with mice::mice() for multiple imputation
  # use methods(mice) to see list of available methods
  # example call: 
  # methods_input_imp_gi <- br_mice_methods_generation(full_gi_imp_vars_df, 
  #                                                     binary = 'logreg', multi = 'polyreg', continuous = 'norm')
  #
  
  input_classes <- sapply(dataset, class) 
  unique_classes <- unique(input_classes)
  if(any(unique_classes %nin% c('factor', 'numeric'))) stop("The mice algorithm requires only factor and numeric data, no logicals, etc")
  
  lookup_method_vector = c(continuous, binary, multi)
  names(lookup_method_vector) <- c('numeric', 'br_binary', 'br_multi')
  
  input_types <- 
    sapply(dataset, function(icol) {
      if(class(icol) == 'numeric') return('numeric')
      if(length(levels(icol)) > 2) return('br_multi')
      if(length(levels(icol)) == 2) return('br_binary') else(stop("Cannot have factor variables with only 1 level..."))
    })
  
  input_names <- names(input_types)
  output_methods <- lookup_method_vector[input_types]
  names(output_methods) <- input_names
  
  if(!is.null(exclude_variables)) output_methods[names(output_methods) %in% exclude_variables] <- ''
  
  return(output_methods)
}


# make some "filter columns" that we will use to cheat and do easy regression in the future and filter out certain rows based on these values by
# including them in the regression but excluding them from the 

# We don't need nor do we want to impute any of the variables that are dependent variables in our regression analyses



depvars_all <- 
  full_gi_df %>%
  select(nct_id,
         starts_with('br_good'), 
         br_time_until_resultsreport_or_present_inmonths,
         br_censor_were_results_reported,
         br_were_results_reported_within_1year,
         br_were_results_reported_within_1year,
         br_censor_earlydiscontinuation,
         early_discontinuation_completed_vs_stoppedearly,
         br_trialduration) %>%
  colnames() 

# collect any independent variable that might be used to regress in the final models...           
explvars_all <-
  quos(
    specialty_source,
    # primary_purpose,
    new_primary_purpose_treatment,
    interv_combo1_behavioral,
    interv_combo1_device,
    interv_combo1_drugs_biologics_or_supplements,
    interv_combo1_procedure,
    interv_combo1_other,
    # phase,
    # br_phase2,
    br_phase4_ref_ph3,
    new_arms,
    new_enroll,
    # new_enroll2,
    # enroll_10,
    # new_first_submit,
    bintime,
    # lead_agency_class, 
    # industry_any2,
    industry_any2b,
    br_masking2,
    br_allocation,
    has_dmc,
    # child,
    # br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
    # br_good3c_single_random,
    # br_good4c_double_random,
    # NorthAmerica,
    # Europe,
    # EastAsia,
    # reg_other3,
    # br_gni_lmic_hic_only,
    br_gni_hic,
    # new_num_regions,
    # new_num_regions2,
    # new_num_facilities,
    new_num_facilities2,
    # br_studystatus, # this doesn't make mechanistic since to "explain" design using a downstream feature
    # disease_other8, # use this if you only want to look at the top 8 diseases and bin the rest into "other"
    !!! rlang::syms(cols_disease_in_order[1:8]) # only look at top n dz in regression
  ) %>%
  sapply(quo_name) # turn into strings



# isolate the variables that we actually wish to use for our data
full_gi_imp_vars_df <- 
  full_gi_df %>% 
  select(one_of(depvars_all, explvars_all)) 

imp_char_log_vecs <- 
  sapply(full_gi_imp_vars_df, class)

full_gi_imp_vars_df <- 
  full_gi_imp_vars_df %>%
  mutate_at(.vars = vars(one_of(names(imp_char_log_vecs[imp_char_log_vecs %in% c('character', 'logical')]))), # mice seems not to work if these aren't factors
            .funs = list(~ factor(.)))

# take a look at what kind of missingness we're dealing with here...
br_mdpattern_gi <- mice::md.pattern(full_gi_imp_vars_df, plot = TRUE, rotate.names = TRUE)
br_mdpattern_gi
br_mdpattern_gi %>% # how many NAs is this by percentage
  {.[nrow(.), ] / nrow(full_gi_imp_vars_df)} %>%
  {. * 100} %>% 
  round(1)

# *** Need to a test first to show that some of the data is not missing completely at random (chisq?) 
# One way to do it would be to loop over all the variables that have missing data, and for each to form a regression to see if any of the other
# variables are significant at predicting the outcome variable (1 = missing, 0 = not missing). There are also some chi-square methods for categorical
# and then Kruskal Wallis for continuous data. finalfit::missing_compare() is supposed to help with this but it doesn't work...

# let's create an input methods vector and a prediction matrix
methods(mice) # to see available methods...
# for binary can consider whether we want to do bootstrapped logistic regressions...probably not since I can't do it for the polyreg as well. 
methods_input_imp_gi <- br_mice_methods_generation(full_gi_imp_vars_df, 
                                                      binary = 'logreg', multi = 'polyreg', continuous = 'norm',
                                                      exclude_variables = depvars_all)
data.frame(input_method = methods_input_imp_gi) %>% tibble::rownames_to_column() %>% filter(input_method != '')

predmatrix_input_imp_gi <- 
  mice::quickpred(full_gi_imp_vars_df, 
                  exclude = depvars_all,  # don't use the things we're trying to predict to predict the supposed "explanatory" variables...
                  include = explvars_all)
predmatrix_input_imp_gi[depvars_all, ] <- 0 # don't predict the things that we are trying to predict

# make sure everything that has missing values has a method that is appropriate
cbind(methods_input_imp_gi %>% as.matrix(),
      full_gi_imp_vars_df %>% summarise_all(list(~ sum(is.na(.)) / n())) %>% as.matrix() %>% t() %>% round(3),
      full_gi_imp_vars_df %>% summarise_all(list(~ class(.))) %>% as.matrix() %>% t()
)

# do the imputation! 
imp1_full_gi_raw <- 
  mice::mice(full_gi_imp_vars_df, 
             # maxit = 10,
             # m = 50,
             maxit = 5,
             m = 5,
             predictorMatrix = predmatrix_input_imp_gi,
             method = methods_input_imp_gi,
             seed = 20)

# add the other variables that I could later filter on if I like...
imp1_full_gi_all_long <- 
  mice::complete(imp1_full_gi_raw, action = 'long', include = TRUE) %>%
  mutate(nct_id = as.character(nct_id)) %>% 
  left_join(full_gi_df %>% select(- one_of(setdiff(c(depvars_all, explvars_all), 'nct_id'))),
            by = 'nct_id') %>%
  mutate_if(~ is.character(.) | is.logical(.),
            ~ as.factor(.)) %>% 
  left_join(fdaaa_tracker_data,
            by = c('nct_id' = 'fdaaatracker_registry_id'))


# repeat for full_combined_df *******************************************
# isolate the variables that we actually wish to use for our data
full_combined_imp_vars_df <- 
  full_spec_combined_df %>% 
  select(one_of(depvars_all, explvars_all)) %>%
  select(- one_of(all_disease_cols)) # disease labeling useless when analyzing non-specialty trials

imp_char_log_vecs <- 
  sapply(full_combined_imp_vars_df, class)

full_combined_imp_vars_df <- 
  full_combined_imp_vars_df %>%
  mutate_at(.vars = vars(one_of(names(imp_char_log_vecs[imp_char_log_vecs %in% c('character', 'logical')]))), # mice seems not to work if these aren't factors
            .funs = list(~ factor(.)))

# take a look at what kind of missingness we're dealing with here...
br_mdpattern_combined <- mice::md.pattern(full_combined_imp_vars_df, plot = TRUE, rotate.names = TRUE)
br_mdpattern_combined
br_mdpattern_combined %>% # how many NAs is this by percentage
  {.[nrow(.), ] / nrow(full_combined_imp_vars_df)} %>%
  {. * 100} %>% 
  round(1)

# let's create an input methods vector and a prediction matrix
methods(mice) # to see available methods...
# for binary can consider whether we want to do bootstrapped logistic regressions...probably not since I can't do it for the polyreg as well. 
methods_input_imp_combined <- br_mice_methods_generation(full_combined_imp_vars_df, 
                                                         binary = 'logreg', multi = 'polyreg', continuous = 'norm',
                                                         exclude_variables = depvars_all)
data.frame(input_method = methods_input_imp_combined) %>% tibble::rownames_to_column() %>% filter(input_method != '')

predmatrix_input_imp_combined <- 
  mice::quickpred(full_combined_imp_vars_df, 
                  exclude = depvars_all,  # don't use the things we're trying to predict to predict the supposed "explanatory" variables...
                  include = explvars_all)
predmatrix_input_imp_combined[depvars_all, ] <- 0 # don't predict the things that we are trying to predict

# make sure everything that has missing values has a method that is appropriate
cbind(methods_input_imp_combined %>% as.matrix(),
      full_combined_imp_vars_df %>% summarise_all(list(~ sum(is.na(.)) / n())) %>% as.matrix() %>% t() %>% round(3),
      full_combined_imp_vars_df %>% summarise_all(list(~ class(.))) %>% as.matrix() %>% t()
)

# do the imputation! This will take a long time! probably ~1-2 minutes for each maxit x m combo, so if 5 and 5, this step would take ~ 25-50 minutes. 
imp1_full_combined_raw <- 
  mice::mice(full_combined_imp_vars_df, 
             # maxit = 10,
             # m = 50,
             maxit = 5,
             m = 5,
             predictorMatrix = predmatrix_input_imp_combined,
             method = methods_input_imp_combined,
             seed = 20)

# add the other variables that I could later filter on if I like...
imp1_full_combined_all_long <- 
  mice::complete(imp1_full_combined_raw, action = 'long', include = TRUE) %>%
  mutate(nct_id = as.character(nct_id)) %>% 
  left_join(full_spec_combined_df %>% select(- one_of(setdiff(c(depvars_all, explvars_all), 'nct_id'))),
            by = 'nct_id') %>%
  mutate_if(~ is.character(.) | is.logical(.),
            ~ as.factor(.)) %>% 
  left_join(fdaaa_tracker_data,
            by = c('nct_id' = 'fdaaatracker_registry_id'))


# ---------------------------------------------------------------------------------------

# General Settings:
remove_exp_narm <- TRUE

include_comparison_combined_analysis <- TRUE


# Table 0 ------------------------------------------------------------

table0_combined_comparison_list <-
  lapply(c(TRUE, FALSE), function(ilogic) {
    full_spec_combined_df %>%
      bexplore_factors(dependent = specialty_source, 
                       primary_purpose,
                       new_primary_purpose_treatment,
                       interv_combo1_behavioral,
                       interv_combo1_device,
                       interv_combo1_drugs_biologics_or_supplements,
                       interv_combo1_procedure,
                       interv_combo1_other,
                       br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                       new_arms, 
                       new_enroll, 
                       new_first_submit,
                       bintime,
                       lead_agency_class, industry_any2, industry_any3, industry_any2b,
                       br_masking2,
                       br_allocation,
                       has_dmc,
                       # child,
                       br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                       NorthAmerica, US_facility,
                       Europe,
                       EastAsia,
                       br_gni_lmic_hic_only,
                       br_gni_hic,
                       reg_other3,
                       new_num_regions,
                       new_num_facilities,
                       br_studystatus,
                       # !!! rlang::syms(cols_disease),
                       pct = 'dependent', addxsqr = TRUE, dep_na.rm = TRUE, expl_na.rm = ilogic)
  })

table0_combined_comparison <- table0_combined_comparison_list[[1]]
table0_combined_comparison_withna <- table0_combined_comparison_list[[2]]

# ------------------------ Table 1 ---------------------------------

# make table for which disease sites 
table1_disease_total_global <-
  full_gi_df %>% 
  # select(one_of(cols_disease_in_order[1:5]), # if you only want to include a subset of the diseases in the table, use these...
  #        cols_disease_other5) %>%
  select(one_of(cols_disease_in_order), 
         one_of(cols_location_in_order),
         other) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  # filter(rowname %nin% c('other')) %>% 
  arrange(desc(V1)) %>%
  mutate(pct = round(V1 / nrow(full_gi_df), 3),
         cumpct = round(cumsum(V1 / nrow(full_gi_df)), 3)) %>%
  rename('totaln' = 'V1') 

table1_disease_total_USA <-
  full_gi_df %>% 
  filter(USA_only_facilities) %>%
  # select(one_of(cols_disease_in_order[1:5]), # if you only want to include a subset of the diseases in the table, use these...
  #        cols_disease_other5) %>%
  select(one_of(cols_disease_in_order), 
         one_of(cols_location_in_order),
         other) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  # filter(rowname %nin% c('other')) %>% 
  arrange(desc(V1)) %>%
  mutate(pct = round(V1 / nrow(full_gi_df %>% filter(USA_only_facilities)), 3),
         cumpct = round(cumsum(V1 / nrow(full_gi_df %>% filter(USA_only_facilities))), 3)) %>%
  rename('totaln' = 'V1') 

# make early and late subtables
table1_disease_early_global <-
  full_gi_df %>% 
  filter(nct_id %in% nct_gi_early_global) %>% 
  select(one_of(all_disease_cols)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  arrange(desc(V1)) %>%
  # filter(rowname %nin% c('other')) %>%
  mutate(pct = round(V1 / length(nct_gi_early_global), 3),
         cumpct = round(cumsum(V1 / length(nct_gi_early_global)), 3)) %>%
  rename('totaln' = 'V1') 

table1_disease_late_global <-
  full_gi_df %>% 
  filter(nct_id %in% nct_gi_late_global) %>% 
  select(one_of(all_disease_cols)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  arrange(desc(V1)) %>%
  # filter(rowname %nin% c('other')) %>% 
  mutate(pct = round(V1 / length(nct_gi_late_global), 3),
         cumpct = round(cumsum(V1 / length(nct_gi_late_global)), 3)) %>%
  rename('totaln' = 'V1') 

table1_disease_early_USA <-
  full_gi_df %>% 
  filter(USA_only_facilities) %>% # this step is really superfluous now...
  filter(nct_id %in% nct_gi_early_USA) %>% 
  select(one_of(all_disease_cols)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  arrange(desc(V1)) %>%
  # filter(rowname %nin% c('other')) %>%
  mutate(pct = round(V1 / length(nct_gi_early_USA), 3),
         cumpct = round(cumsum(V1 / length(nct_gi_early_USA)), 3)) %>%
  rename('totaln' = 'V1') 

table1_disease_late_USA <-
  full_gi_df %>% 
  filter(USA_only_facilities) %>%
  filter(nct_id %in% nct_gi_late_USA) %>% 
  select(one_of(all_disease_cols)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  arrange(desc(V1)) %>%
  # filter(rowname %nin% c('other')) %>% 
  mutate(pct = round(V1 / length(nct_gi_late_USA), 3),
         cumpct = round(cumsum(V1 / length(nct_gi_late_USA)), 3)) %>%
  rename('totaln' = 'V1') 

# make tables for enrollment
table1_disease_enrollment_raw_data <- 
  full_gi_df %>%
  filter(!is.na(enrollment)) %>% # <0.5% of the trials, but causes errors if we don't remove! 
  select(nct_id, enrollment, enrollment_type, industry_any2b, USA_only_facilities, br_studystatus, bintime, study_first_submitted_date, one_of(cols_disease_in_order), one_of(cols_location_in_order), other) %>%
  mutate(year_trial = year(study_first_submitted_date)) %>%
  mutate_at(.vars = vars(c(one_of(cols_disease_in_order), one_of(cols_location_in_order), other)), 
            .funs = funs(. * as.numeric(enrollment))) %>%
  pivot_longer(names_to = 'disease_subgroup', 
               cols = c(one_of(cols_disease_in_order), one_of(cols_location_in_order), 'other'), 
               values_to = 'disease_enrollment') 


table1_disease_enrollment_general_global <- 
  Reduce(f = function(a, b) left_join(a, b, by = 'disease_subgroup'),
         x = list(
           table1_disease_enrollment_raw_data %>% 
             filter(br_studystatus == 'Completed') %>%
             group_by(disease_subgroup) %>% 
             summarise(totalenrollment = sum(disease_enrollment)) %>% 
             arrange(desc(totalenrollment)),
           table1_disease_enrollment_raw_data %>%
             filter(br_studystatus == 'Completed') %>%
             filter(!is.na(bintime)) %>%
             group_by(disease_subgroup, bintime) %>%
             summarise(tenroll = sum(disease_enrollment)) %>%
             pivot_wider(names_from = bintime, values_from = tenroll) %>%
             rename(totalenrollment_early = `2007_2013`, 
                    totalenrollment_late = `2014_2019`),
           table1_disease_enrollment_raw_data %>%
             filter(br_studystatus == 'Completed') %>%
             filter(!is.na(industry_any2b)) %>%
             group_by(disease_subgroup, industry_any2b) %>%
             summarise(tenroll = sum(disease_enrollment)) %>%
             pivot_wider(names_from = industry_any2b, values_from = tenroll) %>%
             mutate(totalenrollment_sponsor = pmap_dbl(.l = list(Industry, `US.Govt`, Other), 
                                                       .f = function(a,b,c) sum(a,b,c)))
         )
  )

table1_disease_enrollment_general_USA <- 
  Reduce(f = function(a, b) left_join(a, b, by = 'disease_subgroup'),
         x = list(
           table1_disease_enrollment_raw_data %>% 
             filter(USA_only_facilities) %>%
             filter(br_studystatus == 'Completed') %>%
             group_by(disease_subgroup) %>% 
             summarise(totalenrollment = sum(disease_enrollment)) %>% 
             arrange(desc(totalenrollment)),
           table1_disease_enrollment_raw_data %>% 
             filter(USA_only_facilities) %>%
             filter(!is.na(bintime)) %>%
             filter(br_studystatus == 'Completed') %>%
             group_by(disease_subgroup, bintime) %>%
             summarise(tenroll = sum(disease_enrollment)) %>%
             pivot_wider(names_from = bintime, values_from = tenroll) %>%
             rename(totalenrollment_early = `2007_2013`, 
                    totalenrollment_late = `2014_2019`),
           table1_disease_enrollment_raw_data %>% 
             filter(USA_only_facilities) %>%
             filter(br_studystatus == 'Completed') %>%
             filter(!is.na(industry_any2b)) %>%
             group_by(disease_subgroup, industry_any2b) %>%
             summarise(tenroll = sum(disease_enrollment)) %>%
             pivot_wider(names_from = industry_any2b, values_from = tenroll) %>%
             mutate(totalenrollment_sponsor = pmap_dbl(.l = list(Industry, `US.Govt`, Other), 
                                                       .f = function(a,b,c) sum(a,b,c)))
         )
  )

table1_dz_enrollment_year_global <- 
  full_gi_df %>%
  filter(!is.na(enrollment)) %>%
  filter(br_studystatus == 'Completed') %>%
  mutate(anydzNA = pmap_lgl(.l = list(!!!rlang::syms(c(cols_disease_in_order, cols_location_in_order, 'other'))), # this part is completely unnecessary as there are no NA for dz
                            .f = function(...) any(sapply(X = list(...), FUN = function(i) is.na(i))))) %>% # but I keep it here as example for how to process 
  filter(!anydzNA) %>%
  mutate(year_trial = year(study_first_submitted_date)) %>% {
    a <- .; b <- .
    left_join(x = a %>% 
                group_by(year_trial) %>% 
                summarise(totalenrollment_year = sum(enrollment)),
              y = b %>%
                filter(!is.na(industry_any2b)) %>%
                group_by(year_trial, industry_any2b) %>% 
                summarise(tenroll = sum(enrollment)) %>% 
                pivot_wider(names_from = industry_any2b, values_from = tenroll) %>% 
                mutate(totalenrollment_sponsor_year = pmap_dbl(.l = list(Industry, `US.Govt`, Other),
                                                               .f = function(a,b,c) sum(a,b,c))),
              by = 'year_trial')
  }

table1_dz_enrollment_year_USA <- 
  full_gi_df %>%
  filter(USA_only_facilities) %>%
  filter(!is.na(enrollment)) %>%
  filter(br_studystatus == 'Completed') %>%
  mutate(anydzNA = pmap_lgl(.l = list(!!!rlang::syms(c(cols_disease_in_order, cols_location_in_order, 'other'))), # this part is completely unnecessary as there are no NA for dz
                            .f = function(...) any(sapply(X = list(...), FUN = function(i) is.na(i))))) %>% # but I keep it here as example for how to process 
  filter(!anydzNA) %>%
  mutate(year_trial = year(study_first_submitted_date)) %>% {
    a <- .; b <- .
    left_join(x = a %>% 
                group_by(year_trial) %>% 
                summarise(totalenrollment_year = sum(enrollment)),
              y = b %>%
                filter(!is.na(industry_any2b)) %>%
                group_by(year_trial, industry_any2b) %>% 
                summarise(tenroll = sum(enrollment)) %>% 
                pivot_wider(names_from = industry_any2b, values_from = tenroll) %>% 
                mutate(totalenrollment_sponsor_year = pmap_dbl(.l = list(Industry, `US.Govt`, Other),
                                                               .f = function(a,b,c) sum(a,b,c))),
              by = 'year_trial')
  }

table1_disease_enrollment_year_global <- 
  Reduce(f = function(a, b) left_join(a, b, by = c('disease_subgroup', 'year_trial')),
         x = list(
           table1_disease_enrollment_raw_data %>%
             filter(!is.na(year_trial)) %>%
             filter(br_studystatus == 'Completed') %>%
             group_by(disease_subgroup, year_trial) %>%
             summarise(enrollment_spec_diseaseyear = sum(disease_enrollment)) %>%
             left_join(table1_dz_enrollment_year_global %>% select(year_trial, totalenrollment_year), by = 'year_trial') %>%
             mutate(USA_vs_Global = 'global'),
           table1_disease_enrollment_raw_data %>%
             filter(!is.na(industry_any2b)) %>%
             filter(!is.na(year_trial)) %>%
             filter(br_studystatus == 'Completed') %>%
             group_by(disease_subgroup, year_trial, industry_any2b) %>%
             summarise(totalenrollment_sponsor_diseaseyear = sum(disease_enrollment)) %>%
             pivot_wider(names_from = industry_any2b, 
                         values_from = totalenrollment_sponsor_diseaseyear,
                         values_fill = list(totalenrollment_sponsor_diseaseyear = 0)) %>%
             mutate(totalenrollment_sponsor_diseaseyear = pmap_dbl(.l = list(Industry, `US.Govt`, Other), 
                                                                   .f = function(a,b,c) sum(a,b,c))) %>%
             left_join(table1_dz_enrollment_year_global %>% 
                         select(year_trial, Industry, `US.Govt`, Other) %>%
                         brename(c('Industry','US.Govt','Other'), c('Industry_year','US.Govt_year','Other_year')), by = 'year_trial')
         )) %>%
  ungroup()

table1_disease_enrollment_year_USA <- 
  Reduce(f = function(a, b) left_join(a, b, by = c('disease_subgroup', 'year_trial')),
         x = list(
           table1_disease_enrollment_raw_data %>%
             filter(USA_only_facilities) %>%
             filter(!is.na(year_trial)) %>%
             filter(br_studystatus == 'Completed') %>%
             group_by(disease_subgroup, year_trial) %>%
             summarise(enrollment_spec_diseaseyear = sum(disease_enrollment)) %>%
             left_join(table1_dz_enrollment_year_USA %>% select(year_trial, totalenrollment_year), by = 'year_trial') %>%
             mutate(USA_vs_Global = 'USA'),
           table1_disease_enrollment_raw_data %>%
             filter(USA_only_facilities) %>%
             filter(!is.na(industry_any2b)) %>%
             filter(!is.na(year_trial)) %>%
             filter(br_studystatus == 'Completed') %>%
             group_by(disease_subgroup, year_trial, industry_any2b) %>%
             summarise(totalenrollment_sponsor_diseaseyear = sum(disease_enrollment)) %>%
             pivot_wider(names_from = industry_any2b, 
                         values_from = totalenrollment_sponsor_diseaseyear, 
                         values_fill = list(totalenrollment_sponsor_diseaseyear = 0)) %>%
             mutate(totalenrollment_sponsor_diseaseyear = pmap_dbl(.l = list(Industry, `US.Govt`, Other), 
                                                                   .f = function(a,b,c) sum(a,b,c))) %>%
             left_join(table1_dz_enrollment_year_USA %>% 
                         select(year_trial, Industry, `US.Govt`, Other) %>%
                         brename(c('Industry','US.Govt','Other'), c('Industry_year','US.Govt_year','Other_year')), by = 'year_trial')
         )) %>%
  ungroup()

# bind the stuff together
# global
tbl1_p1_global <- 
  left_join(table1_disease_early_global,
            table1_disease_late_global,
            by = 'rowname',suffix = c('_early', '_late')) 

table1_disease_combo_global <-
  left_join(table1_disease_total_global %>% brename(names(.)[-1], paste0(names(.)[-1], '_total')), 
            tbl1_p1_global,
            by = 'rowname')

table1_xsqr_global <- table1_disease_combo_global %>% bchisqr(categories = rowname, totaln_early, totaln_late, type = 'wide')
table1_xsqrstat_global <- paste0('df=', table1_xsqr_global$parameter, 
                                 '; X^2=', round(table1_xsqr_global$statistic, 1), 
                                 '; pval=', formatC(table1_xsqr_global$p.value, digits = 3, format = 'e'))
table1_disease_combo_global <- 
  table1_disease_combo_global %>%
  mutate(xsqr  = table1_xsqrstat_global,
         final_total = sprintf('%s (%s)', totaln_total, pct_total* 100),
         final_early = sprintf('%s (%s)', totaln_early, pct_early* 100),
         final_late = sprintf('%s (%s)', totaln_late, pct_late* 100))

# USA
tbl1_p1_USA <- 
  left_join(table1_disease_early_USA,
            table1_disease_late_USA,
            by = 'rowname',suffix = c('_early', '_late')) 

table1_disease_combo_USA <-
  left_join(table1_disease_total_USA %>% brename(names(.)[-1], paste0(names(.)[-1], '_total')), 
            tbl1_p1_USA,
            by = 'rowname')

table1_xsqr_USA <- table1_disease_combo_USA %>% bchisqr(categories = rowname, totaln_early, totaln_late, type = 'wide')
table1_xsqrstat_USA <- paste0('df=', table1_xsqr_USA$parameter, 
                              '; X^2=', round(table1_xsqr_USA$statistic, 1), 
                              '; pval=', formatC(table1_xsqr_USA$p.value, digits = 3, format = 'e'))
table1_disease_combo_USA <- 
  table1_disease_combo_USA %>%
  mutate(xsqr  = table1_xsqrstat_USA,
         final_total = sprintf('%s (%s)', totaln_total, pct_total* 100),
         final_early = sprintf('%s (%s)', totaln_early, pct_early* 100),
         final_late = sprintf('%s (%s)', totaln_late, pct_late* 100))

# Part of Table 1, but easier...
table1_total_p1_list <- 
  lapply(c(TRUE, FALSE), function(ilogic) {
    full_gi_df %>%
      bexplore_factors(dependent = nct_gi, 
                       primary_purpose,
                       new_primary_purpose_treatment,
                       interv_combo1_behavioral,
                       interv_combo1_device,
                       interv_combo1_drugs_biologics_or_supplements,
                       interv_combo1_procedure,
                       interv_combo1_other,
                       br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                       new_arms, 
                       new_enroll, 
                       new_first_submit,
                       bintime,
                       lead_agency_class, industry_any3, industry_any2b,
                       br_masking2,
                       br_allocation,
                       # new_actduration, # hard to use this because can't really measure for trials that aren't done! 
                       has_dmc,
                       # child,
                       br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                       NorthAmerica, US_facility,
                       Europe,
                       EastAsia,
                       reg_other3,
                       br_gni_lmic_hic_only,
                       br_gni_hic,
                       new_num_regions,
                       new_num_facilities,
                       br_studystatus,
                       !!! rlang::syms(all_disease_cols), # pvalue for each
                       pct = 'dependent', expl_na.rm = ilogic)
  })

table1_total_p1 <- table1_total_p1_list[[1]]
table1_total_p1_withna <- table1_total_p1_list[[2]]

table1_total_gi <-
  bind_rows(table1_total_p1, 
            table1_disease_combo_global %>% 
              select(rowname, final_total) %>% 
              brename(c('rowname','final_total'), 
                      c('varlevels', colnames(table1_total_p1)[3])) %>%
              mutate(explvar = 'diseasegroup'))

table1_total_gi_withna <-
  bind_rows(table1_total_p1_withna, 
            table1_disease_combo_global %>% 
              select(rowname, final_total) %>% 
              brename(c('rowname','final_total'), 
                      c('varlevels', colnames(table1_total_p1_withna)[3])) %>%
              mutate(explvar = 'diseasegroup'))

table1_bintime_p1_list <-
  lapply(c(TRUE, FALSE), function(ilogic) {
    full_gi_df %>%
      bexplore_factors(dependent = bintime, 
                       primary_purpose,
                       new_primary_purpose_treatment,
                       interv_combo1_behavioral,
                       interv_combo1_device,
                       interv_combo1_drugs_biologics_or_supplements,
                       interv_combo1_procedure,
                       interv_combo1_other,
                       br_phase2, phase, new_br_phase2,br_phase4_ref_ph3,
                       new_arms, 
                       new_enroll, 
                       # new_first_submit,
                       # bintime,
                       lead_agency_class, industry_any3, industry_any2b,
                       br_masking2,
                       br_allocation,
                       # new_actduration, # hard to compare this because lots of NA for trials that aren't done! 
                       has_dmc,
                       # child,
                       br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                       NorthAmerica, US_facility,
                       Europe,
                       EastAsia,
                       reg_other3,
                       new_num_regions,
                       br_gni_lmic_hic_only,
                       br_gni_hic,
                       new_num_facilities,
                       br_studystatus,
                       !!! rlang::syms(all_disease_cols), # pvalue for each
                       pct = 'dependent', addxsqr = TRUE, expl_na.rm = ilogic)
  })

table1_bintime_p1 <- table1_bintime_p1_list[[1]]
table1_bintime_p1_withna <- table1_bintime_p1_list[[2]]

table1_bintime_gi <-
  bind_rows(table1_bintime_p1, 
            table1_disease_combo_global %>% 
              select(rowname, final_early, final_late, xsqr) %>% 
              brename(c('rowname','final_early','final_late'), 
                      c('varlevels', colnames(table1_bintime_p1)[3:4])) %>%
              mutate(explvar = 'diseasegroup'))

table1_bintime_gi_withna <-
  bind_rows(table1_bintime_p1_withna, 
            table1_disease_combo_global %>% 
              select(rowname, final_early, final_late, xsqr) %>% 
              brename(c('rowname','final_early','final_late'), 
                      c('varlevels', colnames(table1_bintime_p1_withna)[3:4])) %>%
              mutate(explvar = 'diseasegroup'))

if(include_comparison_combined_analysis) {
  table1_bintime_comparison_list <-
    lapply(c(TRUE, FALSE), function(ilogic) {
      full_comparison_df %>%
        bexplore_factors(dependent = bintime, 
                         primary_purpose,
                         new_primary_purpose_treatment,
                         interv_combo1_behavioral,
                         interv_combo1_device,
                         interv_combo1_drugs_biologics_or_supplements,
                         interv_combo1_procedure,
                         interv_combo1_other,
                         br_phase2, phase, new_br_phase2,br_phase4_ref_ph3,
                         new_arms, 
                         new_enroll, 
                         # new_first_submit,
                         # bintime,
                         lead_agency_class, industry_any3, industry_any2b,
                         br_masking2,
                         br_allocation,
                         # new_actduration, # hard to compare this because lots of NA for trials that aren't done! 
                         has_dmc,
                         # child,
                         br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                         NorthAmerica, US_facility,
                         Europe,
                         EastAsia,
                         reg_other3,
                         br_gni_lmic_hic_only,
                         br_gni_hic,
                         new_num_regions,
                         new_num_facilities,
                         br_studystatus,
                         # !!! rlang::syms(cols_disease), # pvalue for each
                         pct = 'dependent', addxsqr = TRUE, expl_na.rm = ilogic)
    })
  
  table1_bintime_comparison <- table1_bintime_comparison_list[[1]]
  table1_bintime_comparison_withna <- table1_bintime_comparison_list[[2]]
}

table1_total_gi
table1_bintime_gi


# generate new percentages
table1_bintime_p1_rowwisepct_list <-
  lapply(c(TRUE, FALSE), function(ilogic) {
    full_gi_df %>%
      bexplore_factors(dependent = bintime, 
                       primary_purpose,
                       new_primary_purpose_treatment,
                       interv_combo1_behavioral,
                       interv_combo1_device,
                       interv_combo1_drugs_biologics_or_supplements,
                       interv_combo1_procedure,
                       interv_combo1_other,
                       br_phase2, phase, new_br_phase2,br_phase4_ref_ph3,
                       new_arms, 
                       new_enroll, 
                       # new_first_submit,
                       # bintime,
                       lead_agency_class, industry_any3, industry_any2b,
                       br_masking2,
                       br_allocation,
                       # new_actduration, # hard to compare this because lots of NA for trials that aren't done! 
                       has_dmc,
                       # child,
                       br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                       NorthAmerica, US_facility,
                       Europe,
                       EastAsia,
                       reg_other3,
                       br_gni_lmic_hic_only,
                       br_gni_hic,
                       new_num_regions,
                       new_num_facilities,
                       br_studystatus,
                       !!! rlang::syms(all_disease_cols), # pvalue for each
                       pct = 'explanatory', addxsqr = TRUE, expl_na.rm = ilogic)
  })

table1_bintime_p1_rowwisepct <- table1_bintime_p1_rowwisepct_list[[1]]
table1_bintime_p1_rowwisepct_withna <- table1_bintime_p1_rowwisepct_list[[2]]

table1_bintime_rowwisepct_gi <- 
  bind_rows(table1_bintime_p1_rowwisepct, 
            table1_disease_combo_global %>% 
              select(rowname, final_early, final_late, xsqr) %>% 
              brename(c('rowname','final_early','final_late'), 
                      c('varlevels', colnames(table1_bintime_p1)[3:4])) %>%
              mutate(explvar = 'diseasegroup'))


table1_bintime_rowwisepct_gi_withna <- 
  bind_rows(table1_bintime_p1_rowwisepct_withna, 
            table1_disease_combo_global %>% 
              select(rowname, final_early, final_late, xsqr) %>% 
              brename(c('rowname','final_early','final_late'), 
                      c('varlevels', colnames(table1_bintime_p1_withna)[3:4])) %>%
              mutate(explvar = 'diseasegroup'))

if(include_comparison_combined_analysis) {
  table1_bintime_rowwisepct_comparison_list <-
    lapply(c(TRUE, FALSE), function(ilogic) {
      full_comparison_df %>%
        bexplore_factors(dependent = bintime, 
                         primary_purpose,
                         new_primary_purpose_treatment,
                         interv_combo1_behavioral,
                         interv_combo1_device,
                         interv_combo1_drugs_biologics_or_supplements,
                         interv_combo1_procedure,
                         interv_combo1_other,
                         br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                         new_arms, 
                         new_enroll, 
                         # new_first_submit,
                         # bintime,
                         lead_agency_class, industry_any3, industry_any2b,
                         br_masking2,
                         br_allocation,
                         # new_actduration, # hard to compare this because lots of NA for trials that aren't done! 
                         has_dmc,
                         # child,
                         br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                         NorthAmerica, US_facility,
                         Europe,
                         EastAsia,
                         reg_other3,
                         br_gni_lmic_hic_only,
                         br_gni_hic,
                         new_num_regions,
                         new_num_facilities,
                         br_studystatus,
                         # !!! rlang::syms(cols_disease), # pvalue for each
                         pct = 'explanatory', addxsqr = TRUE, expl_na.rm = ilogic)
    })
  
  table1_bintime_rowwisepct_comparison <- table1_bintime_rowwisepct_comparison_list[[1]]
  table1_bintime_rowwisepct_comparison_withna <- table1_bintime_rowwisepct_comparison_list[[2]]
}


# ----------- Compare International to USA GI Trials

table1_USAvsWorld_total_p1_list <- 
  lapply(c(TRUE, FALSE), function(ilogic) {
    full_gi_df %>%
      bexplore_factors(dependent = USA_only_facilities, 
                       primary_purpose,
                       new_primary_purpose_treatment,
                       interv_combo1_behavioral,
                       interv_combo1_device,
                       interv_combo1_drugs_biologics_or_supplements,
                       interv_combo1_procedure,
                       interv_combo1_other,
                       br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                       new_arms, 
                       new_enroll, 
                       new_first_submit,
                       bintime,
                       lead_agency_class, industry_any3, industry_any2b,
                       br_masking2,
                       br_allocation,
                       # new_actduration, # hard to use this because can't really measure for trials that aren't done! 
                       has_dmc,
                       # child,
                       br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                       NorthAmerica, US_facility,
                       Europe,
                       EastAsia,
                       reg_other3,
                       br_gni_lmic_hic_only,
                       br_gni_hic,
                       new_num_regions,
                       new_num_facilities,
                       br_studystatus,
                       !!! rlang::syms(all_disease_cols), # pvalue for each
                       pct = 'dependent', expl_na.rm = ilogic)
  })

table1_USAvsWorld_total_p1 <- table1_USAvsWorld_total_p1_list[[1]]
table1_USAvsWorld_total_p1_withna <- table1_USAvsWorld_total_p1_list[[2]]

# --------- for table 2, we do the same thing, but just use industry status

# which disease sites
table2_disease_industry <- 
  full_gi_df %>% 
  filter(nct_id %in% nct_gi_industry_global) %>%
  select(one_of(all_disease_cols)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  arrange(desc(V1)) %>%
  # filter(rowname %nin% c('other')) %>%
  mutate(pct = round(V1 / length(nct_gi_industry_global), 3),
         cumpct = round(cumsum(V1 / length(nct_gi_industry_global)), 3)) %>%
  rename('totaln' = 'V1') 

table2_disease_usgovt <-
  full_gi_df %>% 
  filter(nct_id %in% nct_gi_usgovt_global) %>% 
  select(one_of(all_disease_cols)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  arrange(desc(V1)) %>%
  # filter(rowname %nin% c('other')) %>%
  mutate(pct = round(V1 / length(nct_gi_usgovt_global), 3),
         cumpct = round(cumsum(V1 / length(nct_gi_usgovt_global)), 3)) %>%
  rename('totaln' = 'V1') 

table2_disease_other <-
  full_gi_df %>% 
  filter(nct_id %in% nct_gi_other_global) %>% 
  select(one_of(all_disease_cols)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  arrange(desc(V1)) %>%
  # filter(rowname %nin% c('other')) %>%
  mutate(pct = round(V1 / length(nct_gi_other_global), 3),
         cumpct = round(cumsum(V1 / length(nct_gi_other_global)), 3)) %>%
  rename('totaln' = 'V1') 



tbl2_p1 <- 
  left_join(table2_disease_usgovt,
            table2_disease_other,
            by = 'rowname',suffix = c('_usgovt', '_other')) 
table2_disease_combo <-
  left_join(table2_disease_industry %>% brename(names(.)[-1], paste0(names(.)[-1], '_industry')), 
            tbl2_p1,
            by = 'rowname')

table2_xsqr <- table2_disease_combo %>% bchisqr(categories = rowname, totaln_usgovt, totaln_industry, totaln_other, type = 'wide')
table2_xsqrstat <- paste0('df=', table2_xsqr$parameter, '; X^2=', round(table2_xsqr$statistic, 1), '; pval=', formatC(table2_xsqr$p.value, digits = 3, format = 'e'))
table2_disease_combo <- 
  table2_disease_combo %>%
  mutate(xsqr  = table2_xsqrstat,
         final_industry = sprintf('%s (%s)', totaln_industry, pct_industry* 100),
         final_usgovt = sprintf('%s (%s)', totaln_usgovt, pct_usgovt* 100),
         final_other = sprintf('%s (%s)', totaln_other, pct_other* 100))


table2_sponsor_p1_list <-
  lapply(c(TRUE, FALSE), function(ilogic) {
    full_gi_df %>%
      bexplore_factors(dependent = industry_any2b,
                       primary_purpose,
                       new_primary_purpose_treatment,
                       interv_combo1_behavioral,
                       interv_combo1_device,
                       interv_combo1_drugs_biologics_or_supplements,
                       interv_combo1_procedure,
                       interv_combo1_other,
                       br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                       new_arms, 
                       new_enroll, 
                       new_first_submit,
                       bintime,
                       # lead_agency_class, industry_any2, industry_any3,
                       br_masking2,
                       br_allocation,
                       has_dmc,
                       # child,
                       br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                       NorthAmerica, US_facility,
                       Europe,
                       EastAsia,
                       reg_other3,
                       br_gni_lmic_hic_only,
                       br_gni_hic,
                       new_num_regions,
                       new_num_facilities,
                       br_studystatus,
                       !!! rlang::syms(all_disease_cols),
                       pct = 'dependent', addxsqr = TRUE, dep_na.rm = TRUE, expl_na.rm = ilogic)
  })

table2_sponsor_p1 <- table2_sponsor_p1_list[[1]]
table2_sponsor_p1_withna <- table2_sponsor_p1_list[[2]]

table2_sponsor_gi <-
  bind_rows(table2_sponsor_p1, 
            table2_disease_combo %>% 
              select(rowname, final_industry, final_usgovt, final_other, xsqr) %>% 
              brename(c('rowname','final_industry', 'final_usgovt', 'final_other'),
                      c('varlevels', colnames(table2_sponsor_p1)[3:5])) %>%
              mutate(explvar = 'diseasegroup'))

table2_sponsor_gi_withna <-
  bind_rows(table2_sponsor_p1_withna, 
            table2_disease_combo %>% 
              select(rowname, final_industry, final_usgovt, final_other, xsqr) %>% 
              brename(c('rowname','final_industry', 'final_usgovt', 'final_other'),
                      c('varlevels', colnames(table2_sponsor_p1_withna)[3:5])) %>%
              mutate(explvar = 'diseasegroup'))

if(include_comparison_combined_analysis) {
  table2_sponsor_comparison_list <-
    lapply(c(TRUE, FALSE), function(ilogic) {
      full_comparison_df %>%
        bexplore_factors(dependent = industry_any2b, 
                         primary_purpose,
                         new_primary_purpose_treatment,
                         interv_combo1_behavioral,
                         interv_combo1_device,
                         interv_combo1_drugs_biologics_or_supplements,
                         interv_combo1_procedure,
                         interv_combo1_other,
                         br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                         new_arms, 
                         new_enroll, 
                         new_first_submit,
                         bintime,
                         # lead_agency_class, industry_any2, industry_any3,
                         br_masking2,
                         br_allocation,
                         has_dmc,
                         # child,
                         br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                         NorthAmerica, US_facility,
                         Europe,
                         EastAsia,
                         reg_other3,
                         br_gni_lmic_hic_only,
                         br_gni_hic,
                         new_num_regions,
                         new_num_facilities,
                         br_studystatus,
                         # !!! rlang::syms(cols_disease),
                         pct = 'dependent', addxsqr = TRUE, dep_na.rm = TRUE, expl_na.rm = ilogic)
    })
  
  table2_sponsor_comparison <- table2_sponsor_comparison_list[[1]]
  table2_sponsor_comparison_withna <- table2_sponsor_comparison_list[[2]]
}

table2_sponsor_gi
# table2_sponsor_comparison

table2_sponsor_p1_rowwisepct_list <-
  lapply(c(TRUE, FALSE), function(ilogic) {
    full_gi_df %>%
      bexplore_factors(dependent = industry_any2b, 
                       primary_purpose,
                       new_primary_purpose_treatment,
                       interv_combo1_behavioral,
                       interv_combo1_device,
                       interv_combo1_drugs_biologics_or_supplements,
                       interv_combo1_procedure,
                       interv_combo1_other,
                       br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                       new_arms, 
                       new_enroll, 
                       new_first_submit,
                       bintime,
                       # lead_agency_class, industry_any2, industry_any3,
                       br_masking2,
                       br_allocation,
                       has_dmc,
                       # child,
                       br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                       NorthAmerica, US_facility,
                       Europe,
                       EastAsia,
                       reg_other3,
                       br_gni_lmic_hic_only,
                       br_gni_hic,
                       new_num_regions,
                       new_num_facilities,
                       br_studystatus,
                       !!! rlang::syms(cols_disease),
                       pct = 'explanatory', addxsqr = TRUE, dep_na.rm = TRUE, expl_na.rm = ilogic)
  })

table2_sponsor_p1_rowwisepct <- table2_sponsor_p1_rowwisepct_list[[1]]
table2_sponsor_p1_rowwisepct_withna <- table2_sponsor_p1_rowwisepct_list[[2]]

table2_sponsor_rowwisepct_gi <-
  bind_rows(table2_sponsor_p1_rowwisepct, 
            table2_disease_combo %>% 
              select(rowname, final_industry, final_usgovt, final_other, xsqr) %>% 
              brename(c('rowname','final_industry', 'final_usgovt', 'final_other'),
                      c('varlevels', colnames(table2_sponsor_p1)[3:5])) %>%
              mutate(explvar = 'diseasegroup'))

table2_sponsor_rowwisepct_gi_withna <-
  bind_rows(table2_sponsor_p1_rowwisepct_withna, 
            table2_disease_combo %>% 
              select(rowname, final_industry, final_usgovt, final_other, xsqr) %>% 
              brename(c('rowname','final_industry', 'final_usgovt', 'final_other'),
                      c('varlevels', colnames(table2_sponsor_p1_withna)[3:5])) %>%
              mutate(explvar = 'diseasegroup'))

if(include_comparison_combined_analysis) {
  table2_sponsor_rowwisepct_comparison_list <-
    lapply(c(TRUE, FALSE), function(ilogic) {
      full_comparison_df %>%
        bexplore_factors(dependent = industry_any2b, 
                         primary_purpose,
                         new_primary_purpose_treatment,
                         interv_combo1_behavioral,
                         interv_combo1_device,
                         interv_combo1_drugs_biologics_or_supplements,
                         interv_combo1_procedure,
                         interv_combo1_other,
                         br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                         new_arms, 
                         new_enroll, 
                         new_first_submit,
                         bintime,
                         # lead_agency_class, industry_any2, industry_any3,
                         br_masking2,
                         br_allocation,
                         has_dmc,
                         # child,
                         br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                         NorthAmerica, US_facility,
                         Europe,
                         EastAsia,
                         reg_other3,
                         br_gni_lmic_hic_only,
                         br_gni_hic,
                         new_num_regions,
                         new_num_facilities,
                         br_studystatus,
                         # !!! rlang::syms(cols_disease),
                         pct = 'explanatory', addxsqr = TRUE, dep_na.rm = TRUE, expl_na.rm = ilogic)
    })
  
  table2_sponsor_rowwisepct_comparison <- table2_sponsor_rowwisepct_comparison_list[[1]]
  table2_sponsor_rowwisepct_comparison_withna <- table2_sponsor_rowwisepct_comparison_list[[2]]
}


# -- When you look at the time data, you can see that there are huge differences in whether trials were marking FDA vs non-FDA status
# -- That means this data is not reliably "false", and thus I think our analysis should exclude them

# ---------                   Table 3 Top Sponsors  --------------------
table3_sponsor_name_gi <- 
  full_gi_df %>%
  bcount(lead_agency_name, lead_agency_class, bintime) %>%
  select(-nn) %>% 
  mutate(bintime = case_when(
    bintime == '2014_2019' ~ 'late',
    bintime == '2007_2013' ~ 'early'
  )) %>%
  tidyr::spread(key = 'bintime', value = n) %>% 
  mutate(total = unlist(
    pmap(list(early,late),
         function(x, y) sum(x, y, na.rm = TRUE))
  )
  ) %>% 
  arrange(desc(total)) %>%
  head(100)

if(include_comparison_combined_analysis) {
  table3_sponsor_name_comparison <- 
    full_comparison_df %>%
    bcount(lead_agency_name, lead_agency_class, bintime) %>%
    select(-nn) %>% 
    mutate(bintime = case_when(
      bintime == '2014_2019' ~ 'late',
      bintime == '2007_2013' ~ 'early'
    )) %>%
    tidyr::spread(key = 'bintime', value = n) %>% 
    mutate(total = unlist(
      pmap(list(early,late),
           function(x, y) sum(x, y, na.rm = TRUE))
    )
    ) %>% 
    arrange(desc(total)) %>%
    head(100)
}

table3_sponsor_name_gi %>% print(n = 50)
# table3_sponsor_name_comparison %>% print(n = 50)


# -------------------- Table 4 -------------------------
# for this table, do same thing but for industry? could also do for design quality...
# look at data before and after 2017
print('good design')

# which disease sites
table4_disease_design_good <- 
  full_gi_df %>% 
  filter(nct_id %in% nct_gi_good_3c_good) %>%
  select(one_of(all_disease_cols)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  arrange(desc(V1)) %>%
  # filter(rowname %nin% c('other_disease')) %>%
  mutate(pct = round(V1 / length(nct_gi_good_3c_good), 3),
         cumpct = round(cumsum(V1 / length(nct_gi_good_3c_good)), 3)) %>%
  rename('totaln' = 'V1') 
#MARIJA HERE
table4_disease_design_poor <-
  full_gi_df %>% 
  filter(nct_id %in% nctgi_good_3c_poor) %>% 
  select(one_of(all_disease_cols)) %>%
  summarise_all(sum) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  arrange(desc(V1)) %>%
  # filter(rowname %nin% c('other_disease')) %>%
  mutate(pct = round(V1 / length(nctgi_good_3c_poor), 3),
         cumpct = round(cumsum(V1 / length(nctgi_good_3c_poor)), 3)) %>%
  rename('totaln' = 'V1') 


table4_disease_combo <-
  left_join(table4_disease_design_good,
            table4_disease_design_poor,
            by = 'rowname',suffix = c('_good', '_poor')) %>%
  mutate(pct_good = pmap_dbl(list(totaln_good, totaln_poor), function(good, poor) round(good / (good + poor), 3)),
         pct_poor = pmap_dbl(list(totaln_good, totaln_poor), function(good, poor) round(poor / (good + poor), 3))) %>%
  select(-cumpct_good, -cumpct_poor)


table4_xsqr <- table4_disease_combo %>% bchisqr(categories = rowname, totaln_good, totaln_poor, type = 'wide')
table4_xsqrstat <- paste0('df=', table4_xsqr$parameter, '; X^2=', round(table4_xsqr$statistic, 1), '; pval=', formatC(table4_xsqr$p.value, digits = 3, format = 'e'))
table4_disease_combo <- 
  table4_disease_combo %>%
  mutate(xsqr  = table4_xsqrstat,
         final_good = sprintf('%s (%s)', totaln_good, pct_good* 100),
         final_poor = sprintf('%s (%s)', totaln_poor, pct_poor* 100))

table4_design_p1_list <-
  lapply(c(TRUE, FALSE), function(ilogic) {
    full_gi_df %>%
      bexplore_factors(dependent = br_good3c_single_random, 
                       primary_purpose,
                       new_primary_purpose_treatment,
                       interv_combo1_behavioral,
                       interv_combo1_device,
                       interv_combo1_drugs_biologics_or_supplements,
                       interv_combo1_procedure,
                       interv_combo1_other,
                       br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                       new_arms, 
                       new_enroll, 
                       new_first_submit,
                       bintime,
                       lead_agency_class, industry_any2, industry_any3, industry_any2b,
                       # br_masking2, # We don't include these because they all share components w/ br_goodXc
                       # br_allocation,
                       # has_dmc,
                       # child,
                       # br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                       NorthAmerica, US_facility,
                       Europe,
                       EastAsia,
                       reg_other3,
                       br_gni_lmic_hic_only,
                       br_gni_hic,
                       new_num_regions,
                       new_num_facilities,
                       br_studystatus,
                       !!! rlang::syms(all_disease_cols),
                       pct = 'explanatory', addxsqr = TRUE, dep_na.rm = TRUE, expl_na.rm = ilogic)
  })

table4_design_p1 <- table4_design_p1_list[[1]]
table4_design_p1_withna <- table4_design_p1_list[[2]]

table4_design_gi <-
  bind_rows(table4_design_p1, 
            table4_disease_combo %>% 
              select(rowname, final_good, final_poor, xsqr) %>% 
              brename(c('rowname','final_good', 'final_poor'),
                      c('varlevels', colnames(table4_design_p1)[3:4])) %>%
              mutate(explvar = 'diseasegroup'))

table4_design_gi_withna <-
  bind_rows(table4_design_p1_withna, 
            table4_disease_combo %>% 
              select(rowname, final_good, final_poor, xsqr) %>% 
              brename(c('rowname','final_good', 'final_poor'),
                      c('varlevels', colnames(table4_design_p1_withna)[3:4])) %>%
              mutate(explvar = 'diseasegroup'))

if(include_comparison_combined_analysis) {
  table4_design_comparison_list <-
    lapply(c(TRUE, FALSE), function(ilogic) {
      full_comparison_df %>%
        bexplore_factors(dependent = br_good3c_single_random, 
                         primary_purpose,
                         new_primary_purpose_treatment,
                         interv_combo1_behavioral,
                         interv_combo1_device,
                         interv_combo1_drugs_biologics_or_supplements,
                         interv_combo1_procedure,
                         interv_combo1_other,
                         br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                         new_arms, 
                         new_enroll, 
                         new_first_submit,
                         bintime,
                         lead_agency_class, industry_any2, industry_any3, industry_any2b,
                         # br_masking2, # We don't include these because they all share components w/ br_goodXc
                         # br_allocation,
                         # has_dmc,
                         # child,
                         # br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                         NorthAmerica, US_facility,
                         Europe,
                         EastAsia,
                         reg_other3,
                         br_gni_lmic_hic_only,
                         br_gni_hic,
                         new_num_regions,
                         new_num_facilities,
                         br_studystatus,
                         # !!! rlang::syms(cols_disease),
                         pct = 'explanatory', addxsqr = TRUE, dep_na.rm = TRUE, expl_na.rm = ilogic)
    })
  
  table4_design_comparison <- table4_design_comparison_list[[1]]
  table4_design_comparison_withna <- table4_design_comparison_list[[2]]
}


table4_design_gi
# table4_design_comparison

table4_design_p1_colwisepct_list <-
  lapply(c(TRUE, FALSE), function(ilogic) {
    full_gi_df %>%
      bexplore_factors(dependent = br_good3c_single_random, 
                       primary_purpose,
                       new_primary_purpose_treatment,
                       interv_combo1_behavioral,
                       interv_combo1_device,
                       interv_combo1_drugs_biologics_or_supplements,
                       interv_combo1_procedure,
                       interv_combo1_other,
                       br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                       new_arms, 
                       new_enroll, 
                       new_first_submit,
                       bintime,
                       lead_agency_class, industry_any2, industry_any3, industry_any2b,
                       # br_masking2,
                       # br_allocation,
                       # has_dmc,
                       # child,
                       # br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                       NorthAmerica, US_facility,
                       Europe,
                       EastAsia,
                       reg_other3,
                       br_gni_lmic_hic_only,
                       br_gni_hic,
                       new_num_regions,
                       new_num_facilities,
                       br_studystatus,
                       !!! rlang::syms(all_disease_cols),
                       pct = 'dependent', addxsqr = TRUE, dep_na.rm = TRUE, expl_na.rm = ilogic)
  })

table4_design_p1_colwisepct <- table4_design_p1_colwisepct_list[[1]]
table4_design_p1_colwisepct_withna <- table4_design_p1_colwisepct_list[[2]]

table4_design_colwisepct_gi <-
  bind_rows(table4_design_p1_colwisepct, 
            table4_disease_combo %>% 
              select(rowname, final_good, final_poor, xsqr) %>% 
              brename(c('rowname','final_good', 'final_poor'),
                      c('varlevels', colnames(table4_design_p1)[3:4])) %>%
              mutate(explvar = 'diseasegroup'))

table4_design_colwisepct_gi_withna <-
  bind_rows(table4_design_p1_colwisepct_withna, 
            table4_disease_combo %>% 
              select(rowname, final_good, final_poor, xsqr) %>% 
              brename(c('rowname','final_good', 'final_poor'),
                      c('varlevels', colnames(table4_design_p1_withna)[3:4])) %>%
              mutate(explvar = 'diseasegroup'))

if(include_comparison_combined_analysis) {
  table4_design_colwisepct_comparison_list <-
    lapply(c(TRUE, FALSE), function(ilogic) {
      full_comparison_df %>%
        bexplore_factors(dependent = br_good3c_single_random, 
                         primary_purpose,
                         new_primary_purpose_treatment,
                         interv_combo1_behavioral,
                         interv_combo1_device,
                         interv_combo1_drugs_biologics_or_supplements,
                         interv_combo1_procedure,
                         interv_combo1_other,
                         br_phase2, phase, new_br_phase2, br_phase4_ref_ph3,
                         new_arms, 
                         new_enroll, 
                         new_first_submit,
                         bintime,
                         lead_agency_class, industry_any2, industry_any3, industry_any2b,
                         # br_masking2,
                         # br_allocation,
                         # has_dmc,
                         # child,
                         # br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
                         NorthAmerica, US_facility,
                         Europe,
                         EastAsia,
                         reg_other3,
                         br_gni_lmic_hic_only,
                         br_gni_hic,
                         new_num_regions,
                         new_num_facilities,
                         br_studystatus,
                         # !!! rlang::syms(cols_disease),
                         pct = 'dependent', addxsqr = TRUE, dep_na.rm = TRUE, expl_na.rm = ilogic)
    })
  
  table4_design_colwisepct_comparison <- table4_design_colwisepct_comparison_list[[1]]
  table4_design_colwisepct_comparison_withna <- table4_design_colwisepct_comparison_list[[2]]
}

# --------------------- Table 5 ------------------------------
# for this table, we look at regression output, this is just an example ... 

# in theory you could put all the different design variables here and loop over them to create regressions for each, it would be easy
depvar_design <-
  c('br_good3c_single_random')

explvars_design_all <-
  quos(
    specialty_source, # for comparing gi vs comparison
    # primary_purpose,
    new_primary_purpose_treatment,
    interv_combo1_behavioral,
    interv_combo1_device,
    interv_combo1_drugs_biologics_or_supplements,
    interv_combo1_procedure,
    interv_combo1_other,
    # phase,
    # br_phase2,
    # new_br_phase2,
    br_phase4_ref_ph3,
    # new_arms, 
    new_enroll,
    # new_enroll2,
    # enroll_10,
    # new_first_submit,
    bintime,
    # lead_agency_class, 
    # industry_any2,
    # industry_any3,
    industry_any2b,
    br_masking2,
    br_allocation,
    has_dmc,
    # child,
    # br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
    # br_good3c_single_random,
    # br_good4c_double_random,
    # NorthAmerica,
    # Europe,
    # EastAsia,
    # reg_other3,
    # br_gni_lmic_hic_only,
    br_gni_hic,
    # new_num_regions,
    # new_num_regions2,
    # new_num_facilities,
    new_num_facilities2,
    # br_studystatus, # this doesn't make mechanistic since to "explain" design using a downstream feature
    # disease_other8, # use this if you only want to look at the top 8 diseases and bin the rest into "other_disease"
    !!! rlang::syms(cols_disease_in_order[1:8]) # only look at top n dz in regression
  ) %>%
  sapply(quo_name) # turn into strings

#Duplicated for location <--- MARJ COME BACK TO THIS


explvars_design_gi <- 
  setdiff(explvars_design_all,
          c('specialty_source'))

explvars_design_combined <- 
  setdiff(explvars_design_all,
          c(cols_disease))

table5_design_regression_gi <-
  full_gi_df %>%
  buni_vs_full_glmtable(dependentvar = br_good3c_single_random, 
                        !!! rlang::syms(explvars_design_gi),
                        conflevel = 0.95, uni_to_multi_pval_cutoff = 0.99999999, 
                        uni_to_multi_stringency = c('any_under'), force_inclusion = NA)

if(include_comparison_combined_analysis) {
  table5_design_regression_combined <-
    full_spec_combined_df %>%
    buni_vs_full_glmtable(dependentvar = br_good3c_single_random, 
                          !!! rlang::syms(explvars_design_combined),
                          conflevel = 0.95, uni_to_multi_pval_cutoff = 0.99999999, 
                          uni_to_multi_stringency = c('any_under'), force_inclusion = NA)
}

table5_design_regression_gi
# table5_design_regression_combined

# use the imputed versions for gi
tbl_5_design_gi_impute_glm_list <- 
  full_gi_imp_vars_df %>%
  b_glm_imputation(dependentvar = br_good3c_single_random,
                   !!! rlang::syms(explvars_design_gi), 
                   input_methods = methods_input_imp_gi,
                   input_imp_object = imp1_full_gi_raw, conflevel = 0.95)

tbl_5_design_gi_impute_glm_output_raw <- tbl_5_design_gi_impute_glm_list$raw_output_table
tbl_5_design_gi_impute_glm_output_formatted <- tbl_5_design_gi_impute_glm_list$pooled_formatted_table
imp1_design_gi_impute_glm_model <- tbl_5_design_gi_impute_glm_list$impute_model_full

# use the imputed versions for combined
if(include_comparison_combined_analysis) {
  tbl_5_design_combined_impute_glm_list <- 
    full_combined_imp_vars_df %>%
    b_glm_imputation(dependentvar = br_good3c_single_random,
                     !!! rlang::syms(explvars_design_combined),
                     input_methods = methods_input_imp_gi,
                     input_imp_object = imp1_full_combined_raw, conflevel = 0.95)
  
  tbl_5_design_combined_impute_glm_output_raw <- tbl_5_design_combined_impute_glm_list$raw_output_table
  tbl_5_design_combined_impute_glm_output_formatted <- tbl_5_design_combined_impute_glm_list$pooled_formatted_table
  imp1_design_combined_impute_glm_model <- tbl_5_design_combined_impute_glm_list$impute_model_full
}
# --------------------- Table 6 ------------------------------

explvars_rr_all <-
  quos(
    specialty_source, # for comparing gi vs comparison
    # primary_purpose,
    new_primary_purpose_treatment,
    interv_combo1_behavioral,
    interv_combo1_device,
    interv_combo1_drugs_biologics_or_supplements,
    interv_combo1_procedure,
    interv_combo1_other,
    # phase,
    # br_phase2,
    # new_br_phase2,
    br_phase4_ref_ph3,
    # new_arms, 
    new_enroll,
    # new_enroll2,
    # enroll_10,
    # new_first_submit,
    bintime,
    # lead_agency_class, 
    # industry_any2,
    # industry_any3,
    industry_any2b,
    br_masking2,
    br_allocation,
    has_dmc,
    # child,
    # br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
    # br_good3c_single_random,
    # NorthAmerica,
    # Europe,
    # EastAsia,
    # reg_other3,
    # br_gni_lmic_hic_only,
    br_gni_hic,
    # new_num_regions,
    # new_num_regions2,
    # new_num_facilities,
    new_num_facilities2,
    # br_studystatus, # this doesn't make mechanistic since to "explain" design using a downstream feature
    # disease_other8, # use this if you only want to look at the top 8 diseases and bin the rest into "other_disease"
    !!! rlang::syms(cols_disease_in_order[1:8]) # only look at top n dz in regression
  ) %>%
  sapply(quo_name) # turn into strings

explvars_rr_gi <- 
  setdiff(explvars_rr_all,
          c('specialty_source'))

explvars_rr_combined <- 
  setdiff(explvars_rr_all,
          c(cols_disease))

# GLM - RR - List-wise Deletion versions -----------------------------------#

table6_resultsreport_regression_log_gi <-
  full_gi_df %>%
  filter(br_studystatus == 'Completed') %>%
  # filter(br_phase2 != 'Phase 1') %>% # they are not mandated to report results
  buni_vs_full_glmtable(dependentvar = br_were_results_reported_within_2year, 
                        !!! rlang::syms(explvars_rr_gi), 
                        conflevel = 0.95, uni_to_multi_pval_cutoff = 0.99999, 
                        uni_to_multi_stringency = c('any_under'), force_inclusion = NA)

if(include_comparison_combined_analysis) {
  table6_resultsreport_regression_log_combined <-
    full_spec_combined_df %>%  
    filter(br_studystatus == 'Completed') %>%
    # filter(br_phase2 != 'Phase 1') %>% # they are not mandated to report results
    buni_vs_full_glmtable(dependentvar = br_were_results_reported_within_2year, 
                          !!! rlang::syms(explvars_rr_combined),
                          conflevel = 0.95, uni_to_multi_pval_cutoff = 0.99999, 
                          uni_to_multi_stringency = c('any_under'), force_inclusion = NA) 
}
table6_resultsreport_regression_log_gi
# table6_resultsreport_regression_log_combined

# GLM - RR - Imputation versions -----------------------------------#
# take our long object, filter down the rows we want, then rebuild an imp object
imp1_full_gi_rr <- 
  imp1_full_gi_all_long %>%
  filter(br_studystatus == 'Completed') %>%
  as.mids()

imp1_full_gi_rr_pACT <- 
  imp1_full_gi_all_long %>%
  filter(fdaaatracker_is_pact) %>%
  filter(fdaaatracker_results_due) %>%
  as.mids()

if(include_comparison_combined_analysis) {
  imp1_full_combined_rr <- # this step takes a long time (5-6 minutes?)
    imp1_full_combined_all_long %>%
    filter(br_studystatus == 'Completed') %>%
    as.mids()
  
  imp1_full_combined_rr_pACT <- # this step takes a long time (5-6 minutes?)
    imp1_full_combined_all_long %>%
    filter(fdaaatracker_is_pact) %>%
    filter(fdaaatracker_results_due) %>%
    as.mids()
}

# for gi
tbl_6_rr_gi_impute_glm_list <- 
  imp1_full_gi_all_long %>%
  filter(.imp == 0) %>% 
  filter(br_studystatus == 'Completed') %>%
  b_glm_imputation(dependentvar = br_were_results_reported_within_2year,
                   !!! rlang::syms(explvars_rr_gi),
                   input_methods = methods_input_imp_gi,
                   input_imp_object = imp1_full_gi_rr,
                   conflevel = 0.95)

tbl_6_rr_gi_impute_glm_output_raw <- tbl_6_rr_gi_impute_glm_list$raw_output_table
tbl_6_rr_gi_impute_glm_output_formatted <- tbl_6_rr_gi_impute_glm_list$pooled_formatted_table

# tbl_6_rr_gi_impute_glm_list_pACT <- 
#   imp1_full_gi_all_long %>%
#   filter(.imp == 0) %>% 
#   filter(fdaaatracker_is_pact) %>%
#   filter(fdaaatracker_results_due) %>%
#   b_glm_imputation(dependentvar = fdaaatracker_has_results,
#                    !!! rlang::syms(explvars_rr_gi),
#                    input_methods = methods_input_imp_gi,
#                    input_imp_object = imp1_full_gi_rr_pACT,
#                    conflevel = 0.95)
# 
# tbl_6_rr_gi_impute_glm_output_raw_pACT <- tbl_6_rr_gi_impute_glm_list_pACT$raw_output_table
# tbl_6_rr_gi_impute_glm_output_formatted_pACT <- tbl_6_rr_gi_impute_glm_list_pACT$pooled_formatted_table

# for combined
if(include_comparison_combined_analysis) {
  tbl_6_rr_combined_impute_glm_list <- 
    imp1_full_combined_all_long %>%
    filter(.imp == 0) %>% 
    filter(br_studystatus == 'Completed') %>%
    b_glm_imputation(dependentvar = br_were_results_reported_within_2year,
                     !!! rlang::syms(explvars_rr_combined),
                     input_methods = methods_input_imp_gi,
                     input_imp_object = imp1_full_combined_rr,
                     conflevel = 0.95)
  
  tbl_6_rr_combined_impute_glm_output_raw <- tbl_6_rr_combined_impute_glm_list$raw_output_table
  tbl_6_rr_combined_impute_glm_output_formatted <- tbl_6_rr_combined_impute_glm_list$pooled_formatted_table
  
  tbl_6_rr_combined_impute_glm_list_pACT <- 
    imp1_full_combined_all_long %>%
    filter(.imp == 0) %>% 
    filter(fdaaatracker_is_pact) %>%
    filter(fdaaatracker_results_due) %>%
    b_glm_imputation(dependentvar = fdaaatracker_has_results,
                     !!! rlang::syms(explvars_rr_combined),
                     input_methods = methods_input_imp_gi,
                     input_imp_object = imp1_full_combined_rr_pACT,
                     conflevel = 0.95)
  
  tbl_6_rr_combined_impute_glm_output_raw_pACT <- tbl_6_rr_combined_impute_glm_list_pACT$raw_output_table
  tbl_6_rr_combined_impute_glm_output_formatted_pACT <- tbl_6_rr_combined_impute_glm_list_pACT$pooled_formatted_table
}

# COX - RR - List-wise Deletion versions -----------------------------------#

table6b_resultsreport_regression_cox_gi <-
  full_gi_df %>%
  filter(br_studystatus == 'Completed') %>%
  # filter(br_phase2 != 'Phase 1') %>% # they are not mandated to report results
  # filter(completion_date <= ymd('20160501')) %>% # they had two years to report their results...
  buni_vs_full_coxtable(timevariable = br_time_until_resultsreport_or_present_inmonths,  
                        censorvariable = br_censor_were_results_reported,
                        !!! rlang::syms(explvars_rr_gi),
                        conflevel = 0.95, uni_to_multi_pval_cutoff = 0.99999, 
                        uni_to_multi_stringency = c('any_under'), force_inclusion = NA)

if(include_comparison_combined_analysis) {
  table6b_resultsreport_regression_cox_combined <-
    full_spec_combined_df %>%
    filter(br_studystatus == 'Completed') %>%
    # filter(br_phase2 != 'Phase 1') %>% # they are not mandated to report results
    # filter(completion_date <= ymd('20160501')) %>% # they had two years to report their results...
    buni_vs_full_coxtable(timevariable = br_time_until_resultsreport_or_present_inmonths,  
                          censorvariable = br_censor_were_results_reported,
                          !!! rlang::syms(explvars_rr_combined),
                          conflevel = 0.95, uni_to_multi_pval_cutoff = 0.99999, 
                          uni_to_multi_stringency = c('any_under'), force_inclusion = NA)
}

table6b_resultsreport_regression_cox_gi
# table6b_resultsreport_regression_cox_combined

# COX - RR - Imputation versions -----------------------------------#
# (if you wanted to do this by creating a fresh imputation within the function, this is example code:)
# NB: I deleted all the prepartory code to generate the input matrix and the input methods vector, but the procedure is the typical manner
# tbl_gi_rr_impute_cox_list <- 
#     b_cox_imputation(.data = full_gi_df_reg_rr, 
#                      timevariable = !! rlang::sym(depvar_rr['timevar']), 
#                      censorvariable = !! rlang::sym(depvar_rr['censorvar']), 
#                      !!! rlang::syms(explvars_rr_gi),
#                      input_predictionmatrix = pred_imp1_gi_rr2, input_methods = meth_imp1_gi_rr, n_maxit = 5, n_m = 5, 
#                      mice_seed = 20, conflevel = 0.95)
# 
# tbl_gi_rr_impute_cox_output_raw <- tbl_gi_rr_impute_cox_list$raw_output_table
# tbl_gi_rr_impute_cox_output_formatted <- tbl_gi_rr_impute_cox_list$pooled_formatted_table
# imp1_gi_rr_impute_cox_model <- tbl_gi_rr_impute_cox_list$impute_model_full


# ** I need to change all of this so that we do the imputation at the full_gi_df level and at the full_comparison_df level, and then we just perform 
# ** The pooled analysis on those within the function. This is because if we have a small population that is in the regression, currently it is only
# ** calculating the imputation within that population, which ignores the bulk of the data that can also help us to inform the relationship between 
# ** The various variables, so we should first fill in the imputation, and then do the analyses. Another benefit of this is that it ensures that
# ** All the regressions within the analysis are using the same set of imputed datasets. So for the improved version of the regression function, 
# ** I can have one of the inputs simply be the already imputed data object (imp1). Actually, I should compare the difference in results...
# ** Maybe by imputing using the subsets, we are capturing/preserving certain relationships between variables that differ depending on the subpopulation
# ** we are studying (e.g. relationship between industry and blinding is not necessarily the same within psych and gi). So unless we are providing
# ** Psych and gitetric variables as covariates for the imputation, we wouldn't want to do a full-scale analysis at the top unless
# ** We were confident that the relationship was homogeneous...hmm. Interesting, I'm inclined to leave as is then. Maybe at least at the full_gi_df
# ** level we can do this, and we wouldn't extend beyond that (e.g. imputing the entire Bigtbl). 
#


tbl_6_rr_gi_impute_cox_list <- 
  imp1_full_gi_all_long %>%
  filter(.imp == 0) %>% 
  filter(br_studystatus == 'Completed') %>%
  # filter(br_phase2 != 'Phase 1') %>% # they are not mandated to report results
  # filter(completion_date <= ymd('20160501')) %>% # they had two years to report their results...
  b_cox_imputation(timevariable = br_time_until_resultsreport_or_present_inmonths, 
                   censorvariable = br_censor_were_results_reported, 
                   !!! rlang::syms(explvars_rr_gi),
                   input_methods = methods_input_imp_gi,
                   input_imp_object = imp1_full_gi_rr,
                   conflevel = 0.95)

tbl_6_rr_gi_impute_cox_output_raw <- tbl_6_rr_gi_impute_cox_list$raw_output_table
tbl_6_rr_gi_impute_cox_output_formatted <- tbl_6_rr_gi_impute_cox_list$pooled_formatted_table
imp1_gi_rr_impute_cox_model <- tbl_6_rr_gi_impute_cox_list$impute_model_full

if(include_comparison_combined_analysis) {
  tbl_6_rr_combined_impute_cox_list <-
    imp1_full_combined_all_long %>%
    filter(.imp == 0) %>%
    filter(br_studystatus == 'Completed') %>%
    # filter(br_phase2 != 'Phase 1') %>% # they are not mandated to report results
    # filter(completion_date <= ymd('20160501')) %>% # they had two years to report their results...
    b_cox_imputation(timevariable = br_time_until_resultsreport_or_present_inmonths,
                     censorvariable = br_censor_were_results_reported,
                     !!! rlang::syms(explvars_rr_combined),
                     input_methods = methods_input_imp_gi,
                     input_imp_object = imp1_full_combined_rr,
                     conflevel = 0.95)
  
  tbl_6_rr_combined_impute_cox_output_raw <- tbl_6_rr_combined_impute_cox_list$raw_output_table
  tbl_6_rr_combined_impute_cox_output_formatted <- tbl_6_rr_combined_impute_cox_list$pooled_formatted_table
  imp1_combined_rr_impute_cox_model <- tbl_6_rr_combined_impute_cox_list$impute_model_full
}

# --------------------- Table 7 ------------------------------

# set which variables we will use as covariates in this early discontinuation regression

explvars_early_all <-
  quos(
    specialty_source, # for comparing gi vs comparison
    # primary_purpose,
    new_primary_purpose_treatment,
    interv_combo1_behavioral,
    interv_combo1_device,
    interv_combo1_drugs_biologics_or_supplements,
    interv_combo1_procedure,
    interv_combo1_other,
    # phase,
    # br_phase2,
    # new_br_phase2,
    br_phase4_ref_ph3,
    # new_arms, 
    new_enroll,
    # new_enroll2,
    # enroll_10,
    # new_first_submit,
    bintime,
    # lead_agency_class, 
    # industry_any2,
    # industry_any3,
    industry_any2b,
    br_masking2,
    br_allocation,
    has_dmc,
    # child,
    # br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc,
    # br_good3c_single_random,
    # NorthAmerica,
    # Europe,
    # EastAsia,
    # reg_other3,
    # br_gni_lmic_hic_only,
    br_gni_hic,
    # new_num_regions,
    # new_num_regions2,
    # new_num_facilities,
    new_num_facilities2,
    # br_studystatus, # this doesn't make mechanistic since to "explain" design using a downstream feature
    # disease_other8, # use this if you only want to look at the top 8 diseases and bin the rest into "other_disease"
    !!! rlang::syms(cols_disease_in_order[1:8]) # only look at top n dz in regression
  ) %>%
  sapply(quo_name) # turn into strings

explvars_early_gi <- 
  setdiff(explvars_early_all,
          c('specialty_source'))

explvars_early_combined <- 
  setdiff(explvars_early_all,
          c(cols_disease))

# 7 - GLM - Early Discontinuation - List-wise Deletion versions -----------------------------------

table7_earlydiscontinuation_regression_gi <-
  full_gi_df %>%
  # filter(new_br_phase2 != 'Phase 1') %>% # their behavior/design is abnormal
  filter(br_trialduration >= 0) %>% # only include trials that lasted at least 0 day
  buni_vs_full_glmtable(dependentvar = early_discontinuation_completed_vs_stoppedearly, 
                        !!! rlang::syms(explvars_early_gi),
                        conflevel = 0.95, uni_to_multi_pval_cutoff = 0.99999, 
                        uni_to_multi_stringency = c('any_under'), force_inclusion = NA)

if(include_comparison_combined_analysis) {
  table7_earlydiscontinuation_regression_combined <-
    full_spec_combined_df %>%
    # filter(new_br_phase2 != 'Phase 1') %>% # their behavior/design is abnormal
    filter(br_trialduration >= 0) %>% # only include trials that lasted at least 0 day
    buni_vs_full_glmtable(dependentvar = early_discontinuation_completed_vs_stoppedearly, 
                          !!! rlang::syms(explvars_early_combined),
                          conflevel = 0.95, uni_to_multi_pval_cutoff = 0.99999, 
                          uni_to_multi_stringency = c('any_under'), force_inclusion = NA) 
}

table7_earlydiscontinuation_regression_gi
# table7_earlydiscontinuation_regression_combined

# 7 - GLM - Early Discontinuation - Imputation versions -----------------------------------

# take our long object, filter down the rows we want, then rebuild an imp object
imp1_full_gi_early <- 
  imp1_full_gi_all_long %>%
  # filter(new_br_phase2 != 'Phase 1') %>% # their behavior/design is abnormal
  filter(br_trialduration >= 0) %>% # only include trials that lasted at least 0 day
  as.mids()

if(include_comparison_combined_analysis) {
  imp1_full_combined_early <- 
    imp1_full_combined_all_long %>%
    # filter(new_br_phase2 != 'Phase 1') %>% # their behavior/design is abnormal
    filter(br_trialduration >= 0) %>% # only include trials that lasted at least 0 day
    as.mids()
}

# for gi
tbl_7_early_gi_impute_glm_list <- 
  imp1_full_gi_all_long %>%
  filter(.imp == 0) %>%
  # filter(new_br_phase2 != 'Phase 1') %>% # their behavior/design is abnormal
  filter(br_trialduration >= 0) %>% # only include trials that lasted at least 0 day
  b_glm_imputation(dependentvar = early_discontinuation_completed_vs_stoppedearly,
                   !!! rlang::syms(explvars_early_gi),
                   input_methods = methods_input_imp_gi,
                   input_imp_object = imp1_full_gi_early,
                   conflevel = 0.95)

tbl_7_early_gi_impute_glm_output_raw <- tbl_7_early_gi_impute_glm_list$raw_output_table
tbl_7_early_gi_impute_glm_output_formatted <- tbl_7_early_gi_impute_glm_list$pooled_formatted_table

# for combined
if(include_comparison_combined_analysis) {
  tbl_7_early_combined_impute_glm_list <- 
    imp1_full_combined_all_long %>%
    filter(.imp == 0) %>%
    # filter(new_br_phase2 != 'Phase 1') %>% # their behavior/design is abnormal
    filter(br_trialduration >= 0) %>% # only include trials that lasted at least 0 day
    b_glm_imputation(dependentvar = early_discontinuation_completed_vs_stoppedearly,
                     !!! rlang::syms(explvars_early_combined),
                     input_methods = methods_input_imp_gi,
                     input_imp_object = imp1_full_combined_early,
                     conflevel = 0.95)
  
  tbl_7_early_combined_impute_glm_output_raw <- tbl_7_early_combined_impute_glm_list$raw_output_table
  tbl_7_early_combined_impute_glm_output_formatted <- tbl_7_early_combined_impute_glm_list$pooled_formatted_table
}

# 7 - COX - Early Discontinuation - List-wise Deletion versions -----------------------------------

table7b_earlydiscontinuation_regression_cox_gi <-
  full_gi_df %>%
  # filter(new_br_phase2 != 'Phase 1') %>% # their behavior/design is abnormal
  filter(br_trialduration >= 0) %>% # only include trials that lasted at least 0 day
  buni_vs_full_coxtable(timevariable = br_trialduration,  
                        censorvariable = br_censor_earlydiscontinuation,
                        !!! rlang::syms(explvars_early_gi),
                        conflevel = 0.95, uni_to_multi_pval_cutoff = 0.99999, 
                        uni_to_multi_stringency = c('any_under'), force_inclusion = NA)

if(include_comparison_combined_analysis) {
  table7b_earlydiscontinuation_regression_cox_combined <-
    full_spec_combined_df %>%
    # filter(new_br_phase2 != 'Phase 1') %>% # their behavior/design is abnormal
    filter(br_trialduration >= 0) %>% # only include trials that lasted at least 0 day
    buni_vs_full_coxtable(timevariable = br_trialduration,  
                          censorvariable = br_censor_earlydiscontinuation,
                          !!! rlang::syms(explvars_early_combined),
                          conflevel = 0.95, uni_to_multi_pval_cutoff = 0.99999, 
                          uni_to_multi_stringency = c('any_under'), force_inclusion = NA) 
}

table7b_earlydiscontinuation_regression_cox_gi
# table7b_earlydiscontinuation_regression_cox_combined %>% print(n = Inf)

# 7 - COX - Early Discontinuation - Imputation versions -----------------------------------

tbl_7_early_gi_impute_cox_list <- 
  imp1_full_gi_all_long %>%
  filter(.imp == 0) %>% 
  # filter(new_br_phase2 != 'Phase 1') %>% # their behavior/design is abnormal
  filter(br_trialduration >= 0) %>% # only include trials that lasted at least 0 day
  b_cox_imputation(timevariable = br_trialduration, 
                   censorvariable = br_censor_earlydiscontinuation, 
                   !!! rlang::syms(explvars_early_gi),
                   input_methods = methods_input_imp_gi,
                   input_imp_object = imp1_full_gi_early,
                   conflevel = 0.95)

tbl_7_early_gi_impute_cox_output_raw <- tbl_7_early_gi_impute_cox_list$raw_output_table
tbl_7_early_gi_impute_cox_output_formatted <- tbl_7_early_gi_impute_cox_list$pooled_formatted_table
imp1_gi_early_impute_cox_model <- tbl_7_early_gi_impute_cox_list$impute_model_full

if(include_comparison_combined_analysis) {
  tbl_7_early_combined_impute_cox_list <- 
    imp1_full_combined_all_long %>%
    filter(.imp == 0) %>% 
    # filter(new_br_phase2 != 'Phase 1') %>% # their behavior/design is abnormal
    filter(br_trialduration >= 0) %>% # only include trials that lasted at least 0 day
    b_cox_imputation(timevariable = br_trialduration, 
                     censorvariable = br_censor_earlydiscontinuation, 
                     !!! rlang::syms(explvars_early_combined),
                     input_methods = methods_input_imp_gi,
                     input_imp_object = imp1_full_combined_early,
                     conflevel = 0.95)
  
  tbl_7_early_combined_impute_cox_output_raw <- tbl_7_early_combined_impute_cox_list$raw_output_table
  tbl_7_early_combined_impute_cox_output_formatted <- tbl_7_early_combined_impute_cox_list$pooled_formatted_table
  imp1_combined_early_impute_cox_model <- tbl_7_early_combined_impute_cox_list$impute_model_full
}


# -------------------------------------------------------------#
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  #
# ----------------     Annual Growth Stuff      ---------------#
#            FIRST FOR GLOBAL AND THEN FOR USA ONLY            #
# -------------------------------------------------------------#

fgi_df <- 
  full_gi_df %>%
  bpivotwider_single_factor_to_logical(column = 'industry_any2b', add_prefix = 'funding2b', makenames = TRUE) %>%
  mutate(year_trial = year(study_first_submitted_date)) 

# if(include_comparison_combined_analysis) { # Actually we'll do this regardless so that we can calculate the "percent of database" figures
fcomparison_df <- 
  full_comparison_df %>%
  bpivotwider_single_factor_to_logical(column = 'industry_any2b', add_prefix = 'funding2b', makenames = TRUE) %>%
  mutate(year_trial = year(study_first_submitted_date)) 
# }

# Global + gi
gi_trial_growth_global <-
  bgenerateSummaryGrowthDataStatistics(fgi_df,
                                       additional_columns = list(
                                         'sponsor' = c('funding2b_US.Govt', 'funding2b_Industry', 'funding2b_Other'),
                                         'region' = c('NorthAmerica', 'Europe', 'EastAsia', 'neither3regions'),
                                         'gnistatus' = 'br_gni_hic'
                                       ))

gi_trial_growth_data_global <- gi_trial_growth_global[["Data"]]
gi_trial_growth_statistics_global <- gi_trial_growth_global[["Statistics"]]

# Global + Comparison
# if(include_comparison_combined_analysis) { 
comparison_trial_growth_global <-
  bgenerateSummaryGrowthDataStatistics(fcomparison_df,
                                       additional_columns = list(
                                         'sponsor' = c('funding2b_US.Govt', 'funding2b_Industry', 'funding2b_Other'),
                                         'region' = c('NorthAmerica', 'Europe', 'EastAsia', 'neither3regions'),
                                         'gnistatus' = 'br_gni_hic'
                                       ))

comparison_trial_growth_data_global <- comparison_trial_growth_global[["Data"]]
comparison_trial_growth_statistics_global <- comparison_trial_growth_global[["Statistics"]]
# }

# USA + gi
gi_trial_growth_USA <-
  bgenerateSummaryGrowthDataStatistics(
    fgi_df %>% filter(USA_only_facilities),
    additional_columns = list(
      'sponsor' = c('funding2b_US.Govt', 'funding2b_Industry', 'funding2b_Other'),
      'region' = c('NorthAmerica', 'Europe', 'EastAsia', 'neither3regions'),
      'gnistatus' = 'br_gni_hic'
    )
  )

gi_trial_growth_data_USA <- gi_trial_growth_USA[["Data"]]
gi_trial_growth_statistics_USA <- gi_trial_growth_USA[["Statistics"]]

# USA + Comparison
# if(include_comparison_combined_analysis) {
comparison_trial_growth_USA <-
  bgenerateSummaryGrowthDataStatistics(
    fcomparison_df %>% filter(USA_only_facilities),
    additional_columns = list(
      'sponsor' = c('funding2b_US.Govt', 'funding2b_Industry', 'funding2b_Other'),
      'region' = c('NorthAmerica', 'Europe', 'EastAsia', 'neither3regions'),
      'gnistatus' = 'br_gni_hic'
    )
  )

comparison_trial_growth_data_USA <- comparison_trial_growth_USA[["Data"]]
comparison_trial_growth_statistics_USA <- comparison_trial_growth_USA[["Statistics"]]
# }


# ------------------------------------------------------------------------------#
# Tables for Brannon
btable_enrollment1 <- 
  full_gi_df %>% 
  filter(br_studystatus == 'Completed') %>%
  group_by(industry_any2b) %>%
  summarise(
    pct05 = quantile(enrollment, 0.05, na.rm = TRUE),
    pct10 = quantile(enrollment, 0.10, na.rm = TRUE),
    pct25 = quantile(enrollment, 0.25, na.rm = TRUE),
    pct50 = quantile(enrollment, 0.50, na.rm = TRUE),
    pct75 = quantile(enrollment, 0.75, na.rm = TRUE),
    pct90 = quantile(enrollment, 0.90, na.rm = TRUE),
    pct95 = quantile(enrollment, 0.95, na.rm = TRUE),
    num_trials = n()) %>%
  mutate(interquartilerange = pct75 - pct25) %>% 
  mutate(merge_name = industry_any2b)

btable_enrollment2 <- # I don't recommend using this data, later year trials are going to have smaller median enrollment 
  full_gi_df %>%   # because most of the large ones aren't completed yet (it takes time to enroll!)
  filter(br_studystatus == 'Completed') %>%
  mutate(year_trial = year(study_first_submitted_date)) %>%
  group_by(year_trial) %>%
  summarise(
    pct05 = quantile(enrollment, 0.05, na.rm = TRUE),
    pct10 = quantile(enrollment, 0.10, na.rm = TRUE),
    pct25 = quantile(enrollment, 0.25, na.rm = TRUE),
    pct50 = quantile(enrollment, 0.50, na.rm = TRUE),
    pct75 = quantile(enrollment, 0.75, na.rm = TRUE),
    pct90 = quantile(enrollment, 0.90, na.rm = TRUE),
    pct95 = quantile(enrollment, 0.95, na.rm = TRUE),
    num_trials = n()) %>%
  mutate(interquartilerange = pct75 - pct25) %>%
  mutate(merge_name = as.character(year_trial))

btable_enrollment <-
  bind_rows(btable_enrollment1, btable_enrollment2) %>%
  select(merge_name, industry_any2b, year_trial, everything())

btable_studystatus_enrollment <- 
  full_gi_df %>% 
  group_by(br_studystatus, overall_status) %>% 
  summarise(total_trials = n(),
            total_enroll = sum(enrollment, na.rm = TRUE)); btable_studystatus_enrollment

btable_completion_funding_apr30_2016 <- 
  full_gi_df %>% 
  filter(overall_status == 'Completed') %>% 
  count(primary_completion_date < ymd('20160501'),
        were_results_reported, 
        industry_any2b); btable_completion_funding_apr30_2016


# -------------------------------------------------------------------#
# -----------------            Figures             ------------------#
#                                                                    #
# -------------------------------------------------------------------#

bpadding <- function(num, width = 4, makepercent = FALSE, num_decimals = 2) {
  
  if(makepercent) {
    sprintf('%%%s.%sf', width - 1, num_decimals) %>% sprintf(num*100) %>% paste0('%')
  } else {
    sprintf('%%%s.%sf', width, num_decimals) %>% sprintf(num)
  }
}

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  # from here: https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
  # nice indicates the various base values from 1-10 that you can use...
  
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

# use this to see all the different settings you can set for theme
ggplot2::theme_get()


RColorBrewer::display.brewer.all() # see your color options...
RColorBrewer::display.brewer.pal(3, 'Set1')
RColorBrewer::brewer.pal(3, 'Set1') # test colors we could want...

subcolor13 <- '#2F4F4F' # dark slate gray
subcolor14 <- '#FFA500' # orange
subcolor15 <- '#00B2EE' # deep sky blue

g_industry_color5 <- c(subcolor13, subcolor14, subcolor15) # brannon JAMA colors
names(g_industry_color5) <- c('Industry', 'US.Govt', 'Other')

tableau_10_pal <-
  c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2",
    "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7",
    "#9C755F", "#BAB0AC")

scales::show_col(tableau_10_pal)

tableau_20_pal <- 
  ggthemes::ggthemes_data[["tableau"]][["color-palettes"]][["regular"]][["Tableau 20"]] %>% pull(value)

viridis_4_pal <- 
  viridis::viridis_pal()(4)

scales::show_col(viridis_4_pal)

par(mfrow=c(3,2))
scales::show_col(viridis::viridis_pal(option = 'A')(4)) # magma
scales::show_col(viridis::viridis_pal(option = 'B')(4)) # inferno
scales::show_col(viridis::viridis_pal(option = 'C')(4)) # plasma
scales::show_col(viridis::viridis_pal(option = 'D')(4)) # viridis; the default for viridis
scales::show_col(viridis::viridis_pal(option = 'E')(4)) # cividis
scales::show_col(tableau_10_pal[1:4])

par(mfrow=c(6,5))
ggthemes_palettes <- ggthemes::ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]
for (palname in names(ggthemes_palettes)) {
  pal <- ggthemes::tableau_color_pal(palname)
  max_n <- attr(pal, "max_n")
  scales::show_col(pal(max_n))
  title(main = palname)
}
dev.off()

bcolorviz(g_industry_color5) # using this palette from above for industry (JAMA colors)
# set colors for all of them
color1 <- 'firebrick3' 
color2 <- '#981B1E'
color1 <- '#981B1E'# looks similar to crimson and firebrick, but I think this is JAMA gi header?

subcolor1 <- '#2E2FE3'
subcolor2 <- '#700CBC'
subcolor3 <- '#AE0D7A'

subcolor4 <- '#C21460'
subcolor5 <- '#FE2712'
subcolor6 <- '#FC600A'

subcolor7 <- '#1F77B4' # tableau blue
subcolor8 <- '#FF7F0E' # tableau orange
subcolor9 <- '#2CA02C' # tableau green

# color wheel http://www.paletton.com/#uid=1000u0kpTmlh7umlUq3tLibC0cT
subcolor10 <- '#910A0A' # darker
subcolor11 <- '#BC2222' # firebrick +5 brightness
subcolor12 <- '#D04242' # lighter

subcolor13 <- '#2F4F4F' # dark slate gray
subcolor14 <- '#FFA500' # orange
subcolor15 <- '#00B2EE' # deep sky blue

subcolor16 <- '#00868B' # these are all shades of slate/blue/green
subcolor17 <- '#2F4F4F' #
subcolor18 <- '#B4CDCD' #
subcolor19 <- '#000000' #

subcolor20 <- '#436666' # lighter dark slate gray
subcolor21 <- '#FFBA39' # lighter orange
subcolor22 <- '#29C7FC' # lighter deep sky blue

subcolor23 <- '#AA5558' # Red
subcolor24 <- '#5E59A6' # Purple


g_industry_color1 <- c(subcolor1, subcolor2, subcolor3) # red-blue-purple
names(g_industry_color1) <- c('Industry', 'US.Govt', 'Other')
g_industry_color2 <- c(subcolor4, subcolor5, subcolor6) # red-orange
names(g_industry_color2) <- c('Industry', 'US.Govt', 'Other')
g_industry_color3 <- c(subcolor7, subcolor8, subcolor9) # tableau colors
names(g_industry_color3) <- c('Industry', 'US.Govt', 'Other')
g_industry_color4 <- c(subcolor10, subcolor11, subcolor12) # palette
names(g_industry_color4) <- c('Industry', 'US.Govt', 'Other')

g_industry_color5 <- c(subcolor13, subcolor14, subcolor15) # brannon JAMA colors
names(g_industry_color5) <- c('Industry', 'US.Govt', 'Other')
g_industry_color5b <- c(subcolor13, subcolor14, subcolor15, # brannon JAMA colors w/ light versions for Comparison
                        subcolor20, subcolor21, subcolor22)
names(g_industry_color5b) <- c('Industry_gi', 'US.Govt_gi', 'Other_gi',
                               'Industry_comparison', 'US.Govt_comparison', 'Other_comparison')
g_industry_alpha5 <- c('gi' = 1.0, 'comparison' = 0.3)

g_region_color6_old <- c(subcolor16, subcolor17, subcolor18, subcolor19) 
names(g_region_color6_old) <- c('NorthAmerica', 'Europe', 'EastAsia', 'noneabove')
g_region_color6 <- c(subcolor16, subcolor17, subcolor18, subcolor19) 
names(g_region_color6) <- c('NorthAmerica', 'Europe', 'EastAsia', 'neither3regions')
g_region_color6b <- c(subcolor16, subcolor17, subcolor18, subcolor19) 
names(g_region_color6b) <- c('NorthAmerica', 'Europe', 'EastAsia', 'OtherAndMultiRegion')

g_gni_color7 <- c(subcolor23, subcolor24)
names(g_gni_color7) <- c("hic", "lmiconly")
g_gni_alpha7 <- c('gi' = 1.0, 'comparison' = 0.5)

# Next Figure -----Bar Graph Total Number Trials Per Year ------------------------------

fig_df_1a_yearly_gi_global <- gi_trial_growth_data_global %>% select(year_trial, total_trials_all)
fig_df_1a_yearly_gi_USA <- gi_trial_growth_data_USA %>% select(year_trial, total_trials_all)
max_yearly_gi_global <- max(fig_df_1a_yearly_gi_global$total_trials_all)
max_yearly_gi_USA <- max(fig_df_1a_yearly_gi_USA$total_trials_all)

# if(include_comparison_combined_analysis) {
fig_df_1a_yearly_comparison_global <- comparison_trial_growth_data_global %>% select(year_trial, total_trials_all)
fig_df_1a_yearly_comparison_USA <- comparison_trial_growth_data_USA %>% select(year_trial, total_trials_all)
max_yearly_comparison_global <- max(fig_df_1a_yearly_comparison_global$total_trials_all)
max_yearly_comparison_USA <- max(fig_df_1a_yearly_comparison_USA$total_trials_all)
# }

gg_fig_1a_yearlytotal_gi_global <-
  ggplot(data = fig_df_1a_yearly_gi_global,
         aes(x = year_trial, y = total_trials_all)) + 
  geom_bar(stat = 'identity',
           fill = color1,
           width = 0.5) +
  geom_text(aes(label = total_trials_all),
            size = 3,
            vjust = -0.5) + 
  scale_x_continuous(breaks = c(2008:2017)) + 
  scale_y_continuous(breaks = seq(0, max_yearly_gi_global *1.5, roundUpNice(max_yearly_gi_global/8, nice = c(1,5,10))),
                     labels = function(label) bpadding(label, width = 4, makepercent = FALSE, num_decimals = 0)) +
  coord_cartesian(ylim = c(0, pretty(max_yearly_gi_global*9/8)[2])) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew gilogic\nTrials Per Year');gg_fig_1a_yearlytotal_gi_global

gg_fig_1a_yearlytotal_gi_USA <-
  ggplot(data = fig_df_1a_yearly_gi_USA,
         aes(x = year_trial, y = total_trials_all)) + 
  geom_bar(stat = 'identity',
           fill = color1,
           width = 0.5) +
  geom_text(aes(label = total_trials_all),
            size = 3,
            vjust = -0.5) + 
  scale_x_continuous(breaks = c(2008:2017)) +
  scale_y_continuous(breaks = seq(0, max_yearly_gi_USA *1.5, roundUpNice(max_yearly_gi_USA/8, nice = c(1,5,10))),
                     labels = function(label) bpadding(label, width = 4, makepercent = FALSE, num_decimals = 0)) +
  coord_cartesian(ylim = c(0, pretty(max_yearly_gi_USA*9/8)[2])) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew gilogic\nTrials Per Year');gg_fig_1a_yearlytotal_gi_USA

# if(include_comparison_combined_analysis) {
gg_fig_1a_yearlytotal_comparison_global <-
  ggplot(data = fig_df_1a_yearly_comparison_global,
         aes(x = year_trial, y = total_trials_all)) + 
  geom_bar(stat = 'identity',
           fill = color1,
           width = 0.5) +
  geom_text(aes(label = total_trials_all),
            size = 3,
            vjust = -0.5) + 
  scale_x_continuous(breaks = c(2008:2017)) + 
  scale_y_continuous(breaks = seq(0, max_yearly_comparison_global *1.5, roundUpNice(max_yearly_comparison_global/8, nice = c(1,5,10))),
                     labels = function(label) bpadding(label, width = 4, makepercent = FALSE, num_decimals = 0)) +
  coord_cartesian(ylim = c(0, pretty(max_yearly_comparison_global*9/8)[2])) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew Non-gilogic\nTrials Per Year');gg_fig_1a_yearlytotal_comparison_global

gg_fig_1a_yearlytotal_comparison_USA <-
  ggplot(data = fig_df_1a_yearly_comparison_USA,
         aes(x = year_trial, y = total_trials_all)) + 
  geom_bar(stat = 'identity',
           fill = color1,
           width = 0.5) +
  geom_text(aes(label = total_trials_all),
            size = 3,
            vjust = -0.5) + 
  scale_x_continuous(breaks = c(2008:2017)) + 
  scale_y_continuous(breaks = seq(0, max_yearly_comparison_USA *1.5, roundUpNice(max_yearly_comparison_USA/8, nice = c(1,5,10))),
                     labels = function(label) bpadding(label, width = 4, makepercent = FALSE, num_decimals = 0)) +
  coord_cartesian(ylim = c(0, pretty(max_yearly_comparison_USA*9/8)[2])) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew Non-gilogic\nTrials Per Year');gg_fig_1a_yearlytotal_comparison_USA
# }


# Next Figure -----Line graph gilogic trials as a percentage of all trials ------------------------------

# Global - data % of combined line graph 
fig_df_1c_yearly_gi_pct_all_global <- 
  left_join(gi_trial_growth_data_global %>% select(year_trial, total_trials_all),
            comparison_trial_growth_data_global %>% select(year_trial, total_trials_all),
            by = 'year_trial', suffix = c('_gi', '_comparison')) %>%
  mutate(total_trials_all_combined = total_trials_all_gi + total_trials_all_comparison) %>%
  mutate(gi_pct_all_combined = total_trials_all_gi / total_trials_all_combined) %>% 
  mutate(constant = 'constant') %>%
  mutate(label_gi_pct_all_combined = bpadding(num = gi_pct_all_combined, width = 6, makepercent = TRUE, num_decimals = 2))

fig_df_1c_yearly_comparison_pct_all_global <- 
  left_join(gi_trial_growth_data_global %>% select(year_trial, total_trials_all),
            comparison_trial_growth_data_global %>% select(year_trial, total_trials_all),
            by = 'year_trial', suffix = c('_gi', '_comparison')) %>%
  mutate(total_trials_all_combined = total_trials_all_gi + total_trials_all_comparison) %>%
  mutate(comparison_pct_all_combined = total_trials_all_comparison / total_trials_all_combined) %>% 
  mutate(constant = 'constant') %>%
  mutate(label_comparison_pct_all_combined = bpadding(num = comparison_pct_all_combined, width = 6, makepercent = TRUE, num_decimals = 2))

# USA - data % of combined line graph
fig_df_1c_yearly_gi_pct_all_USA <- 
  left_join(gi_trial_growth_data_USA %>% select(year_trial, total_trials_all),
            comparison_trial_growth_data_USA %>% select(year_trial, total_trials_all),
            by = 'year_trial', suffix = c('_gi', '_comparison')) %>%
  mutate(total_trials_all_combined = total_trials_all_gi + total_trials_all_comparison) %>%
  mutate(gi_pct_all_combined = total_trials_all_gi / total_trials_all_combined) %>% 
  mutate(constant = 'constant') %>%
  mutate(label_gi_pct_all_combined = bpadding(num = gi_pct_all_combined, width = 6, makepercent = TRUE, num_decimals = 2))

fig_df_1c_yearly_comparison_pct_all_USA <- 
  left_join(gi_trial_growth_data_USA %>% select(year_trial, total_trials_all),
            comparison_trial_growth_data_USA %>% select(year_trial, total_trials_all),
            by = 'year_trial', suffix = c('_gi', '_comparison')) %>%
  mutate(total_trials_all_combined = total_trials_all_gi + total_trials_all_comparison) %>%
  mutate(comparison_pct_all_combined = total_trials_all_comparison / total_trials_all_combined) %>% 
  mutate(constant = 'constant') %>%
  mutate(label_comparison_pct_all_combined = bpadding(num = comparison_pct_all_combined, width = 6, makepercent = TRUE, num_decimals = 2))

# Figures #
# Global - gi - % of combined line graph
gg_fig_1c_yearlypct_gi_combined_global <- 
  ggplot(data = fig_df_1c_yearly_gi_pct_all_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), 
             y = gi_pct_all_combined,
             group = constant)) + # not sure if it's a bug but we have to specify a group variable here for this
  geom_line(color = color1, 
            size = 2) +
  geom_point(fill = color1, 
             color = color1, 
             shape = 21, size = 5, alpha = 1) +
  geom_text(aes(label = label_gi_pct_all_combined),
            size = 3,
            vjust = -2.0,
            hjust = 0.35) + 
  scale_y_continuous(breaks = seq(0, 1, 0.04),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, .20)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', 
       y = '\nProportion of All\nClinical Trials'); gg_fig_1c_yearlypct_gi_combined_global

# Global - Comparison - % of combined line graph
# if(include_comparison_combined_analysis) {
gg_fig_1c_yearlypct_comparison_combined_global <- 
  ggplot(data = fig_df_1c_yearly_comparison_pct_all_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), 
             y = comparison_pct_all_combined,
             group = constant)) + # not sure if it's a bug but we have to specify a group variable here for this
  geom_line(color = color1, 
            size = 2) +
  geom_point(fill = color1, 
             color = color1, 
             shape = 21, size = 5, alpha = 1) +
  geom_text(aes(label = label_comparison_pct_all_combined),
            size = 3,
            vjust = -2.0,
            hjust = 0.35) + 
  scale_y_continuous(breaks = seq(0, 1, 0.04),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0.80, 1.0)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year'); gg_fig_1c_yearlypct_comparison_combined_global
# }

# USA - gi - % of combined line graph
gg_fig_1c_yearlypct_gi_combined_USA <- 
  ggplot(data = fig_df_1c_yearly_gi_pct_all_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), 
             y = gi_pct_all_combined,
             group = constant)) + # not sure if it's a bug but we have to specify a group variable here for this
  geom_line(color = color1, 
            size = 2) +
  geom_point(fill = color1, 
             color = color1, 
             shape = 21, size = 5, alpha = 1) +
  geom_text(aes(label = label_gi_pct_all_combined),
            size = 3,
            vjust = -2.0,
            hjust = 0.35) + 
  scale_y_continuous(breaks = seq(0, 1, 0.04),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0.0, .20)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year'); gg_fig_1c_yearlypct_gi_combined_USA

# USA - Comparison - % of combined line graph
# if(include_comparison_combined_analysis) {
gg_fig_1c_yearlypct_comparison_combined_USA <- 
  ggplot(data = fig_df_1c_yearly_comparison_pct_all_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), 
             y = comparison_pct_all_combined,
             group = constant)) + # not sure if it's a bug but we have to specify a group variable here for this
  geom_line(color = color1, 
            size = 2) +
  geom_point(fill = color1, 
             color = color1, 
             shape = 21, size = 5, alpha = 1) +
  geom_text(aes(label = label_comparison_pct_all_combined),
            size = 3,
            vjust = -2.0,
            hjust = 0.35) + 
  scale_y_continuous(breaks = seq(0, 1, 0.04),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0.8, 1.0)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year'); gg_fig_1c_yearlypct_comparison_combined_USA
# }


# Next Figure -----Trial Enrollment Figure (my favorite) ---------------------------------

# Trial Enrollment Figure (my favorite)
fig_df_1b_yearly_enrollment_gi_global <-
  full_gi_df %>%
  mutate(year_trial = year(study_first_submitted_date)) %>%
  filter(br_studystatus == 'Completed') %>%
  select(year_trial, enrollment, enrollment_type, study_first_submitted_date, actual_duration) %>%
  mutate(new_enrollment = Hmisc::cut2(x = enrollment, 
                                      cuts = c(0,1, seq(30, 300, 30), Inf)),
         cap_enroll_2000 = case_when(enrollment > 1000 ~ as.double(1000), TRUE ~ as.double(enrollment))) %>%
  mutate(new_actduration = Hmisc::cut2(actual_duration, c(0, 10, 20, 30, 40, 50, Inf)))

fig_df_1b_yearly_enrollment_comparison_global <-
  full_comparison_df %>%
  mutate(year_trial = year(study_first_submitted_date)) %>%
  filter(br_studystatus == 'Completed') %>%
  select(year_trial, enrollment, enrollment_type, study_first_submitted_date, actual_duration) %>%
  mutate(new_enrollment = Hmisc::cut2(x = enrollment, 
                                      cuts = c(0,1, seq(30, 300, 30), Inf)),
         cap_enroll_2000 = case_when(enrollment > 1000 ~ as.double(1000), TRUE ~ as.double(enrollment))) %>%
  mutate(new_actduration = Hmisc::cut2(actual_duration, c(0, 10, 20, 30, 40, 50, Inf)))

# Global - gi - Brandon Figure
gg_fig_1b_yearly_enrollment_gi_global <- 
  ggplot(data = fig_df_1b_yearly_enrollment_gi_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = cap_enroll_2000)) + 
  geom_jitter(alpha = 0.04, color = color1, width = 0.1, size = 3) +
  geom_boxplot(outlier.colour = NA, 
               alpha = 0,
               coef = 0, 
               outlier.size = 0,
               width = 0.5,
               lwd = 1.0, # thickness of boxplot
               fatten = 1 # thickness of median line
  ) + 
  theme_bw() + 
  # scale_y_continuous(breaks = c(seq(0, 500, 100)), limits = c(0, 500)) + # this removes data! use coord_cartesian to set limits
  # see here for padding labels: https://stackoverflow.com/questions/34677551/is-it-possible-to-fix-axis-margin-with-ggplot2
  scale_y_continuous(breaks = c(seq(0, 500, 50)),
                     labels = function(label) bpadding(label, width = 4, makepercent = FALSE, num_decimals = 0)
                     # labels = function(label) sprintf('%5.0f', label) # width = 4, decimals = 0, padding = space
  ) + 
  coord_cartesian(ylim = c(0, 500)) + 
  xlab(label = 'Year of Trial Submission') + 
  ylab(label = '\nEnrollment Per \nCompleted Trial') +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))); gg_fig_1b_yearly_enrollment_gi_global

# Global - Comparison - Brandon Figure
# if(include_comparison_combined_analysis) {
gg_fig_1b_yearly_enrollment_comparison_global <- 
  ggplot(data = fig_df_1b_yearly_enrollment_comparison_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = cap_enroll_2000)) + 
  geom_jitter(alpha = 0.01, color = color1, width = 0.1, size = 3) +
  geom_boxplot(outlier.colour = NA, 
               alpha = 0,
               coef = 0, 
               outlier.size = 0,
               width = 0.5,
               lwd = 1.0, # thickness of boxplot
               fatten = 1 # thickness of median line
  ) + 
  theme_bw() + 
  # scale_y_continuous(breaks = c(seq(0, 500, 100)), limits = c(0, 500)) + # this removes data! use coord_cartesian to set limits
  # see here for padding labels: https://stackoverflow.com/questions/34677551/is-it-possible-to-fix-axis-margin-with-ggplot2
  scale_y_continuous(breaks = c(seq(0, 500, 50)),
                     labels = function(label) bpadding(label, width = 4, makepercent = FALSE, num_decimals = 0)
                     # labels = function(label) sprintf('%5.0f', label) # width = 4, decimals = 0, padding = space
  ) + 
  coord_cartesian(ylim = c(0, 500)) + 
  xlab(label = 'Year of Trial Submission') + 
  ylab(label = '\nEnrollment Per \nCompleted Trial') +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))); gg_fig_1b_yearly_enrollment_comparison_global
# }

# USA DATA ONLY
fig_df_1b_yearly_enrollment_gi_USA <-
  full_gi_df %>%
  filter(USA_only_facilities) %>%
  mutate(year_trial = year(study_first_submitted_date)) %>%
  filter(br_studystatus == 'Completed') %>%
  select(year_trial, enrollment, enrollment_type, study_first_submitted_date, actual_duration) %>%
  mutate(new_enrollment = Hmisc::cut2(x = enrollment, 
                                      cuts = c(0,1, seq(30, 300, 30), Inf)),
         cap_enroll_2000 = case_when(enrollment > 1000 ~ as.double(1000), TRUE ~ as.double(enrollment))) %>%
  mutate(new_actduration = Hmisc::cut2(actual_duration, c(0, 10, 20, 30, 40, 50, Inf)))

# if(include_comparison_combined_analysis) {
fig_df_1b_yearly_enrollment_comparison_USA <-
  full_comparison_df %>%
  filter(USA_only_facilities) %>%
  mutate(year_trial = year(study_first_submitted_date)) %>%
  filter(br_studystatus == 'Completed') %>%
  select(year_trial, enrollment, enrollment_type, study_first_submitted_date, actual_duration) %>%
  mutate(new_enrollment = Hmisc::cut2(x = enrollment, 
                                      cuts = c(0,1, seq(30, 300, 30), Inf)),
         cap_enroll_2000 = case_when(enrollment > 1000 ~ as.double(1000), TRUE ~ as.double(enrollment))) %>%
  mutate(new_actduration = Hmisc::cut2(actual_duration, c(0, 10, 20, 30, 40, 50, Inf)))
# }

# USA - gi - Brandon Figure
gg_fig_1b_yearly_enrollment_gi_USA <- 
  ggplot(data = fig_df_1b_yearly_enrollment_gi_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = cap_enroll_2000)) + 
  geom_jitter(alpha = 0.06, color = color1, width = 0.1, size = 4) +
  geom_boxplot(outlier.colour = NA, 
               alpha = 0,
               coef = 0, 
               outlier.size = 0,
               width = 0.5,
               lwd = 1.0, # thickness of boxplot
               fatten = 1 # thickness of median line
  ) + 
  theme_bw() + 
  # scale_y_continuous(breaks = c(seq(0, 500, 100)), limits = c(0, 500)) + # this removes data! use coord_cartesian to set limits
  # see here for padding labels: https://stackoverflow.com/questions/34677551/is-it-possible-to-fix-axis-margin-with-ggplot2
  scale_y_continuous(breaks = c(seq(0, 500, 50)),
                     labels = function(label) bpadding(label, width = 4, makepercent = FALSE, num_decimals = 0)
                     # labels = function(label) sprintf('%5.0f', label) # width = 4, decimals = 0, padding = space
  ) + 
  coord_cartesian(ylim = c(0, 500)) + 
  xlab(label = 'Year of Trial Submission') + 
  ylab(label = '\nEnrollment Per \nCompleted Trial') +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))); gg_fig_1b_yearly_enrollment_gi_USA

# USA - Comparison - Brandon Figure
# if(include_comparison_combined_analysis) {
gg_fig_1b_yearly_enrollment_comparison_USA <- 
  ggplot(data = fig_df_1b_yearly_enrollment_comparison_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = cap_enroll_2000)) + 
  geom_jitter(alpha = 0.01, color = color1, width = 0.1, size = 3) +
  geom_boxplot(outlier.colour = NA, 
               alpha = 0,
               coef = 0, 
               outlier.size = 0,
               width = 0.5,
               lwd = 1.0, # thickness of boxplot
               fatten = 1 # thickness of median line
  ) + 
  theme_bw() + 
  # scale_y_continuous(breaks = c(seq(0, 500, 100)), limits = c(0, 500)) + # this removes data! use coord_cartesian to set limits
  # see here for padding labels: https://stackoverflow.com/questions/34677551/is-it-possible-to-fix-axis-margin-with-ggplot2
  scale_y_continuous(breaks = c(seq(0, 500, 50)),
                     labels = function(label) bpadding(label, width = 4, makepercent = FALSE, num_decimals = 0)
                     # labels = function(label) sprintf('%5.0f', label) # width = 4, decimals = 0, padding = space
  ) + 
  coord_cartesian(ylim = c(0, 500)) + 
  xlab(label = 'Year of Trial Submission') + 
  ylab(label = '\nEnrollment Per \nCompleted Trial') +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))); gg_fig_1b_yearly_enrollment_comparison_USA
# }




# Next Figure -----Industry Line Charts As FREQ or as PCT-------------------------

# Global - Data - Sponsorship 
fig_df_1d_yearly_sponsor_gi_global <- 
  gi_trial_growth_data_global %>%
  select(year_trial, total_trials_all, total_trials_sponsor, starts_with('funding2b')) 

fig_df_1d_yearly_sponsor_gi_freq_global <-
  fig_df_1d_yearly_sponsor_gi_global %>%
  select(year_trial, ends_with('_freq')) %>%
  tidyr::pivot_longer(names_to = 'Sponsor',
                      values_to = 'freq_of_all_trials',
                      cols = ends_with('_freq')) %>%
  extract(col = Sponsor,
          into = c('industry_any2b'),
          regex = '[[:alnum:]\\.]*_([[:alnum:]\\.]*)_freq$') %>% # example format is funding2b_US.Govt_feq
  mutate(specialty_group = 'gi')

fig_df_1d_yearly_sponsor_gi_pct_global <- 
  fig_df_1d_yearly_sponsor_gi_global %>%
  select(year_trial, ends_with('_pct')) %>%
  tidyr::pivot_longer(names_to = 'Sponsor',
                      values_to = 'pct_of_all_trials',
                      cols = ends_with('_pct')) %>%
  extract(col = Sponsor,
          into = c('industry_any2b'),
          regex = '[[:alnum:]\\.]*_([[:alnum:]\\.]*)_pct$') %>% # example format is funding2b_US.Govt_feq
  mutate(specialty_group = 'gi')

fig_df_1d_yearly_sponsor_comparison_global <- 
  comparison_trial_growth_data_global %>%
  select(year_trial, total_trials_all, total_trials_sponsor, starts_with('funding2b')) 

fig_df_1d_yearly_sponsor_comparison_freq_global <-
  fig_df_1d_yearly_sponsor_comparison_global %>%
  select(year_trial, ends_with('_freq')) %>%
  tidyr::pivot_longer(names_to = 'Sponsor',
                      values_to = 'freq_of_all_trials',
                      cols = ends_with('_freq')) %>%
  extract(col = Sponsor,
          into = c('industry_any2b'),
          regex = '[[:alnum:]\\.]*_([[:alnum:]\\.]*)_freq$') %>% # example format is funding2b_US.Govt_feq
  mutate(specialty_group = 'comparison')

fig_df_1d_yearly_sponsor_comparison_pct_global <- 
  fig_df_1d_yearly_sponsor_comparison_global %>%
  select(year_trial, ends_with('_pct')) %>%
  tidyr::pivot_longer(names_to = 'Sponsor',
                      values_to = 'pct_of_all_trials',
                      cols = ends_with('_pct')) %>%
  extract(col = Sponsor,
          into = c('industry_any2b'),
          regex = '[[:alnum:]\\.]*_([[:alnum:]\\.]*)_pct$') %>% # example format is funding2b_US.Govt_feq
  mutate(specialty_group = 'comparison')

fig_df_1d_yearly_sponsor_COMBINED_freq_global <- 
  bind_rows(fig_df_1d_yearly_sponsor_gi_freq_global,
            fig_df_1d_yearly_sponsor_comparison_freq_global) %>%
  mutate(group_id = paste(industry_any2b, specialty_group, sep = '_'))

fig_df_1d_yearly_sponsor_COMBINED_pct_global <- 
  bind_rows(fig_df_1d_yearly_sponsor_gi_pct_global,
            fig_df_1d_yearly_sponsor_comparison_pct_global) %>%
  mutate(group_id = paste(industry_any2b, specialty_group, sep = '_'))

# USA - Data - Sponsorship  
fig_df_1d_yearly_sponsor_gi_USA <- 
  gi_trial_growth_data_USA %>%
  select(year_trial, total_trials_all, total_trials_sponsor, starts_with('funding2b')) 

fig_df_1d_yearly_sponsor_gi_freq_USA <-
  fig_df_1d_yearly_sponsor_gi_USA %>%
  select(year_trial, ends_with('_freq')) %>%
  tidyr::pivot_longer(names_to = 'Sponsor',
                      values_to = 'freq_of_all_trials',
                      cols = ends_with('_freq')) %>%
  extract(col = Sponsor,
          into = c('industry_any2b'),
          regex = '[[:alnum:]\\.]*_([[:alnum:]\\.]*)_freq$') %>% # example format is funding2b_US.Govt_feq
  mutate(specialty_group = 'gi')

fig_df_1d_yearly_sponsor_gi_pct_USA <- 
  fig_df_1d_yearly_sponsor_gi_USA %>%
  select(year_trial, ends_with('_pct')) %>%
  tidyr::pivot_longer(names_to = 'Sponsor',
                      values_to = 'pct_of_all_trials',
                      cols = ends_with('_pct')) %>%
  extract(col = Sponsor,
          into = c('industry_any2b'),
          regex = '[[:alnum:]\\.]*_([[:alnum:]\\.]*)_pct$') %>% # example format is funding2b_US.Govt_feq
  mutate(specialty_group = 'gi')

fig_df_1d_yearly_sponsor_comparison_USA <- 
  comparison_trial_growth_data_USA %>%
  select(year_trial, total_trials_all, total_trials_sponsor, starts_with('funding2b'))

fig_df_1d_yearly_sponsor_comparison_freq_USA <-
  fig_df_1d_yearly_sponsor_comparison_USA %>%
  select(year_trial, ends_with('_freq')) %>%
  tidyr::pivot_longer(names_to = 'Sponsor',
                      values_to = 'freq_of_all_trials',
                      cols = ends_with('_freq')) %>%
  extract(col = Sponsor,
          into = c('industry_any2b'),
          regex = '[[:alnum:]\\.]*_([[:alnum:]\\.]*)_freq$') %>% # example format is funding2b_US.Govt_feq
  mutate(specialty_group = 'comparison')

fig_df_1d_yearly_sponsor_comparison_pct_USA <- 
  fig_df_1d_yearly_sponsor_comparison_USA %>%
  select(year_trial, ends_with('_pct')) %>%
  tidyr::pivot_longer(names_to = 'Sponsor',
                      values_to = 'pct_of_all_trials',
                      cols = ends_with('_pct')) %>%
  extract(col = Sponsor,
          into = c('industry_any2b'),
          regex = '[[:alnum:]\\.]*_([[:alnum:]\\.]*)_pct$') %>% # example format is funding2b_US.Govt_feq
  mutate(specialty_group = 'comparison')

fig_df_1d_yearly_sponsor_COMBINED_freq_USA <- 
  bind_rows(fig_df_1d_yearly_sponsor_gi_freq_USA,
            fig_df_1d_yearly_sponsor_comparison_freq_USA) %>%
  mutate(group_id = paste(industry_any2b, specialty_group, sep = '_'))

fig_df_1d_yearly_sponsor_COMBINED_pct_USA <- 
  bind_rows(fig_df_1d_yearly_sponsor_gi_pct_USA,
            fig_df_1d_yearly_sponsor_comparison_pct_USA) %>%
  mutate(group_id = paste(industry_any2b, specialty_group, sep = '_'))

# Global - gi - Sponsorship Line Graph (freq)
gg_fig_1d_yearly_sponsor_gi_freq_global <- 
  ggplot(data = fig_df_1d_yearly_sponsor_gi_freq_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = freq_of_all_trials, group = industry_any2b)) +
  geom_line(aes(color = industry_any2b), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, max(fig_df_1d_yearly_sponsor_gi_freq_global$freq_of_all_trials) + 100, 100),
                     expand = c(0.01,0)) + # this makes the x-axis start close y=0
  coord_cartesian(ylim = c(0, max(fig_df_1d_yearly_sponsor_gi_freq_global$freq_of_all_trials) + 100)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.9, 'line'),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew gilogic \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5); gg_fig_1d_yearly_sponsor_gi_freq_global

# Global - gi - Sponsorship Line Graph (pct)
gg_fig_1d_yearly_sponsor_gi_pct_global <- 
  ggplot(data = fig_df_1d_yearly_sponsor_gi_pct_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = industry_any2b)) +
  geom_line(aes(color = industry_any2b), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, .8)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5); gg_fig_1d_yearly_sponsor_gi_pct_global

# Global - Comparison - Sponsorship Line Graph (freq)
# if(include_comparison_combined_analysis) {
gg_fig_1d_yearly_sponsor_comparison_freq_global <- 
  ggplot(data = fig_df_1d_yearly_sponsor_comparison_freq_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = freq_of_all_trials, group = industry_any2b)) +
  geom_line(aes(color = industry_any2b), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, max(fig_df_1d_yearly_sponsor_comparison_freq_global$freq_of_all_trials) + 1000, 1000),
                     expand = c(0.01,0)) + # this makes the x-axis start close y=0
  coord_cartesian(ylim = c(0, max(fig_df_1d_yearly_sponsor_comparison_freq_global$freq_of_all_trials) + 1000)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.9, 'line'),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew comparisonlogic \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5); gg_fig_1d_yearly_sponsor_comparison_freq_global
# }

# Global - Comparison - Sponsorship Line Graph (pct)
# if(include_comparison_combined_analysis) {
gg_fig_1d_yearly_sponsor_comparison_pct_global <- 
  ggplot(data = fig_df_1d_yearly_sponsor_comparison_pct_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = industry_any2b)) +
  geom_line(aes(color = industry_any2b), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, .8)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5); gg_fig_1d_yearly_sponsor_comparison_pct_global
# }

# Global - COMBINED - Sponsorship Line Graph (freq)
# if(include_comparison_combined_analysis) {
gg_fig_1d_yearly_sponsor_COMBINED_freq_global <- 
  ggplot(data = fig_df_1d_yearly_sponsor_COMBINED_freq_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = freq_of_all_trials, group = group_id)) +
  geom_line(aes(color = industry_any2b,
                alpha = specialty_group), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b, alpha = specialty_group), shape = 21, size = 5) +
  scale_y_continuous(breaks = seq(0, max(fig_df_1d_yearly_sponsor_COMBINED_freq_global$freq_of_all_trials) + 1000, 1000),
                     expand = c(0.01,0)) + # this makes the x-axis start close y=0
  coord_cartesian(ylim = c(0, max(fig_df_1d_yearly_sponsor_COMBINED_freq_global$freq_of_all_trials) + 1000)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.9, 'line'),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew comparisonlogic \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5) + 
  scale_alpha_manual(name = "Shade", 
                     labels = c("All Other Trials", "gilogic Trials"), 
                     values = g_industry_alpha5,
                     guide = guide_legend(title = "Shade", 
                                          reverse = TRUE)); gg_fig_1d_yearly_sponsor_COMBINED_freq_global
# }

# Global - COMBINED - Sponsorship Line Graph (pct; alpha)
# if(include_comparison_combined_analysis) {
gg_fig_1d_yearly_sponsor_COMBINED_pct_global_alpha <- 
  ggplot(data = fig_df_1d_yearly_sponsor_COMBINED_pct_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = group_id)) +
  geom_line(aes(color = industry_any2b,
                alpha = specialty_group,
                linetype = specialty_group),
            size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, 
                 color = industry_any2b, 
                 alpha = specialty_group), 
             shape = 21, 
             size = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, 0.9)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        # legend.spacing.y = unit(0.000, 'line'),
        # legend.spacing.x = unit(0.15, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5,
                    guide = guide_legend(order = 1,
                                         override.aes = list(size = 3))) +
  scale_linetype_manual(values = c("comparison" = 'dotted',
                                   "gi" = 'solid'),
                        guide = guide_legend(reverse = TRUE, # reverse the order so it shows gi first
                                             keywidth = unit(1, 'line'),
                                             override.aes = list(size = 1)), 
                        labels = c("All Other Trials", "gilogic Trials")) + 
  scale_alpha_manual(name = "Shade", 
                     labels = c("All Other Trials", "gilogic Trials"), 
                     values = g_industry_alpha5,
                     guide = guide_legend(title = "Shade", 
                                          reverse = TRUE)) + 
  guides(color = FALSE,  
         alpha = FALSE); gg_fig_1d_yearly_sponsor_COMBINED_pct_global_alpha # this is pretty good
# }

# USA - gi - Sponsorship Line Graph (freq)
gg_fig_1d_yearly_sponsor_gi_freq_USA <- 
  ggplot(data = fig_df_1d_yearly_sponsor_gi_freq_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = freq_of_all_trials, group = industry_any2b)) +
  geom_line(aes(color = industry_any2b), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, max(fig_df_1d_yearly_sponsor_gi_freq_USA$freq_of_all_trials) + 100, 100),
                     expand = c(0.01,0)) + # this makes the x-axis start close y=0
  coord_cartesian(ylim = c(0, max(fig_df_1d_yearly_sponsor_gi_freq_USA$freq_of_all_trials) + 100)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.9, 'line'),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew gilogic \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5); gg_fig_1d_yearly_sponsor_gi_freq_USA

# USA - gi - Sponsorship Line Graph (pct)
gg_fig_1d_yearly_sponsor_gi_pct_USA <- 
  ggplot(data = fig_df_1d_yearly_sponsor_gi_pct_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = industry_any2b)) +
  geom_line(aes(color = industry_any2b), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, .8)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5); gg_fig_1d_yearly_sponsor_gi_pct_USA

# USA - Comparison - Sponsorship Line Graph (freq)
gg_fig_1d_yearly_sponsor_comparison_freq_USA <- 
  ggplot(data = fig_df_1d_yearly_sponsor_comparison_freq_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = freq_of_all_trials, group = industry_any2b)) +
  geom_line(aes(color = industry_any2b), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, max(fig_df_1d_yearly_sponsor_comparison_freq_USA$freq_of_all_trials) + 1000, 1000),
                     expand = c(0.01,0)) + # this makes the x-axis start close y=0
  coord_cartesian(ylim = c(0, max(fig_df_1d_yearly_sponsor_comparison_freq_USA$freq_of_all_trials) + 1000)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.9, 'line'),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew comparisonlogic \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5); gg_fig_1d_yearly_sponsor_comparison_freq_USA

# USA - Comparison - Sponsorship Line Graph (pct)
gg_fig_1d_yearly_sponsor_comparison_pct_USA <- 
  ggplot(data = fig_df_1d_yearly_sponsor_comparison_pct_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = industry_any2b)) +
  geom_line(aes(color = industry_any2b), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, 0.8)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5); gg_fig_1d_yearly_sponsor_comparison_pct_USA

# USA - COMBINED - Sponsorship Line Graph (freq)
gg_fig_1d_yearly_sponsor_COMBINED_freq_USA <- 
  ggplot(data = fig_df_1d_yearly_sponsor_COMBINED_freq_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = freq_of_all_trials, group = group_id)) +
  geom_line(aes(color = industry_any2b,
                alpha = specialty_group), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, color = industry_any2b, alpha = specialty_group), shape = 21, size = 5) +
  scale_y_continuous(breaks = seq(0, max(fig_df_1d_yearly_sponsor_COMBINED_freq_USA$freq_of_all_trials) + 1000, 1000),
                     expand = c(0.01,0)) + # this makes the x-axis start close y=0
  coord_cartesian(ylim = c(0, max(fig_df_1d_yearly_sponsor_COMBINED_freq_USA$freq_of_all_trials) + 1000)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.9, 'line'),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nNew comparisonlogic \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5) + 
  scale_alpha_manual(name = "Shade", 
                     labels = c("All Other Trials", "gilogic Trials"), 
                     values = g_industry_alpha5,
                     guide = guide_legend(title = "Shade", 
                                          reverse = TRUE)); gg_fig_1d_yearly_sponsor_COMBINED_freq_USA

# USA - COMBINED - Sponsorship Line Graph (pct; alpha)
gg_fig_1d_yearly_sponsor_COMBINED_pct_USA_alpha <- 
  ggplot(data = fig_df_1d_yearly_sponsor_COMBINED_pct_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = group_id)) +
  geom_line(aes(color = industry_any2b,
                alpha = specialty_group,
                linetype = specialty_group),
            size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = industry_any2b, 
                 color = industry_any2b, 
                 alpha = specialty_group), 
             shape = 21, 
             size = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, 0.8)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        # legend.spacing.y = unit(0.000, 'line'),
        # legend.spacing.x = unit(0.15, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5,
                    guide = guide_legend(order = 1,
                                         override.aes = list(size = 3))) +
  scale_linetype_manual(values = c("comparison" = 'dotted',
                                   "gi" = 'solid'),
                        guide = guide_legend(reverse = TRUE, # reverse the order so it shows gi first
                                             keywidth = unit(1, 'line'),
                                             override.aes = list(size = 1)), 
                        labels = c("All Other Trials", "gilogic Trials")) + 
  scale_alpha_manual(name = "Shade", 
                     labels = c("All Other Trials", "gilogic Trials"), 
                     values = g_industry_alpha5,
                     guide = guide_legend(title = "Shade", 
                                          reverse = TRUE)) + 
  guides(color = FALSE,  
         alpha = FALSE); gg_fig_1d_yearly_sponsor_COMBINED_pct_USA_alpha # this is partially hideous

# USA - COMBINED - Sponsorship Line Graph (pct; color)
gg_fig_1d_yearly_sponsor_COMBINED_pct_USA_color <- 
  ggplot(data = fig_df_1d_yearly_sponsor_COMBINED_pct_USA %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = group_id)) +
  geom_line(aes(color = group_id), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = group_id, color = group_id), shape = 21, size = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, 0.8)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') + 
  scale_color_manual(values = g_industry_color5b) + 
  scale_fill_manual(values = g_industry_color5b); gg_fig_1d_yearly_sponsor_COMBINED_pct_USA_color # this is absolutely hideous


# Next Figure -----------Region Line Charts As FREQ or as PCT-------------------------

# Global - Data - Region
fig_df_1e_yearly_region_gi_global <-
  gi_trial_growth_data_global %>%
  select(year_trial, total_trials_all,
         total_trials_region, NorthAmerica_freq, Europe_freq, EastAsia_freq, neither3regions_freq,
         NorthAmerica_pct, Europe_pct, EastAsia_pct, neither3regions_pct) # these percentages are a percentage of only trials with regional information

fig_df_1e_yearly_region_gi_freq_global <-
  fig_df_1e_yearly_region_gi_global %>%
  select(year_trial, ends_with('_freq')) %>%
  tidyr::pivot_longer(names_to = 'region',
                      values_to = 'freq_of_all_trials',
                      cols = ends_with('_freq')) %>%
  separate(col = region,
           into = c('region', NA),
           sep = '_') %>%
  mutate(specialty_group = 'gi') %>%
  mutate(region = forcats::fct_relevel(region, c('NorthAmerica','Europe','EastAsia','neither3regions')))

fig_df_1e_yearly_region_gi_pct_global <-
  fig_df_1e_yearly_region_gi_global %>%
  select(year_trial, ends_with('_pct')) %>%
  tidyr::pivot_longer(names_to = 'region',
                      values_to = 'pct_of_all_trials',
                      cols = ends_with('_pct')) %>%
  separate(col = region,
           into = c('region', NA),
           sep = '_') %>%
  mutate(specialty_group = 'gi') %>%
  mutate(region = forcats::fct_relevel(region, c('NorthAmerica','Europe','EastAsia','neither3regions')))

fig_df_1e_yearly_region_comparison_global <-
  comparison_trial_growth_data_global %>%
  select(year_trial, total_trials_all,
         total_trials_region, NorthAmerica_freq, Europe_freq, EastAsia_freq, neither3regions_freq,
         NorthAmerica_pct, Europe_pct, EastAsia_pct, neither3regions_pct) # these percentages are a percentage of only trials with regional information

fig_df_1e_yearly_region_comparison_freq_global <-
  fig_df_1e_yearly_region_comparison_global %>%
  select(year_trial, ends_with('_freq')) %>%
  tidyr::pivot_longer(names_to = 'region',
                      values_to = 'freq_of_all_trials',
                      cols = ends_with('_freq')) %>%
  separate(col = region,
           into = c('region', NA),
           sep = '_') %>%
  mutate(specialty_group = 'comparison') %>%
  mutate(region = forcats::fct_relevel(region, c('NorthAmerica','Europe','EastAsia','neither3regions')))

fig_df_1e_yearly_region_comparison_pct_global <-
  fig_df_1e_yearly_region_comparison_global %>%
  select(year_trial, ends_with('_pct')) %>%
  tidyr::pivot_longer(names_to = 'region',
                      values_to = 'pct_of_all_trials',
                      cols = ends_with('_pct')) %>%
  separate(col = region,
           into = c('region', NA),
           sep = '_') %>%
  mutate(specialty_group = 'comparison') %>%
  mutate(region = forcats::fct_relevel(region, c('NorthAmerica','Europe','EastAsia','neither3regions')))

fig_df_1e_yearly_region_COMBINED_freq_global <-
  bind_rows(fig_df_1e_yearly_region_gi_freq_global,
            fig_df_1e_yearly_region_comparison_freq_global) %>%
  mutate(group_id = paste(region, specialty_group, sep = '_')) %>%
  mutate(region = forcats::fct_relevel(region, c('NorthAmerica','Europe','EastAsia','neither3regions')))

fig_df_1e_yearly_region_COMBINED_pct_global <-
  bind_rows(fig_df_1e_yearly_region_gi_pct_global,
            fig_df_1e_yearly_region_comparison_pct_global) %>%
  mutate(group_id = paste(region, specialty_group, sep = '_')) %>%
  mutate(region = forcats::fct_relevel(region, c('NorthAmerica','Europe','EastAsia','neither3regions')))


# Global - gi - Region Line Graph (freq)
gg_fig_1e_yearly_region_gi_freq_global <-
  ggplot(data = fig_df_1e_yearly_region_gi_freq_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = freq_of_all_trials, group = region)) +
  geom_line(aes(color = region), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = region, color = region), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, max(fig_df_1e_yearly_region_gi_freq_global$freq_of_all_trials) + 100, 100),
                     expand = c(0.01,0)) + # this makes the x-axis start close y=0
  coord_cartesian(ylim = c(0, max(fig_df_1e_yearly_region_gi_freq_global$freq_of_all_trials) + 100)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.9, 'line'),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) +
  labs(x = 'Year of Trial Submission', y = '\nNew gilogic \nTrials Per Year') +
  scale_color_manual(values = g_region_color6) +
  scale_fill_manual(values = g_region_color6); gg_fig_1e_yearly_region_gi_freq_global

# Global - gi - Region Line Graph (pct)
gg_fig_1e_yearly_region_gi_pct_global <-
  ggplot(data = fig_df_1e_yearly_region_gi_pct_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = region)) +
  geom_line(aes(color = region), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = region, color = region), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) +
  coord_cartesian(ylim = c(0, .8)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) +
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') +
  scale_color_manual(values = g_region_color6) +
  scale_fill_manual(values = g_region_color6); gg_fig_1e_yearly_region_gi_pct_global

# Global - Comparison - Region Line Graph (freq)
gg_fig_1e_yearly_region_comparison_freq_global <-
  ggplot(data = fig_df_1e_yearly_region_comparison_freq_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = freq_of_all_trials, group = region)) +
  geom_line(aes(color = region), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = region, color = region), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, max(fig_df_1e_yearly_region_comparison_freq_global$freq_of_all_trials) + 1000, 1000),
                     expand = c(0.01,0)) + # this makes the x-axis start close y=0
  coord_cartesian(ylim = c(0, max(fig_df_1e_yearly_region_comparison_freq_global$freq_of_all_trials) + 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.9, 'line'),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) +
  labs(x = 'Year of Trial Submission', y = '\nNew comparisonlogic \nTrials Per Year') +
  scale_color_manual(values = g_region_color6) +
  scale_fill_manual(values = g_region_color6); gg_fig_1e_yearly_region_comparison_freq_global

# Global - Comparison - Region Line Graph (pct)
gg_fig_1e_yearly_region_comparison_pct_global <-
  ggplot(data = fig_df_1e_yearly_region_comparison_pct_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = region)) +
  geom_line(aes(color = region), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = region, color = region), shape = 21, size = 5, alpha = 1) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) +
  coord_cartesian(ylim = c(0, .8)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) +
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') +
  scale_color_manual(values = g_region_color6) +
  scale_fill_manual(values = g_region_color6); gg_fig_1e_yearly_region_comparison_pct_global

# Global - COMBINED - Region Line Graph (freq)
gg_fig_1e_yearly_region_COMBINED_freq_global <-
  ggplot(data = fig_df_1e_yearly_region_COMBINED_freq_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = freq_of_all_trials, group = group_id)) +
  geom_line(aes(color = region,
                alpha = specialty_group), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = region, color = region, alpha = specialty_group), shape = 21, size = 5) +
  scale_y_continuous(breaks = seq(0, max(fig_df_1e_yearly_region_COMBINED_freq_global$freq_of_all_trials) + 1000, 1000),
                     expand = c(0.01,0)) + # this makes the x-axis start close y=0
  coord_cartesian(ylim = c(0, max(fig_df_1e_yearly_region_COMBINED_freq_global$freq_of_all_trials) + 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.9, 'line'),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) +
  labs(x = 'Year of Trial Submission', y = '\nNew comparisonlogic \nTrials Per Year') +
  scale_color_manual(values = g_region_color6) +
  scale_fill_manual(values = g_region_color6) +
  scale_alpha_manual(name = "Shade",
                     labels = c("All Other Trials", "gilogic Trials"),
                     values = g_industry_alpha5,
                     guide = guide_legend(title = "Shade",
                                          reverse = TRUE)); gg_fig_1e_yearly_region_COMBINED_freq_global

# Global - COMBINED - Region Line Graph (pct; alpha)
gg_fig_1e_yearly_region_COMBINED_pct_global_alpha <-
  ggplot(data = fig_df_1e_yearly_region_COMBINED_pct_global %>% filter(year_trial > 2007, year_trial < 2020),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = group_id)) +
  geom_line(aes(color = region,
                alpha = specialty_group), size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(fill = region, color = region, alpha = specialty_group), shape = 21, size = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) +
  coord_cartesian(ylim = c(0, .8)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) +
  labs(x = 'Year of Trial Submission', y = '\nProportion of \nTrials Per Year') +
  scale_color_manual(values = g_region_color6) +
  scale_fill_manual(values = g_region_color6) +
  scale_alpha_manual(name = "Shade",
                     labels = c("All Other Trials", "gilogic Trials"),
                     values = g_industry_alpha5,
                     guide = guide_legend(title = "Shade",
                                          reverse = TRUE)); gg_fig_1e_yearly_region_COMBINED_pct_global_alpha # this is partially hideous

# Next Figure -----------Gross National Income Line Charts As FREQ or as PCT-------------------------

# Global - Data - GNI  
# gi
fig_df_1f_yearly_gni_gi_global <- 
  gi_trial_growth_data_global %>%
  select(year_trial, total_trials_all, 
         total_trials_gnistatus, br_gni_hic_freq, br_gni_hic_pct) %>%  # these percentages are a percentage of only trials with GNI information
  mutate(br_gni_lmiconly_freq = total_trials_gnistatus - br_gni_hic_freq,
         br_gni_lmiconly_pct = br_gni_lmiconly_freq / total_trials_gnistatus)

# pct - gi
fig_df_1f_yearly_gni_gi_pct_global <- 
  fig_df_1f_yearly_gni_gi_global %>% 
  select(year_trial, ends_with('_pct'), total_trials_gnistatus) %>%
  tidyr::pivot_longer(names_to = 'gnistatus',
                      values_to = 'pct_of_all_trials',
                      cols = ends_with('_pct')) %>%
  mutate(gnistatus = stringr::str_match(string = gnistatus, pattern = 'br_gni_([[:alpha:]]*)_pct')[,2]) %>% 
  mutate(specialty_group = 'gi')

# freq - gi
fig_df_1f_yearly_gni_gi_freq_global <- 
  fig_df_1f_yearly_gni_gi_global %>% 
  select(year_trial, ends_with('_freq'), total_trials_gnistatus) %>%
  tidyr::pivot_longer(names_to = 'gnistatus',
                      values_to = 'freq_of_all_trials',
                      cols = ends_with('_freq')) %>%
  mutate(gnistatus = stringr::str_match(string = gnistatus, pattern = 'br_gni_([[:alpha:]]*)_freq')[,2]) %>% 
  mutate(specialty_group = 'gi')

# comparison
fig_df_1f_yearly_gni_comparison_global <- 
  comparison_trial_growth_data_global %>%
  select(year_trial, total_trials_all, 
         total_trials_gnistatus, br_gni_hic_freq, br_gni_hic_pct) %>%  # these percentages are a percentage of only trials with GNI information
  mutate(br_gni_lmiconly_freq = total_trials_gnistatus - br_gni_hic_freq,
         br_gni_lmiconly_pct = br_gni_lmiconly_freq / total_trials_gnistatus)

# pct - comparison
fig_df_1f_yearly_gni_comparison_pct_global <- 
  fig_df_1f_yearly_gni_comparison_global %>% 
  select(year_trial, ends_with('_pct'), total_trials_gnistatus) %>%
  tidyr::pivot_longer(names_to = 'gnistatus',
                      values_to = 'pct_of_all_trials',
                      cols = ends_with('_pct')) %>%
  mutate(gnistatus = stringr::str_match(string = gnistatus, pattern = 'br_gni_([[:alpha:]]*)_pct')[,2]) %>% 
  mutate(specialty_group = 'comparison')

# freq - comparison
fig_df_1f_yearly_gni_comparison_freq_global <- 
  fig_df_1f_yearly_gni_comparison_global %>% 
  select(year_trial, ends_with('_freq'), total_trials_gnistatus) %>%
  tidyr::pivot_longer(names_to = 'gnistatus',
                      values_to = 'freq_of_all_trials',
                      cols = ends_with('_freq')) %>%
  mutate(gnistatus = stringr::str_match(string = gnistatus, pattern = 'br_gni_([[:alpha:]]*)_freq')[,2]) %>% 
  mutate(specialty_group = 'comparison')

# Combine the gi and Comparison Tables Together
fig_df_1f_yearly_region_COMBINED_pct_global <- 
  bind_rows(fig_df_1f_yearly_gni_gi_pct_global,
            fig_df_1f_yearly_gni_comparison_pct_global) %>%
  mutate(group_id = paste(gnistatus, specialty_group, sep = '_')) 

fig_df_1f_yearly_region_COMBINED_freq_global <- 
  bind_rows(fig_df_1f_yearly_gni_gi_freq_global,
            fig_df_1f_yearly_gni_comparison_freq_global) %>%
  mutate(group_id = paste(gnistatus, specialty_group, sep = '_')) 


## Plot The GNI Status Data
gg_fig_1f_yearly_gni_COMBINED_pct_global_alpha <- 
  ggplot(data = fig_df_1f_yearly_region_COMBINED_pct_global 
         %>% filter(year_trial > 2007, year_trial < 2020) %>%
           filter(gnistatus == 'lmiconly'),
         aes(x = factor(year_trial), y = pct_of_all_trials, group = group_id)) +
  geom_line(aes(alpha = specialty_group,
                # color = gnistatus,
                linetype = specialty_group), 
            color = '#5E59A6',
            size = 2) +
  # geom_point(fill = 'white', color = 'black', shape = 21, size = 5) +
  geom_point(aes(# fill = gnistatus, 
    # color = gnistatus, 
    alpha = specialty_group), 
    shape = 21, 
    fill = '#5E59A6',
    color = '#5E59A6',
    size = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     expand = c(0.01,0), # this makes the x-axis start close to y=0
                     # labels = scales::percent # not using this one because of width padding
                     labels = function(label) bpadding(label, width = 4, makepercent = TRUE, num_decimals = 0)) + 
  coord_cartesian(ylim = c(0, 0.5)) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.90, 0.87),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.05, 'line'),
        legend.spacing.y = unit(0.000, 'line'),
        legend.spacing.x = unit(0.15, 'line'),
        legend.direction = 'horizontal',
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + 
  labs(x = 'Year of Trial Submission', y = '\nProportion of Entirely\nLMIC Trials Per Year') + 
  # scale_color_manual(values = g_gni_color7) + 
  # scale_fill_manual(values = g_gni_color7,
  #                   labels = c("Includes HIC", "LMIC Only"),
  #                   guide = guide_legend(order = 1,
  #                                        override.aes = list(size = 3))) +
  scale_linetype_manual(values = c("comparison" = 'dotted',
                                   "gi" = 'solid'),
                        guide = guide_legend(reverse = TRUE, # reverse the order so it shows OB first
                                             keywidth = unit(1, 'line'),
                                             override.aes = list(size = 1)), 
                        labels = c("All Other Trials", "gilogic Trials")) + 
  scale_alpha_manual(name = "Shade", 
                     labels = c("All Other Trials", "gilogic Trials"), 
                     values = g_industry_alpha5,
                     guide = guide_legend(title = "Shade", 
                                          reverse = TRUE)) + 
  guides(color = FALSE,  
         fill = FALSE,
         alpha = FALSE); gg_fig_1f_yearly_gni_COMBINED_pct_global_alpha # this is pretty good
# ---------------------------------------------------------------------#
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  #
# ----------------     Combine Into 3- and 4- Pane      ---------------#
#                 FIRST FOR GLOBAL AND THEN FOR USA ONLY               #
# ---------------------------------------------------------------------#


# Global
gg_fig_1_5pane_gi_global <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                 gg_fig_1c_yearlypct_gi_combined_global,
                                                 gg_fig_1d_yearly_sponsor_gi_pct_global,
                                                 # gg_fig_1f_yearly_gni_COMBINED_pct_global_alpha,
                                                 gg_fig_1e_yearly_region_gi_pct_global,
                                                 gg_fig_1b_yearly_enrollment_gi_global,
                                                 nrow = 5, ncol = 1,
                                                 heights = c(2, 1, 2, 2, 2),
                                                 labels = c('A', 'B', 'C', 'D', 'E')) %>%
  annotate_figure(top= text_grob('gg_fig_1_5pane_gi_global', face = 'bold'))

gg_fig_1_5pane_gi_global_b <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_COMBINED_pct_global_alpha,
                                                   # gg_fig_1f_yearly_gni_COMBINED_pct_global_alpha,
                                                   gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 5, ncol = 1,
                                                   heights = c(2, 1, 2, 2, 2),
                                                   labels = c('A', 'B', 'C', 'D', 'E')) %>%
  annotate_figure(top= text_grob('gg_fig_1_5pane_gi_global_b', face = 'bold'))

gg_fig_1_5pane_gi_global_c <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_COMBINED_pct_global_alpha,
                                                   gg_fig_1f_yearly_gni_COMBINED_pct_global_alpha,
                                                   # gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 5, ncol = 1,
                                                   heights = c(2, 1, 2, 2, 2),
                                                   labels = c('A', 'B', 'C', 'D', 'E')) %>%
  annotate_figure(top= text_grob('gg_fig_1_5pane_gi_global_c', face = 'bold'))

gg_fig_1_4pane_gi_global_a <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   # gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_gi_pct_global,
                                                   gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 4, ncol = 1,
                                                   heights = c(2, 2, 2, 2),
                                                   labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_gi_global_a', face = 'bold'))

gg_fig_1_4pane_gi_global_b <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   # gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_gi_freq_global,
                                                   gg_fig_1e_yearly_region_gi_freq_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 4, ncol = 1,
                                                   heights = c(2, 2, 2, 2),
                                                   labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_gi_global_b', face = 'bold'))

gg_fig_1_4pane_gi_global_c <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_gi_pct_global,
                                                   # gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 4, ncol = 1,
                                                   heights = c(2, 1, 2, 2),
                                                   labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_gi_global_c', face = 'bold'))

gg_fig_1_4pane_gi_global_d <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_gi_freq_global,
                                                   # gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 4, ncol = 1,
                                                   heights = c(2, 1, 2, 2),
                                                   labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_gi_global_d', face = 'bold'))

gg_fig_1_4pane_gi_global_e <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_COMBINED_pct_global_alpha,
                                                   # gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 4, ncol = 1,
                                                   heights = c(2, 1, 2, 2),
                                                   labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_gi_global_e', face = 'bold'))

gg_fig_1_4pane_gi_global_f <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   # gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_COMBINED_pct_global_alpha,
                                                   gg_fig_1f_yearly_gni_COMBINED_pct_global_alpha,
                                                   # gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 4, ncol = 1,
                                                   heights = c(2, 2, 2, 2),
                                                   labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_gi_global_f', face = 'bold'))

gg_fig_1_3pane_gi_global_a <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   # gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_gi_pct_global,
                                                   # gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 3, ncol = 1,
                                                   heights = c(2, 2, 2),
                                                   labels = c('A', 'B', 'C')) %>%
  annotate_figure(top= text_grob('gg_fig_1_3pane_gi_global_a', face = 'bold'))

gg_fig_1_3pane_gi_global_b <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   # gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_gi_freq_global,
                                                   # gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 3, ncol = 1,
                                                   heights = c(2, 2, 2),
                                                   labels = c('A', 'B', 'C')) %>%
  annotate_figure(top= text_grob('gg_fig_1_3pane_gi_global_b', face = 'bold'))


gg_fig_1_3pane_gi_global_c <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   # gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_COMBINED_pct_global_alpha,
                                                   # gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 3, ncol = 1,
                                                   heights = c(2, 2, 2),
                                                   labels = c('A', 'B', 'C')) %>%
  annotate_figure(top= text_grob('gg_fig_1_3pane_gi_global_c', face = 'bold'))


gg_fig_1_5pane_comparison_global <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_global,
                                                      gg_fig_1c_yearlypct_comparison_combined_global,
                                                      gg_fig_1d_yearly_sponsor_comparison_pct_global,
                                                      gg_fig_1e_yearly_region_comparison_pct_global,
                                                      gg_fig_1b_yearly_enrollment_comparison_global,
                                                      nrow = 5, ncol = 1,
                                                      heights = c(2, 1, 2, 2, 2),
                                                      labels = c('A', 'B', 'C', 'D', 'E')) %>%
  annotate_figure(top= text_grob('gg_fig_1_5pane_comparison_global', face = 'bold'))

gg_fig_1_4pane_comparison_global_a <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_global,
                                                        # gg_fig_1c_yearlypct_comparison_combined_global,
                                                        gg_fig_1d_yearly_sponsor_comparison_pct_global,
                                                        gg_fig_1e_yearly_region_comparison_pct_global,
                                                        gg_fig_1b_yearly_enrollment_comparison_global,
                                                        nrow = 4, ncol = 1,
                                                        heights = c(2, 2, 2, 2),
                                                        labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_comparison_global_a', face = 'bold'))

gg_fig_1_4pane_comparison_global_b <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_global,
                                                        # gg_fig_1c_yearlypct_comparison_combined_global,
                                                        gg_fig_1d_yearly_sponsor_comparison_freq_global,
                                                        gg_fig_1e_yearly_region_comparison_freq_global,
                                                        gg_fig_1b_yearly_enrollment_comparison_global,
                                                        nrow = 4, ncol = 1,
                                                        heights = c(2, 2, 2, 2),
                                                        labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_comparison_global_b', face = 'bold'))

gg_fig_1_4pane_comparison_global_c <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_global,
                                                        gg_fig_1c_yearlypct_comparison_combined_global,
                                                        gg_fig_1d_yearly_sponsor_comparison_pct_global,
                                                        # gg_fig_1e_yearly_region_comparison_pct_global,
                                                        gg_fig_1b_yearly_enrollment_comparison_global,
                                                        nrow = 4, ncol = 1,
                                                        heights = c(2, 1, 2, 2),
                                                        labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_comparison_global_c', face = 'bold'))

gg_fig_1_4pane_comparison_global_d <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_global,
                                                        gg_fig_1c_yearlypct_comparison_combined_global,
                                                        gg_fig_1d_yearly_sponsor_comparison_freq_global,
                                                        # gg_fig_1e_yearly_region_comparison_pct_global,
                                                        gg_fig_1b_yearly_enrollment_comparison_global,
                                                        nrow = 4, ncol = 1,
                                                        heights = c(2, 1, 2, 2),
                                                        labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_comparison_global_d', face = 'bold'))

gg_fig_1_3pane_comparison_global_a <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_global,
                                                        # gg_fig_1c_yearlypct_comparison_combined_global,
                                                        gg_fig_1d_yearly_sponsor_comparison_pct_global,
                                                        # gg_fig_1e_yearly_region_comparison_pct_global,
                                                        gg_fig_1b_yearly_enrollment_comparison_global,
                                                        nrow = 3, ncol = 1,
                                                        heights = c(2, 2, 2),
                                                        labels = c('A', 'B', 'C')) %>%
  annotate_figure(top= text_grob('gg_fig_1_3pane_comparison_global_a', face = 'bold'))

gg_fig_1_3pane_comparison_global_b <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_global,
                                                        # gg_fig_1c_yearlypct_comparison_combined_global,
                                                        gg_fig_1d_yearly_sponsor_comparison_freq_global,
                                                        # gg_fig_1e_yearly_region_comparison_pct_global,
                                                        gg_fig_1b_yearly_enrollment_comparison_global,
                                                        nrow = 3, ncol = 1,
                                                        heights = c(2, 2, 2),
                                                        labels = c('A', 'B', 'C')) %>%
  annotate_figure(top= text_grob('gg_fig_1_3pane_comparison_global_b', face = 'bold'))

# USA - discard any of the figures that include regional data since this is restricted to only the USA
# gg_fig_1_5pane_gi_USA <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_USA, # Anything with regional data does not make sense here
#                                                  gg_fig_1c_yearlypct_gi_combined_USA,
#                                                  gg_fig_1d_yearly_sponsor_gi_pct_USA,
#                                                  gg_fig_1e_yearly_region_gi_pct_USA,
#                                                  gg_fig_1b_yearly_enrollment_gi_USA,
#                                                  nrow = 5, ncol = 1,
#                                                  heights = c(2, 1, 2, 2, 2),
#                                                  labels = c('A', 'B', 'C', 'D', 'E'))

# gg_fig_1_4pane_gi_USA_a <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_USA,
#                                                    # gg_fig_1c_yearlypct_gi_combined_USA,
#                                                    gg_fig_1d_yearly_sponsor_gi_pct_USA,
#                                                    gg_fig_1e_yearly_region_gi_pct_USA,
#                                                    gg_fig_1b_yearly_enrollment_gi_USA,
#                                                    nrow = 4, ncol = 1,
#                                                    heights = c(2, 2, 2, 2),
#                                                    labels = c('A', 'B', 'C', 'D'))

# gg_fig_1_4pane_gi_USA_b <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_USA,
#                                                    # gg_fig_1c_yearlypct_gi_combined_USA,
#                                                    gg_fig_1d_yearly_sponsor_gi_freq_USA,
#                                                    gg_fig_1e_yearly_region_gi_freq_USA,
#                                                    gg_fig_1b_yearly_enrollment_gi_USA,
#                                                    nrow = 4, ncol = 1,
#                                                    heights = c(2, 2, 2, 2),
#                                                    labels = c('A', 'B', 'C', 'D'))

# USA
gg_fig_1_4pane_gi_USA_c <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_USA,
                                                gg_fig_1c_yearlypct_gi_combined_USA,
                                                gg_fig_1d_yearly_sponsor_gi_pct_USA,
                                                # gg_fig_1e_yearly_region_gi_pct_USA,
                                                gg_fig_1b_yearly_enrollment_gi_USA,
                                                nrow = 4, ncol = 1,
                                                heights = c(2, 1, 2, 2),
                                                labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_gi_USA_c', face = 'bold'))

gg_fig_1_4pane_gi_USA_d <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_USA,
                                                gg_fig_1c_yearlypct_gi_combined_USA,
                                                gg_fig_1d_yearly_sponsor_gi_freq_USA,
                                                # gg_fig_1e_yearly_region_gi_pct_USA,
                                                gg_fig_1b_yearly_enrollment_gi_USA,
                                                nrow = 4, ncol = 1,
                                                heights = c(2, 1, 2, 2),
                                                labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_gi_USA_d', face = 'bold'))

gg_fig_1_4pane_gi_USA_e <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_USA,
                                                gg_fig_1c_yearlypct_gi_combined_USA,
                                                gg_fig_1d_yearly_sponsor_COMBINED_pct_USA_alpha,
                                                # gg_fig_1e_yearly_region_gi_pct_USA,
                                                gg_fig_1b_yearly_enrollment_gi_USA,
                                                nrow = 4, ncol = 1,
                                                heights = c(2, 1, 2, 2),
                                                labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_gi_USA_e', face = 'bold'))


gg_fig_1_3pane_gi_USA_a <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_USA,
                                                # gg_fig_1c_yearlypct_gi_combined_USA,
                                                gg_fig_1d_yearly_sponsor_gi_pct_USA,
                                                # gg_fig_1e_yearly_region_gi_pct_USA,
                                                gg_fig_1b_yearly_enrollment_gi_USA,
                                                nrow = 3, ncol = 1,
                                                heights = c(2, 2, 2),
                                                labels = c('A', 'B', 'C')) %>%
  annotate_figure(top= text_grob('gg_fig_1_3pane_gi_USA_a', face = 'bold'))

gg_fig_1_3pane_gi_USA_b <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_USA,
                                                # gg_fig_1c_yearlypct_gi_combined_USA,
                                                gg_fig_1d_yearly_sponsor_gi_freq_USA,
                                                # gg_fig_1e_yearly_region_gi_pct_USA,
                                                gg_fig_1b_yearly_enrollment_gi_USA,
                                                nrow = 3, ncol = 1,
                                                heights = c(2, 2, 2),
                                                labels = c('A', 'B', 'C')) %>%
  annotate_figure(top= text_grob('gg_fig_1_3pane_gi_USA_b', face = 'bold'))

gg_fig_1_3pane_gi_USA_c <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_USA,
                                                # gg_fig_1c_yearlypct_gi_combined_USA,
                                                gg_fig_1d_yearly_sponsor_COMBINED_pct_USA_alpha,
                                                # gg_fig_1e_yearly_region_gi_pct_USA,
                                                gg_fig_1b_yearly_enrollment_gi_USA,
                                                nrow = 3, ncol = 1,
                                                heights = c(2, 2, 2),
                                                labels = c('A', 'B', 'C')) %>%
  annotate_figure(top= text_grob('gg_fig_1_3pane_gi_USA_c', face = 'bold'))


# 
# gg_fig_1_5pane_comparison_USA <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_USA,
#                                                       gg_fig_1c_yearlypct_comparison_combined_USA,
#                                                       gg_fig_1d_yearly_sponsor_comparison_pct_USA,
#                                                       gg_fig_1e_yearly_region_comparison_pct_USA,
#                                                       gg_fig_1b_yearly_enrollment_comparison_USA,
#                                                       nrow = 5, ncol = 1,
#                                                       heights = c(2, 1, 2, 2, 2),
#                                                       labels = c('A', 'B', 'C', 'D', 'E'))
# 
# gg_fig_1_4pane_comparison_USA_a <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_USA,
#                                                         # gg_fig_1c_yearlypct_comparison_combined_USA,
#                                                         gg_fig_1d_yearly_sponsor_comparison_pct_USA,
#                                                         gg_fig_1e_yearly_region_comparison_pct_USA,
#                                                         gg_fig_1b_yearly_enrollment_comparison,
#                                                         nrow = 4, ncol = 1,
#                                                         heights = c(2, 2, 2, 2),
#                                                         labels = c('A', 'B', 'C', 'D'))
# 
# gg_fig_1_4pane_comparison_USA_b <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_USA,
#                                                         # gg_fig_1c_yearlypct_comparison_combined_USA,
#                                                         gg_fig_1d_yearly_sponsor_comparison_freq_USA,
#                                                         gg_fig_1e_yearly_region_comparison_freq_USA,
#                                                         gg_fig_1b_yearly_enrollment_comparison_USA,
#                                                         nrow = 4, ncol = 1,
#                                                         heights = c(2, 2, 2, 2),
#                                                         labels = c('A', 'B', 'C', 'D'))

gg_fig_1_4pane_comparison_USA_c <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_USA,
                                                     gg_fig_1c_yearlypct_comparison_combined_USA,
                                                     gg_fig_1d_yearly_sponsor_comparison_pct_USA,
                                                     # gg_fig_1e_yearly_region_comparison_pct_USA,
                                                     gg_fig_1b_yearly_enrollment_comparison_USA,
                                                     nrow = 4, ncol = 1,
                                                     heights = c(2, 1, 2, 2),
                                                     labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_comparison_USA_c', face = 'bold'))

gg_fig_1_4pane_comparison_USA_d <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_USA,
                                                     gg_fig_1c_yearlypct_comparison_combined_USA,
                                                     gg_fig_1d_yearly_sponsor_comparison_freq_USA,
                                                     # gg_fig_1e_yearly_region_comparison_pct_USA,
                                                     gg_fig_1b_yearly_enrollment_comparison_USA,
                                                     nrow = 4, ncol = 1,
                                                     heights = c(2, 1, 2, 2),
                                                     labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_comparison_USA_d', face = 'bold'))

gg_fig_1_3pane_comparison_USA_a <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_USA,
                                                     # gg_fig_1c_yearlypct_comparison_combined_USA,
                                                     gg_fig_1d_yearly_sponsor_comparison_pct_USA,
                                                     # gg_fig_1e_yearly_region_comparison_pct_USA,
                                                     gg_fig_1b_yearly_enrollment_comparison_USA,
                                                     nrow = 3, ncol = 1,
                                                     heights = c(2, 2, 2),
                                                     labels = c('A', 'B', 'C')) %>%
  annotate_figure(top= text_grob('gg_fig_1_3pane_comparison_USA_a', face = 'bold'))

gg_fig_1_3pane_comparison_USA_b <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_comparison_USA,
                                                     # gg_fig_1c_yearlypct_comparison_combined_USA,
                                                     gg_fig_1d_yearly_sponsor_comparison_freq_USA,
                                                     # gg_fig_1e_yearly_region_comparison_pct_USA,
                                                     gg_fig_1b_yearly_enrollment_comparison_USA,
                                                     nrow = 3, ncol = 1,
                                                     heights = c(2, 2, 2),
                                                     labels = c('A', 'B', 'C')) %>%
  annotate_figure(top= text_grob('gg_fig_1_3pane_comparison_USA_b', face = 'bold'))


# -------------------------------------------------------------------#
# -----------------            Figure 2:           ------------------#
#                 Proportion of Different Disease Groups             #
# -------------------------------------------------------------------#

code_names <- 
  name_table %>%
  filter(!is.na(longname)) %>%
  pull(code_name)
short_names <- 
  name_table %>%
  filter(!is.na(longname)) %>%
  pull(shortname)
long_names <- 
  name_table %>%
  filter(!is.na(longname)) %>%
  pull(longname)

lookup_short <- short_names
names(lookup_short) <- code_names

lookup_long <- long_names
names(lookup_long) <- code_names

# fig_df_2_disease_global <- 
#   table1_disease_combo_global %>%
#   mutate(rowname = fct_relevel(factor(rowname, levels = rowname), 'other_disease', after = Inf)) %>% 
#   mutate(proper_names = fct_reorder(factor(lookup_long[as.character(rowname)]), totaln_total, .desc = TRUE) %>%
#            fct_relevel('Other gilogic Disease', after = Inf)) %>% # Move that one to the end! 
#   mutate(finalnames = str_wrap(proper_names, width = 18)) %>%
#   mutate(finalnames = fct_reorder(finalnames, as.numeric(rowname), .desc = FALSE)) %>% # relevel to match the other factor levels
#   mutate(finalpct = case_when(
#     pct_total >= 0.001 ~ sprintf('%.1f%%', pct_total* 100), # '%%' escapes the % sign, 
#     TRUE ~ '<0.1%'
#   ))
#REPEAT THE SUBSEQUENT WITH LOCATION!!! BELOW

fig_df_2_disease_global <- 
  table1_disease_combo_global %>%
  mutate(rowname = fct_relevel(factor(rowname, levels = rowname), 'other', after = Inf)) %>% 
  mutate(proper_names = fct_reorder(factor(lookup_short[as.character(rowname)]), totaln_total, .desc = TRUE) %>%
           fct_relevel('Other_Disease', after = Inf)) %>% # Move that one to the end! 
  mutate(finalnames = str_wrap(proper_names, width = 18)) %>%
  mutate(finalnames = fct_reorder(finalnames, as.numeric(rowname), .desc = FALSE)) %>% # relevel to match the other factor levels
  mutate(finalpct = case_when(
    pct_total >= 0.001 ~ sprintf('%.1f%%', pct_total* 100), # '%%' escapes the % sign, 
    TRUE ~ '<0.1%'
  )) %>% 
  filter(rowname %nin% c('location')) # location is not included

fig_df_2_disease_USA <- 
  table1_disease_combo_USA %>%
  mutate(rowname = fct_relevel(factor(rowname, levels = rowname), 'other', after = Inf)) %>% 
  mutate(proper_names = fct_reorder(factor(lookup_short[as.character(rowname)]), totaln_total, .desc = TRUE) %>%
           fct_relevel('Other Disease', after = Inf)) %>% # Move that one to the end! 
  mutate(finalnames = str_wrap(proper_names, width = 18)) %>%
  mutate(finalnames = fct_reorder(finalnames, as.numeric(rowname), .desc = FALSE)) %>% # relevel to match the other factor levels
  mutate(finalpct = case_when(
    pct_total >= 0.001 ~ sprintf('%.1f%%', pct_total* 100), # '%%' escapes the % sign, 
    TRUE ~ '<0.1%'
  )) %>% 
  filter(rowname %nin% c('location')) # location is not included

fig_df_2_disease_enrollment_global <- 
  table1_disease_enrollment_general_global %>%
  mutate(proper_names = fct_reorder(factor(lookup_short[as.character(disease_subgroup)]), totalenrollment, .desc = TRUE) %>%
           fct_relevel('Other Disease', after = Inf)) %>% # To the back to the back
  mutate(finalnames = str_wrap(proper_names, width = 18)) %>%
  mutate(finalnames = fct_reorder(finalnames, as.numeric(proper_names), .desc = FALSE)) %>% # relevel to match the other factor levels
  mutate(final_freq = prettyNum(totalenrollment, big.mark = ',', big.interval = 3, scientific = FALSE))

fig_df_2_disease_enrollment_USA <- 
  table1_disease_enrollment_general_USA %>%
  mutate(proper_names = fct_reorder(factor(lookup_short[as.character(disease_subgroup)]), totalenrollment, .desc = TRUE) %>%
           fct_relevel('Other Disease', after = Inf)) %>% # To the back to the back
  mutate(finalnames = str_wrap(proper_names, width = 18)) %>%
  mutate(finalnames = fct_reorder(finalnames, as.numeric(proper_names), .desc = FALSE)) %>% # relevel to match the other factor levels
  mutate(final_freq = prettyNum(totalenrollment, big.mark = ',', big.interval = 3, scientific = FALSE))

fig_df_2_disease_sponsor_enrollment_global <- 
  table1_disease_enrollment_general_global %>%
  pivot_longer(cols = c(Industry,US.Govt,Other),
               names_to = 'sponsor',
               values_to = 'sponsor_enroll') %>% 
  mutate(proper_names = fct_reorder(factor(lookup_short[as.character(disease_subgroup)]), totalenrollment, .desc = TRUE) %>%
           fct_relevel('Other Disease', after = Inf)) %>% # To the back to the back
  mutate(finalnames = str_wrap(proper_names, width = 18)) %>%
  mutate(finalnames = fct_reorder(finalnames, as.numeric(proper_names), .desc = FALSE)) %>% # relevel to match the other factor levels
  mutate(final_freq = prettyNum(sponsor_enroll, big.mark = ',', big.interval = 3, scientific = FALSE))

fig_df_2_disease_sponsor_enrollment_USA <- 
  table1_disease_enrollment_general_USA %>%
  pivot_longer(cols = c(Industry,US.Govt,Other),
               names_to = 'sponsor',
               values_to = 'sponsor_enroll') %>% 
  mutate(proper_names = fct_reorder(factor(lookup_short[as.character(disease_subgroup)]), totalenrollment, .desc = TRUE) %>%
           fct_relevel('Other Disease', after = Inf)) %>% # To the back to the back
  mutate(finalnames = str_wrap(proper_names, width = 18)) %>%
  mutate(finalnames = fct_reorder(finalnames, as.numeric(proper_names), .desc = FALSE)) %>% # relevel to match the other factor levels
  mutate(final_freq = prettyNum(sponsor_enroll, big.mark = ',', big.interval = 3, scientific = FALSE))


fig2_df_enrollment_and_trials_global <- 
  left_join(fig_df_2_disease_global %>% 
              select(rowname, finalnames, totaln_total, finalpct), 
            fig_df_2_disease_enrollment_global %>% 
              select(disease_subgroup, finalnames, totalenrollment, final_freq), 
            by = c('rowname' = 'disease_subgroup', 'finalnames' = 'finalnames')) %>% 
  mutate(finalnames_orderby_totaln = fct_reorder(finalnames, totaln_total, .desc = TRUE) %>% fct_relevel('Other Disease', after = Inf)) %>% 
  mutate(finalnames_orderby_totaln = fct_rev(finalnames_orderby_totaln)) %>%
  mutate(dummy0 = 0) %>%
  as_tibble()

fig2_df_enrollment_and_trials_USA <- 
  left_join(fig_df_2_disease_USA %>% 
              select(rowname, finalnames, totaln_total, finalpct), 
            fig_df_2_disease_enrollment_USA %>% 
              select(disease_subgroup, finalnames, totalenrollment, final_freq), 
            by = c('rowname' = 'disease_subgroup', 'finalnames' = 'finalnames')) %>% 
  mutate(finalnames_orderby_totaln = fct_reorder(finalnames, totaln_total, .desc = TRUE) %>% fct_relevel('Other Disease', after = Inf)) %>% 
  mutate(finalnames_orderby_totaln = fct_rev(finalnames_orderby_totaln)) %>%
  mutate(dummy0 = 0) %>%
  as_tibble()

cols_disease_enrollment_order_global <- 
  table1_disease_enrollment_general_global %>% 
  filter(disease_subgroup != 'other') %>%
  pull(disease_subgroup)

cols_disease_enrollment_order_USA <- 
  table1_disease_enrollment_general_USA %>% 
  filter(disease_subgroup != 'other') %>%
  pull(disease_subgroup)

df_fig_2_disease_sponsor_location <- 
  bind_rows(table1_disease_enrollment_year_global,
            table1_disease_enrollment_year_USA) %>%
  mutate(proper_names = fct_reorder(factor(lookup_short[as.character(disease_subgroup)]), enrollment_spec_diseaseyear, .desc = TRUE) %>%
           fct_relevel('Other Disease', after = Inf)) %>% # To the back to the back
  mutate(finalnames = str_wrap(proper_names, width = 18)) %>%
  mutate(disease_subgroup = factor(disease_subgroup, levels = c(cols_disease_enrollment_order_global, 'other'))) %>%
  mutate(finalnames = fct_reorder(finalnames, as.numeric(disease_subgroup), .desc = FALSE)) %>% # relevel to match the other factor levels
  mutate(final_freq = prettyNum(enrollment_spec_diseaseyear, big.mark = ',', big.interval = 3, scientific = FALSE)) %>%
  filter(year_trial > 2007, # these years are not complete!  
         year_trial < 2020)

# Number of Trials by Disease

# Number of Trials by Disease (bar) - Global
gg_fig_2_disease_bar_global <-
  ggplot(fig_df_2_disease_global, aes(x = finalnames, y = pct_total)) + 
  geom_bar(stat = 'identity',
           fill = color2,
           width = 0.65) + 
  geom_text(aes(x = finalnames, y = pct_total+0.002, label = finalpct),
            vjust = -0.45,
            size = 3.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  xlab('gilogic Disease Subfields') + 
  ylab('Proportion of gilogic Trials (%)') + 
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0, 0.35, 0.06)) + 
  coord_cartesian(ylim = c(0, 0.43)) # set manualy, can comment this out at first to see what the approximate height will be

# Number of Trials by Disease (bar) - USA
gg_fig_2_disease_bar_USA <-
  ggplot(fig_df_2_disease_USA, aes(x = finalnames, y = pct_total)) + 
  geom_bar(stat = 'identity',
           fill = color2,
           width = 0.65) + 
  geom_text(aes(x = finalnames, y = pct_total+0.002, label = finalpct),
            vjust = -0.45,
            size = 3.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  xlab('gilogic Disease Subfields') + 
  ylab('Proportion of gilogic Trials (%)') + 
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0, 0.35, 0.06)) + 
  coord_cartesian(ylim = c(0, 0.5)) # set manualy, can comment this out at first to see what the approximate height will be

gg_fig_2_disease_bar_global
gg_fig_2_disease_bar_USA

# Disease by enrollment
# Enrollment - Global - gi - Disease (bar)
gg_fig_2_disease_enrollment_bar_global <-
  ggplot(data = fig_df_2_disease_enrollment_global,
         aes(x = finalnames, y = totalenrollment)) +
  geom_bar(stat = 'identity',
           fill = color2,
           width = 0.65) +
  geom_text(aes(x =finalnames, 
                y = totalenrollment,
                label = final_freq),
            vjust = -0.85,
            size = 3.5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  xlab('gilogic Disorder Category') + 
  ylab('Total Enrollment \nAmong gilogic Trials') + 
  scale_y_continuous(labels = scales::comma,
                     # breaks = scales::pretty_breaks(n = 5)) + 
                     breaks = seq(0, 
                                  max(fig_df_2_disease_enrollment_global$totalenrollment)*1.15, 
                                  roundUpNice(max(fig_df_2_disease_enrollment_global$totalenrollment)/7, nice = c(1,2,3,4,5,6,7,8,9,10)))) + 
  coord_cartesian(ylim = c(0, max(fig_df_2_disease_enrollment_global$totalenrollment)*1.15))

# Enrollment - USA - gi - Disease (bar)
gg_fig_2_disease_enrollment_bar_USA <-
  ggplot(data = fig_df_2_disease_enrollment_USA,
         aes(x = finalnames, y = totalenrollment)) +
  geom_bar(stat = 'identity',
           fill = color2,
           width = 0.65) +
  geom_text(aes(x =finalnames, 
                y = totalenrollment,
                label = final_freq),
            vjust = -0.85,
            size = 3.5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  xlab('gilogic Disorder Category') + 
  ylab('Total Enrollment \nAmong gilogic Trials') + 
  scale_y_continuous(labels = scales::comma,
                     # breaks = scales::pretty_breaks(n = 5)) + 
                     breaks = seq(0, 
                                  max(fig_df_2_disease_enrollment_USA$totalenrollment)*1.15, 
                                  roundUpNice(max(fig_df_2_disease_enrollment_USA$totalenrollment)/7, nice = c(1,2,3,4,5,6,7,8,9,10)))) + 
  coord_cartesian(ylim = c(0, max(fig_df_2_disease_enrollment_USA$totalenrollment)*1.15))

# Enrollment - Global - gi - Disease - Sponsor - (bar)
gg_fig_2_disease_sponsor_enrollment_bar_global <-
  ggplot(data = fig_df_2_disease_sponsor_enrollment_global,
         aes(x = finalnames, y = sponsor_enroll)) +
  geom_bar(aes(fill = sponsor),
           position = 'dodge2',
           stat = 'identity',
           width = 0.65) + 
  # geom_text(aes(x =finalnames, 
  #               y = sponsor_enroll,
  #               label = final_freq),
  #           vjust = -0.85,
  #           size = 3.5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        # panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  xlab('gilogic Disorder Category') + 
  ylab('Total Enrollment \nAmong gilogic Trials') + 
  scale_y_continuous(labels = scales::comma,
                     # breaks = scales::pretty_breaks(n = 5)) + 
                     breaks = seq(0, 
                                  max(fig_df_2_disease_sponsor_enrollment_global$sponsor_enroll)*1.15, 
                                  roundUpNice(max(fig_df_2_disease_sponsor_enrollment_global$sponsor_enroll)/7, nice = c(1,2,3,4,5,6,7,8,9,10)))) + 
  coord_cartesian(ylim = c(0, max(fig_df_2_disease_sponsor_enrollment_global$sponsor_enroll)*1.15)) + 
  # scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5)

# Enrollment - USA - gi - Disease - Sponsor - (bar)
gg_fig_2_disease_sponsor_enrollment_bar_USA <-
  ggplot(data = fig_df_2_disease_sponsor_enrollment_USA,
         aes(x = finalnames, y = sponsor_enroll)) +
  geom_bar(aes(fill = sponsor),
           position = 'dodge2',
           stat = 'identity',
           width = 0.65) + 
  # geom_text(aes(x =finalnames, 
  #               y = sponsor_enroll,
  #               label = final_freq),
  #           vjust = -0.85,
  #           size = 3.5) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        # panel.grid = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  xlab('gilogic Disorder Category') + 
  ylab('Total Enrollment \nAmong gilogic Trials') + 
  scale_y_continuous(labels = scales::comma,
                     # breaks = scales::pretty_breaks(n = 5)) + 
                     breaks = seq(0, 
                                  max(fig_df_2_disease_sponsor_enrollment_USA$sponsor_enroll)*1.15, 
                                  roundUpNice(max(fig_df_2_disease_sponsor_enrollment_USA$sponsor_enroll)/7, nice = c(1,2,3,4,5,6,7,8,9,10)))) + 
  coord_cartesian(ylim = c(0, max(fig_df_2_disease_sponsor_enrollment_USA$sponsor_enroll)*1.15)) + 
  # scale_color_manual(values = g_industry_color5) + 
  scale_fill_manual(values = g_industry_color5)


# Enrollment - COMBINED - gi - Disease - (lines)
gg_fig_2_disease_location_year_line <- 
  ggplot(data = df_fig_2_disease_sponsor_location,
         aes(x = year_trial, y = enrollment_spec_diseaseyear)) + 
  geom_line(aes(color = USA_vs_Global),
            size = 3) + 
  facet_wrap(~finalnames,
             scales = 'free') + 
  theme_bw() + 
  theme(# panel.grid = element_blank(),
    axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  xlab('Year of Trial Submission') + 
  ylab('Total Enrollment Among Trials') + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(2008, 2011, 2014, 2017),
                     minor_breaks = seq(2008, 2017, 1))

# Enrollment vs Number of Trials - Global - gi - Disease (back-to-back bar)
# middle
g_names_enrollment_trials_global <- 
  ggplot(fig2_df_enrollment_and_trials_global,aes(x=1,y=finalnames_orderby_totaln)) +
  geom_text(aes(label=finalnames_orderby_totaln)) + 
  # geom_bar(aes(x = dummy0, y = finalnames_orderby_totaln),
  #          stat = 'identity',
  #          fill = 'red',
  #          alpha = 0,
  #          width = 0.65) + 
  geom_segment(aes(x=0.50,xend=0.60,yend=finalnames_orderby_totaln)) +
  geom_segment(aes(x=1.40,xend=1.50,yend=finalnames_orderby_totaln)) +
  ggtitle("") +
  ylab(NULL) +
  # theme_bw() +
  scale_x_continuous(expand=c(0,0),limits=c(0.50,1.5)) +
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(6.5,1,-4.0,0), "mm")# c(T,R,B,L); You have to keep adjusting the margins here (top and bottom) to make it line up
        #                                         # It's total trial and error and a bit of a pain...but it works. 
  )


g_enrolldata_for_enrollment_trials_global <-
  ggplot(data = fig2_df_enrollment_and_trials_global,
         aes(x = finalnames_orderby_totaln, y = totalenrollment)) +
  geom_bar(stat = 'identity',
           # width = 0.65,
           fill = color2) +
  # geom_text(aes(x =finalnames_orderby_totaln, 
  #               y = totalenrollment,
  #               label = final_freq),
  #           vjust = -0.85,
  #           size = 3.5) + 
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 35, hjust = 1),
  #       panel.grid = element_blank(),
  #       axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,1,1,0), "mm")) + # c(T,R,B,L)
  xlab('gilogic Disorder Category') + 
  ylab('Total Enrollment \nAmong gilogic Trials') + 
  # scale_y_continuous(labels = scales::comma,  
  scale_y_reverse(labels = scales::comma, # If you use reverse it'll reverse the direction but still takes same arguments as scale_y_continuous
                  # breaks = scales::pretty_breaks(n = 5)) + 
                  expand = expand_scale(mult = c(0.08, .01)),
                  position = 'bottom',
                  breaks = seq(0, 
                               max(fig2_df_enrollment_and_trials_global$totalenrollment)*1.15, 
                               roundUpNice(max(fig2_df_enrollment_and_trials_global$totalenrollment)/7, nice = c(1,2,3,4,5,6,7,8,9,10)))) + 
  coord_cartesian(ylim = c(0, max(fig2_df_enrollment_and_trials_global$totalenrollment)*1.15)) + 
  ggtitle('Total Enrollment') +
  coord_flip(); g_enrolldata_for_enrollment_trials_global


g_numtrialsdata_for_enrollment_trials_global <- 
  ggplot(fig2_df_enrollment_and_trials_global, aes(x = finalnames_orderby_totaln, y = totaln_total)) + 
  geom_bar(stat = 'identity',
           # width = 0.65,
           fill = color2) + 
  # geom_text(aes(x = finalnames_orderby_totaln, y = pct_total+0.002, label = finalpct),
  #           vjust = -0.45,
  #           size = 3.5) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #       panel.grid = element_blank(),
  #       axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  
  xlab('gilogic Disorder Category') + 
  ylab('Proportion of gilogic Trials (%)') + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        # axis.ticks.x.bottom = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,1,1,-1), "mm"), # c(T,R,B,L)
        plot.title = element_text(hjust = 1) # hjust(0 = left, 0.5 = center, 1 = right)
  ) + 
  scale_y_continuous(expand = expand_scale(mult = c(0.01, 0.08)),
                     position = 'bottom') + # this works really bizarre, position = 'bottom'/'left' both move it to the top... who knows
  ggtitle('Number of Trials') +
  coord_flip(); g_numtrialsdata_for_enrollment_trials_global

# ********************************
# Enrollment vs Number of Trials - USA - gi - Disease (back-to-back bar)
# middle
g_names_enrollment_trials_USA <- 
  ggplot(fig2_df_enrollment_and_trials_USA,aes(x=1,y=finalnames_orderby_totaln)) +
  geom_text(aes(label=finalnames_orderby_totaln)) + 
  # geom_bar(aes(x = dummy0, y = finalnames_orderby_totaln),
  #          stat = 'identity',
  #          fill = 'red',
  #          alpha = 0,
  #          width = 0.65) + 
  geom_segment(aes(x=0.50,xend=0.60,yend=finalnames_orderby_totaln)) +
  geom_segment(aes(x=1.40,xend=1.50,yend=finalnames_orderby_totaln)) +
  ggtitle("") +
  ylab(NULL) +
  # theme_bw() +
  scale_x_continuous(expand=c(0,0),limits=c(0.50,1.5)) +
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(6.5,1,-4.0,0), "mm")# c(T,R,B,L); You have to keep adjusting the margins here (top and bottom) to make it line up
        #                                         # It's total trial and error and a bit of a pain...but it works. 
  )

g_enrolldata_for_enrollment_trials_USA <-
  ggplot(data = fig2_df_enrollment_and_trials_USA,
         aes(x = finalnames_orderby_totaln, y = totalenrollment)) +
  geom_bar(stat = 'identity',
           # width = 0.65,
           fill = color2) +
  # geom_text(aes(x =finalnames_orderby_totaln, 
  #               y = totalenrollment,
  #               label = final_freq),
  #           vjust = -0.85,
  #           size = 3.5) + 
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 35, hjust = 1),
  #       panel.grid = element_blank(),
  #       axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,1,1,0), "mm")) + # c(T,R,B,L)
  xlab('gilogic Disorder Category') + 
  ylab('Total Enrollment \nAmong gilogic Trials') + 
  # scale_y_continuous(labels = scales::comma,  
  scale_y_reverse(labels = scales::comma, # If you use reverse it'll reverse the direction but still takes same arguments as scale_y_continuous
                  # breaks = scales::pretty_breaks(n = 5)) + 
                  expand = expand_scale(mult = c(0.08, .01)),
                  position = 'bottom',
                  breaks = seq(0, 
                               max(fig2_df_enrollment_and_trials_USA$totalenrollment)*1.15, 
                               roundUpNice(max(fig2_df_enrollment_and_trials_USA$totalenrollment)/6, nice = c(1,2,3,4,5,6,7,8,9,10)))) + 
  coord_cartesian(ylim = c(0, max(fig2_df_enrollment_and_trials_USA$totalenrollment)*1.15)) + 
  ggtitle('Total Enrollment') +
  coord_flip(); g_enrolldata_for_enrollment_trials_USA


g_numtrialsdata_for_enrollment_trials_USA <- 
  ggplot(fig2_df_enrollment_and_trials_USA, aes(x = finalnames_orderby_totaln, y = totaln_total)) + 
  geom_bar(stat = 'identity',
           # width = 0.65,
           fill = color2) + 
  # geom_text(aes(x = finalnames_orderby_totaln, y = pct_total+0.002, label = finalpct),
  #           vjust = -0.45,
  #           size = 3.5) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #       panel.grid = element_blank(),
  #       axis.title.y = element_text(margin = margin(t=0, r = 10, b = 0, l = 0))) + # give some space between axis title! 
  
  xlab('gilogic Disorder Category') + 
  ylab('Proportion of gilogic Trials (%)') + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        # axis.ticks.x.bottom = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,1,1,-1), "mm"), # c(T,R,B,L)
        plot.title = element_text(hjust = 1) # hjust(0 = left, 0.5 = center, 1 = right)
  ) + 
  scale_y_continuous(expand = expand_scale(mult = c(0.01, 0.08)),
                     position = 'bottom') + # this works really bizarre, position = 'bottom'/'left' both move it to the top... who knows
  ggtitle('Number of Trials') +
  coord_flip(); g_numtrialsdata_for_enrollment_trials_USA

# gg1 <- ggplot_gtable(ggplot_build(g_enrolldata_for_enrollment_trials_global))
# gg2 <- ggplot_gtable(ggplot_build(g_numtrialsdata_for_enrollment_trials_global))
# gg.mid <- ggplot_gtable(ggplot_build(g_names_enrollment_trials_global))
# 
# gridExtra::grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1.5/9,4/9))

gg_fig_2_enrollment_vs_numtrials_disease_global_bar <- 
  ggpubr::ggarrange(g_enrolldata_for_enrollment_trials_global,
                    g_names_enrollment_trials_global,
                    g_numtrialsdata_for_enrollment_trials_global,
                    nrow = 1, ncol = 3,
                    widths = c(4, 1.5, 4)) %>%
  annotate_figure(top= text_grob('gg_fig_2_enrollment_vs_numtrials_disease_global_bar', face = 'bold', color = 'grey'))

gg_fig_2_enrollment_vs_numtrials_disease_global_bar


gg_fig_2_enrollment_vs_numtrials_disease_USA_bar <- 
  ggpubr::ggarrange(g_enrolldata_for_enrollment_trials_USA,
                    g_names_enrollment_trials_USA,
                    g_numtrialsdata_for_enrollment_trials_USA,
                    nrow = 1, ncol = 3,
                    widths = c(4, 1.5, 4)) %>%
  annotate_figure(top= text_grob('gg_fig_2_enrollment_vs_numtrials_disease_USA_bar', face = 'bold', color = 'grey'))

gg_fig_2_enrollment_vs_numtrials_disease_USA_bar

# -------------------------------------------------------------------#
# -----------------            Figure 3:           ------------------#
#       Comparing Prevalence/Burden of Disease w/ Trial Activity     #
# -------------------------------------------------------------------#

####### Figure 3 - Prevalence Comparison --------------------------------------


# # which groups might we need to parse further? 
# full_gi_df %>% filter(all_countries == "UnitedStates") %>% summarise_at(vars(substance, schizophrenia, trauma, sleep), funs(sum(.)))
# 
# nct_usa_substance <- full_gi_df %>% filter(all_countries == 'UnitedStates') %>% filter(substance) %>% pull(nct_id)
# nct_usa_trauma <- full_gi_df %>% filter(all_countries == 'UnitedStates') %>% filter(trauma) %>% pull(nct_id)
# nct_usa_sleep <- full_gi_df %>% filter(all_countries == 'UnitedStates') %>% filter(sleep) %>% pull(nct_id)
# nct_usa_schizophrenia <- full_gi_df %>% filter(all_countries == 'UnitedStates') %>% filter(schizophrenia) %>% pull(nct_id)
# 
# # cases I need Josh & Brannon to parse further
# readr::write_excel_csv(x = raw_gi_list %>% 
#                          select(coder, nct_id, official_title, abstract) %>%
#                          filter(nct_id %in% nct_usa_substance) %>% 
#                          rowid_to_column(), 
#                        path = 'Radiation Trials/PsychiatryAnalysis/USAsubstance.csv')
# 
# readr::write_excel_csv(x = raw_gi_list %>% 
#                          select(coder, nct_id, official_title, abstract) %>%
#                          filter(nct_id %in% nct_usa_trauma) %>% 
#                          rowid_to_column(), 
#                        path = 'Radiation Trials/PsychiatryAnalysis/USAtrauma.csv')
# 
# readr::write_excel_csv(x = raw_gi_list %>% 
#                          select(coder, nct_id, official_title, abstract) %>%
#                          filter(nct_id %in% nct_usa_sleep) %>% 
#                          rowid_to_column(), 
#                        path = 'Radiation Trials/PsychiatryAnalysis/USAsleep.csv')
# 
# readr::write_excel_csv(x = raw_gi_list %>% 
#                          select(coder, nct_id, official_title, abstract) %>%
#                          filter(nct_id %in% nct_usa_schizophrenia) %>% 
#                          rowid_to_column(), 
#                        path = 'Radiation Trials/PsychiatryAnalysis/USAschizophrenia.csv')
# 
# # get josh parsing ptsd/insomnia from general categories of trauma/sleep answers...
# nct_usa_ptsd_josh <-
#   openxlsx::read.xlsx(xlsxFile = 'Radiation Trials/PsychiatryAnalysis/USA_trauma_sleep_joshs.xlsx', sheet = 2) %>%
#   filter(`y/n` == 'y') %>%
#   pull(nct_id)
# nct_usa_insomnia_josh <- openxlsx::read.xlsx(xlsxFile = 'Radiation Trials/PsychiatryAnalysis/USA_trauma_sleep_joshs.xlsx', sheet = 1) %>%
#   filter(`y/n` == 'y') %>%
#   pull(nct_id)
# 
# 
# # load the US prevalence
# gi_prev_data <- 
#   openxlsx::read.xlsx(xlsxFile = 'Radiation Trials/PsychiatryAnalysis/prevalence_data.xlsx', sheet = 2) %>% 
#   as_tibble() %>%
#   select(disease, lower, upper, truename) %>%
#   mutate(lower = lower /100, # convert to pct
#          upper = upper /100)
# 
# # get our US proportions
# fig3_table_disease1 <- 
#   full_gi_df %>%
#   filter(all_countries == 'UnitedStates') %>% # that's 6319 total trials that are only in the US 
#   # select(one_of(cols_disease_in_order[1:5]), # if you only want to include a subset of the diseases in the table, use these...
#   #        cols_disease_other5) %>%
#   select(one_of(cols_disease_in_order), 
#          other_disease) %>%
#   summarise_all(sum) %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   # filter(rowname %nin% c('other_disease')) %>% 
#   arrange(desc(V1)) %>%
#   mutate(total_pct = round(V1 / sum(V1), 3),
#          cumpct = round(cumsum(V1 / sum(V1)), 3)) %>%
#   rename('totaln' = 'V1')
# 
# fig3_totaln <- sum(fig3_table_disease1$totaln)
# 
# # change to update with new insomnia & ptsd data
# fig3_table_disease1 <-
#   fig3_table_disease1 %>%
#   mutate(totaln = case_when(rowname == 'trauma' ~ length(nct_usa_ptsd_josh), rowname == 'sleep' ~ length(nct_usa_insomnia_josh), TRUE ~ totaln)) %>%
#   arrange(desc(totaln)) %>%
#   mutate(total_pct = round(totaln / fig3_totaln, 3))
# 
# # merge the prevalence data, this is the wide version for geom_line
# fig3_table_disease <-
#   fig3_table_disease1 %>%
#   left_join(gi_prev_data,
#             by = c('rowname' = 'disease')) %>%
#   rowwise() %>%
#   mutate(middle_prev = mean(c(upper, lower)),
#          anyDiff = abs(diff(c(upper, lower))) > 0) %>%
#   ungroup() %>%
#   filter(!is.na(lower))
# 
# # make a long version for using geom_point
# fig3_table_disease_points <-
#   fig3_table_disease %>%
#   tidyr::gather(key = 'up_vs_low', value = 'value', lower, upper)
# 
# # make the figure
# ggfig3_prev <-
#   ggplot() +
#   geom_point(data = fig3_table_disease_points, # for the dots
#              aes(x = value, y = total_pct),
#              size = 4,
#              color = 'firebrick') + 
#   # see here for great vignette on using repel: https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
#   ggrepel::geom_text_repel(data = fig3_table_disease, 
#                            aes(x = upper, y = total_pct, label = truename),
#                            hjust = 0, # hjust =0 means L aligned; hjust = 1 means R aligned
#                            nudge_x = 0.003) +
#   geom_segment(data = fig3_table_disease %>% filter(anyDiff), 
#                aes(x = lower, xend = upper,
#                    y = total_pct, yend=total_pct),
#                size = 1.5,
#                color = 'firebrick') +
#   geom_smooth(data = fig3_table_disease, 
#               # aes(x = total_pct, y = middle_prev), 
#               aes(x= middle_prev, y = total_pct),
#               color = 'blue',
#               size = 1.25,
#               se = FALSE, # do you even want to plot the conf bands?
#               level = 0.95, # set the confidence level
#               method = 'lm') +
#   theme_bw() + 
#   theme(panel.grid.major.x = element_blank(), # remove vertical gridlines
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.y = element_blank()) +
#   labs(x = 'Lifetime Prevalence in US Population', y = 'Proportion of US Clinical Trials (2007-2018)') +
#   scale_y_continuous(labels = scales::percent,
#                      breaks = seq(0, .30, 0.05)) + 
#   scale_x_continuous(labels = scales::percent,
#                      breaks = seq(0, 0.30, 0.05)) + 
#   coord_cartesian(ylim = c(0, 0.27), 
#                   xlim = c(0, 0.27))
# 
# ggfig3_prev
# 
# ggsave(filename = 'Radiation Trials/PsychiatryAnalysis/prev_vs_ct.png', 
#        plot = ggfig3_prev,
#        width = 11, height = 5)


# ------------------------------------------------------------------------------#

# -------------------------------------------------------------------#
# -----------------            Figure 4:           ------------------#
#                 "Survival" w/o Early Discontinuation               #
# -------------------------------------------------------------------#

####### Figure 4 - Survival ED --------------------------------------

fig_df_surv_ED_gi_global <- 
  full_gi_df %>%
  mutate(br_singleregion4 = as.character(br_singleregion4)) %>% # This shouldn't be necessary, but currently a bug in survminer that needs to be fixed
  filter(br_trialduration >= 0)

fig_df_surv_ED_comparison_global <- 
  full_comparison_df %>%
  mutate(br_singleregion4 = as.character(br_singleregion4)) %>% # See here for more on the survminer bug: https://github.com/kassambara/survminer/issues/337
  filter(br_trialduration >= 0)

fig_df_surv_ED_gi_USA <- 
  full_gi_df %>%
  mutate(br_singleregion4 = as.character(br_singleregion4)) %>%
  filter(USA_only_facilities) %>%
  filter(br_trialduration >= 0)

fig_df_surv_ED_comparison_USA <- 
  full_comparison_df %>%
  mutate(br_singleregion4 = as.character(br_singleregion4)) %>%
  filter(USA_only_facilities) %>%
  filter(br_trialduration >= 0)

fig_df_surv_ED_combined_global <- 
  full_spec_combined_df %>%
  filter(br_trialduration >= 0)

fig_df_surv_ED_combined_USA <- 
  full_spec_combined_df %>%
  filter(USA_only_facilities) %>%
  filter(br_trialduration >= 0)



fit_surv_sponsor_gi_global <- survfit(Surv(br_trialduration, br_censor_earlydiscontinuation) ~ industry_any2b, data = fig_df_surv_ED_gi_global)
fit_surv_sponsor_comparison_global <- survfit(Surv(br_trialduration, br_censor_earlydiscontinuation) ~ industry_any2b, data = fig_df_surv_ED_comparison_global)

fit_surv_sponsor_gi_USA <- survfit(Surv(br_trialduration, br_censor_earlydiscontinuation) ~ industry_any2b, data = fig_df_surv_ED_gi_USA)
fit_surv_sponsor_comparison_USA <- survfit(Surv(br_trialduration, br_censor_earlydiscontinuation) ~ industry_any2b, data = fig_df_surv_ED_comparison_USA)

fit_surv_region_gi_global <- survfit(Surv(br_trialduration, br_censor_earlydiscontinuation) ~ br_singleregion4, data = fig_df_surv_ED_gi_global)
fit_surv_region_comparison_global <- survfit(Surv(br_trialduration, br_censor_earlydiscontinuation) ~ br_singleregion4, data = fig_df_surv_ED_comparison_global)

fit_surv_specialty_combined_USA <- survfit(Surv(br_trialduration, br_censor_earlydiscontinuation) ~ specialty_source, data = fig_df_surv_ED_combined_USA)
fit_surv_specialty_combined_global <- survfit(Surv(br_trialduration, br_censor_earlydiscontinuation) ~ specialty_source, data = fig_df_surv_ED_combined_global)


# --------- Survival Plots ----------#
# early discontiuation by funding source (simple chart only, no table)
ggsurvplot(fit_surv_sponsor_gi_global, 
           data = fig_df_surv_ED_gi_global, 
           color = 'industry_any2b', 
           palette = g_industry_color5,
           xlim = c(0,60), 
           size = 1.5, 
           # cumevents = TRUE, 
           censor.shape = 124,
           censor.size = 2.0, 
           # risk.table = TRUE,
           break.x.by = 12) +
  ylab('Non-Discontinuation') + 
  xlab("Trial Duration (Months)")

# early discontinuation by funding source
# Global - gi - Sponsor - Early Discontinuation (surv)
gg_fig_surv_ED_sponsor_gi_global <- 
  ggsurvplot(fit_surv_sponsor_gi_global, 
             data = fig_df_surv_ED_gi_global, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Non-Discontinuation') + 
  xlab("Trial Duration (Months)"); gg_fig_surv_ED_sponsor_gi_global

# Global - Comparison - Sponsor - Early Discontinuation (surv)
gg_fig_surv_ED_sponsor_comparison_global <- 
  ggsurvplot(fit_surv_sponsor_comparison_global, 
             data = fig_df_surv_ED_comparison_global, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Non-Discontinuation') + 
  xlab("Trial Duration (Months)"); gg_fig_surv_ED_sponsor_comparison_global

# USA - gi - Sponsor - Early Discontinuation (surv)
gg_fig_surv_ED_sponsor_gi_USA <- 
  ggsurvplot(fit_surv_sponsor_gi_USA, 
             data = fig_df_surv_ED_gi_USA, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Non-Discontinuation') + 
  xlab("Trial Duration (Months)"); gg_fig_surv_ED_sponsor_gi_USA

# Global - Comparison - Sponsor - Early Discontinuation (surv)
gg_fig_surv_ED_sponsor_comparison_USA <- 
  ggsurvplot(fit_surv_sponsor_comparison_USA, 
             data = fig_df_surv_ED_comparison_USA, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Non-Discontinuation') + 
  xlab("Trial Duration (Months)"); gg_fig_surv_ED_sponsor_comparison_USA

# Global - gi - Region - Early Discontinuation (surv)
gg_fig_surv_ED_region_gi_global <- 
  ggsurvplot(fit_surv_region_gi_global, 
             data = fig_df_surv_ED_gi_global,
             color = 'br_singleregion4', # a color without a palette creates trouble right now
             palette = g_region_color6b, # see here: https://github.com/kassambara/survminer/issues/337
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 4.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Non-Discontinuation') + 
  xlab("Trial Duration (Months)"); gg_fig_surv_ED_region_gi_global

# Global - Comparison - Region - Early Discontinuation 
gg_fig_surv_ED_region_comparison_global <- 
  ggsurvplot(fit_surv_region_comparison_global, 
             data = fig_df_surv_ED_comparison_global,
             color = 'br_singleregion4', # a color without a palette creates trouble right now
             palette = g_region_color6b, # see here: https://github.com/kassambara/survminer/issues/337
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 4.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Non-Discontinuation') + 
  xlab("Trial Duration (Months)"); gg_fig_surv_ED_region_comparison_global

# Global - COMBINED - Specialty - Early Discontinuation (surv)
gg_fig_surv_ED_specialty_combined_global <- 
  ggsurvplot(fit_surv_specialty_combined_global, 
             data = fig_df_surv_ED_combined_global, 
             # color = 'industry_any2b', 
             # palette = g_industry_color5,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Non-Discontinuation') + 
  xlab("Trial Duration (Months)"); gg_fig_surv_ED_specialty_combined_global

# USA - COMBINED - Specialty - Early Discontinuation (surv)
gg_fig_surv_ED_specialty_combined_USA <- 
  ggsurvplot(fit_surv_specialty_combined_USA, 
             data = fig_df_surv_ED_combined_USA, 
             # color = 'industry_any2b', 
             # palette = g_industry_color5,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Non-Discontinuation') + 
  xlab("Trial Duration (Months)"); gg_fig_surv_ED_specialty_combined_USA

gg_fig_surv_ED_sponsor_gi_global
gg_fig_surv_ED_sponsor_comparison_global

gg_fig_surv_ED_sponsor_gi_USA
gg_fig_surv_ED_sponsor_comparison_USA

gg_fig_surv_ED_region_gi_global
gg_fig_surv_ED_region_comparison_global

gg_fig_surv_ED_specialty_combined_global
gg_fig_surv_ED_specialty_combined_USA

# more on saving ggsurvplot objects here: https://github.com/kassambara/survminer/issues/152

# ----- Cumulative Incidence Plots ------#
ggsurvplot(fit_surv_sponsor_gi_global, 
           fun = 'event',
           data = fig_df_surv_ED_gi_global, 
           color = 'industry_any2b', 
           palette = g_industry_color5,
           xlim = c(0,60), 
           # ylim = c(0, 0.25), # using ylim in this way removes data outside the range...not what we want
           ylab = 'Cumulative incidence of\nearly discontinuation',
           size = 1.5, 
           # cumevents = TRUE, 
           censor.shape = 124,
           censor.size = 2.0, 
           # risk.table = TRUE,
           break.x.by = 12) +
  ylab('Early Discontinuation') + 
  xlab("Trial Duration (Months)")

# Global - gi - Sponsor - Early Discontinuation (cumulative)
gg_fig_surv_cumulative_ED_sponsor_gi_global <- 
  ggsurvplot(fit_surv_sponsor_gi_global, 
             fun = 'event',
             data = fig_df_surv_ED_gi_global, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nearly discontinuation',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('Early Discontinuation') + 
  xlab("Trial Duration (Months)")
gg_fig_surv_cumulative_ED_sponsor_gi_global$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_ED_sponsor_gi_global$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.40)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))

# Global - Comparison - Sponsor - Early Discontinuation (cumulative)
gg_fig_surv_cumulative_ED_sponsor_comparison_global <- 
  ggsurvplot(fit_surv_sponsor_comparison_global, 
             fun = 'event',
             data = fig_df_surv_ED_comparison_global, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nearly discontinuation',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('Early Discontinuation') + 
  xlab("Trial Duration (Months)")
gg_fig_surv_cumulative_ED_sponsor_comparison_global$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_ED_sponsor_comparison_global$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.40)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))

# USA - gi - Sponsor - Early Discontinuation (cumulative)
gg_fig_surv_cumulative_ED_sponsor_gi_USA <- 
  ggsurvplot(fit_surv_sponsor_gi_USA, 
             fun = 'event',
             data = fig_df_surv_ED_gi_USA, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nearly discontinuation',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('Early Discontinuation') + 
  xlab("Trial Duration (Months)")
gg_fig_surv_cumulative_ED_sponsor_gi_USA$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_ED_sponsor_gi_USA$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.40)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))

# USA - Comparison - Sponsor - Early Discontinuation (cumulative)
gg_fig_surv_cumulative_ED_sponsor_comparison_USA <- 
  ggsurvplot(fit_surv_sponsor_comparison_USA, 
             fun = 'event',
             data = fig_df_surv_ED_comparison_USA, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nearly discontinuation',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('Early Discontinuation') + 
  xlab("Trial Duration (Months)")
gg_fig_surv_cumulative_ED_sponsor_comparison_USA$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_ED_sponsor_comparison_USA$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.40)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))


# Global - gi - Region - Early Discontinuation (cumulative)
gg_fig_surv_cumulative_ED_region_gi_global <- 
  ggsurvplot(fit_surv_region_gi_global, 
             fun = 'event',
             data = fig_df_surv_ED_gi_global, 
             color = 'br_singleregion4', 
             palette = g_region_color6b,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nearly discontinuation',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('Early Discontinuation') + 
  xlab("Trial Duration (Months)")
gg_fig_surv_cumulative_ED_region_gi_global$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_ED_region_gi_global$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.40)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))

# Global - Comparison - Region - Early Discontinuation (cumulative)
gg_fig_surv_cumulative_ED_region_comparison_global <- 
  ggsurvplot(fit_surv_region_comparison_global, 
             fun = 'event',
             data = fig_df_surv_ED_comparison_global, 
             color = 'br_singleregion4', 
             palette = g_region_color6b,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nearly discontinuation',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('Early Discontinuation') + 
  xlab("Trial Duration (Months)")
gg_fig_surv_cumulative_ED_region_comparison_global$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_ED_region_comparison_global$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.40)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))



# USA - COMBINED - Specialty - Early Discontinuation (cumulative)
gg_fig_surv_cumulative_ED_specialty_combined_USA <- 
  ggsurvplot(fit_surv_specialty_combined_USA, 
             fun = 'event',
             data = fig_df_surv_ED_combined_USA, 
             # color = 'industry_any2b', 
             # palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nearly discontinuation',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('Early Discontinuation') + 
  xlab("Trial Duration (Months)")
gg_fig_surv_cumulative_ED_specialty_combined_USA$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_ED_specialty_combined_USA$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.40)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))

# Global - COMBINED - Specialty - Early Discontinuation (cumulative)
gg_fig_surv_cumulative_ED_specialty_combined_global <- 
  ggsurvplot(fit_surv_specialty_combined_global, 
             fun = 'event',
             data = fig_df_surv_ED_combined_global, 
             # color = 'industry_any2b', 
             # palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nearly discontinuation',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('Early Discontinuation') + 
  xlab("Trial Duration (Months)")
gg_fig_surv_cumulative_ED_specialty_combined_global$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_ED_specialty_combined_global$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.40)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))

gg_fig_surv_cumulative_ED_sponsor_gi_global
gg_fig_surv_cumulative_ED_sponsor_comparison_global

gg_fig_surv_cumulative_ED_sponsor_gi_USA
gg_fig_surv_cumulative_ED_sponsor_comparison_USA

gg_fig_surv_cumulative_ED_region_gi_global
gg_fig_surv_cumulative_ED_region_comparison_global

gg_fig_surv_cumulative_ED_specialty_combined_global
gg_fig_surv_cumulative_ED_specialty_combined_USA



# --------------------------------------------------------------#
# -----------------            Figure 5:          --------------#
#                 "Survival" w/o Results Reporting              #
# --------------------------------------------------------------#
####### Figure 5 - Survival RR --------------------------------------

fig_df_surv_RR_gi_global_phase23 <- 
  full_gi_df %>%
  mutate(br_singleregion4 = as.character(br_singleregion4)) %>% # This shouldn't be necessary, but currently a bug in survminer that needs to be fixed
  filter(br_studystatus == 'Completed') %>% 
  filter(study_first_submitted_date > ymd('20071001')) %>%
  filter(phase %in% c('Phase 2', 'Phase 3')) # these are the only ones that I would imagine have a compelling reason to report results
# filter(phase %in% c('Phase 2', 'Phase 3', 'Phase 4'))

fig_df_surv_RR_comparison_global_phase23 <- 
  full_comparison_df %>%
  mutate(br_singleregion4 = as.character(br_singleregion4)) %>% # See here for more on the survminer bug: https://github.com/kassambara/survminer/issues/337
  filter(br_studystatus == 'Completed') %>% 
  filter(study_first_submitted_date > ymd('20071001')) %>%
  filter(phase %in% c('Phase 2', 'Phase 3')) # these are the only ones that I would imagine have a compelling reason to report results
# filter(phase %in% c('Phase 2', 'Phase 3', 'Phase 4'))

fig_df_surv_RR_gi_USA_phase23 <- 
  full_gi_df %>%
  mutate(br_singleregion4 = as.character(br_singleregion4)) %>%
  filter(USA_only_facilities) %>%
  filter(br_studystatus == 'Completed') %>% 
  filter(study_first_submitted_date > ymd('20071001')) %>%
  filter(phase %in% c('Phase 2', 'Phase 3')) # these are the only ones that I would imagine have a compelling reason to report results
# filter(phase %in% c('Phase 2', 'Phase 3', 'Phase 4'))

fig_df_surv_RR_comparison_USA_phase23 <- 
  full_comparison_df %>%
  mutate(br_singleregion4 = as.character(br_singleregion4)) %>%
  filter(USA_only_facilities) %>%
  filter(br_studystatus == 'Completed') %>% 
  filter(study_first_submitted_date > ymd('20071001')) %>%
  filter(phase %in% c('Phase 2', 'Phase 3')) # these are the only ones that I would imagine have a compelling reason to report results
# filter(phase %in% c('Phase 2', 'Phase 3', 'Phase 4'))

fig_df_surv_RR_combined_global_phase23 <- 
  full_spec_combined_df %>%
  filter(br_studystatus == 'Completed') %>% 
  filter(study_first_submitted_date > ymd('20071001')) %>%
  filter(phase %in% c('Phase 2', 'Phase 3')) # these are the only ones that I would imagine have a compelling reason to report results
# filter(phase %in% c('Phase 2', 'Phase 3', 'Phase 4'))

fig_df_surv_RR_combined_USA_phase23 <- 
  full_spec_combined_df %>%
  filter(USA_only_facilities) %>%
  filter(br_studystatus == 'Completed') %>% 
  filter(study_first_submitted_date > ymd('20071001')) %>%
  filter(phase %in% c('Phase 2', 'Phase 3')) # these are the only ones that I would imagine have a compelling reason to report results
# filter(phase %in% c('Phase 2', 'Phase 3', 'Phase 4'))

fit_surv_sponsor_gi_global_phase23 <- 
  survfit(Surv(br_time_until_resultsreport_or_present_inmonths, br_censor_were_results_reported) ~ industry_any2b, 
          data = fig_df_surv_RR_gi_global_phase23)

fit_surv_region_gi_global_phase23 <- 
  survfit(Surv(br_time_until_resultsreport_or_present_inmonths, br_censor_were_results_reported) ~ br_singleregion4, 
          data = fig_df_surv_RR_gi_global_phase23)



fit_surv_sponsor_comparison_global_phase23 <- 
  survfit(Surv(br_time_until_resultsreport_or_present_inmonths, br_censor_were_results_reported) ~ industry_any2b, 
          data = fig_df_surv_RR_comparison_global_phase23)

fit_surv_region_comparison_global_phase23 <- 
  survfit(Surv(br_time_until_resultsreport_or_present_inmonths, br_censor_were_results_reported) ~ br_singleregion4, 
          data = fig_df_surv_RR_comparison_global_phase23)



fit_surv_sponsor_gi_USA_phase23 <- 
  survfit(Surv(br_time_until_resultsreport_or_present_inmonths, br_censor_were_results_reported) ~ industry_any2b, 
          data = fig_df_surv_RR_gi_USA_phase23)

fit_surv_region_gi_USA_phase23 <- 
  survfit(Surv(br_time_until_resultsreport_or_present_inmonths, br_censor_were_results_reported) ~ br_singleregion4, 
          data = fig_df_surv_RR_gi_USA_phase23)



fit_surv_sponsor_comparison_USA_phase23 <- 
  survfit(Surv(br_time_until_resultsreport_or_present_inmonths, br_censor_were_results_reported) ~ industry_any2b, 
          data = fig_df_surv_RR_comparison_USA_phase23)

fit_surv_region_comparison_USA_phase23 <- 
  survfit(Surv(br_time_until_resultsreport_or_present_inmonths, br_censor_were_results_reported) ~ br_singleregion4, 
          data = fig_df_surv_RR_comparison_USA_phase23)



fit_surv_specialty_combined_global_phase23 <- 
  survfit(Surv(br_time_until_resultsreport_or_present_inmonths, br_censor_were_results_reported) ~ specialty_source, 
          data = fig_df_surv_RR_combined_global_phase23)
fit_surv_specialty_combined_USA_phase23 <- 
  survfit(Surv(br_time_until_resultsreport_or_present_inmonths, br_censor_were_results_reported) ~ specialty_source, 
          data = fig_df_surv_RR_combined_USA_phase23)


# --------- Survival Plots ----------#
# Results Reporting by funding source (simple chart only, no table)
ggsurvplot(fit_surv_sponsor_gi_global_phase23, 
           data = fig_df_surv_RR_gi_global_phase23, 
           color = 'industry_any2b', 
           palette = g_industry_color5,
           xlim = c(0,60), 
           size = 1.5, 
           # cumevents = TRUE, 
           censor.shape = 124,
           censor.size = 2.0, 
           # risk.table = TRUE,
           break.x.by = 12) +
  ylab('Trial registration without \nresults reporting') + 
  xlab("Time from Primary Completion (Months)")

# results reporting by funding source
# Global - gi - Sponsor - Results Reporting - Phase2-3 (surv)
gg_fig_surv_RR_sponsor_gi_global_phase23 <- 
  ggsurvplot(fit_surv_sponsor_gi_global_phase23, 
             data = fig_df_surv_RR_gi_global_phase23, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Trial registration without \nresults reporting') + 
  xlab("Time from Primary Completion (Months)"); gg_fig_surv_RR_sponsor_gi_global_phase23

# Global - Comparison - Sponsor - Results Reporting - Phase2-3 (surv)
gg_fig_surv_RR_sponsor_comparison_global_phase23 <- 
  ggsurvplot(fit_surv_sponsor_comparison_global_phase23, 
             data = fig_df_surv_RR_comparison_global_phase23, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Trial registration without \nresults reporting') + 
  xlab("Time from Primary Completion (Months)"); gg_fig_surv_RR_sponsor_comparison_global_phase23

# USA - gi - Sponsor - Results Reporting - Phase2-3 (surv)
gg_fig_surv_RR_sponsor_gi_USA_phase23 <- 
  ggsurvplot(fit_surv_sponsor_gi_USA_phase23, 
             data = fig_df_surv_RR_gi_USA_phase23, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Trial registration without \nresults reporting') + 
  xlab("Time from Primary Completion (Months)"); gg_fig_surv_RR_sponsor_gi_USA_phase23

# USA - Comparison - Sponsor - Results Reporting - Phase2-3 (surv)
gg_fig_surv_RR_sponsor_comparison_USA_phase23 <- 
  ggsurvplot(fit_surv_sponsor_comparison_USA_phase23, 
             data = fig_df_surv_RR_comparison_USA_phase23, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Trial registration without \nresults reporting') + 
  xlab("Time from Primary Completion (Months)"); gg_fig_surv_RR_sponsor_comparison_USA_phase23

# results reporting by region
# Global - gi - Region - Results Reporting - Phase2-3 (surv)
gg_fig_surv_RR_region_gi_global_phase23 <- 
  ggsurvplot(fit_surv_region_gi_global_phase23, 
             data = fig_df_surv_RR_gi_global_phase23, 
             color = 'br_singleregion4',
             palette = g_region_color6,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 4.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Trial registration without \nresults reporting') + 
  xlab("Time from Primary Completion (Months)"); gg_fig_surv_RR_region_gi_global_phase23

# Global - Comparison - Region - Results Reporting - Phase2-3 (surv)
gg_fig_surv_RR_region_comparison_global_phase23 <- 
  ggsurvplot(fit_surv_region_comparison_global_phase23, 
             data = fig_df_surv_RR_comparison_global_phase23, 
             color = 'br_singleregion4',
             palette = g_region_color6,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 4.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Trial registration without \nresults reporting') + 
  xlab("Time from Primary Completion (Months)"); gg_fig_surv_RR_region_comparison_global_phase23

# combined look at effect of specialty source
# Global - COMBINED - Specialty - Results Reporting - Phase2-3 (surv)
gg_fig_surv_RR_specialty_combined_global_phase23 <- 
  ggsurvplot(fit_surv_specialty_combined_global_phase23, 
             data = fig_df_surv_RR_combined_global_phase23, 
             # color = 'br_singlespecialty4',
             # palette = g_specialty_color6,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 4.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Trial registration without \nresults reporting') + 
  xlab("Time from Primary Completion (Months)"); gg_fig_surv_RR_specialty_combined_global_phase23

# USA - COMBINED - Specialty - Results Reporting - Phase2-3 (surv)
gg_fig_surv_RR_specialty_combined_USA_phase23 <- 
  ggsurvplot(fit_surv_specialty_combined_USA_phase23, 
             data = fig_df_surv_RR_combined_USA_phase23, 
             # color = 'br_singlespecialty4',
             # palette = g_specialty_color6,
             xlim = c(0,60), 
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 4.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  ylab('Trial registration without \nresults reporting') + 
  xlab("Time from Primary Completion (Months)"); gg_fig_surv_RR_specialty_combined_USA_phase23

# more on saving ggsurvplot objects here: https://github.com/kassambara/survminer/issues/152

# ----- Cumulative Incidence Plots ------#
ggsurvplot(fit_surv_sponsor_gi_global_phase23, 
           fun = 'event',
           data = fig_df_surv_RR_gi_global_phase23, 
           color = 'industry_any2b', 
           palette = g_industry_color5,
           xlim = c(0,60), 
           # ylim = c(0, 0.25), # using ylim in this way removes data outside the range...not what we want
           ylab = 'Cumulative incidence of\nearly discontinuation',
           size = 1.5, 
           # cumevents = TRUE, 
           censor.shape = 124,
           censor.size = 2.0, 
           # risk.table = TRUE,
           break.x.by = 12) +
  ylab('Cumulative incidence of \nresults reporting') + 
  xlab("Time from Primary Completion (Months)")

# Global - gi - Sponsor - Results Reporting - Phase2-3 (cumulative)
gg_fig_surv_cumulative_RR_sponsor_gi_global_phase23 <- 
  ggsurvplot(fit_surv_sponsor_gi_global_phase23, 
             fun = 'event',
             data = fig_df_surv_RR_gi_global_phase23, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nresults reporting',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('z') + 
  xlab("Time from primary completion (Months)")
gg_fig_surv_cumulative_RR_sponsor_gi_global_phase23$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_RR_sponsor_gi_global_phase23$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.60)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))


# Global - Comparison - Sponsor - Results Reporting - Phase2-3 (cumulative)
gg_fig_surv_cumulative_RR_sponsor_comparison_global_phase23 <- 
  ggsurvplot(fit_surv_sponsor_comparison_global_phase23, 
             fun = 'event',
             data = fig_df_surv_RR_comparison_global_phase23, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nresults reporting',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('z') + 
  xlab("Time from primary completion (Months)")
gg_fig_surv_cumulative_RR_sponsor_comparison_global_phase23$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_RR_sponsor_comparison_global_phase23$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.60)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))


# USA - gi - Sponsor - Results Reporting - Phase2-3 (cumulative)
gg_fig_surv_cumulative_RR_sponsor_gi_USA_phase23 <- 
  ggsurvplot(fit_surv_sponsor_gi_USA_phase23, 
             fun = 'event',
             data = fig_df_surv_RR_gi_USA_phase23, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nresults reporting',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('z') + 
  xlab("Time from primary completion (Months)")
gg_fig_surv_cumulative_RR_sponsor_gi_USA_phase23$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_RR_sponsor_gi_USA_phase23$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.60)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))


# USA - Comparison - Sponsor - Results Reporting - Phase2-3 (cumulative)
gg_fig_surv_cumulative_RR_sponsor_comparison_USA_phase23 <- 
  ggsurvplot(fit_surv_sponsor_comparison_USA_phase23, 
             fun = 'event',
             data = fig_df_surv_RR_comparison_USA_phase23, 
             color = 'industry_any2b', 
             palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nresults reporting',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('z') + 
  xlab("Time from primary completion (Months)")
gg_fig_surv_cumulative_RR_sponsor_comparison_USA_phase23$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_RR_sponsor_comparison_USA_phase23$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.60)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))



# Global - COMBINED - Specialty - Results Reporting - Phase2-3 (cumulative)
gg_fig_surv_cumulative_RR_specialty_combined_global_phase23 <- 
  ggsurvplot(fit_surv_specialty_combined_global_phase23, 
             fun = 'event',
             data = fig_df_surv_RR_combined_global_phase23, 
             # color = 'industry_any2b', 
             # palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nresults reporting',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('z') + 
  xlab("Time from primary completion (Months)")
gg_fig_surv_cumulative_RR_specialty_combined_global_phase23$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_RR_specialty_combined_global_phase23$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.60)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))



# USA - COMBINED - Specialty - Results Reporting - Phase2-3 (cumulative)
gg_fig_surv_cumulative_RR_specialty_combined_USA_phase23 <- 
  ggsurvplot(fit_surv_specialty_combined_USA_phase23, 
             fun = 'event',
             data = fig_df_surv_RR_combined_USA_phase23, 
             # color = 'industry_any2b', 
             # palette = g_industry_color5,
             xlim = c(0,60), ylab = 'Cumulative incidence of\nresults reporting',
             size = 1.5, 
             cumevents = TRUE, 
             censor.shape = 124,
             censor.size = 2.0, 
             risk.table = TRUE,
             break.x.by = 12) +
  # ylab('z') + 
  xlab("Time from primary completion (Months)")
gg_fig_surv_cumulative_RR_specialty_combined_USA_phase23$plot <- # all this is because ggsurvplot is a list output
  gg_fig_surv_cumulative_RR_specialty_combined_USA_phase23$plot + 
  coord_cartesian(xlim = c(0,60), ylim = c(0,0.60)) + 
  scale_y_continuous(breaks = seq(0, 0.85, 0.05))


gg_fig_surv_cumulative_RR_sponsor_gi_global_phase23
gg_fig_surv_cumulative_RR_sponsor_comparison_global_phase23

gg_fig_surv_cumulative_RR_sponsor_gi_USA_phase23
gg_fig_surv_cumulative_RR_sponsor_comparison_USA_phase23

gg_fig_surv_cumulative_RR_specialty_combined_global_phase23
gg_fig_surv_cumulative_RR_specialty_combined_USA_phase23


# --------------------     Output Tables to Excel        -----------------------#

if(include_comparison_combined_analysis) {
  table_output_list <- 
    list('1_table0_combined' = table0_combined_comparison,
         '2_table1_gi' = left_join(table1_total_gi, table1_bintime_gi, by = c('explvar', 'varlevels')),
         '3_table1_rowpct_gi' = left_join(table1_total_gi , table1_bintime_rowwisepct_gi, by = c('explvar', 'varlevels')),
         '4_table1_comparison' = table1_bintime_comparison,
         '5_table1_rowpct_comparison' = table1_bintime_rowwisepct_comparison,
         '5b_table1_USAvsWorld' = table1_USAvsWorld_total_p1,
         
         '6_table2_sponsor_gi' = table2_sponsor_gi,
         '7_table2_sponsor_rowpct_gi' = table2_sponsor_rowwisepct_gi,
         '8_table2_sponsor_comparison' = table2_sponsor_comparison,
         '9_table2_sponsor_rowpct_comparison' = table2_sponsor_rowwisepct_comparison,
         
         '10_table3_sponsor_name_gi' = table3_sponsor_name_gi,
         '11_table3_sponsor_name_comparison' = table3_sponsor_name_comparison,
         
         '12_table4_trial_design_gi' = table4_design_gi,
         '13_table4_trial_design_colpct_gi' = table4_design_colwisepct_gi,
         '14_table4_trial_design_comparison' = table4_design_comparison,
         '15_table4_trial_design_colpct_comparison' = table4_design_colwisepct_comparison,
         
         '16_yearly_growth_statistics_gi_global' = gi_trial_growth_statistics_global,
         '17_yearly_growth_data_gi_global' = gi_trial_growth_data_global,
         '18_yearly_growth_statistics_comparison_global' = comparison_trial_growth_statistics_global,
         '19_yearly_growth_data_comparison_global' = comparison_trial_growth_data_global,
         
         '20_yearly_growth_statistics_gi_USA' = gi_trial_growth_statistics_USA,
         '21_yearly_growth_data_gi_USA' = gi_trial_growth_data_USA,
         '22_yearly_growth_statistics_comparison_USA' = comparison_trial_growth_statistics_USA,
         '23_yearly_growth_data_comparison_USA' = comparison_trial_growth_data_USA,
         
         '24_flow_diagram_numbers' = flowfiguredf,
         '25_brannon_enrollment' = btable_enrollment,
         '26_studystatus_enrollment' = btable_studystatus_enrollment,
         '27_completion_funding' = btable_completion_funding_apr30_2016,
         '28_disease_enrollment_counts' = fig2_df_enrollment_and_trials_global)
  
  table_output_regression_list <- 
    list(
      '01_table5_design_regression_gi' = table5_design_regression_gi,
      '02_table5_design_regression_combined' = table5_design_regression_combined, 
      '03_tbl_5_design_combined_impute_glm_output_formatted' = tbl_5_design_combined_impute_glm_output_formatted,
      '04_tbl_5_design_gi_impute_glm_output_formatted' = tbl_5_design_gi_impute_glm_output_formatted,
      
      '05_table6_RR_regression_log_gi' = table6_resultsreport_regression_log_gi,
      '06_table6b_RR_regression_cox_gi' = table6b_resultsreport_regression_cox_gi,
      '07_table6_RR_regression_log_combined' = table6_resultsreport_regression_log_combined,
      '08_table6b_RR_regression_cox_combined' = table6b_resultsreport_regression_cox_combined, 
      '09_tbl_6_rr_gi_impute_glm_output_formatted' = tbl_6_rr_gi_impute_glm_output_formatted,
      '10_tbl_6_rr_combined_impute_glm_output_formatted' = tbl_6_rr_combined_impute_glm_output_formatted,
      '11_tbl_6_rr_gi_impute_cox_output_formatted' = tbl_6_rr_gi_impute_cox_output_formatted,
      '12_tbl_6_rr_combined_impute_cox_output_formatted' = tbl_6_rr_combined_impute_cox_output_formatted,
      
      '13_table7_ED_cox_gi' = table7b_earlydiscontinuation_regression_cox_gi,
      '14_table7_ED_cox_combined' = table7b_earlydiscontinuation_regression_cox_combined,
      '15_tbl_7_early_gi_impute_cox_output_formatted' = tbl_7_early_gi_impute_cox_output_formatted,
      '16_tbl_7_early_combined_impute_cox_output_formatted' = tbl_7_early_combined_impute_cox_output_formatted
    )
  
  table_output_list_withna <- 
    list('wNA_1_table0_combined' = table0_combined_comparison_withna,
         'wNA_2_table1_gi' = left_join(table1_total_gi_withna, table1_bintime_gi_withna, by = c('explvar', 'varlevels')),
         'wNA_3_table1_rowpct_gi' = left_join(table1_total_gi_withna , table1_bintime_rowwisepct_gi_withna, by = c('explvar', 'varlevels')),
         'wNA_4_table1_comparison' = table1_bintime_comparison_withna,
         'wNA_5_table1_rowpct_comparison' = table1_bintime_rowwisepct_comparison_withna,
         'wNA_5b_table1_USAvsWorld' = table1_USAvsWorld_total_p1_withna,
         
         'wNA_6_table2_sponsor_gi' = table2_sponsor_gi_withna,
         'wNA_7_table2_sponsor_rowpct_gi' = table2_sponsor_rowwisepct_gi_withna,
         'wNA_8_table2_sponsor_comparison' = table2_sponsor_comparison_withna,
         'wNA_9_table2_sponsor_rowpct_comparison' = table2_sponsor_rowwisepct_comparison_withna,
         
         'wNA_10_table3_sponsor_name_gi' = table3_sponsor_name_gi,
         'wNA_11_table3_sponsor_name_comparison' = table3_sponsor_name_comparison,
         
         'wNA_12_table4_trial_design_gi' = table4_design_gi_withna,
         'wNA_13_table4_trial_design_colpct_gi' = table4_design_colwisepct_gi_withna,
         'wNA_14_table4_trial_design_comparison' = table4_design_comparison_withna,
         'wNA_15_table4_trial_design_colpct_comparison' = table4_design_colwisepct_comparison_withna)
} else {
  table_output_list <- 
    list('1_table0_combined' = table0_combined_comparison,
         '2_table1_gi' = left_join(table1_total_gi, table1_bintime_gi, by = c('explvar', 'varlevels')),
         '3_table1_rowpct_gi' = left_join(table1_total_gi , table1_bintime_rowwisepct_gi, by = c('explvar', 'varlevels')),
         # '4_table1_comparison' = table1_bintime_comparison,
         # '5_table1_rowpct_comparison' = table1_bintime_rowwisepct_comparison,
         '5b_table1_USAvsWorld' = table1_USAvsWorld_total_p1,
         
         '6_table2_sponsor_gi' = table2_sponsor_gi,
         '7_table2_sponsor_rowpct_gi' = table2_sponsor_rowwisepct_gi,
         # '8_table2_sponsor_comparison' = table2_sponsor_comparison,
         # '9_table2_sponsor_rowpct_comparison' = table2_sponsor_rowwisepct_comparison,
         
         '10_table3_sponsor_name_gi' = table3_sponsor_name_gi,
         # '11_table3_sponsor_name_comparison' = table3_sponsor_name_comparison,
         
         '12_table4_trial_design_gi' = table4_design_gi,
         '13_table4_trial_design_colpct_gi' = table4_design_colwisepct_gi,
         # '14_table4_trial_design_comparison' = table4_design_comparison,
         # '15_table4_trial_design_colpct_comparison' = table4_design_colwisepct_comparison,
         
         '16_yearly_growth_statistics_gi_global' = gi_trial_growth_statistics_global,
         '17_yearly_growth_data_gi_global' = gi_trial_growth_data_global,
         # '18_yearly_growth_statistics_comparison_global' = comparison_trial_growth_statistics_global,
         # '19_yearly_growth_data_comparison_global' = comparison_trial_growth_data_global,
         
         '20_yearly_growth_statistics_gi_USA' = gi_trial_growth_statistics_USA,
         '21_yearly_growth_data_gi_USA' = gi_trial_growth_data_USA,
         # '22_yearly_growth_statistics_comparison_USA' = comparison_trial_growth_statistics_USA,
         # '23_yearly_growth_data_comparison_USA' = comparison_trial_growth_data_USA,
         
         '24_flow_diagram_numbers' = flowfiguredf,
         '25_brannon_enrollment' = btable_enrollment,
         '26_studystatus_enrollment' = btable_studystatus_enrollment,
         '27_completion_funding' = btable_completion_funding_apr30_2016,
         '28_disease_enrollment_counts' = fig2_df_enrollment_and_trials_global)
  
  table_output_regression_list <- 
    list(
      '01_table5_design_regression_gi' = table5_design_regression_gi,
      # '02_table5_design_regression_combined' = table5_design_regression_combined, 
      # '03_tbl_5_design_combined_impute_glm_output_formatted' = tbl_5_design_combined_impute_glm_output_formatted,
      '04_tbl_5_design_gi_impute_glm_output_formatted' = tbl_5_design_gi_impute_glm_output_formatted,
      
      '05_table6_RR_regression_log_gi' = table6_resultsreport_regression_log_gi,
      '06_table6b_RR_regression_cox_gi' = table6b_resultsreport_regression_cox_gi,
      # '07_table6_RR_regression_log_combined' = table6_resultsreport_regression_log_combined,
      # '08_table6b_RR_regression_cox_combined' = table6b_resultsreport_regression_cox_combined, 
      '09_tbl_6_rr_gi_impute_glm_output_formatted' = tbl_6_rr_gi_impute_glm_output_formatted,
      # '10_tbl_6_rr_combined_impute_glm_output_formatted' = tbl_6_rr_combined_impute_glm_output_formatted,
      '11_tbl_6_rr_gi_impute_cox_output_formatted' = tbl_6_rr_gi_impute_cox_output_formatted,
      # '12_tbl_6_rr_combined_impute_cox_output_formatted' = tbl_6_rr_combined_impute_cox_output_formatted,
      
      '13_table7_ED_cox_gi' = table7b_earlydiscontinuation_regression_cox_gi,
      # '14_table7_ED_cox_combined' = table7b_earlydiscontinuation_regression_cox_combined,
      '15_tbl_7_early_gi_impute_cox_output_formatted' = tbl_7_early_gi_impute_cox_output_formatted
      # '16_tbl_7_early_combined_impute_cox_output_formatted' = tbl_7_early_combined_impute_cox_output_formatted
    )
  
  table_output_list_withna <- 
    list(
      # 'wNA_1_table0_combined' = table0_combined_comparison_withna,
      'wNA_2_table1_gi' = left_join(table1_total_gi_withna, table1_bintime_gi_withna, by = c('explvar', 'varlevels')),
      'wNA_3_table1_rowpct_gi' = left_join(table1_total_gi_withna , table1_bintime_rowwisepct_gi_withna, by = c('explvar', 'varlevels')),
      # 'wNA_4_table1_comparison' = table1_bintime_comparison_withna,
      # 'wNA_5_table1_rowpct_comparison' = table1_bintime_rowwisepct_comparison_withna,
      'wNA_5b_table1_USAvsWorld' = table1_USAvsWorld_total_p1_withna,
      
      'wNA_6_table2_sponsor_gi' = table2_sponsor_gi_withna,
      'wNA_7_table2_sponsor_rowpct_gi' = table2_sponsor_rowwisepct_gi_withna,
      # 'wNA_8_table2_sponsor_comparison' = table2_sponsor_comparison_withna,
      # 'wNA_9_table2_sponsor_rowpct_comparison' = table2_sponsor_rowwisepct_comparison_withna,
      
      'wNA_10_table3_sponsor_name_gi' = table3_sponsor_name_gi,
      # 'wNA_11_table3_sponsor_name_comparison' = table3_sponsor_name_comparison,
      
      'wNA_12_table4_trial_design_gi' = table4_design_gi_withna,
      'wNA_13_table4_trial_design_colpct_gi' = table4_design_colwisepct_gi_withna
      # 'wNA_14_table4_trial_design_comparison' = table4_design_comparison_withna,
      # 'wNA_15_table4_trial_design_colpct_comparison' = table4_design_colwisepct_comparison_withna
    )
}


table_output_list <- 
  c(list("TableOfContents" = data.frame('Table_Full_Names' = names(table_output_list))), 
    table_output_list)

table_output_list_withna <- 
  c(list("TableOfContents" = data.frame('Table_Full_Names' = names(table_output_list_withna))), 
    table_output_list_withna)

table_output_regression_list <- 
  c(list("TableOfContents" = data.frame('Table_Full_Names' = names(table_output_regression_list))), 
    table_output_regression_list)

if(include_comparison_combined_analysis) {
  write.xlsx(table_output_list,
             file = 'gi_tables/gi_output_tables.xlsx')
  write.xlsx(table_output_list_withna,
             file = 'gi_tables/gi_output_tables_withna.xlsx')
  write.xlsx(table_output_regression_list,
             file = 'gi_tables/gi_output_regression_list.xlsx')
} else {
  write.xlsx(table_output_list,
             file = 'gi_tables/gi_output_tables_nocomparison.xlsx')
  write.xlsx(table_output_list_withna,
             file = 'gi_tables/gi_output_tables_withna_nocomparison.xlsx')
  write.xlsx(table_output_regression_list,
             file = 'gi_tables/gi_output_regression_list_nocomparison.xlsx')
  
}

# HOW TO FIND ALL THE GG_OBJECTS?  (exclude everything below this line for gg search)
# 1) Makse sure you save this file first 
# 2) Run this code:
script_text1 <- readLines('gi_trial_analysis.R')
script_cutoff <- which(grepl(pattern = 'exclude everything below this line for gg search', x = script_text1))
script_text2 <- script_text1[1:script_cutoff[1]]
script_text3 <- sapply(script_text2, function(iline) str_trim(iline))
script_comment_line <- script_text3 %>% stringr::str_starts('#')
collapse_script_text <- paste(script_text3[!script_comment_line], collapse = '***')
gg_objects <- str_match_all(string = collapse_script_text, pattern = '[^[:alpha:]](gg_fig\\w*)\\W')[[1]][,2] %>% unique()

dev.off()
pdf("graphs/all_gi_graphs_10x10.pdf", width = 10, height = 10)
for(i in seq(gg_objects)) {
  svMisc::progress(i, progress.bar = TRUE)
  igg <- gg_objects[i]
  print(get(igg) + ggtitle(igg))
}
dev.off()

pdf("graphs/all_gi_graphs_12x12.pdf", width = 12, height = 12)
for(i in seq(gg_objects)) {
  svMisc::progress(i)
  igg <- gg_objects[i]
  print(get(igg) + ggtitle(igg))
}
dev.off()

pdf("graphs/all_gi_graphs_15x15.pdf", width = 15, height = 15)
for(i in seq(gg_objects)) {
  svMisc::progress(i)
  igg <- gg_objects[i]
  print(get(igg) + ggtitle(igg))
}
dev.off()

# make PNG in a separate folder
dir.create('all_gi_graphs_png_files', showWarnings = FALSE)
for(i in seq(gg_objects)) {
  # progress(i) causing error for some reason...
  igg <- gg_objects[i]
  ggsave(filename = paste0('all_gi_graphs_png_files/', igg,'.png'), 
         plot = print(get(igg) + ggtitle(igg)), width = 10, height = 10)
}

# Zip up all the PNG files
filelocations_pngs <- dir('all_gi_graphs_png_files/')
zip::zipr(zipfile = 'all_gi_graphs_png_files.zip', 
          files = 'all_gi_graphs_png_files', 
          compression_level = 9, 
          recurse = TRUE)


# 3) gg_objects is now a character vector of all your gg_fig_* objects in that file. 
# 4) Do some spot saving for special groups...
ggsave('14x6_gg_fig_2_disease_bar_global.png', 
       plot = gg_fig_2_disease_bar_global + ggtitle('gg_fig_2_disease_bar_global'), width = 14, height = 6)
ggsave('14x6_gg_fig_2_disease_bar_USA.png', 
       plot = gg_fig_2_disease_bar_USA + ggtitle('gg_fig_2_disease_bar_USA'), width = 14, height = 6)

ggsave('14x6_gg_fig_2_disease_enrollment_bar_global.png', 
       plot = gg_fig_2_disease_enrollment_bar_global + ggtitle('gg_fig_2_disease_enrollment_bar_global'), width = 14, height = 6)
ggsave('14x6_gg_fig_2_disease_enrollment_bar_USA.png', 
       plot = gg_fig_2_disease_enrollment_bar_USA + ggtitle('gg_fig_2_disease_enrollment_bar_USA'), width = 14, height = 6)

ggsave('15x5_gg_fig_2_enrollment_vs_numtrials_disease_global_bar.png', 
       plot = gg_fig_2_enrollment_vs_numtrials_disease_global_bar, width = 15, height = 5)

ggsave('14x10_gg_fig_2_disease_enrollment_line.png', 
       plot = gg_fig_2_disease_location_year_line + ggtitle('14x10_gg_fig_2_disease_enrollment_line'), width = 14, height = 10)

#*# Need to finish this list of things to save explicitly and not just inside the loop, need to then copy into gilogy code too. 
ggshort_list <- c(
  'gg_fig_1_5pane_gi_global',
  'gg_fig_1_5pane_gi_global_b',
  'gg_fig_1_5pane_gi_global_c',
  'gg_fig_1_4pane_gi_global_a',
  'gg_fig_1_4pane_gi_global_b',
  'gg_fig_1_4pane_gi_global_c',
  'gg_fig_1_4pane_gi_global_d',
  'gg_fig_1_4pane_gi_global_e',
  'gg_fig_1_4pane_gi_global_f',
  'gg_fig_1_3pane_gi_global_c',
  'gg_fig_surv_cumulative_ED_sponsor_gi_global',
  'gg_fig_surv_cumulative_ED_region_gi_global',
  'gg_fig_surv_cumulative_ED_sponsor_gi_global',
  'gg_fig_surv_cumulative_ED_sponsor_gi_USA',
  'gg_fig_surv_cumulative_ED_specialty_combined_global',
  'gg_fig_surv_ED_specialty_combined_global',
  'gg_fig_surv_cumulative_RR_specialty_combined_global_phase23',
  'gg_fig_surv_cumulative_RR_specialty_combined_USA_phase23'
)

for(igg in ggshort_list) {
  ggsave(filename = paste0('10x10_', igg,'.png'), 
         plot = print(get(igg) + ggtitle(igg)), width = 10, height = 10)
}

# As you go through the results manually, you can save specific versions like below (png, svg, pdf)
# ggsave('Radiation Trials/gilogyAnalysis/figure1_3pane.png', ggfigure1_3pane, width = 10, height = 10)
# ggsave('Radiation Trials/gilogyAnalysis/figure1_4pane.png', ggfigure1_4pane, width = 10, height = 10)
# ggsave('Radiation Trials/gilogyAnalysis/figure2.png', plot = gg_fig_2_disease_bar_global, width = 14, height = 6)
# ggsave('Radiation Trials/gilogyAnalysis/figure4_cumulative.png', 
#        plot = print(gg_fig_surv_cumulative_ED_sponsor_gi_global), width = 10, height = 10)
# ggsave('Radiation Trials/gilogyAnalysis/figure4_survival.png', 
#        plot = print(gg_fig_surv_ED_sponsor_gi_global), width = 10, height = 10)
# ggsave('Radiation Trials/gilogyAnalysis/figure5_cumulative.png', 
#        plot = print(gg_fig_surv_cumulative_RR_sponsor_gi_global_phase23), width = 10, height = 10)
# ggsave('Radiation Trials/gilogyAnalysis/figure5_survival.png', 
#        plot = print(gg_fig_surv_RR_sponsor_gi_global_phase23), width = 10, height = 10)

# # ggsave('Radiation Trials/gilogyAnalysis/figure4.svg', 
# #        plot = print(gg_fig_surv_ED_sponsor_gi_global, newpage = FALSE), width = 10, height = 10)

# # useDingbats is a weird thing, see more here, it helped some of my formatting when importing into
# # inkscape for editing: https://stackoverflow.com/questions/9992275/ggplot2-pdf-import-in-adobe-illustrator-missing-font-adobepistd
# # http://boopsboops.blogspot.com/2011/07/importing-pdf-r-plots-into-inkscape-q.html
# ggsave('Radiation Trials/gilogyAnalysis/figure1_3pane.pdf', 
#        plot = ggfigure1_3pane, width = 10, height = 10, useDingbats = FALSE)
# ggsave('Radiation Trials/gilogyAnalysis/figure1_4pane.pdf', 
#        plot = ggfigure1_4pane, width = 10, height = 10, useDingbats = FALSE)
# ggsave('Radiation Trials/gilogyAnalysis/figure2.pdf', 
#        plot = gg_fig_2_disease_bar_global, width = 14, height = 6, useDingbats = FALSE)
# ggsave('Radiation Trials/gilogyAnalysis/figure4_cumulative.pdf', 
#        plot = print(gg_fig_surv_cumulative_ED_sponsor_gi_global, newpage = FALSE), width = 10, height = 10, useDingbats = FALSE)
# ggsave('Radiation Trials/gilogyAnalysis/figure4_survival.pdf', 
#        plot = print(gg_fig_surv_ED_sponsor_gi_global, newpage = FALSE), width = 10, height = 10, useDingbats = FALSE)
# ggsave('Radiation Trials/gilogyAnalysis/figure5_cumulative.pdf', 
#        plot = print(gg_fig_surv_cumulative_RR_sponsor_gi_global_phase23, newpage = FALSE), width = 10, height = 10, useDingbats = FALSE)
# ggsave('Radiation Trials/gilogyAnalysis/figure5_survival.pdf', 
#        plot = print(gg_fig_surv_RR_sponsor_gi_global_phase23, newpage = FALSE), width = 10, height = 10, useDingbats = FALSE)


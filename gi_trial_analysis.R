# File looking at GI trials
set.seed(5)
wants <- c('zip', 'svMisc', 'ggpubr', 'Hmisc', 'mice', 'glmnet', 'tidyverse','RPostgreSQL', 'europepmc', 'RefManageR', 'DT', 'lubridate', 'ggplot2', 'openxlsx', 'survminer', 'Kendall', 'coin', 'dplyr')
# ------------------------------------- On Laptop

has <- wants %in% row.names(installed.packages())
if(any(!has)) install.packages(wants[!has])

obtained <- unlist(lapply(wants, require, character.only = TRUE))
names(obtained) <- wants

data.frame(loaded = obtained)

setwd("~/Downloads/R stuff") 
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
  openxlsx::read.xlsx(xlsxFile = 'all_ctgov_tables_oct_29_2019/gi_hpb_initial_data.xlsx',
                      sheet = 1, startRow = 1) %>%
  as_tibble() %>%
  mutate_all(as.character)

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
  openxlsx::read.xlsx(xlsxFile = 'all_ctgov_tables_oct_29_2019/gi_hpb_initial_data.xlsx',
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

# there should be no no duplicates in this file. 

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


# in my case, there are only '1' or NA, as instructed. all blanks got turned to NA/False

# do some light processing
raw_gi_list <-
  raw_gi_list %>%
  mutate_at(vars(one_of(all_disease_cols)), function(x) !is.na(x)) # convert NA to FALSE for disease

# --------------------------        MERGE GI DATA WITH BIGTBL based on nct_ID      -----------------------------

full_gi_df <- 
  left_join(raw_gi_list %>%
              filter(true_gi == '1') %>%
              select(one_of(c('nct_id','coder', all_disease_cols))),
            Bigtbl,
            by = 'nct_id') %>%
  mutate(bintime = case_when(
    year(study_first_submitted_date) <= 2012 ~ '2007_2012',
    year(study_first_submitted_date) > 2012 ~ '2013_2018',
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


# ------ Get Max Date, basically date it was pulled 
gi_maxdate <- full_gi_df %>% pull(study_first_submitted_date) %>% max(na.rm = TRUE) # get the last date for trials that we used
gi_maxdate

#Oct 24 2019

# -------------------------------------------#
# filter out interventional and stuff before May 1 2018 or after Oct 1 2007 - MARIJA to find out what this is 
        # 10/1/2007 - the date clinical trials mandated to be put in
        # 5/1/2018 arbitrary for neuroanalysis

full_gi_df <- 
  full_gi_df %>%
  filter(study_type == 'Interventional') %>% 
  filter(study_first_submitted_date >= ymd('20071001')) %>%
  filter(study_first_submitted_date < ymd('20180501'))



# -------------------------------------------# 
# -------- Get Size Data

my_studies %>% count(study_first_submitted_date <= gi_maxdate) # how many trials were in database at time that we downloaded stuff? 
btest0 <- my_studies %>% count(study_first_submitted_date <= gi_maxdate) %>% {colnames(.)[1] <- 'totaltrials'; .}
btest0b <- my_studies %>% count(study_first_submitted_date < ymd('20180501')) %>% {colnames(.)[1] <- 'totaltrials'; .}

btest1 <- my_studies %>% filter(study_first_submitted_date < ymd('20180501'))
btest2 <- btest1 %>% filter(study_type == 'Interventional') # how many interventional trials? 
btest3 <- btest2 %>% filter(study_first_submitted_date >= ymd('20071001')) # how many lost because submitted before Oct 2007?

#These numbers are no longer updated but should be larger 
            
# how many lost b/c of interventional status
nrow(btest1) - nrow(btest2)
#56301

# how many additional lost b/c of registration before October 1, 2007
nrow(btest2) - nrow(btest3)
#38102

# how many are we left with before specialty filter? Specialty filter is for GI studies only
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
    'how many trials were in the database at April 30th 2018?' = btest0b %>% filter(totaltrials) %>% pull(n),
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
#these are pulled from the bigtbl
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
    TRUE ~ as.period(ymd('20180501') - primary_completion_date) / months(1)
  )) %>%
  mutate(br_censor_were_results_reported = as.numeric(were_results_reported)) %>%
  mutate(br_were_results_reported_within_2year = case_when(
    br_studystatus != 'Completed' ~ NA,
    primary_completion_date >= ymd('20160501') ~ NA, # we only consider trials completed >=2 years ago (we should later change this to not be hard coded)
    were_results_reported & (br_time_until_resultsreport_or_present_inmonths <= 24) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(br_were_results_reported_within_1year = case_when(
    br_studystatus != 'Completed' ~ NA,
    primary_completion_date >= ymd('20170501') ~ NA, # we only consider trials completed >=1 year ago
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

# -------------------------------------------------------------------------#
# ---------                 CLEANING UP STOPS HERE                 -------------
# -------------------------------------------------------------------------#

# -------------------------------------------------------------------------#
# ---------                JOLIES ANALYSIS                -------------
# -------------------------------------------------------------------------#

# ----------------------------Make Final CSV of Necessary Columns----------------------------#
                   
##add in a column representing number of regions                     
full_gi_df <- full_gi_df %>% mutate(number_of_regions = 1 + str_count(all_regions, ";"))

######creating a list of all the columns in full_gi_df I think might be useful in the analysis and called it cols_to_add
cols_to_add <- c(
  "nct_id", #------ID

  "early_discontinuation", #------PRIMARY OUTCOME OF EARLY DISCONTINUATION
  #“Early discontinuation” was defined as a trial stopped early with the status “Terminated,” or “Suspended.” 
  #In analysis of early discontinuation, we excluded trials documented as having 
  #(1) less than one day duration 
  #(2) trials with the status “Withdrawn” (defined as those terminating prior to the enrollment of participants) and
  #(3) trials without a verified status. 
  #Only trials completed by March 8, 2017 were included in the analysis of results reporting to 
  #align with federal mandates for delayed submission of results information within three years of trial completion

  "industry_any3", 
  #"industry_any2" #changes all US Govt to NIH, similar to new_industry_any3_ref_nih except industry_any2 has an added category of US Fed
  #"new_industry_any3_ref_nih", 
  #"industry_any2b", 
  #"new_industry_any2b_ref_usgovt", #-----Sponsorship/Funding

  #"phase", #problem with this is it has "Early Phase 1, and Phase1/Phase2 and Phas 2/Phase3 categories. Not sure what to do about these
  "br_phase4_ref_ph3", 
  #"br_phase4_ref_ph1", 
  #"br_phase2", 
  #"new_br_phase2", #----Phase

  "enrollment",
  "new_enroll",#-----Number of participants enrolled
  
  "bintime", #------duration 2007-2012, 2013-2018
  
  "new_primary_purpose_treatment", #----primary objective of the intervention
  #"new_primary_purpose_treatment2", 
  #"primary_purpose",
  
  "interv_all_intervention_types", #-----type of intervention
  
  # "new_num_facilities2", #------number of sites
  "num_facilities", 
  
  "all_regions", 
  "num_regions",
  #"br_singleregion", this changes any cell that has more than one region into "MultiRegion"
  #"all_countries", 
  #"USA_only_facilities", 
  #"US_facility", 
  "num_countries",  #-------Geographic region of sites
  
  #"allocation",
  "br_allocation", #this changes all "NA" from allocation into "non-randomized" 
   #-----use of randomization 
  
  #"masking", instead of "None", is called "None (Open label)" used br_masking2 instead just to make text shorter
  #"br_masking1", same as br_masking2 except has quadruple blinded
  "br_masking2", #changes all "quadruple blinded" in br_masking2 to "double", changes all "None (Open Label)" to "None"
  #------use of blinding
  
  "has_dmc", #--------oversight by a data-monitoring committee
  
  "number_of_arms", 
  #"all_comp_num_arms", honestly looks exactly the same as number_of_arms #------number of arms

  "br_gni_lmic_hic_only", #This will give you which were only in HIC and which were only in LMIC.
  #"br_gni_lmic", 
  #"br_gni_hic", #------high income vs low income coutry
  
  "new_first_submit", #------year of first submit

  "br_trialduration", #------trial duration

  "enrollment_type", #------enrollment type

  "overall_status", #-----status, if we want to redefine discontinuation

  "completion_date", #------completion date

  "were_results_reported",  #"months_to_report_results", don't know what this is, mostly NAs 
  "br_time_until_resultsreport_or_present_inmonths", #------how were results reported?
  
  "infection_any",
  "infection_helminth",
  "infection_intestines",
  "infection_hepatitis",
  "neoplasia_primary",
  "neoplasia_metastasis",
  "neoplasia_disease",
  "abdominal_hernia",
  "appendicitis",
  "cirrhosis",
  "diverticular_disease",
  "fecal_diversion",
  "foreign_body",
  "functional_disorder",
  "gallstones",
  "gerd",
  "hemorrhoids",
  "hypoxic",
  "ileus",
  "ibd", 
  "malabsorptive",
  "motility",
  "nafld_nash",
  "nonspecific",
  "pancreatitis",
  "transplant",
  "ulcerative_disease",
  "other",
  "all_conditions",
  "all_mesh", #----------disease

  "location_esophagus",
  "location_stomach",
  "location_small_intestine",
  "location_colon_rectum",
  "location_anus",
  "location_liver",
  "location_biliarytract",
  "location_gallbladder",
  "location_pancreas",
  "location_peritoneum",
  "location_notspecified" #----------anatomic location
  )
                                                               
#####renamed new data table full_gi which takes all the columns from full_gi_df that I thought would be useful (i.e. the ones I put into the cols_to_add group)
full_gi <- subset(full_gi_df, select = cols_to_add)
                     
##created a new function that does the p-values and frequency tables
#---main_cat = variable we are looking at (i.e primary purpose), df = full_gi in this case but columns (i.e. "region") each time it's passed in
#---col_comparison = different variable we want ot compare on (in table 1 it's time bin, in table2 it's industry)
#----treat_as_csv = FALSE means that we are going to treat the row as if it's a list of variables like in a CSV. 
#If you call the get_freqs function without specifying, the default is FALSE meaning it won't treat it as a CSV
               
get_freqs <- function(main_cat, df, col_comparisons, treat_as_csv = FALSE) {

#if you do have a treat_as_csv (such as in regions, it loops and finds all the unique values and counts it for each of the region). 
#If we say FALSE or we don't say anything when we call get_freq, lines 515-522 are not executed and 523-526 is executed.
  if (treat_as_csv) {
    uniques <- c()
    for (val in na.omit(df$var)) {
      for (str in strsplit(val, ";")) {
        uniques <- c(uniques, trimws(str))
      }
    }
    uniques <- unique(uniques)
  } else {
#takes out NAs in all rows
    uniques <- unique(na.omit(df$var))
  }

#all_rows initializes a variables to be all the rows (i.e. randomization, masking, disease type, etc.) and we will use it to populate across each row
#category variable refers to all outputs (i.e. Africa, two or more, NIH, etc.)
#Here we are either counting if a category event (i.e. Africa) occurs in the cell at all AND counting each time it matches the cell exactly
  all_rows <- c()
  for (category in unique(uniques)) {
    row <- c(main_cat, category)
    if (treat_as_csv) {
      row <- c(row, length(which(str_count(df$var, category) >= 1)))
    } else {
      row <- c(row, length(which(df$var == category)))
    }

#Counts all the rows that are not NA, ie. this is the column for total N in Table 1
    row <- c(row, length(which(!is.na(df$var))))
    
#Loop through each of the col_comparisons (i.e. bintimes) what we just did above for the totals. For example, we count the 
#number of times when both category: "phase 1" and cc: "2003-2008" are true.
for (cc in col_comparisons) {
      if (treat_as_csv) {
        row <- c(row, length(which(str_count(df$var, category) >= 1 & df$col == cc)))
      } else {
        row <- c(row, length(which(df$var == category & df$col == cc)))
      }
#total count of everything in that bin where the variable is not NA. This number should be the same for every category
      row <- c(row, length(which(!is.na(df$var) & df$col == cc)))
    }

#Find total percentage of each category (i.e. how many phase 1 trials occured across all bins) 
    row <- c(row, round(100 * as.numeric(row[3]) / as.numeric(row[4]), 1))

#Now we will do chi squre  by computing two chi square values. One for the category and one for the main_cat  
row_chi_sq <- c()
    
#marginal_indexes are the indexes that the *counts* are in that is first created to be empty and gets populated in the next line  
  marginal_indexes <- c()
  for (i in 1:length(col_comparisons)) {
    marginal_indexes <- c(marginal_indexes, 3 + (i * 2))
  }
for (i in marginal_indexes) {
      row_chi_sq <- c(row_chi_sq, row[i])
      row_chi_sq <- c(row_chi_sq, as.numeric(row[i + 1]) - as.numeric(row[i]))
      row <- c(row, round(100 * as.numeric(row[i]) / as.numeric(row[i + 1]), 1))
    }
#converts previous list of 4 numbers into a 2x2 matrix to do first chi square on testing each specific subcategory
    chi_sq_res <- chisq.test(apply(matrix(row_chi_sq, nrow = 2, ncol = 2), c(1,2), as.numeric))
    row <- c(row, chi_sq_res$p.value, NA)

    all_rows <- c(all_rows, row)
  }

  num_cols <- 7 + (3 * length(col_comparisons))
  output_matrix <- matrix(all_rows, ncol = num_cols, byrow = TRUE)

#Chi square on the entire category. For example, this would test the null hypothesis that 2003-2008 bin had no relationship with phase.
#compare this to the previous chi square that tested the null hypothesis that 2003-2008 bin had no relationship with if it was phase 1 or not phase 1 
#This can't be done on any variable that is "treat_as_csv"
  if(!treat_as_csv) {
    try(all_chi_sq <- chisq.test(apply(output_matrix[, marginal_indexes], c(1,2), as.numeric)), silent = TRUE)
    try(output_matrix[,num_cols] <- all_chi_sq$p.value, silent = TRUE)
  }
  return(output_matrix)
}

do_table_analysis <- function(already_mutated, cols, include_disease) {
  
  #Primary Purpose                   
  pp <- get_freqs("Primary Purpose", already_mutated %>% mutate(var = new_primary_purpose_treatment), cols)
  
  #Phase
  phase <- get_freqs("Phase", already_mutated %>% mutate(var = br_phase4_ref_ph3), cols)
  
  #Study Arms
  study_arms <- get_freqs("Study Arms", already_mutated %>% mutate(var = 
    ifelse(!is.na(number_of_arms) & number_of_arms >= 3, "Three or more", 
    ifelse(!is.na(number_of_arms) & number_of_arms == 2, "Two", 
    ifelse(!is.na(number_of_arms) & number_of_arms == 1, "One", NA)))), cols)

  #Masking
  masking <- get_freqs("Masking", already_mutated %>% mutate(var = br_masking2), cols)
  
  #Randomized
  randomized <- get_freqs("Randomized", already_mutated %>% mutate(var = br_allocation), cols)

  #Number of enrollees
  enrollment <- get_freqs("Enrollment Number", already_mutated %>% mutate (var = 
    ifelse(!is.na(enrollment) & enrollment >= 1000, "Greater than or equal to 1000", 
    ifelse(!is.na(enrollment) & enrollment >=500 & enrollment <1000, "Between 500 and 1000", 
    ifelse(!is.na(enrollment) & enrollment >100 & enrollment <500, "Between 100 and 500", 
    ifelse(!is.na(enrollment) & enrollment <= 1, "Less than or Equal to 100", NA))))), cols)
  
  #Had Data Monitoring Committe, code is slightly different because this is a boolean column (True/False)
  has_dmc <- get_freqs("Had Data Monitoring Committee", already_mutated %>% mutate(var = 
    ifelse(!is.na(has_dmc) & has_dmc, "Yes", ifelse(!is.na(has_dmc), "No", NA))), cols)

  #Centers, this is the only one that has TRUE for "treat_as_csv"
  regions <- get_freqs("Region", already_mutated %>% mutate(var = all_regions), cols, TRUE)
 
  #Number of Countries
  num_countries <- get_freqs("Number of Countries", already_mutated %>% mutate(var = 
    ifelse(!is.na(num_countries) & num_countries >= 3, "Three or more", 
    ifelse(!is.na(num_countries) & num_countries == 2, "Two", 
    ifelse(!is.na(num_countries) & num_countries == 1, "One", NA)))), cols)

  #Number of Regions
  num_regions <- get_freqs("Number of Regions", already_mutated %>% mutate(var = 
    ifelse(!is.na(num_regions) & num_regions >= 3, "Three or more", 
    ifelse(!is.na(num_regions) & num_regions == 2, "Two", 
    ifelse(!is.na(num_regions) & num_regions == 1, "One", NA)))), cols)

  #Number of Facilities
  num_facilities <- get_freqs("Number of Facilities", already_mutated %>% mutate(var = 
    ifelse(!is.na(num_facilities) & num_facilities >10, "More than Ten",
    ifelse(!is.na(num_facilities) & num_facilities >= 3 & num_facilities <=10, "Three to Ten", 
    ifelse(!is.na(num_facilities) & num_facilities == 2, "Two", 
    ifelse(!is.na(num_facilities) & num_facilities == 1, "One", NA))))), cols)

  #Sponsor Type
  sponsor <- get_freqs("Sponsor Type", already_mutated %>% mutate(var = industry_any3), cols)

  #Were Results Reported? code is slightly different because this is a boolean column (True/False)
  reported <- get_freqs("Were Results Reported", already_mutated %>% mutate(var = 
    ifelse(!is.na(were_results_reported) & were_results_reported, "Yes", ifelse(!is.na(were_results_reported), "No", NA))), cols)


  #INFECTIONS ANY
  all_diseases <- rbind(
    get_freqs("Any Infection", already_mutated %>% mutate(var = 
      ifelse(!is.na(infection_any) & infection_any, "Yes", ifelse(!is.na(infection_any), "No", NA))), cols),
    #HELMINTHS
    get_freqs("Helminth Infection", already_mutated %>% mutate(var = 
      ifelse(!is.na(infection_helminth) & infection_helminth, "Yes", ifelse(!is.na(infection_helminth), "No", NA))), cols),
    #INTESTINES
    get_freqs("Intestinal Infection", already_mutated %>% mutate(var = 
      ifelse(!is.na(infection_intestines) & infection_intestines, "Yes", ifelse(!is.na(infection_intestines), "No", NA))), cols),
    #HEPATITIS
    get_freqs("Heptatitis", already_mutated %>% mutate(var = 
      ifelse(!is.na(infection_hepatitis) & infection_hepatitis, "Yes", ifelse(!is.na(infection_hepatitis), "No", NA))), cols),
  #NEOPLASIA DISEASE
    get_freqs("Any Neoplasia", already_mutated %>% mutate(var = 
      ifelse(!is.na(neoplasia_disease) & neoplasia_disease, "Yes", ifelse(!is.na(neoplasia_disease), "No", NA))), cols),
    #PRIMARY
    get_freqs("Primary Neoplasia", already_mutated %>% mutate(var = 
      ifelse(!is.na(neoplasia_primary) & neoplasia_primary, "Yes", ifelse(!is.na(neoplasia_primary), "No", NA))), cols),
    #METASTASIS
    get_freqs("Metastatic Neoplasia", already_mutated %>% mutate(var = 
      ifelse(!is.na(neoplasia_metastasis) & neoplasia_metastasis, "Yes", ifelse(!is.na(neoplasia_metastasis), "No", NA))), cols),
  #ABDOMINAL HERNIA
    get_freqs("Abdominal Hernia", already_mutated %>% mutate(var = 
      ifelse(!is.na(abdominal_hernia) & abdominal_hernia, "Yes", ifelse(!is.na(abdominal_hernia), "No", NA))), cols),
  #APPENDICITIS
    get_freqs("Abdominal Hernia", already_mutated %>% mutate(var = 
      ifelse(!is.na(abdominal_hernia) & abdominal_hernia, "Yes", ifelse(!is.na(abdominal_hernia), "No", NA))), cols),
  #CIRRHOSIS
    get_freqs("Appendicitis", already_mutated %>% mutate(var = 
      ifelse(!is.na(appendicitis) & appendicitis, "Yes", ifelse(!is.na(appendicitis), "No", NA))), cols),
  #DIVERTICULAR DISEASE
    get_freqs("Cirrhosis", already_mutated %>% mutate(var = 
      ifelse(!is.na(cirrhosis) & cirrhosis, "Yes", ifelse(!is.na(cirrhosis), "No", NA))), cols),
  #FECAL DIVERSION
    get_freqs("Diverticular Disease", already_mutated %>% mutate(var = 
      ifelse(!is.na(diverticular_disease) & diverticular_disease, "Yes", ifelse(!is.na(diverticular_disease), "No", NA))), cols),
  #FOREIGN BODY
    get_freqs("Fecal Diversion", already_mutated %>% mutate(var = 
      ifelse(!is.na(fecal_diversion) & fecal_diversion, "Yes", ifelse(!is.na(fecal_diversion), "No", NA))), cols),
  #FUNCTIONAL DISORDER
    get_freqs("Foreign Body", already_mutated %>% mutate(var = 
      ifelse(!is.na(foreign_body) & foreign_body, "Yes", ifelse(!is.na(foreign_body), "No", NA))), cols),
  #GALLSTONES
    get_freqs("Gallstones", already_mutated %>% mutate(var = 
      ifelse(!is.na(gallstones) & gallstones, "Yes", ifelse(!is.na(gallstones), "No", NA))), cols),
  #GERD
    get_freqs("GERD", already_mutated %>% mutate(var = 
      ifelse(!is.na(gerd) & gerd, "Yes", ifelse(!is.na(gerd), "No", NA))), cols),
  #HEMORRHOIDS
    get_freqs("Hemorhoids", already_mutated %>% mutate(var = 
      ifelse(!is.na(hemorrhoids) & hemorrhoids, "Yes", ifelse(!is.na(hemorrhoids), "No", NA))), cols),
  #HYPOXIC DISEASE
    get_freqs("Hypoxic Disease", already_mutated %>% mutate(var = 
      ifelse(!is.na(hypoxic) & hypoxic, "Yes", ifelse(!is.na(hypoxic), "No", NA))), cols),
  #ILEUS
    get_freqs("Ileus", already_mutated %>% mutate(var = 
      ifelse(!is.na(ileus) & ileus, "Yes", ifelse(!is.na(ileus), "No", NA))), cols),
  #IBD
    get_freqs("Irritable Bowel Disease", already_mutated %>% mutate(var = 
      ifelse(!is.na(ibd) & ibd, "Yes", ifelse(!is.na(ibd), "No", NA))), cols),
  #MALABSORPTION
    get_freqs("Malabsorptive Disease", already_mutated %>% mutate(var = 
      ifelse(!is.na(malabsorptive) & malabsorptive, "Yes", ifelse(!is.na(malabsorptive), "No", NA))), cols),
  #MOTILITY
    get_freqs("Motility Disease", already_mutated %>% mutate(var = 
      ifelse(!is.na(motility) & motility, "Yes", ifelse(!is.na(motility), "No", NA))), cols),
  #NAFLD/NASH
    get_freqs("NALFD or NASH", already_mutated %>% mutate(var = 
      ifelse(!is.na(nafld_nash) & nafld_nash, "Yes", ifelse(!is.na(nafld_nash), "No", NA))), cols),
  #NONSPECIFIC
    get_freqs("Nonspecific", already_mutated %>% mutate(var = 
      ifelse(!is.na(nonspecific) & nonspecific, "Yes", ifelse(!is.na(nonspecific), "No", NA))), cols),
  #PANCREATITIS
    get_freqs("Pancreatitis", already_mutated %>% mutate(var = 
      ifelse(!is.na(pancreatitis) & pancreatitis, "Yes", ifelse(!is.na(pancreatitis), "No", NA))), cols),
  #TRANSPLANT
    get_freqs("Transplant", already_mutated %>% mutate(var = 
      ifelse(!is.na(transplant) & transplant, "Yes", ifelse(!is.na(transplant), "No", NA))), cols),
  #ULCERATIVE DISEASE
    get_freqs("Ulcerative Disease", already_mutated %>% mutate(var = 
      ifelse(!is.na(ulcerative_disease) & ulcerative_disease, "Yes", ifelse(!is.na(ulcerative_disease), "No", NA))), cols),
  #OTHER
    get_freqs("Other", already_mutated %>% mutate(var = 
      ifelse(!is.na(other) & other, "Yes", ifelse(!is.na(other), "No", NA))), cols),

#ANATOMIC LOCATION
  #ESOPHAGUS
    get_freqs("Esophagus", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_esophagus) & location_esophagus, "Yes", ifelse(!is.na(location_esophagus), "No", NA))), cols),
  #STOMACH
    get_freqs("Stomach", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_stomach) & location_stomach, "Yes", ifelse(!is.na(location_stomach), "No", NA))), cols),
  #SMALL INTESTINE
    get_freqs("Small Intestine", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_small_intestine) & location_small_intestine, "Yes", ifelse(!is.na(location_small_intestine), "No", NA))), cols),
  #COLON/RECTUM
    get_freqs("Colon/Rectum", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_colon_rectum) & location_colon_rectum, "Yes", ifelse(!is.na(location_colon_rectum), "No", NA))), cols),
  #ANUS
    get_freqs("Anus", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_anus) & location_anus, "Yes", ifelse(!is.na(location_anus), "No", NA))), cols),
  #LIVER
    get_freqs("Liver", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_liver) & location_liver, "Yes", ifelse(!is.na(location_liver), "No", NA))), cols),
  #BILIARY TRACT
    get_freqs("Biliary Tract", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_biliarytract) & location_biliarytract, "Yes", ifelse(!is.na(location_biliarytract), "No", NA))), cols),
  #GALLBLADDER
    get_freqs("Gallbladder", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_gallbladder) & location_gallbladder, "Yes", ifelse(!is.na(location_gallbladder), "No", NA))), cols),
  #PANCREAS
    get_freqs("Pancreas", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_pancreas) & location_pancreas, "Yes", ifelse(!is.na(location_pancreas), "No", NA))), cols),
  #PERITONEUM
    get_freqs("Peritoneum", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_peritoneum) & location_peritoneum, "Yes", ifelse(!is.na(location_peritoneum), "No", NA))), cols),
  #NOT SPECIFIED 
    get_freqs("Not Specified", already_mutated %>% mutate(var = 
      ifelse(!is.na(location_notspecified) & location_notspecified, "Yes", ifelse(!is.na(location_notspecified), "No", NA))), cols))
if(include_disease){
  output <- rbind(
    all_diseases,
    pp, 
    phase, 
    study_arms, 
    masking, 
    enrollment, 
    randomized, 
    has_dmc, 
    num_countries, 
    regions, 
    num_regions, 
    num_facilities, 
    sponsor, 
    reported)
  } else {
    output <- rbind(
    pp, 
    phase, 
    study_arms, 
    masking, 
    enrollment, 
    randomized, 
    has_dmc, 
    num_countries, 
    regions, 
    num_regions, 
    num_facilities, 
    sponsor, 
    reported)
  }
  return(output)
}

#------TABLE 1 SIMILAR TO OPHTHO TRIAL------# 
#-----STRATIFIED BY YEAR USING BIN--------#                    
table1 <- as.data.frame(do_table_analysis(full_gi %>% mutate(col = bintime), c("2007_2012", "2013_2018"), FALSE)) 
colnames(table1) <- c(
  "Trial Characteristic", 
  "Value", 
  "Total Number of Rows Equal to Value", 
  "Total Number", 
  "Total Value for 2007-2012", 
  "Total for 2007-2012", 
  "Total Value for 2012-2018", 
  "Total for 2012-2018", 
  "Percentage of Total", 
  "Percentage of 2007-2012", 
  "Percentage of 2012-2018", 
  "p-value for row", 
  "p-value for trial characteristic")

#------TABLE 2 SIMILAR TO OPHTHO TRIAL------# 
#-----STRATIFIED BY SPONSORSHIP--------#                    
table2 <- as.data.frame(do_table_analysis(full_gi %>% mutate(col = industry_any3), c("Industry", "NIH", "Other"), TRUE))
colnames(table2) <- c(
  "Trial Characteristic", 
  "Value", 
  "Total Number of Rows Equal to Value", 
  "Total Number", 
  "Total Value for Industry", 
  "Total for Industry", 
  "Total Value for US.Govt", 
  "Total for US.Govt", 
  "Total Value for Other", 
  "Total for Other", 
  "Percentage of Total", 
  "Percentage of Industry", 
  "Percentage of US.Govt", 
  "Percentage of Other", 
  "p-value for row", 
  "p-value for trial characteristic")

#-------UNIVARIATE ANALYSIS--------#        
table3 <- as.data.frame(do_table_analysis(full_gi %>% mutate(col = early_discontinuation), c(TRUE, FALSE), TRUE))
colnames(table3) <- c(
  "Trial Characteristic", 
  "Value", 
  "Total Number of Rows Equal to Value", 
  "Total Number", 
  "Total Value of Discontinued", 
  "Total for Discontinued", 
  "Total Value of Study Completed", 
  "Total for Study Completed", 
  "Percentage of Total", 
  "Percentage of Discontinued", 
  "Percentage of Completed Studies", 
  "p-value for row", 
  "p-value for trial characteristic")
                     
  #-------MULTIPLE IMPUTATION------#
 cols_to_add_for_imputation <- c(
"nct_id",
  "early_discontinuation", 
  "industry_any3", 
  "br_phase4_ref_ph3", 
"enrollment", 
  "new_enroll", #-----Number of participants enrolled
  "bintime", #------duration 2007-2012, 2013-2018
  "new_primary_purpose_treatment", #----primary objective of the intervention  
  "interv_all_intervention_types", #-----type of intervention
"num_facilities",
"all_regions",
"num_regions",
"num_countries",
  "br_allocation", #this changes all "NA" from allocation into "non-randomized" 
  "br_masking2", #changes all "quadruple blinded" in br_masking2 to "double", changes all "None (Open Label)" to "None"
  "has_dmc", #--------oversight by a data-monitoring committee
"number_of_arms",
  "br_gni_lmic_hic_only", #This will give you which were only in HIC and which were only in LMIC.
   #new_first_submit
"br_trialduration", 
  "enrollment_type", #------enrollment type
  "overall_status", #-----status, if we want to redefine discontinuation
   #completion_date
  "were_results_reported",
"br_time_until_resultsreport_or_present_inmonths", #--just added

  "infection_any",
  "infection_helminth",
  "infection_intestines",
  "infection_hepatitis",
  "neoplasia_primary",
  "neoplasia_metastasis",
  "neoplasia_disease",
  "abdominal_hernia",
  "appendicitis",
  "cirrhosis",
  "diverticular_disease",
  "fecal_diversion",
  "foreign_body",
  "functional_disorder",
  "gallstones",
  "gerd",
  "hemorrhoids",
  "hypoxic",
  "ileus",
  "ibd", 
  "malabsorptive",
  "motility",
  "nafld_nash",
  "nonspecific",
  "pancreatitis",
  "transplant",
  "ulcerative_disease",
  "other", #----------disease

  "location_esophagus",
  "location_stomach",
  "location_small_intestine",
  "location_colon_rectum",
  "location_anus",
  "location_liver",
  "location_biliarytract",
  "location_gallbladder",
  "location_pancreas",
  "location_peritoneum",
  "location_notspecified" #----------anatomic location
  )

#####renamed new data table full_gi_imputated which takes all the columns from full_gi_df to impute with
##cannot be date object
full_gi_imputed <- subset(full_gi_df, select = cols_to_add_for_imputation)                                         

library(dplyr)
library(mice)
library(tidyverse)


# Set factor variables
micedata <- full_gi_imputed %>%
    mutate(
        early_discontinuation = as.factor(early_discontinuation),
        industry_any3 = as.factor(industry_any3),
        br_phase4_ref_ph3 = as.factor(br_phase4_ref_ph3),
        bintime = as.factor(bintime),
        new_primary_purpose_treatment = as.factor(new_primary_purpose_treatment),
        interv_all_intervention_types = as.factor(interv_all_intervention_types),
        br_allocation = as.factor(br_allocation),
        br_masking2 = as.factor(br_masking2),
        has_dmc = as.factor(has_dmc),
        br_gni_lmic_hic_only = as.factor(br_gni_lmic_hic_only),
        enrollment_type = as.factor(enrollment_type),
        were_results_reported = as.factor(were_results_reported),
        overall_status = as.factor(overall_status),
        infection_any = as.factor(infection_any),
        infection_helminth = as.factor(infection_helminth),
        infection_intestines = as.factor(infection_intestines),
        infection_hepatitis = as.factor(infection_hepatitis),
        neoplasia_metastasis = as.factor(neoplasia_metastasis),
        neoplasia_disease = as.factor(neoplasia_disease),
        abdominal_hernia = as.factor(abdominal_hernia),
        appendicitis = as.factor(appendicitis),
        cirrhosis = as.factor(cirrhosis),
        diverticular_disease = as.factor(diverticular_disease),
        fecal_diversion = as.factor(fecal_diversion),
        foreign_body = as.factor(foreign_body),
        functional_disorder = as.factor(functional_disorder),
        gallstones = as.factor(gallstones),
        gerd = as.factor(gerd),
        hemorrhoids = as.factor(hemorrhoids),
        hypoxic = as.factor(hypoxic),
        ileus = as.factor(ileus),
        ibd = as.factor(ibd),
        malabsorptive = as.factor(malabsorptive),
        motility = as.factor(motility),
        nafld_nash = as.factor(nafld_nash),
        nonspecific = as.factor(nonspecific),
        pancreatitis = as.factor(pancreatitis),
        transplant = as.factor(transplant),
        ulcerative_disease = as.factor(ulcerative_disease),
        other = as.factor(other),
      
        location_esophagus = as.factor(location_esophagus),
        location_stomach = as.factor(location_stomach),
        location_small_intestine = as.factor(location_small_intestine),
        location_colon_rectum = as.factor(location_colon_rectum),
        location_anus = as.factor(location_anus),
        location_liver = as.factor(location_liver),
        location_biliarytract = as.factor(location_biliarytract),
        location_gallbladder = as.factor(location_gallbladder),
        location_pancreas = as.factor(location_pancreas),
        location_peritoneum = as.factor(location_peritoneum),
        location_notspecified = as.factor(location_notspecified)
    )



# Relevel to reference groups, picked reference group based on which group had the most, 
# relevel can only be done for unordered factors, commented out ordered variables
#full_gi$industry_any3 <- relevel(full_gi$industry_any3, ref = "Other")
full_gi_imputed$br_phase4_ref_ph3 <- relevel(full_gi_imputed$br_phase4_ref_ph3, ref = "Phase 1/2-2")
full_gi_imputed$new_primary_purpose_treatment <- relevel(full_gi_imputed$new_primary_purpose_treatment, ref = "Treatment")
#full_gi$interv_all_intervention_types <- relevel(full_gi$interv_all_intervention_types, ref = "Biological") #----this one has multiple categories in each separated by ;
#full_gi$br_allocation <- relevel(full_gi$br_allocation, ref = "Randomized")
full_gi_imputed$br_masking2 <- relevel(full_gi_imputed$br_masking2, ref = "None")
#full_gi$br_gni_lmic_hic_only <- relevel(full_gi$br_gni_lmic_hic_only, ref = "HIC Only")
#full_gi$enrollment_type <- relevel(full_gi$enrollment_type, ref = "Actual")
#full_gi$overall_status <- relevel(full_gi$overall_status, ref = "Completed")


# Set random seed
random_seed_num <- 3249
set.seed(random_seed_num)

# This number was originally set by Rubin, and 5 was believed to be enough. 
# Since then, Bodner (2008), White et al. (2011) and Graham, Olchowski, 
# and Gilreath (2007) have all suggested this can and should be higher. 
# Graham et. al suggests that "researchers using MI should perform many 
# more imputations than previously considered sufficient", and White 
# suggested a lower bound to be 100 * the percent of cases and then to 
# go slightly higher, which here is 28. Graham suggests 20 imputations 
# for 10% to 30% of missing data. The main conclusion of the recent literature
# is, "the number of imputations should be similar to the percentage of 
# cases that are incomplete." Given the computational expense and the above
# literature, plus the small amount of missing data, a value of 10 seems valid
num_imputations <- 1

# Royston and White (2011) and Van Buuren et al. (1999) have all suggested
# that more than 10 cycles are needed for the convergence of the sampling
# distribution of imputed values, but it has also been found that it can be
# satisfactory with just 5-10 (Brand 1999; vanBuuren et al. 2006b). However,
# they also note that while slower, added extra iterations is not a bad thing.
# Van Buuren 2018 says 5-20 iterations is enough to reach convergence. However,
# we ran the well-known method described in "MICE in R" from the Journal of 
# Statistical Software (2011), and found good convergence using just 10 
# iterations. As a precaution, I've upped this to 20.
iterations <- 1

# Simply just set up the methods and predictor matrices, as suggested in Heymans and Eekhout's "Applied Missing Data Analysis"
init <- mice(micedata, maxit = 0) 
methods <- init$method
predM <- init$predictorMatrix

# For dichotomous variables, use logistic regression predictors, and for
# categorical variables, use polytonomous regression
# For continuous variables, use predictive mean matching by default 
methods[c("industry_any3", "br_phase4_ref_ph3","new_primary_purpose_treatment", 
      "interv_all_intervention_types","br_masking2","overall_status")] = "polyreg"
methods[c("early_discontinuation", "bintime", "br_allocation", "has_dmc", "br_gni_lmic_hic_only", "enrollment_type",  
      "were_results_reported", 
      "infection_any",
      "infection_helminth",
      "infection_intestines",
      "infection_hepatitis",
      "neoplasia_primary",
      "neoplasia_metastasis",
      "neoplasia_disease",
      "abdominal_hernia",
      "appendicitis",
      "cirrhosis",
      "diverticular_disease",
      "fecal_diversion",
      "foreign_body",
      "functional_disorder",
      "gallstones",
      "gerd",
      "hemorrhoids",
      "hypoxic",
      "ileus",
      "ibd", 
      "malabsorptive",
      "motility",
      "nafld_nash",
      "nonspecific",
      "pancreatitis",
      "transplant",
      "ulcerative_disease",
      "other",
      "location_esophagus",
      "location_stomach",
      "location_small_intestine",
      "location_colon_rectum",
      "location_anus",
      "location_liver",
      "location_biliarytract",
      "location_gallbladder",
      "location_pancreas",
      "location_peritoneum",
      "location_notspecified")] = "logreg" 

# Set all variables to 0 to begin with
predM <- ifelse(predM < 0, 1, 0)

# Variables which will be used for prediction
predictor_vars <- c(
      "early_discontinuation", 
      "industry_any3", 
      "br_phase4_ref_ph3", 
  #"enrollment", #--just added
      "new_enroll",
      "bintime",
      "new_primary_purpose_treatment", 
      "interv_all_intervention_types",
  #"num_facilities",#--just added
  #"all_regions", #--just added
  #"num_regions", #--just added
  #"num_countries", #--just added
      "br_allocation",
      "br_masking2",
      "has_dmc",
  #"number_of_arms",#--just added
      "br_gni_lmic_hic_only", 
 #"br_trialduration", #--just added
      "enrollment_type",
      "overall_status", 
      "were_results_reported",
  #"br_time_until_resultsreport_or_present_inmonths", #--just added
      "infection_any",
      "infection_helminth",
      "infection_intestines",
      "infection_hepatitis",
      "neoplasia_primary",
      "neoplasia_metastasis",
      "neoplasia_disease",
      "abdominal_hernia",
      "appendicitis",
      "cirrhosis",
      "diverticular_disease",
      "fecal_diversion",
      "foreign_body",
      "functional_disorder",
      "gallstones",
      "gerd",
      "hemorrhoids",
      "hypoxic",
      "ileus",
      "ibd", 
      "malabsorptive",
      "motility",
      "nafld_nash",
      "nonspecific",
      "pancreatitis",
      "transplant",
      "ulcerative_disease",
      "other",
 
      "location_esophagus",
      "location_stomach",
      "location_small_intestine",
      "location_colon_rectum",
      "location_anus",
      "location_liver",
      "location_biliarytract",
      "location_gallbladder",
      "location_pancreas",
      "location_peritoneum",
      "location_notspecified"
)

# Pick which factors should be involved in imputation. This is a well-known
# issue in multiple imputation. Meng (1994), Rubin (1996), 
# Taylor et al. (2002), and White, Royston, and Wood (2011) advocate 
# including all variables associated with the probability of missingness, 
# along with the variables contained in the dataset, and van Buuren (1999) 
# found that, "Asa a general rule, using all available information yields 
# multiple imputations that have minimal bias and maximal certainty. This 
# principle implies that the number of predictors should be as large as 
# possible."  Enders, Dietz, Montague, and Dixon (2006), Graham (2009), and 
# Jolani, Van Buuren, and Frank (2011), the imputation model should be more
#  general than the analysis model in order to capture more associations 
# between the variables. Finally, it is summed up by Hardt (2012): "the 
# imputation model should include all variables of the analysis, plus those 
# highly correlated with responses or explanatory variables". For this reason,
# we've included all variables
for (predictor_var in predictor_vars) {
    predM[predictor_var, predictor_vars] <- 1
    predM[predictor_var, predictor_var] <- 0
}

# We use multiple imputation using MICE. This is a set of multiple imputations for 
# data that is MNAR. 
imputed <- mice(
    data = micedata, 
    method = methods, 
    predictorMatrix = predM, 
    m = num_imputations, 
    maxit = iterations, 
    seed = random_seed_num
)

## Bibliogrpahy
# Allison, PD. (2002). Missing data. Thousand Oaks, CA: Sage.
# Brand JPL (1999). Development, Implementation and Evaluation of Multiple Imputation Strategies for the Statistical Analysis of Incomplete Data Sets. Ph.D. thesis, Erasmus University, Rotterdam.
# Bodner, Todd E. (2008) “What improves with increased missing data imputations?” Structural Equation Modeling: A Multidisciplinary Journal 15: 651-675.
# Moons, KG, Donders, RA, Stijnen, T, & Harrell, FE, Jr. (2006). Using the outcome for imputation of missing predictor values was preferred. Journal of Clinical Epidemiology, 59, 1092–1101.
# Royston, P, & White, IR. (2011). Multiple imputation by chained equations (MICE): implementation in Stata. Journal of Statistical Software, 45(4), 1–20.
# White, IR, Royston, P, & Wood, AM. (2011). Multiple imputation using chained equations: Issues and guidance for practice. Statistics in Medicine, 30, 377–399
# Graham, JW, Olchowski, AE, & Gilreath, TD. (2007). How many imputations are really needed? Some practical clarifications of multiple imputation theory. Prevention Science, 8, 206–213.
# Van Buuren, S, Boshuizen, HC, & Knook, DL. (1999). Multiple imputation of missing blood pressure covariates in survival analysis. Statistics in Medicine, 18, 681–694.
# van Buuren S, Brand JPL, Groothuis-Oudshoorn CGM, Rubin DB (2006b). “Fully Conditional Specification in Multivariate Imputation.” Journal of Statistical Computation and Simulation, 76(12), 1049–1064.
# Van Buuren, S. 2018. Flexible Imputation of Missing Data. Second Edition. Boca Raton, FL: Chapman & Hall/CRC.
                                           

# File looking at GI trials
set.seed(5)
wants <- c('zip', 'svMisc', 'ggpubr', 'Hmisc', 'mice', 'glmnet', 'tidyverse','RPostgreSQL', 'europepmc', 'RefManageR', 'DT', 'lubridate', 'ggplot2', 'openxlsx', 'survminer', 'Kendall', 'coin')

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
full_gi_df <- full_gi_df %>% mutate(number_of_regions = str_count(all_regions, ";"))

######creating a list of all the columns in full_gi_df I think might be useful in the analysis and called it cols_to_add
cols_to_add <- c(
	"nct_id", #------ID

	"early_discontinuation", #------PRIMARY OUTCOME OF EARLY DISCONTINUATION

	"industry_any2", #changes all US Govt to NIH, similar to new_industry_any3_ref_nih except industry_any2 has an added category of US Fed
	#"new_industry_any3_ref_nih", 
	#"industry_any2b", 
	#"new_industry_any2b_ref_usgovt", #-----Sponsorship

	"phase", #problem with this is it has "Early Phase 1, and Phase1/Phase2 and Phas 2/Phase3 categories. Not sure what to do about these
	#"br_phase4_ref_ph3", 
	#"br_phase4_ref_ph1", 
	#"br_phase2", 
	#"new_br_phase2", #----Phase

	"enrollment", #-----Number of participants enrolled
	
	"bintime", #------duration 2007-2012, 2013-2018
	
	#"new_primary_purpose_treatment", 
	#"new_primary_purpose_treatment2", 
	"primary_purpose", #----primary objective of the intervention
	
	"interv_all_intervention_types", #-----type of intervention
	
	#"new_num_facilities2", 
	"num_facilities",  #------number of sites
	
	"all_regions", 
	"num_regions",
	#"br_singleregion", this changes any cell that has more than one region into "MultiRegion"
	#"all_countries", 
	#"USA_only_facilities", 
	#"US_facility", 
	"num_countries",  #-------Geographic region of sites
	
	"allocation",
	#"br_allocation" #this changes all "NA" from allocation into "non-randomized" 
	 #-----use of randomization 
	
	#"masking", instead of "None", is called "None (Open label)" used br_masking2 instead just to make text shorter
	#"br_masking1", same as br_masking2 except has quadruple blinded
	"br_masking2", #changes all "quadruple blinded" in br_masking2 to "double", changes all "None (Open Label)" to "None"
	#------use of blinding
	
	"has_dmc", #--------oversight by a data-monitoring committee
	
	"number_of_arms", 
	#"all_comp_num_arms", honestly looks exactly the same as number_of_arms #------number of arms

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
	"location_notspecified", #----------anatomic location

	#"br_gni_lmic_hic_only", 
	#"br_gni_lmic", 
	#"br_gni_hic", #------high income vs low income coutry
	
	"new_first_submit", #------year of first submit

	"br_trialduration", #------trial duration

	"enrollment_type", #------enrollment type

	"overall_status", #-----status, if we want to redefine discontinuation

	"completion_date", #------completion date

	"were_results_reported", 
	#"months_to_report_results", don't know what this is, mostly NAs 
	"br_time_until_resultsreport_or_present_inmonths" #------how were results reported?
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
	all_chi_sq <- chisq.test(apply(output_matrix[, marginal_indexes], c(1,2), as.numeric))
	output_matrix[,13] <- all_chi_sq$p.value
	}
  return(output_matrix)
}

do_table_analysis <- function(already_mutated, cols) {
      #Primary Purpose						       
  pp <- get_freqs("Primary Purpose", already_mutated %>% mutate(var = primary_purpose), cols)
  #Phase
  phase <- get_freqs("Phase", already_mutated %>% mutate(var = phase), cols)
  #Study Arms
  study_arms <- get_freqs("Study Arms", already_mutated %>% mutate(var = 
    ifelse(!is.na(number_of_arms) & number_of_arms >= 3, "Three or more", 
    ifelse(!is.na(number_of_arms) & number_of_arms == 2, "Two", 
    ifelse(!is.na(number_of_arms) & number_of_arms == 1, "One", NA)))), cols)
  #Masking
  masking <- get_freqs("Masking", already_mutated %>% mutate(var = br_masking2), cols)
  #Randomized
  randomized <- get_freqs("Randomized", already_mutated %>% mutate(var = allocation), cols)

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
  sponsor <- get_freqs("Sponsor Type", already_mutated %>% mutate(var = industry_any2), cols)

  #Were Results Reported? code is slightly different because this is a boolean column (True/False)
  reported <- get_freqs("Were Results Reported", already_mutated %>% mutate(var = 
    ifelse(!is.na(were_results_reported) & were_results_reported, "Yes", ifelse(!is.na(were_results_reported), "No", NA))), cols)

  output <- rbind(pp, phase, study_arms, masking, randomized, has_dmc, num_countries, regions, num_regions, num_facilities, sponsor, reported)
  return(output)
}

table1 <- do_table_analysis(full_gi %>% mutate(col = bintime), c("2007_2012", "2013_2018"))
table2 <- do_table_analysis(full_gi %>% mutate(col = industry_any2), c("Industry", "NIH", "U.S. Fed", "Other"))

                                   
                                                               
# -------------------------------------------------------------------------#
# ---------                 ACTUAL FIGURES                    -------------
# -------------------------------------------------------------------------#

# for table1
nct_gi <- full_gi_df %>% pull(nct_id)
nct_gi_global <- full_gi_df %>% pull(nct_id)
nct_gi_USA <- full_gi_df %>% pull(nct_id)
nct_gi_early_global <- full_gi_df %>% filter(bintime == '2007_2012') %>% pull(nct_id)
nct_gi_late_global <- full_gi_df %>% filter(bintime == '2013_2018') %>% pull(nct_id)
nct_gi_early_USA <- full_gi_df %>% filter(bintime == '2007_2012') %>% filter(USA_only_facilities) %>% pull(nct_id)
nct_gi_late_USA <- full_gi_df %>% filter(bintime == '2013_2018') %>% filter(USA_only_facilities) %>% pull(nct_id)

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
             rename(totalenrollment_early = `2007_2012`, 
                    totalenrollment_late = `2013_2018`),
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
             rename(totalenrollment_early = `2007_2012`, 
                    totalenrollment_late = `2013_2018`),
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

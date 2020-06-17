# File looking at GI trials
set.seed(5)
libraries <- c(
  'zip', 'DescTools', 'svMisc', 'ggpubr', 'Hmisc', 'mice', 'glmnet', 
  'tidyverse', 'RefManageR', 'DT', 'lubridate', 'ggplot2', 'openxlsx', 
  'survminer', 'Kendall', 'coin', 'dplyr'
)


# Load libraries
loaded <- lapply(libraries, library, character.only = TRUE)

# Set working directory
setwd("~/Downloads/R stuff")

# --------------------------------------------------------------------------------------------------------- #
# --------------------------           Load The Rest Of The Clinical Trials Data          -------------------
# ----------------------------------------------------------------------------------------------------------#

# ----- WHAT VERSION OF DATA DO WE WANT TO USE?? --------------------
gianalysis_data_directory <- 'all_ctgov_tables_oct_29_2019'

# load most of the supporting tables
# includes things like my_fac2 or my_studies, already processed
load(file = file.path(gianalysis_data_directory, 'nct_startupfiles_1b.RData')) 
load(file = file.path(gianalysis_data_directory, 'Bigtbl.Rdata'))

# Load the FDAAA Tracker Data
fdaaa_tracker_data <- readRDS(
  file = file.path(gianalysis_data_directory, 'fdaaa_tracker_data.rds')
)
fdaa_cols <- colnames(fdaaa_tracker_data)
colnames(fdaaa_tracker_data) <- paste0('fdaaatracker_', fdaa_cols)


# -------------------------------------------------------------------------------------------------------- #
# --------------------        load and organize data from GI Team       --------------------------
# -------------------------------------------------------------------------------------------------------- #

xls_file_path <- 'all_ctgov_tables_oct_29_2019/gi_hpb_initial_data.xlsx'
raw_gi_list <- 
  openxlsx::read.xlsx(xlsxFile = xls_file_path,
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
    'other'
  )

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

cols_interventions <- 
  c(
    "interv_drug",
    "interv_other",
    "interv_device",
    "interv_procedure",
    "interv_behavioral",
    "interv_biological",
    "interv_dietary",
    "interv_radiation",
    "interv_diagnostic",
    "interv_genetic",
    "interv_combination"
  )

all_disease_cols <- c(cols_location, cols_disease_full, spec_disease)
cols_disease <- c(cols_disease_full, spec_disease)
cols_location <- c(cols_location)

raw_gi_list <- raw_gi_list %>%
  select(nct_id, true_gi, one_of(all_disease_cols), coder, codecount)


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

joined_df <- 
  left_join(raw_gi_list %>%
              filter(true_gi == '1') %>%
              select(one_of(c('nct_id','coder', all_disease_cols))),
            Bigtbl,
            by = 'nct_id')

joined_df <- joined_df %>%
  mutate(
    coder = as.factor(coder),
    study_type = as.factor(study_type),
    overall_status = as.factor(overall_status),
    last_known_status = as.factor(last_known_status),
    phase = as.factor(phase),
    enrollment_type = as.factor(enrollment_type),
    br_phase1 = as.factor(br_phase1),
    br_phase2 = as.factor(br_phase2),
    br_phase3 = as.factor(br_phase3),
    br_phase4 = as.factor(br_phase4),
    br_studystatus = as.factor(br_studystatus),
    br_gni_lmic_hic = as.factor(br_gni_lmic_hic),
    lead_agency_class = as.factor(lead_agency_class),
    allocation = as.factor(allocation),
    masking = as.factor(masking),
    primary_purpose = as.factor(primary_purpose),
    br_masking1 = as.factor(br_masking1),
    br_masking2 = as.factor(br_masking2),
    br_allocation = as.factor(br_allocation),
    br_singleregion = as.factor(br_singleregion),
    industry_any1 = as.factor(industry_any1),
    industry_any1b = as.factor(industry_any1b),
    industry_any2 = as.factor(industry_any2),
    industry_any2b = as.factor(industry_any2b),
    industry_any3 = as.factor(industry_any3),
    lead_agency_class_govt = as.factor(lead_agency_class_govt),
    numeric_study_first_submitted_date = as.numeric(as.Date(study_first_submitted_date) - as.Date("1970-01-01")),
    numeric_start_date = as.numeric(as.Date(start_date) - as.Date("1970-01-01")),
    numeric_results_first_submitted_date = as.numeric(as.Date(results_first_submitted_date) - as.Date("1970-01-01")),
    numeric_primary_completion_date = as.numeric(as.Date(primary_completion_date) - as.Date("1970-01-01"))
  )

# Adds in all of the computed columns used by full_gi_df. This is factored
# out into another function so that after imputation, we can re-compute
# columns based on that
add_additional_columns <- function(input_df, recompute_dates = FALSE) {
  if (recompute_dates) {
    input_df <- input_df %>%
      mutate(study_first_submitted_date = 
        as.Date("1970-01-01") + numeric_study_first_submitted_date) %>%
      mutate(start_date = 
        as.Date("1970-01-01") + numeric_start_date) %>%
      mutate(results_first_submitted_date = 
        as.Date("1970-01-01") + numeric_results_first_submitted_date) %>%
      mutate(primary_completion_date = 
        as.Date("1970-01-01") + numeric_primary_completion_date)
  }

  full_gi_df <- input_df %>% 
    mutate(early_discontinuation = 
      ifelse(br_studystatus == 'Stopped early', TRUE, FALSE),
      early_discontinuation_completed_vs_stoppedearly = case_when(
        br_studystatus == 'Completed' ~ FALSE,
        br_studystatus == 'Stopped early' ~ TRUE,
        TRUE ~ NA
      )
    ) %>%
    mutate(bintime = case_when(
      year(study_first_submitted_date) <= 2013 ~ '2007_2013',
      year(study_first_submitted_date) > 2013 ~ '2014_2018',
      TRUE ~ NA_character_
    )) %>%
    mutate(nct_gi = TRUE)

  # any in our set no longer in the full set? We should remove these...
  diff <- setdiff(full_gi_df %>% pull(nct_id), Bigtbl %>% pull(nct_id))
  full_gi_df <- full_gi_df %>% filter(nct_id %nin% diff)

  # ------ Get Max Date, basically date it was pulled 
   # get the last date for trials that we used
  gi_maxdate <- full_gi_df %>% 
    pull(study_first_submitted_date) %>% 
    max(na.rm = TRUE)
  gi_maxdate

  #Oct 24 2019

  # -------------------------------------------#
  # filter out interventional and stuff before May 1 2018 or after Oct 1 2007 - MARIJA to find out what this is 
          # 10/1/2007 - the date clinical trials mandated to be put in
          # 5/1/2018 arbitrary for neuroanalysis
  early_date <- '20071001'
  late_date <- '20180501'
  full_gi_df <- 
    full_gi_df %>%
    filter(study_type == 'Interventional') %>% 
    filter(study_first_submitted_date >= ymd(early_date)) %>%
    filter(study_first_submitted_date < ymd(late_date))

  # -------------------------------------------# 
  # -------- Get Size Data

  # how many trials were in database at time that we downloaded stuff? 
  my_studies %>% count(study_first_submitted_date <= gi_maxdate) 
  btest0 <- my_studies %>% 
    count(study_first_submitted_date <= gi_maxdate) %>% 
    {colnames(.)[1] <- 'totaltrials'; .}
  btest0b <- my_studies %>% 
    count(study_first_submitted_date < ymd(late_date)) %>% 
    {colnames(.)[1] <- 'totaltrials'; .}

  btest1 <- my_studies %>% 
    filter(study_first_submitted_date < ymd(late_date))

  # how many interventional trials? 
  btest2 <- btest1 %>% filter(study_type == 'Interventional') 

  # how many lost because submitted before Oct 2007?
  btest3 <- btest2 %>% filter(study_first_submitted_date >= ymd(early_date))

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

  # --------------------------------------------#
  #these are pulled from the bigtbl
  col_regions <- c('Africa', 'CentralAmerica', 'EastAsia', 'Europe', 
                  'MiddleEast', 'NorthAmerica', 'Oceania',
                  'Other', 'SouthAmerica', 'SouthAsia', 'SoutheastAsia')

  for (col in col_regions) {
    full_gi_df <- full_gi_df %>% 
      mutate(!! rlang::sym(col) := as.logical(!! rlang::sym(col)))
  }

  for (col in cols_disease) {
    full_gi_df <- full_gi_df %>% 
      mutate(!! rlang::sym(col) := as.logical(!! rlang::sym(col)))
  }

  for (col in cols_location) {
    full_gi_df <- full_gi_df %>% 
      mutate(!! rlang::sym(col) := as.logical(!! rlang::sym(col)))
  }

  # add regional data
  full_gi_df <-
    full_gi_df %>%
    mutate_at(.vars = col_regions,
              .funs = rlang::list2(~case_when(is.na(all_regions) ~ NA, 
                                              is.na(.) ~ FALSE, 
                                              TRUE ~ .)))

  full_gi_df$were_results_reported <- as.logical(full_gi_df$were_results_reported)
  full_gi_df$br_gni_hic <- as.logical(full_gi_df$br_gni_hic)

  # add a bunch of other useful columns        
  full_gi_df <-
    full_gi_df %>%
    mutate(
      new_arms = Hmisc::cut2(x = number_of_arms, cuts = c(1,2,3,Inf)),
      new_arms2 = Hmisc::cut2(x = number_of_arms, cuts = c(2, Inf)),
      new_enroll = Hmisc::cut2(x = enrollment, cuts = c(10, 50, 100, 500, 1000, Inf)),
      new_enroll2 = Hmisc::cut2(x = enrollment, cuts = c(100, Inf)),
      enroll_10 = enrollment / 10,
      enroll_20 = enrollment / 20,
      new_first_submit = year(study_first_submitted_date),
      new_num_regions = Hmisc::cut2(x = num_regions, cuts = c(1,2,3, Inf)),
      new_num_regions2 = Hmisc::cut2(x = num_regions, cuts = c(2, Inf)),
      new_num_facilities = Hmisc::cut2(x = num_facilities, cuts = c(1,2,3,10,Inf)),
      new_num_facilities2 = Hmisc::cut2(x = num_facilities, cuts = c(2, Inf)),
      new_br_phase2 = fct_relevel(
        fct_explicit_na(br_phase2, na_level = 'Unknown Phase'), 'Phase 2'
      ),
      primary_purpose = fct_relevel(primary_purpose, 'Treatment'),
      new_primary_purpose_treatment = fct_collapse(
        .f = primary_purpose, # should be able to use group_other here rather than use setdiff
        Treatment = 'Treatment', 
        Prevention = 'Prevention', 
        `Basic Science` = 'Basic Science',  # but there is a known forcats bug right now
        Other = setdiff(
          primary_purpose, 
          c("Treatment", "Prevention", "Basic Science")
        ), 
        group_other = FALSE
      ),
      new_primary_purpose_treatment2 = fct_lump(primary_purpose, n = 3),
      new_actduration = Hmisc::cut2(actual_duration, c(0, 10, 20, 30, 40, 50, Inf)),
      br_masking2 = fct_relevel(br_masking2, 'None'),
      num_disease_groups = pmap_dbl(
        list(!!! rlang::syms(cols_disease)),
        function(...) sum(...)
      ),
      single_disease_group = pmap_chr(
        list(!!! rlang::syms(cols_disease)),
        function(...) paste0(cols_disease[which(x = c(...))], collapse = ',')
      ),
      single_disease_group = case_when(
        num_disease_groups > 1 ~ 'multi_disease',
        TRUE ~ single_disease_group
      ),
      num_location_group = pmap_dbl(
        list(!!! rlang::syms(cols_location)),
        function(...) sum(...)
      ),
      single_location_group = pmap_chr(
        list(!!! rlang::syms(cols_location)),
        function(...) paste0(cols_location[which(x = c(...))], collapse = ',')
      ),
      single_location_group = case_when(
        num_location_group > 1 ~ 'multi_location',
        TRUE ~ single_location_group
      ),
      br_singleregion4 = fct_collapse(
        .f = br_singleregion,
        NorthAmerica = 'NorthAmerica', 
        Europe = 'Europe', 
        EastAsia = 'EastAsia', 
        OtherAndMultiRegion = c(
          'MultiRegion', 'MiddleEast', 'SouthAmerica', 'SoutheastAsia', 
          'SouthAsia', 'Africa', 'Oceania', 'CentralAmerica'
        )
      ),
      br_time_until_resultsreport_or_present_inmonths = case_when(
        br_studystatus != 'Completed' ~ NA_real_,
        were_results_reported ~ as.period(results_first_submitted_date - primary_completion_date) / months(1),
        TRUE ~ as.period(ymd('20180501') - primary_completion_date) / months(1)
      ),
      br_censor_were_results_reported = as.numeric(were_results_reported),
      br_were_results_reported_within_2year = case_when(
        br_studystatus != 'Completed' ~ NA,
        primary_completion_date >= ymd('20160501') ~ NA,
        were_results_reported & (br_time_until_resultsreport_or_present_inmonths <= 24) ~ TRUE,
        TRUE ~ FALSE
      ),
      br_were_results_reported_within_1year = case_when(
        br_studystatus != 'Completed' ~ NA,
        primary_completion_date >= ymd('20170501') ~ NA,
        were_results_reported & (br_time_until_resultsreport_or_present_inmonths <= 12) ~ TRUE,
        TRUE ~ FALSE
      ),
      USA_only_facilities = case_when(
        all_countries == 'UnitedStates' ~ TRUE,
        is.na(all_countries) ~ NA,
        TRUE ~ FALSE
      ),
      USA_any_facilities = case_when(
        is.na(all_countries) ~ NA,
        grepl(pattern = 'UnitedStates', x = all_countries) ~ TRUE,
        TRUE ~ FALSE
      ),
      NorthAmerica_only_facilities = case_when(
        is.na(all_regions) ~ NA,
        all_regions == 'NorthAmerica' ~ TRUE,
        TRUE ~ FALSE
      ),
      NorthAmerica_any_facilities = case_when(
        is.na(all_regions) ~ NA,
        grepl(pattern = 'NorthAmerica', x = all_regions) ~ TRUE,
        TRUE ~ FALSE
      ),
      neither3regions = pmap_lgl(
        list(!!! rlang::syms(c("NorthAmerica", "Europe", "EastAsia"))),
        function(...) ! any(sapply(list(...), function(i) i))
      ),
      new_industry_any3_ref_nih = fct_relevel(industry_any3, 'NIH'),
      new_industry_any3_ref_other = fct_relevel(industry_any3, 'Other'),
      new_industry_any2b_ref_usgovt = fct_relevel(industry_any2b, 'US.Govt'),
      new_industry_any2b_ref_other = fct_relevel(industry_any2b, 'Other'),
      br_gni_lmic_hic_only = ifelse(
        br_gni_lmic_hic == 'LMIC and HIC', 
        NA_character_, 
        br_gni_lmic_hic
      ),
      br_gni_hic_text = case_when(
        is.na(br_gni_hic) ~ NA_character_,
        br_gni_hic ~ 'IncludesHIC',
        ! br_gni_hic ~ 'OnlyLMIC'
      ),
      br_phase4_ref_ph3 = fct_relevel(br_phase4, 'Phase 2/3-3'),
      br_phase4_ref_ph1 = fct_relevel(br_phase4, 'Phase 1'),
      number_of_regions = 1 + str_count(all_regions, ";"),
      new_num_countries = Hmisc::cut2(num_countries, c(1, 2, 3, Inf)),
      year_trial = year(study_first_submitted_date)
    ) %>%
    left_join(fdaaa_tracker_data,
              by = c('nct_id' = 'fdaaatracker_registry_id'))
  
  return(full_gi_df)
}

full_gi_df <- add_additional_columns(joined_df)



# -------------------------------------------------------------------------#
# ---------                 CLEANING UP STOPS HERE                 -------------
# -------------------------------------------------------------------------#

# -------------------------------------------------------------------------#
# ---------                JOLIES ANALYSIS                -------------
# -------------------------------------------------------------------------#

####################################

# FREQUENCY TABLES and CHI-SQUARE ANALYSIS					       
							       
####################################	
format_p_val <- function(p_val) {
  if (is.na(p_val) || is.nan(p_val) || !is.numeric(p_val)) {
    return("-")
  } else if (p_val < 0.0001) {
    return(paste0(format(round(p_val, 3), nsmall = 3), "****"))
  } else if (p_val < 0.001) {
    return(paste0(format(round(p_val, 3), nsmall = 3), "***"))
  } else if (p_val < 0.01) {
    return(paste0(format(round(p_val, 3), nsmall = 3), "**"))
  } else if (p_val < 0.05) {
    return(paste0(format(round(p_val, 3), nsmall = 3), "*"))
  } else {
    return(paste0(format(round(p_val, 3), nsmall = 3), ""))
  }
}


get_freq_table <- function(group_name, input_df, col1_name, col2_name, display_group_pval = TRUE) {
  df <- input_df %>%
    mutate(var1 = !! rlang::sym(col1_name)) %>%
    mutate(var2 = !! rlang::sym(col2_name))
  
  tbl <- table(df$var1, df$var2, useNA = c("always"))
  tbl <- tbl[, colnames(tbl) %in% as.character(na.omit(colnames(tbl)))]
  rownames(tbl) <- c(as.character(na.omit(rownames(tbl))), "Missing")
  new_df <- data.frame(
    group = group_name,
    name = rownames(tbl)
  )

  if (nrow(tbl) == 3 & "TRUE" %in% rownames(tbl)) {
    new_df <- new_df %>% filter(name != "FALSE")
  }

  if (sum(tbl["Missing", ]) == 0) {
    new_df <- new_df %>% filter(name != "Missing")
  }

  for (col in colnames(tbl)) {
    col_total <- sum(tbl[, col])
    missing_total <- tbl["Missing", col]
    new_df <- new_df %>% 
      mutate(!! rlang::sym(paste0(col)) := 
        ifelse(name != "Missing",
          paste0(tbl[, col], " (", round(100 * tbl[, col] / (col_total - missing_total), 0), "%)"),
          paste0(tbl[, col], " (", round(100 * tbl[, col] / col_total, 0), "%)")
        )
      )
  }

  new_df$row_p_val <- lapply(new_df$name, function(name) {
    curr_row <- tbl[as.character(name), ]
    if (name == "Missing") {
      other_rows <- colSums(tbl) - curr_row
    } else {
      other_rows <- colSums(tbl) - tbl["Missing", ] - curr_row
    }
    p_val <- chisq.test(rbind(curr_row, other_rows))$p.value
    return(format_p_val(p_val))
  })

  if (display_group_pval) {
    tbl <- tbl[!rownames(tbl) %in% c("Missing"), ]
    new_df <- new_df %>% mutate(group_p_val = format_p_val(chisq.test(tbl)$p.value))
  } else {
    new_df <- new_df %>% mutate(group_p_val = "-")
  }

  return(new_df)
}

#Adding in Rows 			      
do_table_analysis <- function(df, cols) {                
  pp <- get_freq_table("Primary Purpose", df, "new_primary_purpose_treatment", cols)
  phase <- get_freq_table("Phase", df, "br_phase4_ref_ph3", cols)
  study_arms <- get_freq_table("Study Arms", df, "new_arms", cols)
  masking <- get_freq_table("Masking", df, "br_masking2", cols)
  randomized <- get_freq_table("Randomized", df, "br_allocation", cols)
  enrollment <- get_freq_table("Enrollment Number", df, "new_enroll", cols)
  has_dmc <- get_freq_table("Had Data Monitoring Committee", df, "has_dmc", cols)
  num_countries <- get_freq_table("Number of Countries", df, "new_num_countries", cols)
  num_regions <- get_freq_table("Number of Regions", df, "new_num_regions", cols)
  num_facilities <- get_freq_table("Number of Facilities", df, "new_num_facilities", cols)
  sponsor <- get_freq_table("Sponsor Type", df, "industry_any2b", cols)
  reported <- get_freq_table("Were Results Reported", df, "were_results_reported", cols)
  study_status <- get_freq_table("Study Status", df, "br_studystatus", cols)
  hmic_vs_lmic <- get_freq_table("LMIC and HIC", df, "br_gni_lmic_hic_only", cols)

  regions_na <- get_freq_table("Region-NorthAmerica", df, "NorthAmerica", cols, FALSE)
  regions_europe <- get_freq_table("Region-Europe", df, "Europe", cols, FALSE)
  regions_east_asia <- get_freq_table("Region-EastAsia", df, "EastAsia", cols, FALSE)
  regions_other <- get_freq_table("Region-Other", df, "neither3regions", cols, FALSE)

  interventions <- do.call(rbind, lapply(cols_interventions, function(i) {
    get_freq_table(i, full_gi_df, i, cols)
  }))

  diseases <- do.call(rbind, lapply(cols_disease, function(i) {
    get_freq_table(i, full_gi_df, i, cols)
  }))

  locations <- do.call(rbind, lapply(cols_location, function(i) {
    get_freq_table(i, full_gi_df, i, cols)
  }))

  total <- rbind(
    pp,
    phase,
    study_arms,
    masking,
    randomized,
    enrollment,
    has_dmc,
    regions_na,
    regions_europe,
    regions_east_asia,
    regions_other,
    num_countries,
    num_regions,
    num_facilities,
    sponsor,
    reported,
    study_status,
    hmic_vs_lmic,
    interventions,
    diseases,
    locations
  )

  return(total)
}


#------TABLE 1 SIMILAR TO OPHTHO TRIAL------# 
#-----STRATIFIED BY YEAR USING BIN--------#                    
table1 <- do_table_analysis(full_gi_df, "bintime")

#------TABLE 2 SIMILAR TO OPHTHO TRIAL------# 
#-----STRATIFIED BY SPONSORSHIP--------#        
table2 <- do_table_analysis(full_gi_df, "industry_any2b")            

#-------UNIVARIATE ANALYSIS--------#  
table3 <- do_table_analysis(full_gi_df, "early_discontinuation")


######################

# MANN-KENDALL AND TIME-SERIES ANALYSIS
# COCHRANE ARMITAGE                  

######################



# input = A dataframe, with one column that is year_trial, which represents the year of the trial. The other columns
# are completely arbitrary, but should be considered covering an entire category, and must be true/false, unless the
# non_boolean_column_name is set.
# 
do_time_series_analysis <- function(classification, input, num_comparisons, non_boolean_column_name = NA, year_limits = c(2008, 2017)) {
  if (length(which(is.na(input$year_trial))) > 0) {
    stop("Can't have NAs in the year column")
  }

  df <- input
  if (!is.na(non_boolean_column_name)) {
    df <- df %>% 
      # Add a column "col" that is an exact copy of the column non_boolean_column_name
      # The double-exclam means R is converting the string value held in non_boolean_column to
      # a reference to the actual column with that name in the table `input`
      mutate(col = !! rlang::sym(non_boolean_column_name)) %>%
      # Only select the year_trial and previously made `col` columns
      select(year_trial, col)
    
    # Loop through all unique, non-NA values in the `col` column. For each of these unique
    # values, we will create a new boolean column in the dataframe, representing whether or
    # not the value in `col` is equal to it. If it is NA, the value remains NA. e.g.:
    # YEAR_TRIAL    col    Other    NIH   Industry
    # 2009          Other  TRUE     FALSE FALSE
    # 2010          NIH    FALSE    TRUE  FALSE
    # 2011          NA     NA       NA    NA
    unique_values_in_col <- unique(na.omit(df$col))
    for (val in unique_values_in_col) {
      df <- df %>% 
        mutate(!! rlang::sym(as.character(val)) := 
          ifelse(is.na(col), NA, 
            ifelse(as.character(col) == val, TRUE, FALSE))
          )
    }

    # Deletes the `col` column, as it's no longer needed
    df <- df %>% select(-col)
  }

  # Remove all rows where all non-year columns are NA
  df <- df[rowSums(is.na(df)) < ncol(df) - 1, ]

  # Only include years in the target date range
  df <- df %>% 
    filter(year_trial >= year_limits[1] & year_trial <= year_limits[2])

  # Calculate frequencies for each year and column
  freqs <- df %>%
    # Set a new column all to true, as we will sum this in a couple lines, resulting in
    # all rows being counted
    mutate(total = TRUE) %>%
    group_by(year_trial) %>%
    # After being grouped by year, counts up the number of TRUEs in each column
    summarise_all(.funs = list( ~sum(., na.rm = TRUE)))

  # Calculate percentages for each frequency, out of the total number of trials
  percentages <- freqs %>%
    # Mutate all columns except year_trial and total, and perform the function `~. / total` on each cell,
    # effectively dividing the number in each column's cell by the total -- i.e., a %
    mutate_at(vars(-one_of('year_trial', 'total')), .funs = list( ~. / total)) %>%
    select(-total) %>%
    # Rename all columns except year_trial the same name but suffixed with _pct
    rename_at(vars(-one_of('year_trial')),
      function(i) paste0(i, '_pct'))

  # Merge the tables into one table that has both frequencies and percentages
  combined <- merge(freqs, percentages, by = "year_trial")
  combined <- combined %>% select(-total)

  # Average annual growth rate, taken from Brandon's code
  # Compute the year-over-year growth % for each sub-category, then take the average of all of
  # those over the observed years
  aagr <- combined %>%
    mutate_at(vars(-year_trial), 
      function(x) (x - lag(x))/lag(x)) %>%
    summarise_at(vars(-year_trial), 
      function(x) mean(x, na.rm = TRUE)) %>%
    t()

  # Compound growth rate, taken from Brandon's code
  # Just computes normal compound growth rate, i.e., ((finish / start) ^ (1 / num_years)) - 1
  cagr <- combined %>%
    summarise_at(vars(-year_trial),
      function(x) ((last(x)/first(x))^ (1/(length(x) - 1))) - 1) %>%
    t()

  # Computes Kendall-Mann p-values for each time series
  kendall_mann <- combined %>%
    summarise_at(vars(-year_trial), 
      function(x) format_p_val(Kendall::MannKendall(x)$sl)) %>%
    t()

  # Corrects for multiple comparisons problem
  bonferroni_kendall <- combined %>%
    summarise_at(vars(-year_trial), 
      function(x) format_p_val(((num_comparisons - 1)/2) * Kendall::MannKendall(x)$sl)) %>% 
    Reduce(f = cbind) %>%
    t()

  # ordinary least squares
  ols <- combined %>%
    summarise_at(vars(-year_trial), 
      function(x) format_p_val(summary(lm(x ~ year_trial, data = .))$coefficients[2, 'Pr(>|t|)'])) %>%
    t()

  # To visualize linear model, run:
  # ggplot(data = combined %>% select(year_trial, Other_pct), aes(x = year_trial, y = Other_pct)) + geom_point(color = 'blue') + geom_smooth(method = "lm", color = 'red', se = TRUE)

  # correcting Multiple comparisons problem, same as in Kendall-Mann 
  bonferroni_ols <- combined %>%
    summarise_at(vars(-year_trial), 
      function(x) format_p_val(((num_comparisons - 1)/2) * summary(lm(x ~ year_trial, data = .))$coefficients[2, 'Pr(>|t|)'])) %>%
    t()

  growth_statistics <- 
      cbind.data.frame(aagr = paste0(round(100 * aagr, 1), "%"),
                       cagr = paste0(round(100 * cagr, 1), "%"),
                       kendall_pval = kendall_mann,
                       kendall_pval_bonferroni = bonferroni_kendall,
                       ols_pval = ols,
                       ols_pval_bonferroni = bonferroni_ols,
                       cochrane_armitage = NA) %>%
      tibble::rownames_to_column('trialvar') %>%
      tbl_df()

  # Run Cochrane-Armitage
  for (name in colnames(freqs %>% select(-year_trial, -total))) {
    # For each column, create a contingency table of all the values that (1) are equal to it
    # and (2) all the ones that aren't
    contingency <- freqs %>% 
      mutate(not = total - !!rlang::sym(name)) %>%
      select(!!rlang::sym(name), not)
    cat <- CochranArmitageTest(contingency)

    # Add p-val to growth_statistics data frame for the variable of interest
    growth_statistics$cochrane_armitage[growth_statistics$trialvar == name] <- format_p_val(cat$p.value)
  }

  # Prefix all the variables in the table with a "classification" for easier sorting
  growth_statistics <- growth_statistics %>% 
    mutate(trialvar = paste0(classification, "-", trialvar))
  return(growth_statistics)
}

# Do we want to do these with multiple imputation?
num_comparisons <- 40
ts_table <- rbind(
  do_time_series_analysis("total", full_gi_df %>% select(year_trial) %>% mutate(dummy = TRUE), num_comparisons),
  do_time_series_analysis("purpose", full_gi_df, num_comparisons, "new_primary_purpose_treatment"),
  do_time_series_analysis("industry", full_gi_df, num_comparisons, "industry_any2b"),
  do_time_series_analysis("region", full_gi_df %>% select(year_trial, NorthAmerica, Europe, EastAsia, neither3regions), num_comparisons),
  do_time_series_analysis("br_gni_hic", full_gi_df, num_comparisons, "br_gni_hic"),
  do_time_series_analysis("early_discontinuation", full_gi_df, num_comparisons, "early_discontinuation"),
  do_time_series_analysis("randomization", full_gi_df, num_comparisons, "br_allocation"),
  do_time_series_analysis("masking", full_gi_df, num_comparisons, "br_masking2"),
  do_time_series_analysis("DMC", full_gi_df, num_comparisons, "has_dmc"),
  do_time_series_analysis("enrollment_type", full_gi_df, num_comparisons, "enrollment_type"),
  do_time_series_analysis("reported", full_gi_df, num_comparisons, "were_results_reported"),
  do_time_series_analysis("infection_any", full_gi_df, num_comparisons, "infection_any"),
  do_time_series_analysis("br_gni_lmic_hic_only", full_gi_df, num_comparisons, "br_gni_lmic_hic_only"),
  do_time_series_analysis("disease", full_gi_df %>% select(year_trial, all_of(cols_disease)), num_comparisons),
  do_time_series_analysis("disease_location", full_gi_df %>% select(year_trial, all_of(cols_location)), num_comparisons)
)

#######################################
           
#MULTIPLE IMPUTATION
           
#######################################

# Set factor variables
micedata <- joined_df %>%
    select(
      -start_date, 
      -completion_date, 
      -results_first_submitted_date, 
      -primary_completion_date
    )

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
num_imputations <- 2

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
init <- mice(micedata, maxit = 0, m = 1) 
methods <- init$method
predM <- init$predictorMatrix

# For dichotomous variables, use logistic regression predictors, and for
# categorical variables, use polytonomous regression
# For continuous variables, use predictive mean matching by default 
methods[c(
  "industry_any2b", 
  "br_phase4", 
  "industry_any3", 
  "primary_purpose", 
  "br_gni_lmic_hic",
  "interv_all_intervention_types",
  "br_masking2",
  "br_studystatus", 
  "phase")] = "polyreg"

methods[c(
  "br_allocation",
  "has_dmc", 
  "enrollment_type",  
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

methods[c(
  "number_of_arms", 
  "br_trialduration", 
  "actual_duration",
  "enrollment",
  "numeric_study_first_submitted_date",
  "numeric_start_date",
  "numeric_results_first_submitted_date",
  "numeric_primary_completion_date")] = "pmm"

methods[c(
  "num_facilities", 
  "num_regions", 
  "num_countries")] = "cart"

# Set all variables to 0 to begin with
predM <- ifelse(predM < 0, 1, 0)

# Variables which will be used for prediction
predictor_vars <- c(
  all_disease_cols,
  "numeric_study_first_submitted_date",
  "numeric_start_date",
  "numeric_results_first_submitted_date",
  "numeric_primary_completion_date",
  "study_type",
  "overall_status",
  "last_known_status", 
  "phase",
  "enrollment",
  "enrollment_type",
  "target_duration",
  "number_of_arms",
  "number_of_groups",
  "has_dmc",
  "is_fda_regulated_device",
  "is_fda_regulated_drug",
  "br_trialduration",
  "br_censor_earlydiscontinuation",
  "num_facilities",
  "num_countries",
  "num_regions",
  "con_other3",
  "con_other5",
  "con_other8",
  "con_other10",
  "reg_other3",
  "reg_other4",
  "reg_other5",
  "br_gni_lmic_hic",
  "UnitedStates",
  "Germany",
  "France",
  "Canada",
  "Japan",
  "UnitedKingdom",
  "Spain",
  "Italy",
  "China",
  "Poland",
  "RussianFederation",
  "KoreaRepublicof",
  "Belgium",
  "Australia",
  "Netherlands",
  "Brazil",
  "Hungary",
  "India",
  "Israel",
  "Sweden",
  "Africa",
  "CentralAmerica",
  "EastAsia",
  "Europe",
  "MiddleEast",
  "NorthAmerica",
  "Oceania",
  "Other",
  "SouthAmerica",
  "SouthAsia",
  "SoutheastAsia",
  "number_of_nsae_subjects",
  "number_of_sae_subjects",
  "actual_duration",
  "were_results_reported",
  "excludes_65_and_over",
  "excludes_18_and_under",
  "any_collab_other",
  "any_collab_industry",
  "any_collab_nih",
  "any_collab_usfed",
  "lead_agency_class_govt",
  "industry_any3",
  "br_masking2",
  "br_allocation",
  "has_placebo",
  "has_active_comparator",
  "br_singleregion",
  "interv_all_intervention_types",
  "interv_all_interv_names",
  "interv_all_interv_descriptions",
  "interv_drug",
  "interv_other",
  "interv_device",
  "interv_procedure",
  "interv_behavioral",
  "interv_biological",
  "interv_dietary",
  "interv_radiation",
  "interv_diagnostic",
  "interv_genetic",
  "interv_combination",
  "interv_combo1_behavioral",
  "interv_combo1_device",
  "interv_combo1_drugs_biologics_or_supplements",
  "interv_combo1_other",
  "interv_combo1_procedure",
  "interv_combo2_behavioral",
  "interv_combo2_device",
  "interv_combo2_drugs_biologics_or_supplements",
  "interv_combo2_other",
  "interv_combo3_behavioral",
  "interv_combo3_device_and_other",
  "interv_combo3_drugs_biologics_or_supplements",
  "allocation",
  "masking",
  "subject_masked",
  "caregiver_masked",
  "investigator_masked",
  "outcomes_assessor_masked",
  "primary_purpose",
  "all_comparators",
  "all_comp_num_arms",
  "all_unique_comparators"
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

vars_to_predict <- c(
  "br_phase4",
  "number_of_arms",
  "enrollment",
  "enrollment_type",
  "has_dmc",
  "br_trialduration",
  "num_facilities",
  "num_regions",
  "br_gni_lmic",
  "br_gni_hic",
  "br_gni_lmic_hic",
  "actual_duration",
  "industry_any3",
  "allocation",
  "industry_any2b",
  "primary_purpose",
  "br_masking2",
  "br_allocation",
  "br_singleregion",
  "numeric_study_first_submitted_date",
  "numeric_start_date",
  "numeric_results_first_submitted_date",
  "numeric_primary_completion_date"
)

for (var in vars_to_predict) {
  for (var2 in predictor_vars) {
    predM[var2, var] <- 1
  }
  predM[var, var] <- 0
}

# We use multiple imputation using MICE. This is a set of multiple imputations for 
# data that is MAR. 
imputed <- mice(
    data = micedata, 
    method = methods, 
    predictorMatrix = predM, 
    m = num_imputations, 
    maxit = iterations, 
    nnet.MaxNWts = 10000
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

          
############################
#LOGISTIC REGRESSION

# Pool uses Rubin's Rules to pool models built on a matrix of imputed data sets 
# (basically builds a model for every single imputed dataset, i.e. all 25, and 
# then uses this set of rules to pool to coefficents into an average)
# glm is the logistic regression function. adjusted risk ratio is the e^coefficient value provided

do_logistic <- function(output_variable, imputed) {
  fmla <- as.formula(paste0(output_variable, " ~ 
    industry_any2b +
    new_primary_purpose_treatment +
    br_phase4_ref_ph3 +
    new_enroll +
    br_masking2  +
    br_allocation +
    has_dmc +
    br_gni_lmic_hic +
    num_facilities +
    infection_any +
    infection_helminth +
    infection_intestines +
    infection_hepatitis +
    neoplasia_primary +
    neoplasia_metastasis +
    neoplasia_disease +
    abdominal_hernia +
    appendicitis +
    cirrhosis +
    diverticular_disease +
    fecal_diversion +
    br_singleregion4 +
    functional_disorder +
    gallstones +
    gerd +
    hemorrhoids +
    hypoxic +
    ileus +
    ibd +
    malabsorptive +
    motility +
    nafld_nash +
    nonspecific +
    pancreatitis +
    transplant +
    ulcerative_disease +
    other +
    location_esophagus +
    location_stomach +
    location_small_intestine +
    location_colon_rectum +
    location_anus +
    location_liver +
    location_biliarytract +
    location_gallbladder +
    location_pancreas +
    location_peritoneum +
    location_notspecified +
    interv_drug +
    interv_other +
    interv_device +
    interv_procedure +
    interv_behavioral +
    interv_biological +
    interv_dietary +
    interv_radiation  +
    interv_diagnostic  +
    interv_genetic  +
    interv_combination"))

  output <- imputed %>% 
    mice::complete("all") %>%
    lapply(function(i) {
      add_additional_columns(i, TRUE) %>% 
        filter(br_trialduration > 0) %>%
        filter(!is.na(!! rlang::sym(output_variable)))
    }) %>%
    lapply(glm, formula = fmla, family = binomial(link = logit)) %>%
    pool()

  summ <- summary(output, conf.int = TRUE, exponentiate = TRUE)
  ret <- do.call(rbind, lapply(seq(length(summ$term)), function(i) {
    data.frame(
        variable = summ$term[i],
        aOR = round(summ$estimate[i], 3),
        low_ci = round(summ$`2.5`[i], 3),
        hi_ci = round(summ$`97.5`[i], 3),
        p_val = format_p_val(summ$p.value[i])
    )
  }))

  return(ret)
}

results_reported <- do_logistic("br_were_results_reported_within_1year", imputed)
early_disc <- do_logistic("early_discontinuation_completed_vs_stoppedearly", imputed)

###############
		 
# KAPLAN-MEIER
		 
#############
save_kaplain_meier <- function(data, var, file_path, file_name = NA) {
  if (is.na(file_name)) {
    file_name <- paste0(var, ".png")
  }
  filtered <- data %>% filter(br_trialduration > 0)
  ffmla <- as.formula(paste0("Surv(br_trialduration, br_censor_earlydiscontinuation) ~ ", var))
  survival_fit <- surv_fit(formula = ffmla, data = filtered)
  plot <- ggsurvplot(survival_fit, 
            fun = 'event',
            data = filtered,
            xlim = c(0,60),
            censor.shape = 124,
            censor.size = 2.0,
            risk.table = TRUE,
            pval.size = 10,
            ylab = 'Cumulative incidence of\nearly discontinuation',
            size = 1.5, 
            pval = TRUE,
            break.x.by = 12) +
    xlab("Trial Duration (Months)")
    png(paste0(file_path, file_name), width = 1500, height = 1000)
    print(plot)
    dev.off()
}

save_kaplain_meier(full_gi_df, "industry_any2b", "~/Desktop/km_curves/")
save_kaplain_meier(full_gi_df, "br_phase4_ref_ph3", "~/Desktop/km_curves/")
save_kaplain_meier(full_gi_df, "new_primary_purpose_treatment", "~/Desktop/km_curves/")
save_kaplain_meier(full_gi_df, "br_allocation", "~/Desktop/km_curves/")
save_kaplain_meier(full_gi_df, "br_singleregion4", "~/Desktop/km_curves/")
save_kaplain_meier(full_gi_df, "new_num_facilities", "~/Desktop/km_curves/")
save_kaplain_meier(full_gi_df, "has_dmc", "~/Desktop/km_curves/")
save_kaplain_meier(full_gi_df, "br_masking2", "~/Desktop/km_curves/")
save_kaplain_meier(full_gi_df, "new_enroll", "~/Desktop/km_curves/")


###########

# Cox regression with all variables

###########
bformat_num <- function(num, dec = 2, cap = Inf, na_response = NA) {
  # try bformat_num(64.42424)
  # returns a string

  # note that this does not do much for really small, positive numbers! 
  # if dec = 2, anything below 0.01 becomes '0.00'; if dec = 4 anything below 0.0001 becomes '0.0000', etc
  if(is.na(num)) return(na_response)
  
  if(abs(num) > cap) {
    if(num < 0) {
      finalnum <- paste0('<-',cap) 
    } else {
      finalnum <- paste0('>',cap)
    }
    return(finalnum)
  }
  formatting <- paste0('%.',dec,'f')
  sprintf(formatting, num)
}

bvec_format_num <- function(numvec, dec = 2, cap = Inf, na_response = NA, alignWidth = F) {
  vec <- sapply(numvec, function(inum) bformat_num(inum, dec = dec, cap = cap, na_response = na_response))
  
  if(alignWidth) {
    maxn <- max(nchar(vec))
    vec <- sprintf(paste0('% ',maxn, 's'), vec)
  }
  
  return(vec)
}

do_cox <- function(imputed, do_lasso = FALSE, vars_to_select = NA, alpha = 1) {
	output <- imputed %>% 
	    mice::complete("all") %>%
	    lapply(function(i) {
	      add_additional_columns(i, TRUE) %>% 
          filter(br_trialduration > 0)
	    }) %>%
	    lapply(function(i) {
		    x <- model.matrix(
		      ~ industry_any2b +
		      ~ new_primary_purpose_treatment +
		      ~ br_phase4_ref_ph3 +
		      ~ new_enroll +
		      ~ br_masking2  +
		      ~ br_allocation +
		      has_dmc +
		      ~ br_gni_lmic_hic +
          ~ br_singleregion4 +
		      ~ new_num_facilities +
		      infection_any +
		      infection_helminth +
		      infection_intestines +
		      infection_hepatitis +
		      neoplasia_primary +
		      neoplasia_metastasis +
		      neoplasia_disease +
		      abdominal_hernia +
		      appendicitis +
		      br_trialduration +
		      br_censor_earlydiscontinuation +
		      cirrhosis +
		      diverticular_disease +
		      fecal_diversion +
		      functional_disorder +
		      gallstones +
		      gerd +
		      hemorrhoids +
		      hypoxic +
		      ileus +
		      ibd +
		      malabsorptive +
		      motility +
		      nafld_nash +
		      nonspecific +
		      pancreatitis +
		      transplant +
		      ulcerative_disease +
		      other +
		      location_esophagus +
		      location_stomach +
		      location_small_intestine +
		      location_colon_rectum +
		      location_anus +
		      location_liver +
		      location_biliarytract +
		      location_gallbladder +
		      location_pancreas +
		      location_peritoneum +
		      location_notspecified +
		      interv_drug +
		      interv_other +
		      interv_device +
		      interv_procedure +
		      interv_behavioral +
		      interv_biological +
		      interv_dietary +
		      interv_radiation  +
		      interv_diagnostic  +
		      interv_genetic  +
		      interv_combination,
		      i
		    )
		    if (length(vars_to_select) > 1) {
		    	vars_to_select <- as.character(vars_to_select)
		    	if (!("br_censor_earlydiscontinuation" %in% vars_to_select)) {
		    		vars_to_select <- c(vars_to_select, "br_censor_earlydiscontinuation")
		    	}
		    	if (!("br_trialduration" %in% vars_to_select)) {
		    		vars_to_select <- c(vars_to_select, "br_trialduration")
		    	}

		    	x <- x[, vars_to_select]
		    }

		    return(as.data.frame(x))
	    })


	if (!do_lasso) {
		output <- output %>% 
      lapply(coxph, formula = Surv(br_trialduration, br_censor_earlydiscontinuation) ~ .) %>%
      pool()

		summary_table <- 
		  summary(output, conf.int = TRUE, exponentiate = TRUE, conf.level = 0.95)

		coef_table <- 
		  summary_table %>%
		  tibble::rownames_to_column('coxlevels') %>%
		  dplyr::select(coxlevels, name = term, Estimate = estimate, `Std. Error` = std.error, `z value` = statistic, `Pr(>|z|)` = p.value)

		# select the two columns that correspond to the upper and lower confidence estimates
		conf_table <- 
		  summary_table[, c((ncol(summary_table) - 1), ncol(summary_table))]

		colnames(conf_table) <- c('cox_HR_conf_low','cox_HR_conf_high')

		conf_table <- 
		  conf_table %>%
		  tibble::rownames_to_column('coxlevels') 

		stats_table <- 
      left_join(conf_table, # this should go first or else you lose the rows that are NAs
                coef_table, 
                by = 'coxlevels') %>%
      mutate(coxHR = Estimate, # I don't need to exponentiate in this version because I've already exponentiated in the summary_table
              coxpvals = `Pr(>|z|)`)

		# format everything so the strings look nice
		coef_full_table <- 
		    stats_table %>% 
		    mutate(FMT_HR = bvec_format_num(coxHR, cap=100) %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
		            FMT_PVAL = formatC(coxpvals, digits = 2, format = 'e') %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
		            FMT_up = bvec_format_num(cox_HR_conf_high, cap=100) %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'), .)},
		            FMT_low = bvec_format_num(cox_HR_conf_low, cap=100),
		            FMT_conf = paste0('(',FMT_low,' - ',FMT_up,')') %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'),.)},
		            HR_full_p = paste0(FMT_HR, ' ', FMT_conf, '; p<',FMT_PVAL),
		            HR_full = paste0(FMT_HR, ' ', FMT_conf)) %>%
		    mutate(FMT_PVAL = case_when(
		                coxpvals < 0.0001 ~ paste0(format(round(coxpvals, 3), nsmall = 3), "***"),
		                coxpvals < 0.001 ~ paste0(format(round(coxpvals, 3), nsmall = 3), "***"),
		                coxpvals < 0.01 ~ paste0(format(round(coxpvals, 3), nsmall = 3), "**"),
		                coxpvals < 0.05 ~ paste0(format(round(coxpvals, 3), nsmall = 3), "*"),
		                TRUE ~ as.character(format(round(coxpvals, 3), nsmall = 3))
		            )) %>%
		    dplyr::select(name, FMT_HR, FMT_conf, FMT_PVAL)
		
		return(coef_full_table)
	} else {
    should_standardize <- TRUE
		good_cols <- c()
		for (data in output) {
			mat <- as.matrix(data)
			mat <- mat[ , ! colnames(mat) %in% c("br_trialduration", "br_censor_earlydiscontinuation")]
			cv.fit <- cv.glmnet(mat, Surv(data$br_trialduration, data$br_censor_earlydiscontinuation), alpha = alpha, standardize = should_standardize, family = "cox", nfolds = 20, grouped = TRUE, maxit = 1000)
			fit <- glmnet(mat, Surv(data$br_trialduration, data$br_censor_earlydiscontinuation), alpha = alpha, standardize = should_standardize, family = "cox", maxit = 1000)
			coefs <- coef(cv.fit, s = "lambda.min")
			for (i in 1:length(coefs[, "1"])) {
				if (coefs[, "1"][i] != 0) {
					good_cols <- c(good_cols, attr(coefs[, "1", ][i], "names"))
				}
			}
		}

    # https://pubmed.ncbi.nlm.nih.gov/18203127/
		all_vars <- as.data.frame(table(good_cols)) %>% 
			filter(Freq >= imputed$m / 2)

		return(all_vars$good_cols)
	}
}

# Still need to do: Assess Cox fit, prove MAR, and prove PH assumption
# Fit PH models based on all variables, variables significant in bivariate, stepwise p-values,
# stepwise AIC, domain knowledge, and lasso regression

cox_results <- do_cox(imputed)
lasso_columns <- do_cox(imputed, do_lasso = TRUE)
lasso_cox_results <- do_cox(imputed, do_lasso = FALSE, vars_to_select = lasso_columns)
joined <- full_join(cox_results, lasso_cox_results, by = "name", suffix = c(".full", ".lasso"))


#########
View(table1)
View(table2)
View(table3)
View(ts_table)
View(results_reported)
View(early_disc)
View(joined)

		
##########
#DATA VIS#
##########

#Stream Graphs, can edit "new_primary_purpose_treatment" into any variable, net variable is the title
make_plots <- function(full_gi_df, col_name, title) {
	data <- full_gi_df %>% 
		filter(!is.na(!!rlang::sym(col_name))) %>%
		select(year_trial, !!rlang::sym(col_name)) %>% 
		group_by(year_trial, !!rlang::sym(col_name)) %>% 
		summarise(n = n())

	totals <- data %>%
		group_by(year_trial) %>%
		summarise(n = sum(n))

	joined <- left_join(data, totals, by =  "year_trial")
	joined <- joined %>% 
		mutate(pct = 100 * n.x / n.y) %>%
		select(year_trial, !!rlang::sym(col_name), pct)

	ggplot(joined, aes(x = year_trial, y = pct, fill = !!rlang::sym(col_name))) +
		geom_area(alpha=0.6 , size=.5, colour="white") +
		scale_fill_viridis(discrete = T) +
		ggtitle(title)
}

make_plots(full_gi_df, "new_primary_purpose_treatment", "Purpose Over Time")

		
		
###MUST LOAD BRANDONS MASTER DOCUMENT BEFORE RuNNING THIS CHECK TO MAKE SURE DATA GOES TO 2019###

####gg_fig_1a_yearlytotal_gi_global######
fig_df_1a_yearly_gi_global <- gi_trial_growth_data_global %>% select(year_trial, total_trials_all)
fig_df_1a_yearly_gi_USA <- gi_trial_growth_data_USA %>% select(year_trial, total_trials_all)
max_yearly_gi_global <- max(fig_df_1a_yearly_gi_global$total_trials_all)
max_yearly_gi_USA <- max(fig_df_1a_yearly_gi_USA$total_trials_all)

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


############################
####gg_fig_1c_yearlypct_gi_combined_global######

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

##########################
####gg_fig_1d_yearly_sponsor_gi_pct_global######

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

##########################
####gg_fig_1b_yearly_enrollment_gi_global######


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


  gg_fig_1_4pane_gi_global_c <- ggpubr::ggarrange(gg_fig_1a_yearlytotal_gi_global,
                                                   gg_fig_1c_yearlypct_gi_combined_global,
                                                   gg_fig_1d_yearly_sponsor_gi_pct_global,
                                                   # gg_fig_1e_yearly_region_gi_pct_global,
                                                   gg_fig_1b_yearly_enrollment_gi_global,
                                                   nrow = 4, ncol = 1,
                                                   heights = c(2, 1, 2, 2),
                                                   labels = c('A', 'B', 'C', 'D')) %>%
  annotate_figure(top= text_grob('gg_fig_1_4pane_gi_global_c', face = 'bold'))
		
		

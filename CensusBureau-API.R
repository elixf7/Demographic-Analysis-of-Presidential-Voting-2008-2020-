# 0. Install & load
rm(list=ls())

library(tidycensus)  # for get_acs() & load_variables()
library(dplyr)       
library(tidyr)       # for pivot_wider()
library(stringr)     # for string matching


# Simplified function to fetch ACS data with age groups
fetch_acs_year <- function(endyr) {
  # Load variable definitions
  var_defs <- load_variables(year = endyr, dataset = "acs5", cache = TRUE)
  
  # Get education variables (B15002)
  edu_defs <- var_defs %>% filter(str_starts(name, "B15002_"))
  
  # Define education bins
  not_hs_codes     <- edu_defs$name[str_detect(edu_defs$label, "No schooling|grade, no diploma|Nursery|5th|7th|0th|10th|11th")]
  hs_grad_codes    <- edu_defs$name[str_detect(edu_defs$label, "High school graduate|GED or alternative")]
  some_coll_codes  <- edu_defs$name[str_detect(edu_defs$label, "Some college|Associate")]
  bachelors_codes  <- edu_defs$name[str_detect(edu_defs$label, "Bachelor's degree$")]
  higher_deg_codes <- edu_defs$name[str_detect(edu_defs$label, "Master's degree|Professional school|Doctorate")]
  
  # Race & Hispanic variables
  white_code   <- "B02001_002"  # White alone
  black_code   <- "B02001_003"  # Black or African American alone
  asian_code   <- "B02001_005"  # Asian alone
  hisp_code    <- "B03001_003"  # Hispanic or Latino
  
  # Median Income
  inc_code <- "B19013_001"
  
  # All age variables
  age_codes <- paste0("B01001_", str_pad(1:49, 3, pad = "0"))
  
  # Pull data from API
  all_vars <- c(edu_defs$name, white_code, black_code, asian_code, hisp_code, inc_code, age_codes)
  
  df <- get_acs(
    geography   = "county",
    variables   = all_vars,
    year        = endyr,
    survey      = "acs5",
    cache_table = TRUE
  ) %>%
    select(GEOID, NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(end_year = endyr)
  
  # Process education variables
  df <- df %>%
    mutate(
      not_hs        = rowSums(across(all_of(not_hs_codes)), na.rm = TRUE),
      hs_grad       = rowSums(across(all_of(hs_grad_codes)), na.rm = TRUE),
      some_college  = rowSums(across(all_of(some_coll_codes)), na.rm = TRUE),
      bachelors     = rowSums(across(all_of(bachelors_codes)), na.rm = TRUE),
      higher_degree = rowSums(across(all_of(higher_deg_codes)), na.rm = TRUE),
      white         = .data[[white_code]],
      black         = .data[[black_code]],
      asian         = .data[[asian_code]],
      hispanic      = .data[[hisp_code]],
      med_income    = .data[[inc_code]]
    )
  
  # Define simple age groups (based on variables that should be available)
  df <- df %>%
    mutate(
      # Children (Under 18)
      under_18_male = rowSums(across(c(B01001_003, B01001_004, B01001_005, B01001_006)), na.rm = TRUE),
      under_18_female = rowSums(across(c(B01001_027, B01001_028, B01001_029, B01001_030)), na.rm = TRUE),
      under_18 = under_18_male + under_18_female,
      
      # Young adults (18-24)
      young_adults_male = rowSums(across(c(B01001_007, B01001_008, B01001_009, B01001_010)), na.rm = TRUE),
      young_adults_female = rowSums(across(c(B01001_031, B01001_032, B01001_033, B01001_034)), na.rm = TRUE),
      young_adults = young_adults_male + young_adults_female,
      
      # Working age I (25-44)
      working_age_I_male = rowSums(across(c(B01001_011, B01001_012, B01001_013, B01001_014)), na.rm = TRUE),
      working_age_I_female = rowSums(across(c(B01001_035, B01001_036, B01001_037, B01001_038)), na.rm = TRUE),
      working_age_I = working_age_I_male + working_age_I_female,
      
      # Working age II (45-64)
      working_age_II_male = rowSums(across(c(B01001_015, B01001_016, B01001_017, B01001_018, B01001_019)), na.rm = TRUE),
      working_age_II_female = rowSums(across(c(B01001_039, B01001_040, B01001_041, B01001_042, B01001_043)), na.rm = TRUE),
      working_age_II = working_age_II_male + working_age_II_female,
      
      # Seniors (65+)
      seniors_male = rowSums(across(c(B01001_020, B01001_021, B01001_022, B01001_023, B01001_024, B01001_025)), na.rm = TRUE),
      seniors_female = rowSums(across(c(B01001_044, B01001_045, B01001_046, B01001_047, B01001_048, B01001_049)), na.rm = TRUE),
      seniors = seniors_male + seniors_female,
      
      # Total population
      total_population = B01001_001
    )
  
  return(df)
}

# Run for all years
run_acs_analysis <- function() {
  # Set Census API key
  census_api_key("ff663d6a21d1067227543328ad3d2d3d0c5f40c8", install = FALSE)
  
  # Try each year separately
  available_years <- c(2009, 2012, 2016, 2020)
  acs_list <- list()
  
  for(yr in available_years) {
    tryCatch({
      cat("Processing year", yr, "\n")
      acs_list[[as.character(yr)]] <- fetch_acs_year(yr)
    }, error = function(e) {
      cat("Error for year", yr, ":", conditionMessage(e), "\n")
    })
  }
  
  # Combine successful years
  acs_all <- bind_rows(acs_list)
  
  # Calculate percentages
  if(nrow(acs_all) > 0) {
    acs_all <- acs_all %>%
      mutate(
        # Education percentages
        total_edu = not_hs + hs_grad + some_college + bachelors + higher_degree,
        pct_not_hs = not_hs / total_edu * 100,
        pct_hs_grad = hs_grad / total_edu * 100,
        pct_some_college = some_college / total_edu * 100,
        pct_bachelors = bachelors / total_edu * 100,
        pct_higher_degree = higher_degree / total_edu * 100,
        
        # Race percentages
        race_total = white + black + asian + hispanic,
        pct_white = white / race_total * 100,
        pct_black = black / race_total * 100,
        pct_asian = asian / race_total * 100,
        pct_hispanic = hispanic / race_total * 100,
        
        # Age percentages
        pct_under_18 = under_18 / total_population * 100,
        pct_young_adults = young_adults / total_population * 100,
        pct_working_age_I = working_age_I / total_population * 100,
        pct_working_age_II = working_age_II / total_population * 100,
        pct_seniors = seniors / total_population * 100
      )
  }
  
  return(acs_all)
}

# Save to csv
# setwd('/Users/elifried/Desktop/Poli281/Final Project/Data-Final')
# write.csv(acs_data, 'acs-2009-2020.csv')

# Load acs dataset
acs_all <- read.csv('acs-2009-2020.csv')

# Remove the columns labeled with Table ID from ACS
acs <- acs_all %>%
  select(-starts_with("B"))
glimpse(acs)

# Load presidential returns
voting_df <- read.csv('countypres_2000-2020.csv')
glimpse(voting_df %>%
          filter(year == 2009))
# ---------------------------------------------------------------- #

##### Loading relevant libraries. #####

# _Note_: please install any packages you do not already have using
# `install.packages(<package_name>)`, or run the conda environment.

# ---------------------------------------------------------------- #

### Importing
library(readr)

### Cleaning
library(tidyverse)
library(lubridate)
library(data.table)

### Summary Statistics
library(skimr)
library(Hmisc)

# ---------------------------------------------------------------- #

##### Importing data. #####

# ---------------------------------------------------------------- #

# First, for overall national and state totals.
raw_nmr_panel_a_totals <- read.csv("input/nmr_panel_a_totals.csv")
cat("Overall national mortality rates for panel A: 1999 - 2006 have been loaded... \n") 
head(raw_nmr_panel_a_totals)

raw_nmr_panel_b_totals <- read.csv("input/nmr_panel_b_totals.csv")
cat("Overall national mortality rates for panel B: 2007 - 2022 have been loaded... \n") 
head(raw_nmr_panel_b_totals)

raw_smr_panel_a_totals <- read.csv("input/smr_panel_a_totals.csv")
cat("Overall state mortality rates for panel A: 1999 - 2006 have been loaded... \n") 
head(raw_smr_panel_a_totals)

raw_smr_panel_b_totals <- read.csv("input/smr_panel_b_totals.csv")
cat("Overall state mortality rates for panel B: 2007 - 2022 have been loaded... \n") 
head(raw_smr_panel_b_totals)

# Next, for age-adjusted totals by gender.
raw_nmr_panel_a_gender <- read.csv("input/nmr_panel_a_gender.csv")
cat("National mortality rates by gender for panel A: 1999 - 2006 have been loaded... \n") 
head(raw_nmr_panel_a_gender)

raw_nmr_panel_b_gender <- read.csv("input/nmr_panel_b_gender.csv")
cat("National mortality rates by gender for panel B: 2007 - 2022 have been loaded... \n") 
head(raw_nmr_panel_b_gender)

# Next, for age-specific totals by gender.
specific_nmr_panel_a_gender <- read.csv("input/nmr_panel_a_gender_specific.csv")
cat("Age-specific national mortality rates by gender for panel A: 1999 - 2006 have been loaded... \n") 
head(specific_nmr_panel_a_gender)

specific_nmr_panel_b_gender <- read.csv("input/nmr_panel_b_gender_specific.csv")
cat("Age-specific national mortality rates by gender for panel B: 2007 - 2022 have been loaded... \n") 
head(specific_nmr_panel_b_gender)

# ---------------------------------------------------------------- #

##### Cleaning dataframes. #####

# ---------------------------------------------------------------- #


# First, building a function to clean our raw dataframes.
clean_raw_dfs <- function(raw_dat, icd_input) {

    # Defining a match list for our totals across time periods.
    match_list <- c("1999 - 2006", "2007 - 2020")

    # Generating a new cleaned dataframe.
    clean_dat <- raw_dat %>% 
        
        # Renaming variables.
        dplyr::rename(time_period = `Year`,
                      raw_death_count = `Deaths`,
                      us_time_period_pop = `Population`,
                      raw_death_rate = `Crude.Rate`,
                      raw_death_rate_ci_low = `Crude.Rate.Lower.95..Confidence.Interval`,
                      raw_death_rate_ci_hi = `Crude.Rate.Upper.95..Confidence.Interval`,
                      raw_death_rate_se = `Crude.Rate.Standard.Error`,
                      age_adj_death_rate = `Age.Adjusted.Rate`,
                      age_adj_death_rate_ci_low = `Age.Adjusted.Rate.Lower.95..Confidence.Interval`,
                      age_adj_death_rate_ci_hi = `Age.Adjusted.Rate.Upper.95..Confidence.Interval`,
                      age_adj_death_rate_se = `Age.Adjusted.Rate.Standard.Error`,
                      time_period_death_share = `X..of.Total.Deaths`) %>%
        
        # Replacing missing values with NAs.
        dplyr::mutate_if(is.character, list(~na_if(., "Unrestricted"))) %>%

        # Initializing variable for ICD-code based on specified inputs and
        # formatting numeric values.
        dplyr::mutate(icd_code = icd_input,
                      us_time_period_pop = format(as.numeric(us_time_period_pop), big.mark = ","),
                      raw_death_rate = as.numeric(raw_death_rate) * 100,
                      raw_death_rate_ci_low = as.numeric(raw_death_rate_ci_low) * 100,
                      raw_death_rate_ci_hi = as.numeric(raw_death_rate_ci_hi) * 100,
                      raw_death_rate_se = as.numeric(raw_death_rate_se) * 100,
                      age_adj_death_rate = as.numeric(age_adj_death_rate) * 100,
                      age_adj_death_rate_ci_low = as.numeric(age_adj_death_rate_ci_low) * 100,
                      age_adj_death_rate_ci_hi = as.numeric(age_adj_death_rate_ci_hi) * 100,
                      age_adj_death_rate_se = as.numeric(age_adj_death_rate_se) * 100,
                      time_period_death_share = readr::parse_number(time_period_death_share)) %>%
    
        # Reordering dataframe rows so totals across time periods are at bottom.
        dplyr::arrange(time_period %in% match_list)

        return(as.data.frame(clean_dat))

}

# Now, building a function to clean our age-specific dataframes.
clean_specific_dfs <- function(raw_dat, icd_input) {

    # Defining a match list for our totals across time periods.
    match_list <- c("1999 - 2006", "2007 - 2020")

    # Generating a new cleaned dataframe.
    clean_dat <- raw_dat %>% 
        
        # Renaming variables.
        dplyr::rename(time_period = `Year`,
                      gender = `Gender`,
                      age_groups = `Ten.Year.Age.Groups`,
                      raw_death_count = `Deaths`,
                      us_time_period_pop = `Population`,
                      age_specific_death_rate = `Crude.Rate`,
                      age_specific_death_rate_ci_low = `Crude.Rate.Lower.95..Confidence.Interval`,
                      age_specific_death_rate_ci_hi = `Crude.Rate.Upper.95..Confidence.Interval`,
                      age_specific_death_rate_se = `Crude.Rate.Standard.Error`,
                      time_period_death_share = `X..of.Total.Deaths`) %>%
        
        # Replacing missing values with NAs.
        dplyr::mutate_if(is.character, list(~na_if(., "Unrestricted"))) %>%

        # Initializing variable for ICD-code based on specified inputs and
        # formatting numeric values.
        dplyr::mutate(icd_code = icd_input,
                      us_time_period_pop = format(as.numeric(us_time_period_pop), big.mark = ","),
                      age_specific_death_rate = as.numeric(age_specific_death_rate) * 100,
                      age_specific_death_rate_ci_low = as.numeric(age_specific_death_rate_ci_low) * 100,
                      age_specific_death_rate_ci_hi = as.numeric(age_specific_death_rate_ci_hi) * 100,
                      age_specific_death_rate_se = as.numeric(age_specific_death_rate_se) * 100,
                      time_period_death_share = readr::parse_number(time_period_death_share)) %>%
    
        # Reordering dataframe rows so totals across time periods are at bottom.
        dplyr::arrange(time_period %in% match_list)

        return(as.data.frame(clean_dat))

}

# Applying our cleaning function to the relevant dataframes...

# First, for our overall national and state totals.
cleaned_nmr_panel_a_totals <- clean_raw_dfs(raw_dat = raw_nmr_panel_a_totals, icd_input = "B94.8")
cleaned_nmr_panel_b_totals <- clean_raw_dfs(raw_dat = raw_nmr_panel_b_totals, icd_input = "A81.0")

cleaned_smr_panel_a_totals <- clean_raw_dfs(raw_dat = raw_smr_panel_a_totals, icd_input = "B94.8")
cleaned_smr_panel_b_totals <- clean_raw_dfs(raw_dat = raw_smr_panel_b_totals, icd_input = "A81.0")

# Next, for our overall totals split by gender.
cleaned_nmr_panel_a_gender <- clean_raw_dfs(raw_dat = raw_nmr_panel_a_gender, icd_input = "B94.8")
cleaned_nmr_panel_b_gender <- clean_raw_dfs(raw_dat = raw_nmr_panel_b_gender, icd_input = "A81.0")

# Next, for our age-specific totals by gender.
cleaned_nmr_panel_a_gender_specific <- clean_specific_dfs(raw_dat = specific_nmr_panel_a_gender, icd_input = "B94.8")
cleaned_nmr_panel_b_gender_specific <- clean_specific_dfs(raw_dat = specific_nmr_panel_b_gender, icd_input = "A81.0")

# Completing final renames for state-level data.
cleaned_smr_panel_a_totals <- cleaned_smr_panel_a_totals %>%
    dplyr::rename(state = `State`,
                  state_code = `State.Code`)

cleaned_smr_panel_b_totals <- cleaned_smr_panel_b_totals %>%
    dplyr::rename(state = `State`,
                  state_code = `State.Code`)

# Completing final renames for national data by gender.
cleaned_nmr_panel_a_gender <- cleaned_nmr_panel_a_gender %>%
    dplyr::rename(gender = `Gender`)

cleaned_nmr_panel_b_gender <- cleaned_nmr_panel_b_gender %>%
    dplyr::rename(gender = `Gender`)

cat("Completed cleaning process ... \n")

# ---------------------------------------------------------------- #

##### Appending dataframes. #####

# ---------------------------------------------------------------- #

cleaned_nmr_totals <- rbind(cleaned_nmr_panel_a_totals, cleaned_nmr_panel_b_totals)
cleaned_smr_totals <- rbind(cleaned_smr_panel_a_totals, cleaned_smr_panel_b_totals)
cleaned_nmr_gender <- rbind(cleaned_nmr_panel_a_gender, cleaned_nmr_panel_b_gender)
cleaned_nmr_gender_specific <- rbind(cleaned_nmr_panel_a_gender_specific, cleaned_nmr_panel_b_gender_specific)

# ---------------------------------------------------------------- #

##### Exploring dataframe and saving summary statistics. #####

# ---------------------------------------------------------------- #

# First, for our overall national totals.
Hmisc::describe(cleaned_nmr_totals)
skimr::skim(cleaned_nmr_totals)

cat("Completed initial data exploration for overall national totals... \n")

# Next, for our overall state totals.
Hmisc::describe(cleaned_smr_totals)
skimr::skim(cleaned_smr_totals)

cat("Completed initial data exploration for overall state totals... \n")

# Now, for our overall totals split by gender.
Hmisc::describe(cleaned_nmr_gender)
skimr::skim(cleaned_nmr_gender)

# Now, for our age-specific totals split by gender.
Hmisc::describe(cleaned_nmr_gender_specific)
skimr::skim(cleaned_nmr_gender_specific)

# ---------------------------------------------------------------- #

##### Saving dataframe outputs as RDS and CSV files. #####

# ---------------------------------------------------------------- #

# RDS format.
saveRDS(cleaned_nmr_totals, file = "output/cleaned_nmr_totals.RDS")
saveRDS(cleaned_smr_totals, file = "output/cleaned_smr_totals.RDS")
saveRDS(cleaned_nmr_gender, file = "output/cleaned_nmr_gender.RDS")
saveRDS(cleaned_nmr_gender_specific, file = "output/cleaned_nmr_gender_specific.RDS")

# CSV format.
write.csv(cleaned_nmr_totals, "output/cleaned_nmr_totals.csv", row.names = FALSE)
write.csv(cleaned_smr_totals, "output/cleaned_smr_totals.csv", row.names = FALSE)
write.csv(cleaned_nmr_gender, "output/cleaned_nmr_gender.csv", row.names = FALSE)
write.csv(cleaned_nmr_gender_specific, "output/cleaned_nmr_gender_specific.csv", row.names = FALSE)

cat("Saved output dataframes... \n \n \n")

cat("##### CLEANING COMPLETED ##### \n \n \n")

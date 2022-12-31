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
library(summarytools)

# ---------------------------------------------------------------- #

##### Importing data. #####

# ---------------------------------------------------------------- #

# First, for overall totals.
raw_nmr_panel_a_totals <- read.csv("raw/nmr_panel_a_totals.csv")
cat("Overall national mortality rates for panel A: 1999 - 2006 have been loaded... \n") 
head(raw_nmr_panel_a_totals)

raw_nmr_panel_b_totals <- read.csv("raw/smr_panel_b_totals.csv")
cat("Overall national mortality rates for panel B: 2007 - 2022 have been loaded... \n") 
head(raw_nmr_panel_b_totals)

raw_smr_panel_a_totals <- read.csv("raw/smr_panel_a_totals.csv")
cat("Overall state mortality rates for panel A: 1999 - 2006 have been loaded... \n") 
head(raw_smr_panel_a_totals)

raw_smr_panel_b_totals <- read.csv("raw/smr_panel_b_totals.csv")
cat("Overall state mortality rates for panel B: 2007 - 2022 have been loaded... \n") 
head(raw_smr_panel_b_totals)

# Next, for totals by gender.

raw_nmr_panel_a_gender <- read.csv("raw/nmr_panel_a_gender.csv")
cat("National mortality rates by gender for panel A: 1999 - 2006 have been loaded... \n") 
head(raw_nmr_panel_a_gender)

raw_nmr_panel_b_gender <- read.csv("raw/smr_panel_b_gender.csv")
cat("National mortality rates by gender for panel B: 2007 - 2022 have been loaded... \n") 
head(raw_nmr_panel_b_gender)

# ---------------------------------------------------------------- #

##### Cleaning dataframes. #####

# ---------------------------------------------------------------- #

clean_dfs <- function(raw_dat, icd_input) {
    
    clean_dat <- raw_dat %>% 
        
        # Renaming variables.
        dplyr::rename(time_period = `Year`,
                      raw_death_count = `Deaths`,
                      us_time_period_pop = `Population`,
                      raw_death_rate = `Crude Rate`,
                      raw_death_rate_ci_low = `Crude Rate Lower 95% Confidence Interval`,
                      raw_death_rate_ci_hi = `Crude Rate Upper 95% Confidence Interval`,
                       = `Crude Rate Standard Error`,
                      age_adj_death_rate = `Age Adjusted Rate`,
                      age_adj_death_rate_ci_low = `Age Adjusted Rate Lower 95% Confidence Interval`,
                      age_adj_death_rate_ci_hi = `Age Adjusted Rate Upper 95% Confidence Interval`,
                      time_period_death_share = `% of Total Deaths`) %>%
        
        # Initializing variable for ICD-code based on specified inputs and
        # formatting numeric values.
        dplyr::mutate(!!icd_code := icd_input,
                      us_time_period_pop = format(as.numeric(us_time_period_pop), big.mark = ","),
                      raw_death_rate = raw_death_rate * 100,
                      raw_death_rate_ci_low = raw_death_rate_ci_low * 100,
                      raw_death_rate_ci_hi = raw_death_rate_ci_hi * 100,
                      raw_death_rate_se = raw_death_rate_se * 100,
                      age_adj_death_rate = age_adj_death_rate * 100,
                      age_adj_death_rate_ci_low = age_adj_death_rate_ci_low * 100,
                      age_adj_death_rate_ci_hi = age_adj_death_rate_ci_hi * 100,
                      time_period_death_share = readr::parse_number(time_period_death_share))
    
    cat(paste0("Completed cleaning process for...", raw_dat, "\n"))
    return(as.data.frame(clean_dat))

}

# Applying our cleaning function to the relevant dataframes...

# First, for our overall national and state totals.
cleaned_nmr_panel_a_totals <- clean_dfs(raw_dat = raw_nmr_panel_a_totals, icd_input = "B94.8")
cleaned_nmr_panel_b_totals <- clean_dfs(raw_dat = raw_nmr_panel_b_totals, icd_input = "A81.0")

cleaned_smr_panel_a_totals <- clean_dfs(raw_dat = raw_smr_panel_a_totals, icd_input = "B94.8")
cleaned_smr_panel_b_totals <- clean_dfs(raw_dat = raw_smr_panel_b_totals, icd_input = "A81.0")

# Next, for our national totals split by gender.
cleaned_nmr_panel_a_gender <- clean_dfs(raw_dat = raw_nmr_panel_a_gender, icd_input = "B94.8")
cleaned_nmr_panel_b_gender <- clean_dfs(imp_df = raw_nmr_panel_b_gender, icd_input = "A81.0")

# ---------------------------------------------------------------- #

##### Appending dataframes. #####

# ---------------------------------------------------------------- #

cleaned_nmr_totals <- rbind(cleaned_nmr_panel_a_totals, cleaned_nmr_panel_b_totals)
cleaned_smr_totals <- rbind(cleaned_smr_panel_a_totals, cleaned_smr_panel_b_totals)
cleaned_nmr_gender <- rbind(cleaned_nmr_panel_a_gender, cleaned_nmr_panel_b_gender)

# ---------------------------------------------------------------- #

##### Exploring dataframe and saving summary statistics. #####

# ---------------------------------------------------------------- #

# First, for our overall national totals.
Hmisc::describe(cleaned_nmr_totals)
skimr::skim(cleaned_nmr_totals)
saved_x11_option <- st_options("use.x11")
st_options(use.x11 = TRUE)
view(summarytools::dfSummary(cleaned_nmr_totals))

# Exporting dataframe summary statistics to HTML output.
cleaned_nmr_totals_summ <- summarytools::dfSummary(cleaned_nmr_totals, plain.ascii = FALSE, style = "grid", 
                        graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")

cat("Completed initial data exploration for overall national totals... \n")

# Next, for our overall state totals.
Hmisc::describe(cleaned_smr_totals)
skimr::skim(cleaned_smr_totals)
saved_x11_option <- st_options("use.x11")
st_options(use.x11 = TRUE)
view(summarytools::dfSummary(cleaned_smr_totals))

# Exporting dataframe summary statistics to HTML output.
cleaned_smr_totals_summ <- summarytools::dfSummary(cleaned_smr_totals, plain.ascii = FALSE, style = "grid", 
                        graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")

cat("Completed initial data exploration for overall state totals... \n")

# Lastly, for our national totals split by gender.
Hmisc::describe(cleaned_nmr_gender)
skimr::skim(cleaned_nmr_gender)
saved_x11_option <- st_options("use.x11")
st_options(use.x11 = TRUE)
view(summarytools::dfSummary(cleaned_nmr_gender))

# Exporting dataframe summary statistics to HTML output.
cleaned_nmr_gender_summ <- summarytools::dfSummary(cleaned_nmr_gender, plain.ascii = FALSE, style = "grid", 
                        graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")

cat("Completed initial data exploration for national totals split by gender... \n")

# ---------------------------------------------------------------- #

##### Saving dataframe outputs as RDS files. #####

# ---------------------------------------------------------------- #

saveRDS(cleaned_nmr, file = "code/output/cleaned_nmr.RDS")

cat("Saved output dataframe... \n \n \n")

cat("##### CLEANING COMPLETED #####")

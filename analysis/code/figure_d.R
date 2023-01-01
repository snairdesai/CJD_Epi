# ---------------------------------------------------------------- #

##### Loading relevant libraries. #####

# _Note_: please install any packages you do not already have using
# `install.packages(<package_name>)`, or run the conda environment.

# ---------------------------------------------------------------- #

### Cleaning
library(tidyverse)

### Visualization
library(RColorBrewer)
library(patchwork)
library(gridExtra)

# ----------------------------------------------------------------------- #

##### Compiling Figure D: Age-Specific Rate of CJD Deaths for Females #####

# ----------------------------------------------------------------------- #

# Importing dataframe.
cleaned_nmr_gender_specific <- readRDS("input/cleaned_nmr_gender_specific.RDS")
cat("Age-specific national mortality rates by gender have been loaded... \n")

# Rearranging data for figure.
fig_d_dat_females <- cleaned_nmr_gender_specific %>%
    dplyr::select(time_period, age_specific_death_rate, gender, age_groups) %>%
    dplyr::filter(time_period != "1999-2006" &
                  time_period != "2007 - 2020" &
                  gender == "Female" &
                  age_groups != "All years" &
                  # Too many missing values in the following age cohorts...
                  age_groups != "25-34 years" &
                  age_groups != "35-44 years") %>%
    dplyr::rename(`Gender` = gender, `Age Groups` = age_groups) %>%
    dplyr::mutate(`Year` = as.Date(ISOdate(time_period, 1, 1))) %>%
    tidyr::drop_na()

head(fig_d_dat_females)

cat("Figure D data has been compiled... \n")

# Plotting line plot of relevant statistic.
female_age_specific_deaths_plot <- fig_d_dat_females %>%
  ggplot(aes(x = `Year`, y = age_specific_death_rate,
             group = `Age Groups`, color = `Age Groups`)) +
    geom_line(size = 1.25) +
    scale_color_manual(values = c("#D53E4F", "#FC8D59",
                                  "#FEE08B", "#99D594",
                                  "#3288BD")) +
    theme_classic() +
    labs(x = "Year", y = "Female Death Rate by Age Group") +
    geom_vline(xintercept = as.Date(ISOdate(2007, 1, 1)), linetype = "dotted",
               color = "black", size = 1.5) +
   theme(legend.position = "bottom") +
   annotate(x = as.Date(ISOdate(2007, 1, 1)), y = 425,
        label = "Switch in ICD Codes", vjust = 2, geom = "label")

ggsave("output/female_age_specific_deaths_plot.eps")
ggsave("output/female_age_specific_deaths_plot.pdf")

cat("Figure D has been mapped and exported... \n \n \n")

cat("##### FIGURE D COMPILATION COMPLETED ##### \n \n \n")

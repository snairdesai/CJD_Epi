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

# --------------------------------------------------------------------- #

##### Compiling Figure C: Age-Specific Rate of CJD Deaths for Males #####

# --------------------------------------------------------------------- #

# Importing dataframe.
cleaned_nmr_gender_specific <- readRDS("input/cleaned_nmr_gender_specific.RDS")
cat("Age-specific national mortality rates by gender have been loaded... \n")

# Rearranging data for figure.
fig_c_dat_males <- cleaned_nmr_gender_specific %>%
    dplyr::select(time_period, age_specific_death_rate, gender, age_groups) %>%
    dplyr::filter(time_period != "2007 - 2020" &
                  gender == "Male" &
                  age_groups != "All years" &
                  # Too many missing values in the following age cohorts...
                  age_groups != "25-34 years" &
                  age_groups != "35-44 years" &
                  age_groups != "45-54 years" &
                  age_groups != "85+ years") %>%
    dplyr::rename(`Gender` = gender, `Age Groups` = age_groups) %>%
    dplyr::mutate(`Year` = as.Date(ISOdate(time_period, 1, 1))) %>%
    tidyr::drop_na()

fig_c_dat_males$`Age Groups` <- ifelse(
                                fig_c_dat_males$`Age Groups` == "75-84 years",
                                "75+ years",
                                fig_c_dat_males$`Age Groups`)

head(fig_c_dat_males)

cat("Figure C data has been compiled... \n")

# Plotting line plot of relevant statistic.
male_age_specific_deaths_plot <- fig_c_dat_males %>%
  ggplot(aes(x = `Year`, y = age_specific_death_rate,
             group = `Age Groups`, color = `Age Groups`)) +
    geom_line(size = 1.25) +
    scale_color_manual(values = c("#D53E4F", "#FC8D59",
                                  "#FEE08B", "#99D594",
                                  "#3288BD")) +
    theme_classic() +
    labs(x = "Year", y = "Male Death Rate by Age Group") +
    theme(legend.position = "bottom")

ggsave("output/male_age_specific_deaths_plot.eps")
ggsave("output/male_age_specific_deaths_plot.pdf")

cat("Figure C has been mapped and exported... \n \n \n")

cat("##### FIGURE C COMPILATION COMPLETED ##### \n \n \n")

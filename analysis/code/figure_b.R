# ---------------------------------------------------------------- #

##### Loading relevant libraries. #####

# _Note_: please install any packages you do not already have using
# `install.packages(<package_name>)`, or run the conda environment.

# ---------------------------------------------------------------- #

### Cleaning
library(tidyverse)

### Visualization
library(patchwork)
library(gridExtra)

# --------------------------------------------------------------------- #

##### Compiling Figure B: Age-Adjusted Rate of CJD Deaths by Gender #####

# --------------------------------------------------------------------- #

# Importing dataframe.
cleaned_nmr_gender <- readRDS("input/cleaned_nmr_gender.RDS")
cat("National mortality rates by gender have been loaded... \n")
head(cleaned_nmr_gender)

# Rearranging data for figure.
fig_b_dat <- cleaned_nmr_gender %>%
    dplyr::select(time_period, age_adj_death_rate, gender) %>%
    dplyr::filter(time_period != "1999-2006" &
                  time_period != "2007 - 2020" &
                  gender != "Both") %>%
    dplyr::rename(`Gender` = gender) %>%
    dplyr::mutate(`Year` = as.Date(ISOdate(time_period, 1, 1)))

cat("Figure A data has been compiled... \n")

# Plotting line plot of relevant statistic.
raw_deaths_by_gender_plot <- fig_b_dat %>%
  ggplot(aes(x = `Year`, y = age_adj_death_rate,
             group = `Gender`, color = `Gender`)) +
    geom_line(aes(linetype = `Gender`), size = 2) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    scale_color_manual(values = c("brown2", "dodgerblue2")) +
    theme_classic() +
    labs(x = "Year", y = "Age-Adjusted Death Rate") +
    geom_vline(xintercept = as.Date(ISOdate(2007, 1, 1)), linetype = "dotted",
               color = "black", size = 1.5) +
   theme(legend.position = "bottom") +
   annotate(x = as.Date(ISOdate(2007, 1, 1)), y = 30,
        label = "Switch in ICD Codes", vjust = 2, geom = "label")

ggsave("output/age_adj_deaths_by_gender_plot.eps")
ggsave("output/age_adj_deaths_by_gender_plot.pdf")

cat("Figure B has been mapped and exported... \n \n \n")

cat("##### FIGURE B COMPILATION COMPLETED ##### \n \n \n")
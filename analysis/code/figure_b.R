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

# Rearranging data for figure.
fig_b_dat <- cleaned_nmr_gender %>%
    dplyr::select(time_period, age_adj_death_rate, gender) %>%
    dplyr::filter(time_period != "2007 - 2020" & gender != "Both" & gender != "") %>%
    dplyr::rename(`Gender` = gender) %>%
    dplyr::mutate(`Year` = as.Date(ISOdate(time_period, 1, 1)))

cat("Figure B data has been compiled... \n")

# Plotting line plot of relevant statistic.
raw_deaths_by_gender_plot <- fig_b_dat %>%
  ggplot(aes(x = `Year`, y = age_adj_death_rate,
             group = `Gender`, color = `Gender`)) +
    geom_line(aes(linetype = `Gender`), size = 2) +
    scale_linetype_manual(values = c("solid", "solid")) +
    scale_color_manual(values = c("brown2", "dodgerblue2")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_classic() +
    labs(x = "Year", y = "Age-Adjusted Death Rate") +
    theme(legend.position = "bottom", text = element_text(size = 18),
    legend.background = element_rect(color = "black", fill = NA, size = 1))
    # expand_limits(y = 0)

ggsave("output/age_adj_deaths_by_gender_plot.eps", width = 14, height = 8)
ggsave("output/age_adj_deaths_by_gender_plot.pdf", width = 14, height = 8)

cat("Figure B has been mapped and exported... \n \n \n")

# RDS format.
saveRDS(fig_b_dat, file = "output/ageadj_deaths_gender.RDS")

# CSV format.
write.csv(fig_b_dat, "output/ageadj_deaths_gender.csv", row.names = FALSE)

cat("##### FIGURE B COMPILATION COMPLETED ##### \n \n \n")

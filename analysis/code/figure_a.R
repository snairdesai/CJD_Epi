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

# ---------------------------------------------------------------- #

##### Compiling Figure A: Raw Number of CJD Deaths by Gender #####

# ---------------------------------------------------------------- #

# Importing dataframe.
cleaned_nmr_gender <- readRDS("input/cleaned_nmr_gender.RDS")
cat("National mortality rates by gender have been loaded... \n")
head(cleaned_nmr_gender)

# Rearranging data for figure.
fig_a_dat <- cleaned_nmr_gender %>%
    dplyr::select(time_period, raw_death_count, gender) %>%
    dplyr::filter(time_period != "2007 - 2020" &
                  gender != "Both") %>%
    dplyr::rename(`Gender` = gender) %>%
    dplyr::mutate(`Year` = as.Date(ISOdate(time_period, 1, 1)))

cat("Figure A data has been compiled... \n")

# Plotting line plot of relevant statistic.
raw_deaths_by_gender_plot <- fig_a_dat %>%
  ggplot(aes(x = `Year`, y = raw_death_count,
             group = `Gender`, color = `Gender`)) +
    geom_line(aes(linetype = `Gender`), size = 2) +
    scale_linetype_manual(values = c("solid", "solid")) +
    scale_color_manual(values = c("brown2", "dodgerblue2")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_classic() +
    labs(x = "Year", y = "Raw Death Count") +
    theme(legend.position = "bottom", text = element_text(size = 18),
    legend.background = element_rect(color = "black", fill = NA, size = 1))
    # expand_limits(y = 0)

ggsave("output/raw_deaths_by_gender_plot.eps", width = 14, height = 8)
ggsave("output/raw_deaths_by_gender_plot.pdf", width = 14, height = 8)

cat("Figure A has been mapped and exported... \n \n \n")

# RDS format.
saveRDS(fig_a_dat, file = "output/raw_deaths_gender.RDS")

# CSV format.
write.csv(fig_a_dat, "output/raw_deaths_gender.csv", row.names = FALSE)

cat("##### FIGURE A COMPILATION COMPLETED ##### \n \n \n")

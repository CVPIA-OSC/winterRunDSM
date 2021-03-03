library(tidyverse)

adult_seeds <- matrix(0, nrow = 31, ncol = 30)
adult_seeds[ 1, 1] <- 2786.6

rownames(adult_seeds) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(adult_seeds, overwrite = TRUE)

#TODO check on why in OG model they only have a single number not a vector of values
#line 1552 OG model
proportion_hatchery <- c(0.1759966, rep(0, 30)) #proportion hatchery based on CWT reports
names(proportion_hatchery) <- DSMhabitat::watershed_metadata$watershed[-32]

usethis::use_data(proportion_hatchery, overwrite = TRUE)

# @title Proportion of Adults Spawning January to April
# @export
# month_return_proportions differs based on run
# Proportion adults spawners (including hatchery fish) across 4 months (January-April)
month_return_proportions <- c(0.125, 0.375, 0.375, 0.125)
names(month_return_proportions) <- month.abb[1:4]
usethis::use_data(month_return_proportions, overwrite = TRUE)

# Mass by size class
mass_by_size_class <- c(0.5, 1.8, 9.1, 31.4)
names(mass_by_size_class) <- c("s", "m", "l", "vl")
usethis::use_data(mass_by_size_class, overwrite = TRUE)

# Do not use straying in the original WR model
# cross_channel_stray_rate <- c(rep(1, 15), 0, 0, 2, 2, 2, 0, 0, 3, 0, rep(0, 7)) / 24
# names(cross_channel_stray_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
# usethis::use_data(cross_channel_stray_rate, overwrite = TRUE)
#
# stray_rate <- c(rep(1, 15), 0, 0, 1, 1, 1, 0, 0, 1, 0, rep(1, 6), 0) / 25
# names(stray_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
# usethis::use_data(stray_rate, overwrite = TRUE)

# differs based on run ------
adult_harvest_rate <- c(0.2, rep(0, 30)) # from Corey Phillis
names(adult_harvest_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(adult_harvest_rate, overwrite = TRUE)

natural_adult_removal_rate <- c(mean(c(0.18,0.09,0.07,0.13,0.02,0.03)), rep(0, 30)) # from Doug Killam 2012 - 2017 data  # differs based on run
names(natural_adult_removal_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(natural_adult_removal_rate, overwrite = TRUE)

#TODO fix this one for WR
hatchery_allocation <- c(1, rep(0, 30)) # differs based on run
names(hatchery_allocation) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(hatchery_allocation, overwrite = TRUE)


original_groups <- read_csv("data-raw/misc/Grouping.csv")

diversity_group <- original_groups$DiversityGroup
names(diversity_group) <- original_groups$watershed
usethis::use_data(diversity_group, overwrite = TRUE)



# Read in Baseling_2019.rds to get inpts from OG model 
baseline_2019 <- readRDS("data-raw/baseline_2019.rds")
usethis::use_data(baseline_2019, overwrite = TRUE)

# Mean egg temp effect different in winter run 
# TODO update in DSMtemperature 
mean_egg_temp_effect <- rep(0.6466230, 31)
names(mean_egg_temp_effect) <- watershed_attributes$watershed
usethis::use_data(mean_egg_temp_effect, overwrite = TRUE)


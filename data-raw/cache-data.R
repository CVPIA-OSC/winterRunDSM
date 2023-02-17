library(tidyverse)

watershed_labels <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                      "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                      "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                      "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                      "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                      "Feather River", "Yuba River", "Lower-mid Sacramento River",
                      "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                      "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                      "Tuolumne River", "San Joaquin River")

usethis::use_data(watershed_labels, overwrite = TRUE)


adult_seeds <- matrix(0, nrow = 31, ncol = 30)
no_wr_spawn <- !as.logical(DSMhabitat::watershed_species_present[1:31, ]$wr *
                             DSMhabitat::watershed_species_present[1:31,]$spawn)

adult_seed_values <- DSMCalibrationData::mean_escapement_2013_2017 %>%
  bind_cols(no_wr_spawn = no_wr_spawn) %>%
  select(watershed, Winter, no_wr_spawn) %>%
  mutate(corrected_winter = case_when(
    no_wr_spawn ~ 0,
    is.na(Winter) | Winter < 10 ~ 12,
    TRUE ~ Winter)
  ) %>% pull(corrected_winter)

rownames(adult_seeds) <- watershed_labels

adult_seeds[ , 1] <- adult_seed_values
adult_seeds["Battle Creek", 1] <- 1200

usethis::use_data(adult_seeds, overwrite = TRUE)

rownames(adult_seeds) <- watershed_labels
usethis::use_data(adult_seeds, overwrite = TRUE)

#TODO check on why in OG model they only have a single number not a vector of values
#line 1552 OG model
proportion_hatchery <- rep(0, 31) #proportion hatchery based on CWT reports
names(proportion_hatchery) <- watershed_labels
proportion_hatchery["Upper Sacramento River"] <- 0.1759966
proportion_hatchery["Battle Creek"] <- .2

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

# differs based on run ------
adult_harvest_rate <- c(0.2, rep(0, 30)) # from Corey Phillis
names(adult_harvest_rate) <- watershed_labels
usethis::use_data(adult_harvest_rate, overwrite = TRUE)

natural_adult_removal_rate <- c(mean(c(0.18,0.09,0.07,0.13,0.02,0.03)), rep(0, 30)) # from Doug Killam 2012 - 2017 data  # differs based on run
names(natural_adult_removal_rate) <- watershed_labels
usethis::use_data(natural_adult_removal_rate, overwrite = TRUE)

#TODO fix this one for WR
hatchery_allocation <- c(1, rep(0, 30)) # differs based on run
names(hatchery_allocation) <- watershed_labels
usethis::use_data(hatchery_allocation, overwrite = TRUE)


original_groups <- read_csv("data-raw/misc/Grouping.csv")
diversity_group <- original_groups$diversity_group
names(diversity_group) <- original_groups$watershed
usethis::use_data(diversity_group, overwrite = TRUE)

size_class_labels <- c('s', 'm', 'l', 'vl')

usethis::use_data(size_class_labels, overwrite = TRUE)

# calculate growth rates
growth_rates_inchannel <- growth()
usethis::use_data(growth_rates_inchannel, overwrite = TRUE)
growth_rates_floodplain <- growth_floodplain()
usethis::use_data(growth_rates_floodplain, overwrite = TRUE)


# cache new growth rates
bioenergetics_transitions <- read_rds("data-raw/growTPM.rds")
usethis::use_data(bioenergetics_transitions, overwrite = TRUE)

prey_density <- rep("hi", 31) # NOTE this is to drive the new prey density dependent growth
usethis::use_data(prey_density, overwrite = TRUE)


# should be moved to a data package?
prey_density_delta <- c("med", "med")
usethis::use_data(prey_density_delta, overwrite = TRUE)

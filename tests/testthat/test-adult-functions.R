library(testthat)
library(winterRunDSM)
# tests for adult functions
# Lists inputs to use in testing
list2env(load_baseline_data(), envir = .GlobalEnv)
year <- 1
month <- 9
bypass_is_overtopped <- as.logical(tisdale_bypass_watershed + yolo_bypass_watershed)
avg_migratory_temp <- rowMeans(migratory_temperature_proportion_over_20[ , 10:12])
accumulated_degree_days <- cbind(jan = rowSums(degree_days[ , 1:4, year]),
                                 feb = rowSums(degree_days[ , 2:4, year]),
                                 march = rowSums(degree_days[ , 3:4, year]),
                                 april = degree_days[ , 4, year])

average_degree_days <- apply(accumulated_degree_days, 1, weighted.mean, month_return_proportions)

# Tests adult straying function
expected_straying_output <- c(`Upper Sacramento River` = 0.0179218144440285, `Antelope Creek` = 0.0740104504838898,
                              `Battle Creek` = 0.0678754718644023, `Bear Creek` = 0.0755029971698723,
                              `Big Chico Creek` = 0.0748474212008828, `Butte Creek` = 0.0730690238859734,
                              `Clear Creek` = 0.0709270693771585, `Cottonwood Creek` = 0.0709270693771585,
                              `Cow Creek` = 0.0704102088441598, `Deer Creek` = 0.0722674057922913,
                              `Elder Creek` = 0.0755511108365283, `Mill Creek` = 0.0717077263560781,
                              `Paynes Creek` = 0.0755029971698723, `Stony Creek` = 0.0756043218542417,
                              `Thomes Creek` = 0.0739657235650295, `Upper-mid Sacramento River` = 0.0110961379974576,
                              `Sutter Bypass` = 0.0758581800212435, `Bear River` = 0.0736293343892286,
                              `Feather River` = 0.0409341987251872, `Yuba River` = 0.0526037516322295,
                              `Lower-mid Sacramento River` = 0.0110961379974576, `Yolo Bypass` = 0.0758581800212435,
                              `American River` = 0.0579554510917732, `Lower Sacramento River` = 0.0110961379974576,
                              `Calaveras River` = 0.0110961379974576, `Cosumnes River` = 0.0110961379974576,
                              `Mokelumne River` = 0.0110961379974576, `Merced River` = 0.0429069359657317,
                              `Stanislaus River` = 0.0293554482561996, `Tuolumne River` = 0.0468862825202343,
                              `San Joaquin River` = 0.0110961379974576)

test_that('The straying function returns the expected values for year 1', {
  expect_equal(adult_stray(wild = 1,
                           natal_flow = prop_flow_natal[ , year],
                           south_delta_watershed = south_delta_routed_watersheds,
                           cross_channel_gates_closed = cc_gates_days_closed[10]),
               expected_straying_output)
})

#tests adult surv_en_route
expected_surv_en_route <- c(`Upper Sacramento River` = 0.770142230517313, `Antelope Creek` = 0.970142230517313, 
                            `Battle Creek` = 0.970142230517313, `Bear Creek` = 0.970142230517313, 
                            `Big Chico Creek` = 0.970142230517313, `Butte Creek` = 0.970142230517313, 
                            `Clear Creek` = 0.970142230517313, `Cottonwood Creek` = 0.970142230517313, 
                            `Cow Creek` = 0.970142230517313, `Deer Creek` = 0.970142230517313, 
                            `Elder Creek` = 0.970142230517313, `Mill Creek` = 0.970142230517313, 
                            `Paynes Creek` = 0.970142230517313, `Stony Creek` = 0.970142230517313, 
                            `Thomes Creek` = 0.970142230517313, `Upper-mid Sacramento River` = 0.970142230517313, 
                            `Sutter Bypass` = 0.970687700961309, `Bear River` = 0.970142230517313, 
                            `Feather River` = 0.970142230517313, `Yuba River` = 0.970142230517313, 
                            `Lower-mid Sacramento River` = 0.970687700961309, `Yolo Bypass` = 0.970687700961309, 
                            `American River` = 0.970687700961309, `Lower Sacramento River` = 0.970687700961309, 
                            `Calaveras River` = 0.970528189037085, `Cosumnes River` = 0.970528189037085, 
                            `Mokelumne River` = 0.970528189037085, `Merced River` = 0.970408003063303, 
                            `Stanislaus River` = 0.970408003063303, `Tuolumne River` = 0.970408003063303, 
                            `San Joaquin River` = 0.970687700961309)

test_that('The adult enroute survival function returns the expected values for year 1', {
  expect_equal(surv_adult_enroute(migratory_temp = avg_migratory_temp,
                                  bypass_overtopped = bypass_is_overtopped,
                                  adult_harvest = adult_harvest_rate),
               expected_surv_en_route)
})

# Tests prespawn survival
expected_prespawn_surv <- c(`Upper Sacramento River` = 0.922, `Antelope Creek` = 0.924, 
                            `Battle Creek` = 0.919, `Bear Creek` = 0.912, `Big Chico Creek` = 0.927, 
                            `Butte Creek` = 0.928, `Clear Creek` = 0.932, `Cottonwood Creek` = 0.923, 
                            `Cow Creek` = 0.922, `Deer Creek` = 0.925, `Elder Creek` = 0.922, 
                            `Mill Creek` = 0.923, `Paynes Creek` = 0.922, `Stony Creek` = 0.925, 
                            `Thomes Creek` = 0.924, `Upper-mid Sacramento River` = 0.953, 
                            `Sutter Bypass` = 0.953, `Bear River` = 0.928, `Feather River` = 0.938, 
                            `Yuba River` = 0.918, `Lower-mid Sacramento River` = 0.953, `Yolo Bypass` = 0.953, 
                            `American River` = 0.925, `Lower Sacramento River` = 0.953, `Calaveras River` = 0.924, 
                            `Cosumnes River` = 0.919, `Mokelumne River` = 0.918, `Merced River` = 0.916, 
                            `Stanislaus River` = 0.923, `Tuolumne River` = 0.921, `San Joaquin River` = 0.953
)
test_that('The prespawn survival function returns the expected values for year 1', {
  expect_equal(round(surv_adult_prespawn(average_degree_days), 3),
               expected_prespawn_surv)
})

# Tests egg to fry surv
expected_egg_surv <- c(`Upper Sacramento River` = 0.381246067692528, `Antelope Creek` = 0.409716513722015, 
                       `Battle Creek` = 0.409716513722015, `Bear Creek` = 0.409716513722015, 
                       `Big Chico Creek` = 0.410698870109287, `Butte Creek` = 0.410698870109287, 
                       `Clear Creek` = 0.40873243623163, `Cottonwood Creek` = 0.412658353683433, 
                       `Cow Creek` = 0.409716513722015, `Deer Creek` = 0.40873243623163, 
                       `Elder Creek` = 0.413635448257544, `Mill Creek` = 0.410698870109287, 
                       `Paynes Creek` = 0.409716513722015, `Stony Creek` = 0.40873243623163, 
                       `Thomes Creek` = 0.410698870109287, `Upper-mid Sacramento River` = 0.395790554477812, 
                       `Sutter Bypass` = 0.413635448257544, `Bear River` = 0.409716513722015, 
                       `Feather River` = 0.393776360769257, `Yuba River` = 0.411679488887492, 
                       `Lower-mid Sacramento River` = 0.412658353683433, `Yolo Bypass` = 0.413635448257544, 
                       `American River` = 0.412658353683433, `Lower Sacramento River` = 0.412658353683433, 
                       `Calaveras River` = 0.40873243623163, `Cosumnes River` = 0.411679488887492, 
                       `Mokelumne River` = 0.409716513722015, `Merced River` = 0.409716513722015, 
                       `Stanislaus River` = 0.409716513722015, `Tuolumne River` = 0.40873243623163, 
                       `San Joaquin River` = 0.412658353683433)

test_that('The egg_to_fry survival function returns the expected values for year 1', {
  expect_equal(surv_egg_to_fry(proportion_natural = 1 - proportion_hatchery,
                               scour = prob_nest_scoured,
                               temperature_effect = rep(0.6466230, 31)),
               expected_egg_surv)
})

# Test get_spawning_adults
adults <- structure(c(22012, 72, 12626, 12, 12, 885, 8555, 1251, 1649,
                      569, 12, 1332, 51, 12, 12, 0, 0, 12, 52408, 7184, 0, 0, 24959,
                      0, 12, 499, 4514, 2145, 5405, 984, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = 31:30, .Dimnames = list(c("Upper Sacramento River",
                                                                                   "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek",
                                                                                   "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek",
                                                                                   "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek",
                                                                                   "Thomes Creek", "Upper-mid Sacramento River", "Sutter Bypass",
                                                                                   "Bear River", "Feather River", "Yuba River", "Lower-mid Sacramento River",
                                                                                   "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                                                                                   "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                                                                                   "Tuolumne River", "San Joaquin River"), NULL))
hatch_adults <- c(`Upper Sacramento River` = 6926L, `Antelope Creek` = 13L, `Battle Creek` = 16007L,
                  `Bear Creek` = 15L, `Big Chico Creek` = 18L, `Butte Creek` = 80L,
                  `Clear Creek` = 1082L, `Cottonwood Creek` = 914L, `Cow Creek` = 274L,
                  `Deer Creek` = 132L, `Elder Creek` = 15L, `Mill Creek` = 66L,
                  `Paynes Creek` = 25L, `Stony Creek` = 16L, `Thomes Creek` = 13L,
                  `Upper-mid Sacramento River` = 0L, `Sutter Bypass` = 0L, `Bear River` = 21L,
                  `Feather River` = 50417L, `Yuba River` = 8136L, `Lower-mid Sacramento River` = 0L,
                  `Yolo Bypass` = 0L, `American River` = 14083L, `Lower Sacramento River` = 0L,
                  `Calaveras River` = 28L, `Cosumnes River` = 14L, `Mokelumne River` = 2926L,
                  `Merced River` = 1402L, `Stanislaus River` = 1506L, `Tuolumne River` = 475L,
                  `San Joaquin River` = 0L)
seeds <- NULL

expected_spawners <- list(init_adults = c(20071, 72, 12626, 12, 12, 885, 8555, 1251, 
                                          1649, 569, 12, 1332, 51, 12, 12, 0, 0, 12, 52408, 7184, 0, 0, 
                                          24959, 0, 12, 499, 4514, 2145, 5405, 984, 0), 
                          proportion_natural = c(`Upper Sacramento River` = 0.8240034, `Antelope Creek` = 1, `Battle Creek` = 1, `Bear Creek` = 1, `Big Chico Creek` = 1, 
                                                 `Butte Creek` = 1, `Clear Creek` = 1, `Cottonwood Creek` = 1, 
                                                 `Cow Creek` = 1, `Deer Creek` = 1, `Elder Creek` = 1, `Mill Creek` = 1, 
                                                 `Paynes Creek` = 1, `Stony Creek` = 1, `Thomes Creek` = 1, `Upper-mid Sacramento River` = 1, 
                                                 `Sutter Bypass` = 1, `Bear River` = 1, `Feather River` = 1, `Yuba River` = 1, 
                                                 `Lower-mid Sacramento River` = 1, `Yolo Bypass` = 1, `American River` = 1, 
                                                 `Lower Sacramento River` = 1, `Calaveras River` = 1, `Cosumnes River` = 1, 
                                                 `Mokelumne River` = 1, `Merced River` = 1, `Stanislaus River` = 1, 
                                                 `Tuolumne River` = 1, `San Joaquin River` = 1), 
                          natural_adults = c(22012,  72, 12626, 12, 12, 885, 8555, 1251, 1649, 569, 12, 1332, 51, 12, 12, 0, 0, 12, 52408, 7184, 0, 0, 24959, 0, 12, 499, 4514,  2145, 5405, 984, 0), 
                          init_adults_by_month = structure(c(2487L, 
                                                               10L, 1574L, 4L, 2L, 119L, 1030L, 169L, 185L, 70L, 1L, 154L, 3L, 
                                                               1L, 2L, 0L, 0L, 1L, 6595L, 876L, 0L, 0L, 3134L, 0L, 1L, 59L, 
                                                               544L, 273L, 659L, 129L, 0L, 7629L, 24L, 4718L, 3L, 6L, 300L, 
                                                               3169L, 463L, 642L, 211L, 6L, 517L, 26L, 5L, 8L, 0L, 0L, 8L, 19873L, 
                                                               2654L, 0L, 0L, 9216L, 0L, 4L, 202L, 1696L, 784L, 2092L, 358L, 
                                                               0L, 7437L, 26L, 4707L, 4L, 2L, 333L, 3241L, 456L, 621L, 227L, 
                                                               5L, 508L, 19L, 5L, 2L, 0L, 0L, 2L, 19417L, 2734L, 0L, 0L, 9409L, 
                                                               0L, 6L, 174L, 1697L, 819L, 1981L, 357L, 0L, 2518L, 12L, 1627L, 
                                                               1L, 2L, 133L, 1115L, 163L, 201L, 61L, 0L, 153L, 3L, 1L, 0L, 0L, 0L, 1L, 6523L, 920L, 0L, 0L, 3200L, 0L, 1L, 64L, 577L, 269L, 673L, 140L, 0L), .Dim = c(31L, 4L)))
test_that("Get spawning adults returns the expected values", {
  
  set.seed(2021)
  spawning_adults <- get_spawning_adults(year = year, adults = adults, hatch_adults = hatch_adults, seeds = seeds)
  expect_equal(spawning_adults, expected_spawners)
})

# Tests spawn success function
init_adults <- expected_spawners$init_adults
min_spawn_habitat <- apply(spawning_habitat[ , 10:12, year], 1, min)

expected_juveniles <- structure(c(15972966, 0, 1421639, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(31L, 4L), .Dimnames = list(
                                    c("Upper Sacramento River", "Antelope Creek", "Battle Creek", 
                                      "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek", 
                                      "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek", 
                                      "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek", 
                                      "Upper-mid Sacramento River", "Sutter Bypass", "Bear River", 
                                      "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                                      "Yolo Bypass", "American River", "Lower Sacramento River", 
                                      "Calaveras River", "Cosumnes River", "Mokelumne River", "Merced River", 
                                      "Stanislaus River", "Tuolumne River", "San Joaquin River"
                                    ), c("fry", "", "", "")))

test_that("spawn success function returns the expected value", {
  juveniles <- spawn_success(escapement = init_adults,
                             adult_prespawn_survival = expected_prespawn_surv,
                             egg_to_fry_survival = expected_egg_surv,
                             prob_scour = prob_nest_scoured,
                             spawn_habitat = min_spawn_habitat)
  expect_equal(round(juveniles), round(expected_juveniles))
})


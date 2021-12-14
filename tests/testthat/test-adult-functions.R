library(testthat)
library(winterRunDSM)

# Model state ------------ 
year <- 1
month <- 9


bypass_is_overtopped <- sapply(1:4, function(month) {
  # sheds connected to tisdale and sutter gates overtopped
  tis <- params$gates_overtopped[month, year, 1] * params$tisdale_bypass_watershed
  # sheds connected to yolo and yolo bypass overtopped
  yolo <- params$gates_overtopped[month, year, 2] * params$yolo_bypass_watershed
  
  as.logical(tis + yolo)
})

avg_migratory_temp <- rowMeans(params$migratory_temperature_proportion_over_20[ , 10:12])
accumulated_degree_days <- cbind(jan = rowSums(params$degree_days[ , 1:4, year]),
                                 feb = rowSums(params$degree_days[ , 2:4, year]),
                                 march = rowSums(params$degree_days[ , 3:4, year]),
                                 april = params$degree_days[ , 4, year])

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
                           natal_flow = params$prop_flow_natal[ , year],
                           south_delta_watershed = params$south_delta_routed_watersheds,
                           cross_channel_gates_closed = params$cc_gates_days_closed[10]),
               expected_straying_output)
})

#tests adult surv_en_route
expected_surv_en_route <- c(`Upper Sacramento River` = 0.609393012443221, `Antelope Creek` = 0.809393012443221, 
                            `Battle Creek` = 0.809393012443221, `Bear Creek` = 0.809393012443221, 
                            `Big Chico Creek` = 0.809393012443221, `Butte Creek` = 0.809393012443221, 
                            `Clear Creek` = 0.809393012443221, `Cottonwood Creek` = 0.809393012443221, 
                            `Cow Creek` = 0.809393012443221, `Deer Creek` = 0.809393012443221, 
                            `Elder Creek` = 0.809393012443221, `Mill Creek` = 0.809393012443221, 
                            `Paynes Creek` = 0.809393012443221, `Stony Creek` = 0.809393012443221, 
                            `Thomes Creek` = 0.809393012443221, `Upper-mid Sacramento River` = 0.809393012443221, 
                            `Sutter Bypass` = 0.812307038089034, `Bear River` = 0.809393012443221, 
                            `Feather River` = 0.809393012443221, `Yuba River` = 0.809393012443221, 
                            `Lower-mid Sacramento River` = 0.812307038089034, `Yolo Bypass` = 0.812307038089034, 
                            `American River` = 0.812307038089034, `Lower Sacramento River` = 0.812307038089034, 
                            `Calaveras River` = 0.812307038089034, `Cosumnes River` = 0.812307038089034, 
                            `Mokelumne River` = 0.812307038089034, `Merced River` = 0.812307038089034, 
                            `Stanislaus River` = 0.812307038089034, `Tuolumne River` = 0.812307038089034, 
                            `San Joaquin River` = 0.812307038089034)

test_that('The adult enroute survival function returns the expected values for year 1', {
  en_route_temps <- params$migratory_temperature_proportion_over_20[, 1:4]
  expect_equal(surv_adult_enroute(migratory_temp = en_route_temps[, 1],
                                  bypass_overtopped = bypass_is_overtopped[, 1],
                                  adult_harvest = params$.adult_en_route_adult_harvest_rate),
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
expected_egg_surv <- c(`Upper Sacramento River` = 0.381, `Antelope Creek` = 0.41, 
                       `Battle Creek` = 0.393, `Bear Creek` = 0.41, `Big Chico Creek` = 0.411, 
                       `Butte Creek` = 0.411, `Clear Creek` = 0.409, `Cottonwood Creek` = 0.413, 
                       `Cow Creek` = 0.41, `Deer Creek` = 0.409, `Elder Creek` = 0.414, 
                       `Mill Creek` = 0.411, `Paynes Creek` = 0.41, `Stony Creek` = 0.409, 
                       `Thomes Creek` = 0.411, `Upper-mid Sacramento River` = 0.396, 
                       `Sutter Bypass` = 0.414, `Bear River` = 0.41, `Feather River` = 0.394, 
                       `Yuba River` = 0.412, `Lower-mid Sacramento River` = 0.413, `Yolo Bypass` = 0.414, 
                       `American River` = 0.413, `Lower Sacramento River` = 0.413, `Calaveras River` = 0.409, 
                       `Cosumnes River` = 0.412, `Mokelumne River` = 0.41, `Merced River` = 0.41, 
                       `Stanislaus River` = 0.41, `Tuolumne River` = 0.409, `San Joaquin River` = 0.413
)

test_that('The egg_to_fry survival function returns the expected values for year 1', {
  expect_equal(round(surv_egg_to_fry(proportion_natural = 1 - params$proportion_hatchery,
                                     scour = params$prob_nest_scoured), 3),
               expected_egg_surv)
})

# Test get_spawning_adults

# seed mode
adults <- structure(c(2787, 0, 1200, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
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
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = 31:30, .Dimnames = list(
                        c("Upper Sacramento River", "Antelope Creek", "Battle Creek", 
                          "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek", 
                          "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek", 
                          "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek", 
                          "Upper-mid Sacramento River", "Sutter Bypass", "Bear River", 
                          "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                          "Yolo Bypass", "American River", "Lower Sacramento River", 
                          "Calaveras River", "Cosumnes River", "Mokelumne River", "Merced River", 
                          "Stanislaus River", "Tuolumne River", "San Joaquin River"
                        ), NULL))

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

expected_spawners <- list(
  init_adults = c(`Upper Sacramento River` = 2544, `Antelope Creek` = 0, 
                  `Battle Creek` = 1200, `Bear Creek` = 0, `Big Chico Creek` = 0, 
                  `Butte Creek` = 0, `Clear Creek` = 0, `Cottonwood Creek` = 0, 
                  `Cow Creek` = 0, `Deer Creek` = 0, `Elder Creek` = 0, `Mill Creek` = 0, 
                  `Paynes Creek` = 0, `Stony Creek` = 0, `Thomes Creek` = 0, `Upper-mid Sacramento River` = 0, 
                  `Sutter Bypass` = 0, `Bear River` = 0, `Feather River` = 0, `Yuba River` = 0, 
                  `Lower-mid Sacramento River` = 0, `Yolo Bypass` = 0, `American River` = 0, 
                  `Lower Sacramento River` = 0, `Calaveras River` = 0, `Cosumnes River` = 0, 
                  `Mokelumne River` = 0, `Merced River` = 0, `Stanislaus River` = 0, 
                  `Tuolumne River` = 0, `San Joaquin River` = 0), 
  proportion_natural = c(`Upper Sacramento River` = 0.8240034, 
                         `Antelope Creek` = 1, `Battle Creek` = 0.8, `Bear Creek` = 1, 
                         `Big Chico Creek` = 1, `Butte Creek` = 1, `Clear Creek` = 1, 
                         `Cottonwood Creek` = 1, `Cow Creek` = 1, `Deer Creek` = 1, `Elder Creek` = 1, 
                         `Mill Creek` = 1, `Paynes Creek` = 1, `Stony Creek` = 1, `Thomes Creek` = 1, 
                         `Upper-mid Sacramento River` = 1, `Sutter Bypass` = 1, `Bear River` = 1, 
                         `Feather River` = 1, `Yuba River` = 1, `Lower-mid Sacramento River` = 1, 
                         `Yolo Bypass` = 1, `American River` = 1, `Lower Sacramento River` = 1, 
                         `Calaveras River` = 1, `Cosumnes River` = 1, `Mokelumne River` = 1, 
                         `Merced River` = 1, `Stanislaus River` = 1, `Tuolumne River` = 1, 
                         `San Joaquin River` = 1), 
  init_adults_by_month = structure(c(318, 
                                     0, 150, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 954, 0, 450, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 954, 0, 450, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 318, 0, 150, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0), .Dim = c(31L, 4L), .Dimnames = list(c("Upper Sacramento River", 
                                                                                        "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek", 
                                                                                        "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek", 
                                                                                        "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek", 
                                                                                        "Thomes Creek", "Upper-mid Sacramento River", "Sutter Bypass", 
                                                                                        "Bear River", "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                                                                                        "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River", 
                                                                                        "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River", 
                                                                                        "Tuolumne River", "San Joaquin River"), NULL)))
test_that("Get spawning adults (seed mode) returns the expected values", {
  spawning_adults <- get_spawning_adults(year, 
                                         round(adults), 
                                         hatch_adults, 
                                         mode = "seed",
                                         month_return_proportions = params$month_return_proportions,
                                         prop_flow_natal = params$prop_flow_natal,
                                         south_delta_routed_watersheds = params$south_delta_routed_watersheds,
                                         gates_overtopped = params$gates_overtopped,
                                         tisdale_bypass_watershed = params$tisdale_bypass_watershed,
                                         yolo_bypass_watershed = params$yolo_bypass_watershed,
                                         migratory_temperature_proportion_over_20 = params$migratory_temperature_proportion_over_20,
                                         natural_adult_removal_rate = params$natural_adult_removal_rate,
                                         ..surv_adult_enroute_int = params$..surv_adult_enroute_int,
                                         .adult_stray_intercept = params$.adult_stray_intercept,
                                         .adult_stray_wild = params$.adult_stray_wild,
                                         .adult_stray_natal_flow = params$.adult_stray_natal_flow,
                                         .adult_stray_cross_channel_gates_closed = params$.adult_stray_cross_channel_gates_closed,
                                         .adult_stray_prop_bay_trans = params$.adult_stray_prop_bay_trans,
                                         .adult_stray_prop_delta_trans = params$.adult_stray_prop_delta_trans,
                                         .adult_en_route_migratory_temp = params$.adult_en_route_migratory_temp,
                                         .adult_en_route_bypass_overtopped = params$.adult_en_route_bypass_overtopped,
                                         .adult_en_route_adult_harvest_rate = params$.adult_en_route_adult_harvest_rate,
                                         stochastic = FALSE)
  expect_equal(spawning_adults, expected_spawners)
})

# Tests spawn success function
init_adults <- expected_spawners$init_adults
min_spawn_habitat <- apply(params$spawning_habitat[ , 10:12, year], 1, min)

expected_juveniles <- structure(c(2023267, 0, 1148754, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0), 
                                .Dim = c(31L, 4L), 
                                .Dimnames = list(c("Upper Sacramento River", 
                                                   "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek", 
                                                   "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek", 
                                                   "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek", 
                                                   "Thomes Creek", "Upper-mid Sacramento River", "Sutter Bypass", 
                                                   "Bear River", "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                                                   "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River", 
                                                   "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River", 
                                                   "Tuolumne River", "San Joaquin River"), c("fry", "", "", "")))

test_that("spawn success function returns the expected value", {
  juveniles <- spawn_success(escapement = init_adults,
                             adult_prespawn_survival = expected_prespawn_surv,
                             egg_to_fry_survival = expected_egg_surv,
                             prob_scour = params$prob_nest_scoured,
                             spawn_habitat = min_spawn_habitat, 
                             stochastic = FALSE)
  expect_equal(round(juveniles), round(expected_juveniles))
})

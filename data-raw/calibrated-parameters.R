library(tidyverse)

vect<-c(-0.6558315, # 1 default calibration intercept
        -3.4999845, # 2 bypass calibration intercept
        1.4933417, # 3 delta calibration intercept
        -3.0188308, # 4 SJ calibration intercept
        2.0000003,  # 5 sac outmigration surv intercept 1
        0.7999889, # 6 sac outmigration surv intercept 2
        -3.5000000, # 7 delta outmigration surv intercept 1
        -0.1999996, # 8 delta outmigration surv intercept 2
        -3.4999920, # 9 delta outmigration surv intercept 3
        -2.9839253, # 10 adult ocean entry success survival
        3.4999976,  # 11 adult en route survival
        0.6466230,  # 12 Egg to fry survival rate
        0.0194795,  # 13 contact point beta
        0.1000000,  # 14 proportion diverted beta
        0.3000000,  # 15 total diverted beta
        0.4820249) # 16 delta total diverted beta

surv.adj <- rep(1, 31)

# Create a dataframe that contains the watersheds and corresponding betas for each
# Looks like for winterRunDSM we do not need to update rear suv in watersheds (just outmigtarion)
# index_ws_to_update <- c(1, 6, 10, 12, 16, 21, 24, 18, 19, 20, 23, 25, 26, 27, 28, 29, 30, 31, 22, 17)
# `2nd calibration adjustment` <- c(vect[2:5], rep(vect[6], 3), vect[7], vect[7], vect[8],
#                                   vect[9], vect[10], vect[10], vect[11], vect[12],
#                                   vect[13], vect[14], vect[15], vect[16], vect[16])

beta_to_update <- tibble(
  order = index_ws_to_update,
  `2nd calibration adjustment`
)

survival_betas <- winterRunDSM::watershed_attributes %>% select(order, watershed) %>% 
  add_column(`2nd calibration adjustment` = -0.6558315,
             `average temperature` = -0.717,
             predation = -0.122,
             `contact points` = 0.0194795,
             `contact points scaler` = -0.189,
             `proportion diverted` = 0.1,
             `proportion diverted scaler` = -3.51,
             `total diverted` = 0.3,
             `total diverted scaler` = -0.0021,
             stranded = -1.939,
             medium = 1.48,
             large = 2.223,
             `floodplain habitat` = 0.47,
             `survival adjustments` = surv.adj)

usethis::use_data(survival_betas, overwrite = TRUE)

# TODO make the model use these tibbles!

# delta_survival_betas <- tibble(
#   watershed = c("North Delta", "South Delta"),
#   intercept = 1.4, # vect[17]
#   `avg temp thresh` = -0.717,
#   predation = -0.122,
#   contact = 0.0358 * -0.189,
#   `prop diversions` = -3.51,
#   `total diversions` = 0.5 * -0.0021,
#   medium = 1.48,
#   large = 2.223
# )
#
# usethis::use_data(delta_survival_betas, overwrite = TRUE)
#
# outmigration_survival_betas <- tibble(
#   `intercept 1` = 2.5, flow = 0.0092,
#   `proportion diversion` = -3.51 * 0.05,
#   `total diversion` = -0.0021 * 0.215,
#   `intercept 2` = 0.3,
#   `average temperature` = 0.554,
#   `model weight` = .5,
#   medium = 1.48, large = 2.223
# )

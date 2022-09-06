library(tidyverse)
library(purrr)
library(parallel)
library(doParallel)


df <- expand.grid(location_surv = winterRunDSM::watershed_labels[c(1, 16, 21, 24)], 
                  month_surv = c(9:12, 1:5),
                  which_surv = c("juv_rear", "juv_migratory", "egg_to_fry"))

df <- head(df, 5)

no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)
registerDoParallel(cl)

clusterExport(cl, list('winter_run_model'))

egg_to_fry <- winter_run_model(scenario = NULL, mode = c("simulate"),
                 seeds = winterRunDSM::adult_seeds, 
                 ..params = winterRunDSM::params, 
                 stochastic = FALSE, 
                 which_surv = c("egg_to_fry"),
                 location_surv = "Upper Sacramento River",
                 month_surv = 1)

no_action <- winter_run_model(scenario = NULL, mode = c("simulate"),
                 seeds = winterRunDSM::adult_seeds, 
                 ..params = winterRunDSM::params, 
                 stochastic = FALSE, 
                 which_surv = c("no_action"),
                 location_surv = "Upper Sacramento River",
                 month_surv = 1)

# sanity check 
spawners <- egg_to_fry$spawners * egg_to_fry$proportion_natural
spawners_no_action <- no_action$spawners * no_action$proportion_natural


# juvenile rearing -------------------------------------------------------

winter_run_model(scenario = NULL, mode = c("simulate"),
                 seeds = winterRunDSM::adult_seeds, 
                 ..params = winterRunDSM::params, 
                 stochastic = FALSE, 
                 which_surv = c("juv_rear"),
                 location_surv = "Upper-mid Sacramento River",
                 month_surv = 9)


# juv_migratory -----------------------------------------------------------

winter_run_model(scenario = NULL, mode = c("simulate"),
                 seeds = winterRunDSM::adult_seeds, 
                 ..params = winterRunDSM::params, 
                 stochastic = FALSE, 
                 which_surv = c("juv_migratory"),
                 location_surv = "Upper-mid Sacramento River",
                 month_surv = 9)


# run through scenarios  --------------------------------------------------

test <- df %>%
  pmap_dfr(~data_frame(x = winter_run_model(scenario = NULL, mode = c("simulate"),
                                           seeds = winterRunDSM::adult_seeds, 
                                           ..params = winterRunDSM::params, 
                                           stochastic = FALSE) %>% pluck("spawners")))

test2 <- df %>%
  map_dfr(~winter_run_model(.))

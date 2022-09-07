library(dplyr)

set.seed(123119)
sensi_seeds <- winterRunDSM::winter_run_model(mode = "seed")


sensitivity_winter_run_model <- function(scenario, scenarios, sensi_seeds) {
  
  which_surv = scenarios[scenario, ]$which_surv
  location_surv = scenarios[scenario, ]$location_surv
  month_surv = scenarios[scenario, ]$month_surv
  
  model_results <- winterRunDSM::winter_run_model(mode = "simulate",
                                                  seeds = sensi_seeds, 
                                                  which_surv = which_surv,
                                                  location_surv = location_surv,
                                                  month_surv = month_surv)
  
  output <- dplyr::as_tibble((model_results$spawners * model_results$proportion_natural)[c(1, 3), ]) |> 
    dplyr::mutate(location = c("Upper Sacramento River", "Battle Creek"),
                  survival_target = which_surv, 
                  location_target = location_surv, 
                  month_target = month_surv,
                  id = scenario) |> 
    dplyr::select(id, location, survival_target, location_target, month_target, `1`:`20`)
  
  return(output)
  
}

# run scenarios in parallel -------
library(tictoc) # measure time to run
library(parallel)
library(doParallel)

# scenarios to evaluate 
scenarios1 <- expand.grid(location_surv = winterRunDSM::watershed_labels[c(16, 21, 24, 17, 22, 3)], 
                          month_surv = c(9:12, 1:5),
                          which_surv = c("juv_rear", "juv_migratory")) 

scenarios2  <- expand.grid(location_surv = c("Upper Sacramento River", "South Delta"),
                           month_surv = c(9:12, 1:5),
                           which_surv = "juv_rear") 

scenarios3  <- expand.grid(location_surv = c("Delta","Bay Delta"), 
                           month_surv = c(9:12, 1:5),
                           which_surv = "juv_migratory")

scenarios4  <- expand.grid(location_surv = c("Upper Sacramento River","Battle Creek"),
                           month_surv = NA,
                           which_surv = "egg_to_fry")

scenarios5  <- expand.grid(location_surv = "North Delta",
                           month_surv = c(9:12, 1:5),
                           which_surv = "juv_rear") 

# set up for running function in parallel
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)

clusterExport(cl, list("sensitivity_winter_run_model", "sensi_seeds", 
                       "scenarios1", "scenarios2", "scenarios3", "scenarios4", "scenarios5"))


tic("parallel 1")
scenario_results_list1 <- parLapply(cl, 1:nrow(scenarios1),
                                    fun = function(scenario) {
                                      sensitivity_winter_run_model(scenario, scenarios1, sensi_seeds)
                                    })
toc()

tic("parallel 2")
scenario_results_list2 <- parLapply(cl, 1:nrow(scenarios2),
                                    fun = function(scenario) {
                                      sensitivity_winter_run_model(scenario, scenarios2, sensi_seeds)
                                    })
toc()

tic("parallel 3")
scenario_results_list3 <- parLapply(cl, 1:nrow(scenarios3),
                                    fun = function(scenario) {
                                      sensitivity_winter_run_model(scenario, scenarios3, sensi_seeds)
                                    })
toc()

tic("parallel 4")
scenario_results_list4 <- parLapply(cl, 1:nrow(scenarios4),
                                    fun = function(scenario) {
                                      sensitivity_winter_run_model(scenario, scenarios4, sensi_seeds)
                                    })
toc()

tic("parallel 5")
scenario_results_list5 <- parLapply(cl, 1:nrow(scenarios5),
                                    fun = function(scenario) {
                                      sensitivity_winter_run_model(scenario, scenarios5, sensi_seeds)
                                    })
toc()

# combine into one 
r1 <- scenario_results_list1 |> dplyr::bind_rows()
r2 <- scenario_results_list2 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r1$id))
r3 <- scenario_results_list3 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r2$id))
r4 <- scenario_results_list4 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r3$id))
r5 <- scenario_results_list5 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r4$id))

# do nothing
model_results <- winterRunDSM::winter_run_model(mode = "simulate",
                                                seeds = sensi_seeds, 
                                                which_surv = NA,
                                                location_surv = NA,
                                                month_surv = NA)

do_nothing <- dplyr::as_tibble((model_results$spawners * model_results$proportion_natural)[c(1, 3), ]) |> 
  dplyr::mutate(location = c("Upper Sacramento River", "Battle Creek"),
                survival_target = NA, 
                location_target = NA, 
                month_target = NA,
                id = max(r5$id) + 1) |> 
  dplyr::select(id, location, survival_target, location_target, month_target, `1`:`20`)

results <- dplyr::bind_rows(r1, r2, r3, r4, r5, do_nothing)
write_csv(results, "analysis/survival_sensi_model_ouput.csv")

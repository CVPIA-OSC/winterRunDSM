library(tidyverse)
library(winterRunDSM)

og_results <- read_rds("data-raw/total-spawners-results.rds")

run <- function() {
  list2env(load_2019_baseline_data(), envir = .GlobalEnv)
  s <- winter_run_model()
  winter_run_model(seeds = s)
}


results <- replicate(10, run())

results_spawners <- map_df(1:10, ~results["spawners", ][[.]][1, ]) %>% 
  mutate(run = 1:10) %>% 
  gather("year", "spawners", -run) %>% 
  mutate(year = as.numeric(year))

results_og_spawners <- map_df(1:10, ~og_results["spawners", ][[.]][1, ]) %>% 
  mutate(run = 1:10) %>% 
  gather("year", "spawners", -run) %>% 
  mutate(year = as.numeric(year))

results_spawners %>% 
  ggplot(aes(year, spawners, group = run)) + geom_line(alpha=.4) + 
  scale_y_continuous(breaks = seq(0, 60000, by=5000)) + 
  geom_hline(yintercept = 8896.4, linetype=2)

results_og_spawners %>% 
  ggplot(aes(year, spawners, group = run)) + geom_line(alpha=.4) + 
  scale_y_continuous(breaks = seq(0, 60000, by=5000)) + 
  geom_hline(yintercept = 8896.4, linetype=2)

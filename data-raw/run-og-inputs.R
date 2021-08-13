library(tidyverse)
source('data-raw/load_old_inputs.R')

old_seeds <- read_csv('calibration/filledknownAdults_1998_2016.csv') %>%
  select(-watershed, -order) %>%
  as.matrix()

sim <- fall_run_model(seeds = old_seeds, stochastic = FALSE,
                      mode = "calibrate", ..params = params)

keep_num <- c(1,6,7,10,12,19,20,23,26:30)

# grand_tab <- as_tibble(old_seeds[keep_num, ]) %>%
#   mutate(watershed = DSMscenario::watershed_labels[keep_num]) %>%
#   gather(year, spawners, -watershed) %>%
#   filter(!is.na(spawners)) %>%
#   mutate(year = parse_number(year),
#          observed_nat_spawn = spawners*(1-fallRunDSM::params$proportion_hatchery[watershed]))

grand_tab <- as_tibble(adam_grand_tab) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep_num]) %>%
  gather(year, spawners, -watershed) %>%
  filter(!is.na(spawners)) %>%
  mutate(year = parse_number(year) + 5,
         observed = spawners)

nat_spawn <- as_tibble(sim[keep_num, ]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep_num]) %>%
  gather(year, predicted, -watershed) %>%
  mutate(year = parse_number(year) + 5)

both <- nat_spawn %>%
  left_join(grand_tab)

both %>%
  select(-spawners) %>%
  # group_by(watershed) %>%
  summarise(r = cor(predicted, observed, use = 'pairwise.complete.obs'))

both %>%
  select(-spawners) %>%
  gather(type, spawners, -watershed, -year) %>%
  ggplot(aes(year, spawners, color = type)) +
  geom_line() +
  facet_wrap(~watershed, scales = 'free_y')

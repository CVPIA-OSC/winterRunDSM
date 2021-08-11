old_params <- winterRunDSM::params

# this is the same to our adult_seed
# old_seeds <- read_csv('data-raw/misc/SimulaitonSeed_WinterRunAdults.csv') 


# return mistake of up sac total diverted is prop diverted
old_params$total_diverted[1,,] <-  old_params$proportion_diverted[1,,]

# look at delta temp and flow
# double check coefficients
# look at delt.Rear
# double check calculated survival rates

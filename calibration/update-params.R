update_params <- function(x, params) {

  params$surv_adult_enroute_int = x[1]
  params$surv_egg_to_fry_mean_egg_temp_effect = x[2]
  params$surv_juv_rear_int = rep(x[3], 31)
  params$surv_juv_rear_contact_points = x[4]
  params$surv_juv_rear_prop_diversions = x[5]
  params$surv_juv_rear_total_diversions = x[6]
  params$surv_juv_bypass_int = x[7]
  params$surv_juv_delta_int = x[8]
  params$surv_juv_delta_contact_points = x[9]
  params$surv_juv_delta_total_diverted = x[10]
  params$surv_juv_outmigration_sj_int = x[11]
  params$surv_juv_outmigration_sac_delta_intercept_one = x[12]
  params$surv_juv_outmigration_sac_delta_intercept_two = x[13]
  params$surv_juv_outmigration_sac_delta_intercept_three = x[14]
  params$ocean_entry_success_int = rep(x[15], 31)

  return(params)

}

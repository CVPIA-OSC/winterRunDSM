update_params <- function(x, params) {

  params$..surv_adult_enroute_int = x[1]
  params$..surv_juv_rear_int = rep(x[2], 31)
  params$..surv_juv_rear_contact_points = x[3]
  params$..surv_juv_rear_prop_diversions = x[4]
  params$..surv_juv_rear_total_diversions = x[5]
  params$..surv_juv_bypass_int = x[6]
  params$..surv_juv_delta_int = x[7]
  params$..surv_juv_delta_contact_points = x[8]
  params$..surv_juv_delta_total_diverted = x[9]
  params$..surv_juv_outmigration_sj_int = x[10]
  params$..ocean_entry_success_int = rep(x[11], 31)

  return(params)

}

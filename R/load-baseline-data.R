#' @title Load Baseline Data
#' @description loads baseline data from DSMflow, DSMhabitat and DSMtemperature
#' @details
#' More details for each item in the list available below.
#'
#' \strong{Flow Inputs}
#' \itemize{
#'   \item freeport_flows - \link[DSMflow]{freeport_flow}
#'   \item vernalis_flows - \link[DSMflow]{vernalis_flow}
#'   \item stockton_flows - \link[DSMflow]{stockton_flow}
#'   \item CVP_exports - \link[DSMflow]{cvp_exports}
#'   \item SWP_exports - \link[DSMflow]{swp_exports}
#'   \item proportion_diverted - \link[DSMflow]{proportion_diverted}
#'   \item total_diverted - \link[DSMflow]{total_diverted}
#'   \item delta_proportion_diverted - \link[DSMflow]{delta_proportion_diverted}
#'   \item delta_total_diverted - \link[DSMflow]{delta_total_diverted}
#'   \item prop_pulse_flows - \link[DSMflow]{proportion_pulse_flows}
#'   \item prop_flow_natal - \link[DSMflow]{proportion_flow_natal}
#'   \item upper_sacramento_flows - \link[DSMflow]{upper_sacramento_flows}
#'   \item delta_inflow - \link[DSMflow]{delta_inflow}
#'   \item cc_gates_days_closed - \link[DSMflow]{delta_cross_channel_closed}
#'   \item cc_gates_prop_days_closed - \link[DSMflow]{delta_cross_channel_closed}
#'   \item proportion_flow_bypass - \link[DSMflow]{proportion_flow_bypasses}
#'   \item gates_overtopped - \link[DSMflow]{gates_overtopped}
#' }
#'
#' \strong{Temperature Inputs}
#' \itemize{
#'   \item vernalis_temps - \link[DSMTemperature]{vernalis_temperature}
#'   \item prisoners_point_temps - \link[DSMTemperature]{prisoners_point_temperature}
#'   \item degree_days - \link[DSMTemperature]{degree_days}
#'   \item mean_egg_temp_effect - \link[DSMTemperature]{egg_temperature_effect}
#'   \item avg_temp - \link[DSMTemperature]{stream_temperature}
#'   \item avg_temp_delta - \link[DSMTemperature]{delta_temperature}
#'   \item migratory_temperature_proportion_over_20 - \link[DSMTemperature]{migratory_temperature_proportion_over_20}
#' }
#'
#' \strong{Habitat Inputs}
#' \itemize{
#'   \item spawning_habitat - \link[DSMhabitat]{wr_spawn}
#'   \item inchannel_habitat_fry - \link[DSMhabitat]{wr_fry}
#'   \item inchannel_habitat_juvenile - \link[DSMhabitat]{wr_juv}
#'   \item floodplain_habitat - \link[DSMhabitat]{wr_fp}
#'   \item weeks_flooded - \link[DSMhabitat]{weeks_flooded}
#'   \item delta_habitat - \link[DSMhabitat]{delta_habitat}
#'   \item sutter_habitat - \link[DSMhabitat]{sutter_habitat}
#'   \item yolo_habitat - \link[DSMhabitat]{yolo_habitat}
#'   \item tisdale_bypass_watershed - \link[DSMhabitat]{tisdale_bypass_watershed}
#'   \item yolo_bypass_watershed - \link[DSMhabitat]{yolo_bypass_watershed}
#'   \item south_delta_routed_watersheds - \link[DSMhabitat]{south_delta_routed_watersheds}
#'   \item prop_high_predation - \link[DSMhabitat]{prop_high_predation}
#'   \item contact_points - \link[DSMhabitat]{contact_points}
#'   \item delta_contact_points - \link[DSMhabitat]{delta_contact_points}
#'   \item delta_prop_high_predation - \link[DSMhabitat]{delta_prop_high_predation}
#'   \item prob_strand_early - \link[DSMhabitat]{prob_strand_early}
#'   \item prob_strand_late - \link[DSMhabitat]{prob_strand_late}
#'   \item prob_nest_scoured - \link[DSMhabitat]{prob_nest_scoured}
#' }
#'
#' @export
load_baseline_data <- function() {
  # DSMflow variables -----
  freeport_flows <- DSMflow::freeport_flow
  vernalis_flows <- DSMflow::vernalis_flow
  stockton_flows <- DSMflow::stockton_flow
  CVP_exports <- DSMflow::cvp_exports
  SWP_exports <- DSMflow::swp_exports
  proportion_diverted <- DSMflow::proportion_diverted
  total_diverted <- DSMflow::total_diverted
  delta_proportion_diverted <- DSMflow::delta_proportion_diverted
  delta_total_diverted <- DSMflow::delta_total_diverted
  prop_pulse_flows <- DSMflow::proportion_pulse_flows
  prop_flow_natal <- DSMflow::proportion_flow_natal
  upper_sacramento_flows <- DSMflow::upper_sacramento_flows
  delta_inflow <- DSMflow::delta_inflow
  cc_gates_days_closed <- DSMflow::delta_cross_channel_closed["count", ]
  cc_gates_prop_days_closed <- DSMflow::delta_cross_channel_closed["proportion", ]
  proportion_flow_bypass <- DSMflow::proportion_flow_bypasses
  gates_overtopped <- DSMflow::gates_overtopped

  # DSMtemperature variables -----
  vernalis_temps <- DSMtemperature::vernalis_temperature
  prisoners_point_temps <- DSMtemperature::prisoners_point_temperature
  degree_days <- DSMtemperature::degree_days
  mean_egg_temp_effect <- DSMtemperature::egg_temperature_effect$winter_run
  avg_temp <- DSMtemperature::stream_temperature
  avg_temp_delta <- DSMtemperature::delta_temperature
  migratory_temperature_proportion_over_20 <- DSMtemperature::migratory_temperature_proportion_over_20

  # DSMhabitat variables -----
  spawning_habitat <- DSMhabitat::wr_spawn
  inchannel_habitat_fry <- DSMhabitat::wr_fry
  inchannel_habitat_juvenile <- DSMhabitat::wr_juv
  floodplain_habitat <- DSMhabitat::wr_fp
  weeks_flooded <- DSMhabitat::weeks_flooded
  delta_habitat <- DSMhabitat::delta_habitat
  sutter_habitat <- DSMhabitat::sutter_habitat
  yolo_habitat <- DSMhabitat::yolo_habitat
  tisdale_bypass_watershed <- DSMhabitat::tisdale_bypass_watershed
  yolo_bypass_watershed <- DSMhabitat::yolo_bypass_watershed
  south_delta_routed_watersheds <- DSMhabitat::south_delta_routed_watersheds
  prop_high_predation <- DSMhabitat::prop_high_predation
  contact_points <- DSMhabitat::contact_points
  delta_contact_points <- DSMhabitat::delta_contact_points
  delta_prop_high_predation <- DSMhabitat::delta_prop_high_predation
  prob_strand_early <- DSMhabitat::prob_strand_early
  prob_strand_late <- DSMhabitat::prob_strand_late
  prob_nest_scoured <- DSMhabitat::prob_nest_scoured

  list(
    freeport_flows = freeport_flows,
    vernalis_flows = vernalis_flows,
    stockton_flows = stockton_flows,
    CVP_exports = CVP_exports,
    SWP_exports = SWP_exports,
    proportion_diverted = proportion_diverted,
    total_diverted = total_diverted,
    delta_proportion_diverted = delta_proportion_diverted,
    delta_total_diverted = delta_total_diverted,
    prop_pulse_flows = prop_pulse_flows,
    prop_flow_natal = prop_flow_natal,
    upper_sacramento_flows = upper_sacramento_flows,
    delta_inflow = delta_inflow,
    cc_gates_days_closed = cc_gates_days_closed,
    cc_gates_prop_days_closed = cc_gates_prop_days_closed,
    proportion_flow_bypass = proportion_flow_bypass,
    gates_overtopped = gates_overtopped,
    vernalis_temps = vernalis_temps,
    prisoners_point_temps = prisoners_point_temps,
    degree_days = degree_days,
    mean_egg_temp_effect = mean_egg_temp_effect,
    avg_temp = avg_temp,
    avg_temp_delta = avg_temp_delta,
    migratory_temperature_proportion_over_20 = migratory_temperature_proportion_over_20,
    spawning_habitat = spawning_habitat,
    inchannel_habitat_fry = inchannel_habitat_fry,
    inchannel_habitat_juvenile = inchannel_habitat_juvenile,
    floodplain_habitat = floodplain_habitat,
    weeks_flooded = weeks_flooded,
    delta_habitat = delta_habitat,
    sutter_habitat = sutter_habitat,
    yolo_habitat = yolo_habitat,
    tisdale_bypass_watershed = tisdale_bypass_watershed,
    yolo_bypass_watershed = yolo_bypass_watershed,
    south_delta_routed_watersheds = south_delta_routed_watersheds,
    prop_high_predation = prop_high_predation,
    contact_points = contact_points,
    delta_contact_points = delta_contact_points,
    delta_prop_high_predation = delta_prop_high_predation,
    prob_strand_early = prob_strand_early,
    prob_strand_late = prob_strand_late,
    prob_nest_scoured = prob_nest_scoured
  )
}


#' @title Load 2019 Baseline Data
#' @description loads baseline data from OG model and calibrates for model comparisons 
#' @export
load_2019_baseline_data <- function(){
  # Vect2 contains habitat scalars 
  vect2<-c(1.1975408, 0.9999999, 0.5000000, 0.9934527, 1.0031454, 1.2284905, 0.9940085, 1.0307768, 1.0000048)
  
  
  current_data <- load_baseline_data()
  
  current_data$spawning_habitat <- baseline_2019$IChab.spawn
  current_data$inchannel_habitat_fry <- baseline_2019$IChab.fry
  current_data$inchannel_habitat_juvenile <- baseline_2019$IChab.juv
  
  current_data$sutter_habitat <- baseline_2019$IChab.bypass[1,,] + baseline_2019$IChab.bypass[2,,] +
                                 baseline_2019$IChab.bypass[3,,] + baseline_2019$IChab.bypass[4,,]
  current_data$yolo_habitat <- baseline_2019$IChab.bypass[5,,] + baseline_2019$IChab.bypass[6,,]
  current_data$delta_habitat <- baseline_2019$DLThab
  
  
  current_data$spawning_habitat[1,,] <- current_data$spawning_habitat[1,,] * vect2[1] #Upper Sac 
  
  current_data$inchannel_habitat_fry[1,,] <- current_data$inchannel_habitat_fry[1,,] * vect2[2] # Upper Sac 
  current_data$inchannel_habitat_fry[16,,] <- current_data$inchannel_habitat_fry[16,,] * vect2[3] # Upper-mid Sac
  current_data$inchannel_habitat_fry[21,,] <- current_data$inchannel_habitat_fry[21,,] * vect2[4] # Lower-mid Sac Sutter
  current_data$inchannel_habitat_fry[24,,] <- current_data$inchannel_habitat_fry[24,,] * vect2[5] # Lower-mid Sac Yolo
  
  current_data$inchannel_habitat_juvenile[1,,] <- current_data$inchannel_habitat_juvenile[1,,] * vect2[2] # Upper Sac 
  current_data$inchannel_habitat_juvenile[16,,] <- current_data$inchannel_habitat_juvenile[16,,] * vect2[3] # Upper-mid Sac
  current_data$inchannel_habitat_juvenile[21,,] <- current_data$inchannel_habitat_juvenile[21,,] * vect2[4] # Lower-mid Sac Sutter 
  current_data$inchannel_habitat_juvenile[24,,] <- current_data$inchannel_habitat_juvenile[24,,] * vect2[5] # Lower-mid Sac Yolo
  
  current_data$sutter_habitat <- current_data$sutter_habitat * vect2[6] # sutter bypass hab 
  current_data$yolo_habitat <- current_data$yolo_habitat * vect2[7] # yolo bypass hab 
  
  current_data$delta_habitat[,,1] <- current_data$delta_habitat[,,1] * vect2[8] # North Delta Hab 
  current_data$delta_habitat[,,2] <- current_data$delta_habitat[,,2] * vect2[8] # South Delta Hab 
  
  return(current_data)
}



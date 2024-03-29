#' Adult Harvest Rate
#' @title Adult Harvest Rate
#' @description Proportion of adults harvested from golden gate until they reach their natal shed
#' @format 1 dimensional array [31 watersheds]
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
"adult_harvest_rate"

#' Natural Spawners Removal Rate
#' @title Natural Spawners Removal Rate
#' @description Spawners removed for hatcheries
#' @format 1 dimensional array [31 watersheds]
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
"natural_adult_removal_rate"

#' @title Hatchery Allocation
#' @description The proportion of hatchery fish spawning
#' @format 1 dimensional array [31 watersheds]
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
"hatchery_allocation"

#' Adult Seeds
#' @title Adult Seeds
#' @description Adult fish for the initial 5 years of the simulations.
#' @format A matrix with dimension 31 x 30 (watershed x year)
#' @source Derived from average escapement estimates from 2013 to 2017 \href{https://dsm-docs.s3.us-west-2.amazonaws.com/Azat+2019.pdf}{Azat 2019}.
"adult_seeds"

#' @title Proportion Hatchery
#' @description Proportion of spawning fish that are from a hatchery.
#' @format 1 dimensional array [31 watersheds]
#' @source Proportion hatchery was derived using coded wire tag analysis. Details and methodology are available \href{https://cvpia-osc.github.io/fallRunDSM/articles/hatchery-analysis.html}{here}.
"proportion_hatchery"

#' @title Month Return Proportions
#' @description The proportion of spawning fish in Jan-Apr
#' @format Vector of length 4
#' @source Expert opinion from SIT members.
"month_return_proportions"

#' @title Mass by Size Class
#' @description Mass of fish by the size class
#' @format Vector of length 4 (4 size classes)
#' @source Size classes were derived from Analysis of Stanislaus data.
"mass_by_size_class"

#' @title Cross Channel Stray Rate
#' @description Natural straying allocation across channels.
#' @format Vector of length 31 [31 watersheds]
#' @source Expert opinion from SIT members.
"cross_channel_stray_rate"

#' @title Stray Rate
#' @description Natural straying allocation
#' @format Vector of length 31 [31 watersheds]
#' @source Estimated with coded wire tag data 2010–2013 (\href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{Kormos et al. 2012},
#' \href{https://www.rmpc.org/files/mark-mtg-2014-docs/2011_CFM_CWT_Recovery_Report_FAR_2013(Melodie_Palmer-Zwahlen).pdf}{Palmer-Zwahlen & Kormos 2013-2015},
#' \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg=AOvVaw3wSbd-zJk1eQa085Xuxiyg}{Palmer-Zwahlen et al. 2018})
"stray_rate"

#' @title Diversity Groups
#' @description Watershed groups.
#' @format Vector of length 31 [31 watersheds]
#' @source The diversity groups are sourced from the \href{http://escholarship.org/uc/item/3653x9xc}{"Framework for Assessing Viability of Threatened and Endangered Chinook Salmon and Steelhead in the Sacramento-San Joaquin Basin"} with slight modification.
#' The "Northern Sierra" diversity group covers a very large geographic area and was divided into two groups based on proximity.
"diversity_group"

#' @title Growth Rates
#' @description Chinook growth rates in floodplain and inchannel habitat.
#' @format 4 x 4 x 4 array [4 size classes x 4 size classes x 4 weeks]
#' @source Growth rates determined by expert SIT judgment based on the following:
#' \itemize{
#'   \item \href{https://link.springer.com/article/10.1007/s10641-009-9473-8}{Limm, M.P., Marchetti, M.P.  (2009)}
#'   \item \href{https://www.tandfonline.com/doi/abs/10.1577/T08-112.1}{Rene E. Henery, Ted R. Sommer & Charles R. Goldman (2010)}
#'   \item \href{https://link.springer.com/article/10.1007/s10641-008-9367-1}{Jeffres, C.A., Opperman, J.J. & Moyle, P.B. (2008)}
#' }
#' @name growth_rates
NULL

#' @rdname growth_rates
#' @format NULL
"growth_rates_inchannel"

#' @rdname growth_rates
#' @format NULL
"growth_rates_floodplain"

#' @title Model Parameters
#' @description A list containing all parameters needed for running the \code{\link{winter_run_model}}. Parameters with a single
#' period in front are model or submodel coefficients. Parameters with double periods in front of them are calibrated model parameters.
#' @usage NULL
#' @format NULL
#' @section Habitat Inputs:
#' \itemize{
#'   \item \code{spawning_habitat}: More details at \code{\link[DSMhabitat]{wr_spawn}}
#'   \item \code{inchannel_habitat_fry}: More details at \code{\link[DSMhabitat]{wr_fry}}
#'   \item \code{inchannel_habitat_juvenile}: More details at \code{\link[DSMhabitat]{wr_juv}}
#'   \item \code{floodplain_habitat}: More details at \code{\link[DSMhabitat]{wr_fp}}
#'   \item \code{sutter_habitat}: More details at \code{\link[DSMhabitat]{sutter_habitat}}
#'   \item \code{yolo_habitat}: More details at \code{\link[DSMhabitat]{yolo_habitat}}
#'   \item \code{delta_habitat}: More details at \code{\link[DSMhabitat]{delta_habitat}}
#'   \item \code{spawn_decay_rate}: More details at \code{\link[DSMscenario]{spawn_decay_rate}}
#'   \item \code{rear_decay_rate}: More details at \code{\link[DSMscenario]{rear_decay_rate}}
#' }
#' @section Spawning Adults:
#' \itemize{
#'   \item \code{prop_flow_natal}: More details at \code{\link[DSMflow]{proportion_flow_natal}}
#'   \item \code{south_delta_routed_watersheds}: More details at \code{\link[DSMhabitat]{south_delta_routed_watersheds}}
#'   \item \code{cc_gates_days_closed}: More details at \code{\link[DSMflow]{delta_cross_channel_closed}}
#'   \item \code{cross_channel_stray_rate}: More details at \code{\link{cross_channel_stray_rate}}
#'   \item \code{stray_rate}: More details at \code{\link{stray_rate}}
#'   \item \code{gates_overtopped}: More details at \code{\link[DSMflow]{gates_overtopped}}
#'   \item \code{tisdale_bypass_watershed}: More details at \code{\link[DSMhabitat]{tisdale_bypass_watershed}}
#'   \item \code{yolo_bypass_watershed}: More details at \code{\link[DSMhabitat]{yolo_bypass_watershed}}
#'   \item \code{migratory_temperature_proportion_over_20}: More details at \code{\link[DSMtemperature]{migratory_temperature_proportion_over_20}}
#'   \item \code{..surv_adult_enroute_int}:  Intercept, source: calibration
#'   \item \code{.adult_stray_intercept}: Empirical model fit using 2008–2011 tagging data provided by East Bay Municipal Utility District
#'   \item \code{.adult_stray_wild}: Coefficient for \code{wild} variable, source: Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#'   \item \code{.adult_stray_natal_flow}: Coefficient for \code{natal_flow} variable, source: Empirical model fit using 2008–2011 tagging data provided by East Bay Municipal Utility District
#'   \item \code{.adult_stray_cross_channel_gates_closed}: Coefficient for \code{cross_channel_gates_closed} variable, source: Empirical model fit using  2008–2011 tagging data provided by East Bay Municipal Utility District.
#'   \item \code{.adult_stray_prop_bay_trans}: Coefficient for \code{prop_bay_trans} variable, source: Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#'   \item \code{.adult_stray_prop_delta_trans}: Coefficient for \code{prop_delta_trans} variable, source: Estimated with coded wire tag data 2010–2013 \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Kormos et al. 2012, Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#'   \item \code{.adult_en_route_migratory_temp}: Coefficient for \code{migratory_temp} variable, source: \href{https://dsm-docs.s3.us-west-2.amazonaws.com/schrek_cb_1994.pdf}{Schreck et al. (1994)}
#'   \item \code{.adult_en_route_bypass_overtopped}: Coefficient for \code{bypass_overtopped} variable, source: Expert opinion Ted Sommer, California Department of Water Resources (tributaries above bypasses only)
#'   \item \code{.adult_en_route_adult_harvest_rate}:  Adult harvest rate, source:  \href{https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=162355&usg= AOvVaw0VgMOwD7knFfSxRZy6k8RG}{(Palmer-Zwahlen & Kormos 2013-2015, Palmer-Zwahlen et al. 2018)}
#'   \item \code{degree_days}: More details at \code{\link[DSMtemperature]{degree_days}}
#'   \item \code{month_return_proportions}: More details at \code{\link[winterRunDSM]{month_return_proportions}}
#'   \item \code{.adult_prespawn_int}:  Intercept, source: Inherited calibration value from FallRunDSM
#'   \item \code{.adult_prespawn_deg_day}: Coefficient for \code{deg_day} variable, source: \href{https://onlinelibrary.wiley.com/doi/epdf/10.1002/rra.3348}{Colvin et al. (2018)}
#'   \item \code{prob_nest_scoured}: More details at \code{\link[DSMhabitat]{prob_nest_scoured}}
#'   \item \code{spawn_success_sex_ratio}: Variable describing the female to male spawning ratio, default 0.5, source: expert opinion from SIT members.
#'   \item \code{spawn_success_redd_size}: Variable describing the size of redds including defensible space, default value 9.29 square meters, source: expert opinion from SIT members.
#'   \item \code{spawn_success_fecundity}: Variable describing the number of eggs per female, default value 5522, source: \href{https://www.ucpress.edu/book/9780520227545/inland-fishes-of-california}{Moyle, P. B. 2002. Inland Fishes of California. University of California Press, Berkeley CA}
#'   \item \code{hatchery_allocation}: More details at \code{\link{hatchery_allocation}}
#'   \item \code{natural_adult_removal_rate}: More details at \code{\link{natural_adult_removal_rate}}
#'
#' }
#' @section Egg to Fry Survival:
#' \itemize{
#'   \item \code{proportion_hatchery}: More details at \code{\link[winterRunDSM]{proportion_hatchery}}
#'   \item \code{prob_nest_scoured}: More details at \code{\link[DSMhabitat]{prob_nest_scoured}}
#'   \item \code{mean_egg_temp_effect}: More details at \code{\link[DSMtemperature]{egg_temperature_effect}}
#'   \item \code{.surv_egg_to_fry_proportion_natural}: Coefficient for \code{proportion_natural} variable, source: \href{https://cdnsciencepub.com/doi/abs/10.1139/F10-168}{Chilcote et al. (2011)}
#'   \item \code{.surv_egg_to_fry_scour}: Coefficient for \code{scour} variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/montogemery_1996.pdf}{Montgomery et al. (1996)}
#'   \item \code{..surv_egg_to_fry_int}:  Intercept, source: Calibration
#' }
#' @section Tributary Rearing Survival:
#' \itemize{
#'   \item \code{avg_temp}: More details at \code{\link[DSMtemperature]{stream_temperature}}
#'   \item \code{prob_strand_early}: More details at \code{\link[DSMhabitat]{prob_strand_early}}
#'   \item \code{prob_strand_late}: More details at \code{\link[DSMhabitat]{prob_strand_late}}
#'   \item \code{proportion_diverted}: More details at \code{\link[DSMflow]{proportion_diverted}}
#'   \item \code{total_diverted}: More details at \code{\link[DSMflow]{total_diverted}}
#'   \item \code{weeks_flooded}: More details at \code{\link[DSMhabitat]{weeks_flooded}}
#'   \item \code{prop_high_predation}: More details at \code{\link[DSMhabitat]{prop_high_predation}}
#'   \item \code{contact_points}: More details at \code{\link[DSMhabitat]{contact_points}}
#'   \item \code{..surv_juv_rear_int}: Intercept, source: calibration (varies by tributary)
#'   \item \code{.surv_juv_rear_contact_points}: Coefficient for \code{contact_points} variable, source: inherited from previous calibration
#'   \item \code{..surv_juv_rear_contact_points}: Coefficient for \code{contact_points} variable, source: calibration
#'   \item \code{.surv_juv_rear_prop_diversions}: Coefficient for \code{prop_diversions} variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Hierarchical_Modeling_of_Juvenile_Chinook_Salmon_S+(1).pdf}{Newman and Brandes (2010)}
#'   \item \code{..surv_juv_rear_prop_diversions}: Coefficient for \code{prop_diversions} variable, source: calibration
#'   \item \code{.surv_juv_rear_total_diversions}: Coefficient for \code{total_diversions} variable, source: inherited from previous calibration
#'   \item \code{..surv_juv_rear_total_diversions}: Coefficient for \code{total_diversions} variable, source: calibration
#'   \item \code{.surv_juv_rear_avg_temp_thresh}:  Coefficient for \code{avg_temp_thresh} variable, source: \href{https://www.tandfonline.com/doi/full/10.1577/M07-130.1?scroll=top&needAccess=true}{Runge et al (2008)}
#'   \item \code{.surv_juv_rear_high_predation}: Coefficient for \code{high_predation} variable, source: \href{https://pubag.nal.usda.gov/catalog/512123}{Cavallo et al. (2012)}
#'   \item \code{.surv_juv_rear_stranded}: Coefficient for \code{stranded} variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/DWR-1140+USFWS+2006.pdf}{USFWS (2006) and CDWR (2006)}
#'   \item \code{.surv_juv_rear_medium}: Size related intercept for medium sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#'   \item \code{.surv_juv_rear_large}: Size related intercept for large sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#'   \item \code{.surv_juv_rear_floodplain}: Additional intercept for floodplain rearing benefit, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/SOMMER_T-SDWA+180+Floodplain+rearing+of+juvenile+chinook+salmon+evidence+of+enhanced+growth+and+survival+.pdf}{Sommer et al. (2001)}
#' }
#' @section Bypass Rearing Survival:
#' \itemize{
#'   \item \code{avg_temp}: See Tributary Rearing Survival \code{avg_temp} above
#'   \item \code{prop_high_predation}: See Tributary Rearing Survival \code{prop_high_predation} above
#'   \item \code{..surv_juv_bypass_int}: Intercept, Source: calibration
#'   \item \code{.surv_juv_bypass_avg_temp_thresh}: Coefficient for \code{avg_temp_thresh} variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/marine_cech_water_temp_effects.pdf}{Marine and Chech (2004)}
#'   \item \code{.surv_juv_bypass_high_predation}: Coefficient for \code{high_predation} variable, source: \href{https://pubag.nal.usda.gov/catalog/512123}{Cavallo et al. (2012)}
#'   \item \code{.surv_juv_bypass_medium}: Size related intercept for medium sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#'   \item \code{.surv_juv_bypass_large}: Size related intercept for large sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#'   \item \code{.surv_juv_bypass_floodplain}: Additional intercept for floodplain rearing benefit, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/SOMMER_T-SDWA+180+Floodplain+rearing+of+juvenile+chinook+salmon+evidence+of+enhanced+growth+and+survival+.pdf}{Sommer et al. (2001)}
#' }
#' @section Delta Rearing Survival:
#' \itemize{
#'   \item \code{avg_temp_delta}: More details at \code{\link[DSMtemperature]{delta_temperature}}
#'   \item \code{delta_proportion_diverted}: More details at \code{\link[DSMflow]{delta_proportion_diverted}}
#'   \item \code{delta_total_diverted}: More details at \code{\link[DSMflow]{delta_total_diverted}}
#'   \item \code{delta_contact_points}: More details at \code{\link[DSMhabitat]{delta_contact_points}}
#'   \item \code{delta_prop_high_predation}: More details at \code{\link[DSMhabitat]{delta_prop_high_predation}}
#'   \item \code{..surv_juv_delta_int}: Intercept, Source: calibration
#'   \item \code{.surv_juv_delta_contact_points}: Coefficient for \code{contact_points} variable, Source: inherited from previous calibration.
#'   \item \code{..surv_juv_delta_contact_points}: Coefficient for \code{contact_points} variable, Source: calibration
#'   \item \code{.surv_juv_delta_total_diverted}: Coefficient for \code{total_diversions} variable, Source: inherited from previous calibration.
#'   \item \code{..surv_juv_delta_total_diverted}: Coefficient for \code{total_diversions} variable, Source: calibration
#'   \item \code{.surv_juv_delta_avg_temp_thresh}: Coefficient for \code{avg_temp_thresh} variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/marine_cech_water_temp_effects.pdf}{Marine and Chech (2004)}
#'   \item \code{.surv_juv_delta_high_predation}: Coefficient for \code{high_predation} variable, source: \href{https://pubag.nal.usda.gov/catalog/512123}{Cavallo et al. (2012)}
#'   \item \code{.surv_juv_delta_prop_diverted}: Coefficient for \code{prop_diversions} variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Hierarchical_Modeling_of_Juvenile_Chinook_Salmon_S+(1).pdf}{Newman and Brandes (2010)}
#'   \item \code{.surv_juv_delta_medium}: Size related intercept for medium sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#'   \item \code{.surv_juv_delta_large}: Size related intercept for large sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#' }
#' @section Tributary Migratory Survival:
#' \itemize{
#'   \item \code{cc_gates_prop_days_closed}: More details at \code{\link[DSMflow]{delta_cross_channel_closed}}
#'   \item \code{upper_sacramento_flows}: More details at \code{\link[DSMflow]{upper_sacramento_flows}}
#'   \item \code{freeport_flows}: More details at \code{\link[DSMflow]{freeport_flow}}
#'   \item \code{vernalis_flows}: More details at \code{\link[DSMflow]{vernalis_flow}}
#'   \item \code{stockton_flows}: More details at \code{\link[DSMflow]{stockton_flow}}
#'   \item \code{vernalis_temps}: More details at \code{\link[DSMtemperature]{vernalis_temperature}}
#'   \item \code{prisoners_point_temps}: More details at \code{\link[DSMtemperature]{prisoners_point_temperature}}
#'   \item \code{CVP_exports}: More details at \code{\link[DSMflow]{cvp_exports}}
#'   \item \code{SWP_exports}: More details at \code{\link[DSMflow]{swp_exports}}
#'   \item \code{..surv_juv_outmigration_sj_int} Intercept, source: calibration
#'   \item \code{.surv_juv_outmigration_san_joquin_medium} Size related intercept for medium sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#'   \item \code{.surv_juv_outmigration_san_joaquin_large} Size related intercept for large sized fish, source: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
#'   \item \code{min_survival_rate} Estimated survival rate if temperature threshold is exceeded, source: expert opinion
#'   \item \code{avg_temp}: More details at \code{\link[DSMtemperature]{stream_temperature}}
#'   }
#' @section Delta Migratory Survival:
#' \itemize{
#'   \item \code{delta_inflow}: More details at \code{\link[DSMflow]{delta_inflow}}
#'   \item \code{avg_temp_delta}: More details at \code{\link[DSMtemperature]{delta_temperature}}
#'   \item \code{delta_proportion_diverted}: More details at \code{\link[DSMflow]{delta_proportion_diverted}}
#' }
#' @section Delta Routing and Rearing:
#' \itemize{
#'   \item \code{freeport_flows}: More details at \code{\link[DSMflow]{freeport_flow}}
#'   \item \code{cc_gates_days_closed}: More details at \code{\link[DSMflow]{delta_cross_channel_closed}}
#'   \item \code{growth_rates}: More details at: \code{\link{growth_rates_inchannel}}
#' }
#' @section Tributary Routing:
#' \itemize{
#'   \item \code{prop_pulse_flows}: More details at \code{\link[DSMflow]{proportion_pulse_flows}}
#'   \item \code{proportion_flow_bypass}: More details at \code{\link[DSMflow]{proportion_flow_bypasses}}
#'   \item \code{.pulse_movement_intercept}: Intercept, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#'   \item \code{.pulse_movement_proportion_pulse}: Coefficient for \code{proportion_pulse} variable, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#'   \item \code{.pulse_movement_medium}: Size related intercept for medium sized fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#'   \item \code{.pulse_movement_large}: Size related intercept for large sized fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#'   \item \code{.pulse_movement_vlarge}: Size related intercept for very large sized fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#'   \item \code{.pulse_movement_medium_pulse}: Additional coefficient for \code{proportion_pulse} variable for medium size fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#'   \item \code{.pulse_movement_large_pulse}: Additional coefficient for \code{proportion_pulse} variable for large size fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#'   \item \code{.pulse_movement_very_large_pulse}: Additional coefficient for \code{proportion_pulse} variable for very large size fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.
#'   \item \code{territory_size}: More details at \code{\link{territory_by_size}}
#' }
#' @section Tributary Rearing:
#' \itemize{
#'   \item \code{growth_rates}: More details at: \code{\link{growth_rates_inchannel}}
#'   \item \code{growth_rates_floodplain}: More details at: \code{\link{growth_rates_floodplain}}
#'   \item \code{weeks_flooded}: More details at: \code{\link[DSMhabitat]{weeks_flooded}}
#'
#' }
#' @section Ocean Entry Success:
#' \itemize{
#'   \item \code{.ocean_entry_success_length}: Size related intercept representing the fork lengths for each size classes, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#'   \item \code{..ocean_entry_success_int}: Intercept, source: Calibration (Varies by tributary)
#'   \item \code{.ocean_entry_success_months}: Coefficient for \code{month} variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' }
#' @section Generating Model Outputs:
#' \itemize{
#'   \item \code{mass_by_size_class}: More details at \code{\link{mass_by_size_class}}
#'   \item \code{diversity_group}: More details at \code{\link{diversity_group}}
#' }
#' @usage params
"params"

#' @rdname params
#' @format NULL
"params_2019"

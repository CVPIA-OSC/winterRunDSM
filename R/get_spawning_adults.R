#' @title Get Spawning Adults
#' @description Calculates the number of initial adults, initial natural adults, and initial adults by month.
#' @details See \code{\link{params}} for details on parameter sources
#' @param year The year of simulation
#' @param adults Potential spawning adults for each watershed (length = 31) values must be integer
#' @param hatch_adults Total hatchery adults
#' @param mode A value meant to be inherited to determine if model is in "seed", "calibrate", or "simulate" mode
#' @param month_return_proportions The proportion of fish returning for each month
#' @param prop_flow_natal A dataset containing the proportion flows at tributary junction coming from natal watershed using October CALSIM II flows. These proportions are used to estimate straying in the CVPIA SIT Salmon Population Model. More details at \code{\link[DSMflow]{proportion_flow_natal}}
#' @param south_delta_routed_watersheds Indicator of whether fish have access to a region. More details at \code{\link[DSMhabitat]{south_delta_routed_watersheds}}

#' @param gates_overtopped A monthly TRUE or FALSE value to describe if the gates are overtopped to the bypasses (years 1980-2000) for use with the CVPIA SIT Salmon Population Model to apportion fish onto the bypasses. More details at \code{\link[DSMflow]{gates_overtopped}}
#' @param tisdale_bypass_watershed Indicator of whether fish have access to a region. More details at \code{\link[DSMhabitat]{tisdale_bypass_watershed}}
#' @param yolo_bypass_watershed Indicator of whether fish have access to a region, More details at \code{\link[DSMhabitat]{yolo_bypass_watershed}}
#' @param migratory_temperature_proportion_over_20 The median proportion of days over 20°C per month. More details at \code{\link[DSMtemperature]{migratory_temperature_proportion_over_20}}
#' @param .adult_stray_intercept Intercept for \code{\link{adult_stray}}
#' @param .adult_stray_wild Coefficient for \code{\link{adult_stray}} \code{wild} variable
#' @param .adult_stray_natal_flow  Coefficient for \code{\link{adult_stray}} \code{natal_flow} variable
#' @param .adult_stray_cross_channel_gates_closed  Coefficient for \code{\link{adult_stray}} \code{cross_channel_gates_closed} variable
#' @param .adult_stray_prop_bay_trans  Coefficient for \code{\link{adult_stray}} \code{prop_bay_trans} variable
#' @param .adult_stray_prop_delta_trans  Coefficient for \code{\link{adult_stray}} \code{prop_delta_trans} variable
#' @param ..surv_adult_enroute_int Intercept for \code{\link{surv_adult_enroute}}
#' @param .adult_en_route_migratory_temp Coefficient for \code{\link{surv_adult_enroute}} \code{migratory_temp} variable
#' @param .adult_en_route_bypass_overtopped Coefficient for \code{\link{surv_adult_enroute}} \code{bypass_overtopped} variable
#' @param .adult_en_route_adult_harvest_rate  Adult harvest rate for \code{\link{surv_adult_enroute}}
#' @source IP-117068
#' @export
get_spawning_adults <- function(year, adults, hatch_adults, mode,
                                month_return_proportions,
                                prop_flow_natal,
                                south_delta_routed_watersheds,
                                natural_adult_removal_rate, 
                                gates_overtopped,
                                tisdale_bypass_watershed,
                                yolo_bypass_watershed,
                                migratory_temperature_proportion_over_20,
                                ..surv_adult_enroute_int,
                                .adult_stray_intercept,
                                .adult_stray_wild,
                                .adult_stray_natal_flow,
                                .adult_stray_cross_channel_gates_closed,
                                .adult_stray_prop_bay_trans,
                                .adult_stray_prop_delta_trans,
                                .adult_en_route_migratory_temp,
                                .adult_en_route_bypass_overtopped,
                                .adult_en_route_adult_harvest_rate,
                                stochastic) {

  # during the seeding stage just reuse the seed adults as the input, and apply no
  # en-route survival
  if (mode %in% c("seed", "calibrate")) {
    adult_index <- ifelse(mode == "seed", 1, year)
    adults_by_month <- t(sapply(1:31, function(watershed) {
      if (stochastic) {
        rmultinom(1, adults[watershed, adult_index], month_return_proportions)
      } else {
        round(adults[watershed, adult_index] * month_return_proportions)
      }
    }))

    adults_by_month_hatchery_removed <- sapply(1:4, function(month) {
      if (stochastic) {
        rbinom(n = 31,
               size = round(adults_by_month[, month]),
               prob = 1 - natural_adult_removal_rate)
      } else {
        round(adults_by_month[, month] * (1 - natural_adult_removal_rate))
      }
    })

    init_adults <- rowSums(adults_by_month_hatchery_removed)
    proportion_natural <- 1 - winterRunDSM::params$proportion_hatchery
    init_adults_by_month <- adults_by_month_hatchery_removed

  } else  {

    adults_by_month <- t(sapply(1:31, function(watershed) {
      if (stochastic) {
        rmultinom(1, adults[watershed, year], month_return_proportions)
      } else {
        round(adults[watershed, year] * month_return_proportions)
      }
    }))

    hatchery_by_month <- t(sapply(1:31, function(watershed) {
      if (stochastic) {
        rmultinom(1, hatch_adults[watershed], month_return_proportions)
      } else {
        round(hatch_adults[watershed] * month_return_proportions)
      }
    }))



    # are tisdale or yolo bypasses overtopped?
    # for all years and months 10-12 there is always at least one true

    bypass_is_overtopped <- sapply(1:4, function(month) {

      tis <- gates_overtopped[month, year, 1] * tisdale_bypass_watershed
      yolo <- gates_overtopped[month, year, 2] * yolo_bypass_watershed
      as.logical(tis + yolo)
    })

    en_route_temps <- migratory_temperature_proportion_over_20[, 1:4]

    adult_en_route_surv <- pmin(sapply(1:4, function(month) {
      adult_en_route_surv <- surv_adult_enroute(migratory_temp = en_route_temps[,month],
                                                bypass_overtopped = bypass_is_overtopped[,month],
                                                adult_harvest = .adult_en_route_adult_harvest_rate,
                                                ..surv_adult_enroute_int = ..surv_adult_enroute_int,
                                                .migratory_temp = .adult_en_route_migratory_temp,
                                                .bypass_overtopped = .adult_en_route_bypass_overtopped)
    }), 1)


    adults_survived_to_spawning <- sapply(1:4, function(month) {
      if (stochastic) {
        rbinom(31, round(adults_by_month[, month]), adult_en_route_surv[, month])
      } else {
        round(adults_by_month[, month] * adult_en_route_surv[, month])
      }
    })

    surviving_natural_adults_by_month <- sapply(1:4, function(month) {
      if (stochastic) {
        rbinom(31, round(adults_survived_to_spawning[, month]), (1 - natural_adult_removal_rate))
      } else {
        round(adults_survived_to_spawning[, month] * (1 - natural_adult_removal_rate))
      }
    })

    surviving_hatchery_adults_by_month <- sapply(1:4, function(month) {
      if (stochastic) {
        rbinom(31, round(hatchery_by_month[, month]), adult_en_route_surv[, month])
      } else {
        round(hatchery_by_month[, month] * adult_en_route_surv[, month])
      }
    })

    surviving_natural_adults <- rowSums(surviving_natural_adults_by_month)
    surviving_hatchery_adults <- rowSums(surviving_hatchery_adults_by_month)
    init_adults <- surviving_natural_adults + surviving_hatchery_adults
    init_adults_by_month <- surviving_natural_adults_by_month + surviving_hatchery_adults_by_month
    proportion_natural <- surviving_natural_adults / init_adults

  }


  list(init_adults = init_adults,
       proportion_natural = replace(proportion_natural, is.nan(proportion_natural), 0),
       init_adults_by_month = init_adults_by_month)

}


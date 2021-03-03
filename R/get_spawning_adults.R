#' @title Get Spawning Adults
#' @param year the year of simulation
#' @param adults potential spawning adults for each watershed (length = 31) values must be integer
#' @source IP-117068
#' @export
get_spawning_adults <- function(year, adults, hatch_adults, seeds) {
  
  adults_by_month <- t(sapply(1:31, function(watershed) {
    rmultinom(1, adults[watershed], month_return_proportions)
  }))
  
  if (is.null(seeds)) {
    natural_adults_by_month <- sapply(1:4, function(month) {
      rbinom(n = 31, 
             size = round(adults_by_month[, month]), 
             prob = 1 - natural_adult_removal_rate)
    })
    
    init_adults <- rowSums(natural_adults_by_month)
    surviving_natural_adults <- rowSums(adults_by_month)
    proportion_natural <- 1 - proportion_hatchery
    init_adults_by_month <- natural_adults_by_month
    
    
  } else  {
    hatchery_by_month <- t(sapply(1:31, function(watershed) {
      rmultinom(1, hatch_adults[watershed], month_return_proportions)
    }))
    # No Straying in WinterRunDSM We will use adult_by_month 
    
    # are tisdale or yolo bypasses overtopped?
    # for all years and months 10-12 there is always at least one true
    bypass_is_overtopped <- sapply(1:4, function(month) {
      
      tis <- gates_overtopped[month, year, 1] * tisdale_bypass_watershed
      yolo <- gates_overtopped[month, year, 2] * yolo_bypass_watershed
      as.logical(tis + yolo)
    })
    
    en_route_temps <- migratory_temperature_proportion_over_20[, 1:4]
    
    adult_en_route_surv <- sapply(1:4, function(month) {
      adult_en_route_surv <- surv_adult_enroute(migratory_temp = en_route_temps[,month],
                                                bypass_overtopped = bypass_is_overtopped[,month],
                                                adult_harvest = adult_harvest_rate)  
    })
    
    adults_survived_to_spawning <- sapply(1:4, function(month) {
      rbinom(31, round(adults_by_month[, month]), adult_en_route_surv[, month])
    })
    
    surviving_natural_adults_by_month <- sapply(1:4, function(month) {
      rbinom(31, round(adults_survived_to_spawning[, month]), (1 - natural_adult_removal_rate))
    })
    
    surviving_hatchery_adults_by_month <- sapply(1:4, function(month) {
      rbinom(31, round(hatchery_by_month[, month]), adult_en_route_surv[, month])
    })
    
    surviving_natural_adults <- rowSums(surviving_natural_adults_by_month)
    surviving_hatchery_adults <- rowSums(surviving_hatchery_adults_by_month)
    init_adults <- surviving_natural_adults + surviving_hatchery_adults
    init_adults_by_month <- surviving_natural_adults_by_month + surviving_hatchery_adults_by_month
    proportion_natural <- surviving_natural_adults / init_adults
    
  }
  
  list(init_adults = init_adults,
       proportion_natural = replace(proportion_natural, is.nan(proportion_natural), NA_real_),
       natural_adults = surviving_natural_adults, 
       init_adults_by_month = init_adults_by_month)
}




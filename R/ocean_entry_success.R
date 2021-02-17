#' @title Ocean Entry Success
#' @description Calculates the number of juveniles that survive entering the ocean
#' @param migrants The number of juveniles at golden gate bridge
#' @param month The current simulation month
#' @param avg_ocean_transition_month The average month juveniles transition to the ocean
#' @param length Fork lengths for each size classes. \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' @param betas Parameters estimated through calibration
#' @section Parameters:
#' Parameters from the model are obtained from either literature, calibration, export elicitation,
#' and meta-analysis. The source for each parameter in this function are detailed below.
#' \itemize{
#' \item intercept 1-31: calibration estimate; varies by tributary
#' \item months: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' }
#' @source IP-117068
#' @export
ocean_entry_success <- function(migrants, month, avg_ocean_transition_month,
                                length = c(-0.0897309864, -0.0709704348, -0.0208590732, 0.0732620916),
                                betas = c(`intercept 1` = -2.9839253, `intercept 2` = -2.9839253, `intercept 3` = -2.9839253,
                                          `intercept 4` = -2.9839253, `intercept 5` = -2.9839253, `intercept 6` = -2.9839253,
                                          `intercept 7` = -2.9839253, `intercept 8` = -2.9839253, `intercept 9` = -2.9839253,
                                          `intercept 10` = -2.9839253, `intercept 11` = -2.9839253, `intercept 12` = -2.9839253,
                                          `intercept 13` = -2.9839253, `intercept 14` = -2.9839253, `intercept 15` = -2.9839253,
                                          `intercept 16` = -2.9839253, `intercept 17` = -2.9839253, `intercept 18` = -2.9839253,
                                          `intercept 19` = -2.9839253, `intercept 20` = -2.9839253, `intercept 21` = -2.9839253,
                                          `intercept 22` = -2.9839253, `intercept 23` = -2.9839253, `intercept 24` = -2.9839253,
                                          `intercept 25` = -2.9839253, `intercept 26` = -2.9839253, `intercept 27` = -2.9839253,
                                          `intercept 28` = -2.9839253, `intercept 29` = -2.9839253, `intercept 30` = -2.9839253,
                                          `intercept 31` = -2.9839253, months = 0.35)){

  month_since <- ifelse(month <= avg_ocean_transition_month, 0, max(1, month - avg_ocean_transition_month))

  survival_rate <- NULL
  for(i in 1:dim(migrants)[1]) {
    survival_rate <- rbind(survival_rate,
                           boot::inv.logit(betas[i] + betas[32] * month_since + length))
  }

  if (month_since == 0) rep(0, 31) else rowSums(round(survival_rate * migrants))

}


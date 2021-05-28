#' @title Ocean Entry Success
#' @description Calculates the number of juveniles that survive entering the ocean
#' @param migrants Variable representing the number of juveniles at golden gate bridge
#' @param month Variable representing the current simulation month
#' @param avg_ocean_transition_month Variable representing the average month juveniles transition to the ocean
#' @param length Variable representing the fork lengths for each size classes. \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' @param ..ocean_entry_success_int Intercept, source: Calibration (Varies by tributary )
#' @param .month Coefficient for \code{month} variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' @source IP-117068
#' @export
ocean_entry_success <- function(migrants, month, avg_ocean_transition_month,
                                length = c(-0.0897309864, -0.0709704348, -0.0208590732, 0.0732620916),
                                ..ocean_entry_success_int =  c(
                                  `Upper Sacramento River` = -2.9839253, 
                                  `Antelope Creek` = -2.9839253, 
                                  `Battle Creek` = -2.9839253, 
                                  `Bear Creek` = -2.9839253, 
                                  `Big Chico Creek` = -2.9839253, 
                                  `Butte Creek` = -2.9839253, 
                                  `Clear Creek` = -2.9839253, 
                                  `Cottonwood Creek` = -2.9839253, 
                                  `Cow Creek` = -2.9839253, 
                                  `Deer Creek` = -2.9839253, 
                                  `Elder Creek` = -2.9839253, 
                                  `Mill Creek` = -2.9839253, 
                                  `Paynes Creek` = -2.9839253, 
                                  `Stony Creek` = -2.9839253, 
                                  `Thomes Creek` = -2.9839253, 
                                  `Upper-mid Sacramento River` = -2.9839253, 
                                  `Sutter Bypass` = -2.9839253, 
                                  `Bear River` = -2.9839253, 
                                  `Feather River` = -2.9839253, 
                                  `Yuba River` = -2.9839253, 
                                  `Lower-mid Sacramento River` = -2.9839253, 
                                  `Yolo Bypass` = -2.9839253, 
                                  `American River` = -2.9839253, 
                                  `Lower Sacramento River` = -2.9839253, 
                                  `Calaveras River` = -2.9839253, 
                                  `Cosumnes River` = -2.9839253, 
                                  `Mokelumne River` = -2.9839253, 
                                  `Merced River` = -2.9839253, 
                                  `Stanislaus River` = -2.9839253, 
                                  `Tuolumne River` = -2.9839253, 
                                  `San Joaquin River` = -2.9839253),
                                 .month = 0.35){

  month_since <- ifelse(month <= avg_ocean_transition_month, 0, max(1, month - avg_ocean_transition_month))

  survival_rate <- NULL
  for(i in 1:dim(migrants)[1]) {
    survival_rate <- rbind(survival_rate,
                           boot::inv.logit(..ocean_entry_success_int[[i]] + .month * month_since + length))
  }

  if (month_since == 0) rep(0, 31) else rowSums(round(survival_rate * migrants))

}


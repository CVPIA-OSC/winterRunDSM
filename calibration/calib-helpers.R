monitor_model_fit <- function(object) {
  sol <- object@solution
  
  params
  preds <- winter_run_model(mode = "calibrate",
                            seeds = DSMCalibrationData::grandtab_imputed$winter,
                            stochastic = FALSE,
                            ..params = params_init)
  keep <- c(1, 3)
  watershed_cor <- sapply(1:length(keep), function(i) {
    cor(preds[i,], known_nats[i,], use = "pairwise.complete.obs")
  })
  
  print(watershed_cor)
  watershed_cor
}

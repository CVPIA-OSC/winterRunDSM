library(testthat)
library(winterRunDSM)
# tests for survival functions
# Lists inputs to use in testing
list2env(load_baseline_data(), envir = .GlobalEnv)
year <- 1
month <- 9
aveT20 <- c(0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L,
            0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 1L)
maxT25 <- c(0L, 0L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L,
            1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L)
aveT20D <- c(1L, 1L)
maxT25D <- 0:1
high_predation <- c(1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L,
                    0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L)
ws_strand <- c(1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
               0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L)
betas <- as.matrix(survival_betas[, 3:16])
bp_survival_betas <- as.matrix(survival_betas[c(17, 22), c(3, 4, 5, 13, 14, 15)])

# Tests surv_juv_rear survival function
expected_surv_juv_rear <- list(inchannel = structure(c(0.0552891931835547, 0.204516509881086, 
                                                       0.350850830114141, 1), .Dim = c(1L, 4L), 
                                                     .Dimnames = list(NULL,  c("s", "m", "l", "vl"))), 
                               floodplain = structure(c(0.153045556355774,  0.395165627345185, 0.552992878491198, 1), .Dim = c(1L, 4L), 
                                                     .Dimnames = list( "Upper Sacramento River", c("s", "m", "l", "vl"))))

test_that('The surv_juv_rear function returns the expected values for year 1 month 9 watershed 1', {
  expect_equal(surv_juv_rear(max_temp_thresh = maxT25[1],
                             avg_temp_thresh = aveT20[1],
                             high_predation = high_predation[1],
                             contact_points = contact_points[1],
                             prop_diversions = proportion_diverted[1],
                             total_diversions = total_diverted[1],
                             stranded = ws_strand[1],
                             weeks_flooded = weeks_flooded[, month, year][1]),
               expected_surv_juv_rear)
})

# Tests surv_juv_delta survival function
expected_delta_juv_surv <- structure(c(0.120086786831867, 1e-04, 0.374816513483691, 1e-04, 
                                       0.55759112300614, 1e-04, 1, 1), 
                                     .Dim = c(2L, 4L), 
                                     .Dimnames = list(NULL, c("s", "m", "l", "vl")))

test_that('The delta_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_delta(max_temp_thresh = maxT25D,
                              avg_temp_thresh = aveT20D,
                              high_predation = delta_prop_high_predation,
                              contact_points = delta_contact_points,
                              prop_diverted = delta_proportion_diverted,
                              total_diverted = delta_total_diverted),
               expected_delta_juv_surv)
})

# Tests surv_juv_bypass survival function
expected_bypass_juv_surv <- structure(c(1e-04, 1e-04, 1e-04, 1), .Dim = c(1L, 4L), .Dimnames = list(
  NULL, c("s", "m", "l", "vl")))

test_that('The bypass_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_bypass(max_temp_thresh = maxT25[22],
                               avg_temp_thresh = aveT20[22],
                               high_predation = 0),
               expected_bypass_juv_surv)
})

# Tests migratory survival for lower mid sac fish survival function
expected_lms_mig_surv <- structure(c(0.986238485681342, 0.996799333575431, 0.998472365549382, 
                                     0.998472365549382), .Dim = c(1L, 4L), .Dimnames = list(NULL, 
                                                                                            c("s", "m", "l", "vl")))

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac(flow_cms = upper_sacramento_flows[month, year],
                                         avg_temp = avg_temp[21, month, year],
                                         total_diversions = total_diverted[21],
                                         prop_diversions = proportion_diverted[21]),
               expected_lms_mig_surv)
})


# Tests migratory survival for san joaquin fish survival function
expected_lms_mig_surv <- structure(c(0.0465823740391211, 0.176705306337067, 0.310918056813447, 
                                     0.310918056813447), .Dim = c(1L, 4L), .Dimnames = list(NULL, 
                                                                                            c("s", "m", "l", "vl")))

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_san_joaquin(),
               expected_lms_mig_surv)
})

# Tests migratory survival for sac delta outmigration survival function
expected_sac_delta_mig_surv <- c(s = 0.362239720665406, m = 0.443043568529653, l = 0.526436871636591, 
                                 vl = 0.526436871636591)
test_that('The migratory_juv_surv function for sac delta returns the expected values for row one of year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac_delta(delta_flow = delta_inflow[month, year, ],
                                               avg_temp = avg_temp_delta[month, year, ],
                                               perc_diversions = delta_proportion_diverted * 100)[1,],
               expected_sac_delta_mig_surv)
})
# tests the surv_juv_outmigration_delta function'
expected_surv_juv_outmigration <- structure(c(0.266668614822945, 2.26283033759458e-26, 1.49657237445669e-25,
                                              3.67469661043849e-14, 0.266668614822945, 2.26283033759458e-26,
                                              1.49657237445669e-25, 3.67469661043849e-14, 0.266668614822945,
                                              2.26283033759458e-26, 1.49657237445669e-25, 3.67469661043849e-14,
                                              0.373914118050784, 4.49218800782043e-26, 2.2667851513676e-25,
                                              8.17576203365024e-14), .Dim = c(4L, 4L),
                                            .Dimnames = list(c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish"),
                                                             c("s", "m", "l", "vl")))
test_that('tests that the surv_juv_outmigration_delta function returns the correct value', {
  expect_equal(surv_juv_outmigration_delta(prop_DCC_closed = cc_gates_prop_days_closed[month],
                                           hor_barr = 0,
                                           freeport_flow = freeport_flows[month, year],
                                           vernalis_flow = vernalis_flows[month, year],
                                           stockton_flow = stockton_flows[month, year],
                                           vernalis_temperature = vernalis_temps[month, year],
                                           prisoners_point_temperature = prisoners_point_temps[month, year],
                                           CVP_exp = CVP_exports[month, year],
                                           SWP_exp = SWP_exports[month, year],
                                           trap_trans = 0),
               expected_surv_juv_outmigration)
})

## Tests survival functions with randomness (set.seed() for testing these)
# Tests the rearing survival rates function
expected_survival <- list(inchannel = structure(c(0.33376848093276, 1e-04, 1e-04, 
                                                  1e-04, 1e-04, 1e-04, 0.306904430242205, 1e-04, 1e-04, 1e-04, 
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.055572252286752, 
                                                  0.0754030226499081, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.0495134672031533, 
                                                  1e-04, 1e-04, 1e-04, 0.226102447436398, 1e-04, 1e-04, 0.687575831609784, 
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.660465314415322, 1e-04, 
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
                                                  0.205397450150183, 0.263761160051432, 1e-04, 1e-04, 1e-04, 1e-04, 
                                                  1e-04, 0.186224848296393, 1e-04, 1e-04, 1e-04, 0.562065417459115, 
                                                  1e-04, 1e-04, 0.822271034960882, 1e-04, 1e-04, 1e-04, 1e-04, 
                                                  1e-04, 0.803508485717392, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.352083111658519, 0.429593321564825, 
                                                  1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.324815360087302, 1e-04, 
                                                  1e-04, 1e-04, 0.72959104428737, 1e-04, 1e-04, 1, 1, 1, 1, 1, 
                                                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                  1, 1, 1, 1, 1), .Dim = c(31L, 4L)), 
                          floodplain = structure(c(0.389130486955771, 
                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.360580450548609, 1e-04, 
                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
                                                   0.055572252286752, 0.423644133176015, 1e-04, 1e-04, 1e-04, 1e-04, 
                                                   1e-04, 0.0495134672031533, 1e-04, 1e-04, 1e-04, 0.226102447436398, 
                                                   1e-04, 1e-04, 0.734605478032945, 1e-04, 1e-04, 1e-04, 1e-04, 
                                                   1e-04, 0.710133470086752, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.205397450150183, 0.763536758601585, 
                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.186224848296393, 1e-04, 
                                                   1e-04, 1e-04, 0.562065417459115, 1e-04, 1e-04, 0.852887833925524, 
                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.836861364253675, 1e-04, 
                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
                                                   0.352083111658519, 0.871598689284835, 1e-04, 1e-04, 1e-04, 1e-04, 
                                                   1e-04, 0.324815360087302, 1e-04, 1e-04, 1e-04, 0.72959104428737, 
                                                   1e-04, 1e-04, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(31L, 4L
                                                   )), 
                          sutter = structure(c(0.01, 0.01, 0.01, 1), .Dim = c(1L, 4L), 
                                             .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
                          yolo = structure(c(0.01,  0.01, 0.01, 1), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s",  "m", "l", "vl"))), 
                          delta = structure(c(0.119014854512939, 1e-04, 0.372433194151987, 1e-04, 0.555077478858059, 1e-04, 1, 1), 
                                            .Dim = c(2L,  4L), 
                                            .Dimnames = list(NULL, c("s", "m", "l", "vl"))))

test_that("get_rearing_survival returns the expected result", {
  set.seed(2021)
  survival <- get_rearing_survival_rates(year = year, month = month, scenario = 0)
  expect_equal(survival, expected_survival)
})

expected_migratory_survival <- list(delta = structure(c(0.266668614822945, 2.26283033759458e-26, 
                                                        1.49657237445669e-25, 3.67469661043849e-14, 0.266668614822945, 
                                                        2.26283033759458e-26, 1.49657237445669e-25, 3.67469661043849e-14, 
                                                        0.266668614822945, 2.26283033759458e-26, 1.49657237445669e-25, 
                                                        3.67469661043849e-14, 0.373914118050784, 4.49218800782043e-26, 
                                                        2.2667851513676e-25, 8.17576203365024e-14), 
                                                      .Dim = c(4L, 4L), 
                                                      .Dimnames = list(
                                                          c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish", 
                                                            "southern_fish"), c("s", "m", "l", "vl"))), 
                                    san_joaquin = structure(c(0.0465823740391211, 0.176705306337067, 0.310918056813447, 0.310918056813447), 
                                                            .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
                                    uppermid_sac = structure(c(0.989615264655101, 0.997567906218805, 0.998837910610785, 0.998837910610785), 
                                                             .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
                                    lowermid_sac = structure(c(0.993095406132433, 0.998398384201132, 0.999235890843289, 0.999235890843289), 
                                                             .Dim = c(1L,  4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
                                    lower_sac = structure(c(0.992550225010562, 0.998269329007175, 0.999174124474189, 0.999174124474189), 
                                                          .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
                                    sutter = structure(c(0.01, 0.01, 0.01, 1), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
                                    yolo = structure(c(0.01, 0.01, 0.01, 1), .Dim = c(1L,  4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))))

test_that("get_migratory_survival returns the expected result", {
  set.seed(2021)
  migratory_survival <- get_migratory_survival_rates(year = year, month = month)[1:7]
  expect_equal(migratory_survival, expected_migratory_survival)
})

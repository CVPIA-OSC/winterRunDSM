library(testthat)
library(winterRunDSM)
# tests for survival functions
# Lists inputs to use in testing
test_data <- winterRunDSM::load_baseline_data()
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
                             contact_points = test_data$contact_points[1],
                             prop_diversions = test_data$proportion_diverted[1],
                             total_diversions = test_data$total_diverted[1],
                             stranded = ws_strand[1],
                             weeks_flooded = test_data$weeks_flooded[, month, year][1]),
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
                              high_predation = test_data$delta_prop_high_predation,
                              contact_points = test_data$delta_contact_points,
                              prop_diverted = test_data$delta_proportion_diverted,
                              total_diverted = test_data$delta_total_diverted),
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
  expect_equal(surv_juv_outmigration_sac(flow_cms = test_data$upper_sacramento_flows[month, year],
                                         avg_temp = test_data$avg_temp[21, month, year],
                                         total_diversions = test_data$total_diverted[21],
                                         prop_diversions = test_data$proportion_diverted[21]),
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
  expect_equal(surv_juv_outmigration_sac_delta(delta_flow = test_data$delta_inflow[month, year, ],
                                               avg_temp = test_data$avg_temp_delta[month, year, ],
                                               perc_diversions = test_data$delta_proportion_diverted * 100)[1,],
               expected_sac_delta_mig_surv)
})


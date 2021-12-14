library(testthat)
library(winterRunDSM)

# Model state -------
year <- 1
month <- 9

# Test constants -----
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

# Tests surv_juv_rear survival function
expected_surv_juv_rear <- list(inchannel = structure(c(0.162604214178863, 0.460339070349737, 
                                                       0.641992409821651, 1), .Dim = c(1L, 4L), 
                                                     .Dimnames = list("Upper Sacramento River", c("s", "m", "l", "vl"))), 
                               floodplain = structure(c(0.238706799591519, 
                                                                                                                                                                    0.568977105205772, 0.73121289582857, 1), .Dim = c(1L, 4L), .Dimnames = list(
                                                                                                                                                                      "Upper Sacramento River", c("s", "m", "l", "vl"))))

test_that('The surv_juv_rear function returns the expected values for year 1 month 9 watershed 1', {
  expect_equal(surv_juv_rear(max_temp_thresh = maxT25[1],
                             avg_temp_thresh = aveT20[1],
                             high_predation = high_predation[1],
                             contact_points = params$contact_points[1],
                             prop_diversions = params$proportion_diverted[1],
                             total_diversions = params$total_diverted[1],
                             stranded = ws_strand[1],
                             weeks_flooded = params$weeks_flood[1],
                             ..surv_juv_rear_int = params$..surv_juv_rear_int[1],
                             .surv_juv_rear_contact_points = params$.surv_juv_rear_contact_points,
                             ..surv_juv_rear_contact_points = params$..surv_juv_rear_contact_points,
                             .surv_juv_rear_prop_diversions = params$.surv_juv_rear_prop_diversions,
                             ..surv_juv_rear_prop_diversions = params$..surv_juv_rear_prop_diversions,
                             .surv_juv_rear_total_diversions = params$.surv_juv_rear_total_diversions,
                             ..surv_juv_rear_total_diversions = params$..surv_juv_rear_total_diversions,
                             .avg_temp_thresh = params$.surv_juv_rear_avg_temp_thresh,
                             .high_predation = params$.surv_juv_rear_high_predation,
                             .stranded = params$.surv_juv_rear_stranded,
                             .medium = params$.surv_juv_rear_medium,
                             .large = params$.surv_juv_rear_large,
                             .floodplain = params$.surv_juv_rear_floodplain,
                             min_survival_rate = params$min_survival_rate,
                             stochastic = FALSE),
               expected_surv_juv_rear)
})

expected_delta_juv_surv <- structure(c(0.0932457862245425, 1e-04, 0.0932457862245425, 1e-04, 
                                       0.0932457862245425, 1e-04, 1, 1), .Dim = c(2L, 4L), .Dimnames = list(
                                         c("North Delta", "South Delta"), c("s", "m", "l", "vl")))

test_that('The delta_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_delta(avg_temp = params$avg_temp_delta[month, year, "North Delta"],
                              max_temp_thresh = maxT25D,
                              avg_temp_thresh = aveT20D,
                              high_predation = params$delta_prop_high_predation,
                              contact_points = params$delta_contact_points,
                              prop_diverted = params$delta_proportion_diverted,
                              total_diverted = params$delta_total_diverted,
                              stochastic = FALSE),
               expected_delta_juv_surv)
})

# Tests surv_juv_bypass survival function
expected_bypass_juv_surv <- structure(c(1e-04, 1e-04, 1e-04, 1), .Dim = c(1L, 4L), .Dimnames = list(
  NULL, c("s", "m", "l", "vl")))

test_that('The bypass_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_bypass(max_temp_thresh = maxT25[22],
                               avg_temp_thresh = aveT20[22],
                               high_predation = 0, 
                               stochastic = FALSE),
               expected_bypass_juv_surv)
})

# Tests migratory survival for lower mid sac fish survival function
expected_lms_mig_surv <- c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189)

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac(flow_cms = params$upper_sacramento_flows[month, year]),
               expected_lms_mig_surv)
})


# Tests migratory survival for san joaquin fish survival function
expected_lms_mig_surv <- structure(c(0.0426739282499506, 0.163754222436521, 0.291614460825888, 
                                     0.291614460825888), .Dim = c(1L, 4L), .Dimnames = list(NULL, 
                                                                                            c("s", "m", "l", "vl")))

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_san_joaquin(),
               expected_lms_mig_surv)
})

# tests the surv_juv_outmigration_delta function'
expected_surv_juv_outmigration <- structure(c(0.266668614822945, 0.000123932662831837, 0.000819655793037249, 
                                              0.00566155265467863, 0.266668614822945, 0.000123932662831837, 
                                              0.000819655793037249, 0.00566155265467863, 0.266668614822945, 
                                              0.000123932662831837, 0.000819655793037249, 0.00566155265467863, 
                                              0.373914118050784, 0.000245928323835351, 0.00124096914866476, 
                                              0.0110614050155086), .Dim = c(4L, 4L), .Dimnames = list(c("northern_fish", 
                                                                                                        "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish"), 
                                                                                                      c("s", "m", "l", "vl")))
test_that('tests that the surv_juv_outmigration_delta function returns the correct value', {
  expect_equal(surv_juv_outmigration_delta(prop_DCC_closed = params$cc_gates_prop_days_closed[month],
                                           hor_barr = 0,
                                           freeport_flow = params$freeport_flows[month, year],
                                           vernalis_flow = params$vernalis_flows[month, year],
                                           stockton_flow = params$stockton_flows[month, year],
                                           vernalis_temperature = params$vernalis_temps[month, year],
                                           prisoners_point_temperature = params$prisoners_point_temps[month, year],
                                           CVP_exp = params$CVP_exports[month, year],
                                           SWP_exp = params$SWP_exports[month, year],
                                           trap_trans = 0),
               expected_surv_juv_outmigration)
})


## Tests survival functions with randomness (set.seed() for testing these)
# Tests the rearing survival rates function
expected_survival <- list(
  inchannel = structure(c(0.19105646240334, 0.0903106834397182, 
                          0.227352425068896, 0.00470808360749883, 0.000470730266560436, 
                          0.000183743388259278, 0.296736784171575, 0.000151947952164085, 
                          0.000159348212670315, 0.00284223757262566, 0.0423244710223926, 
                          0.00233316776626047, 0.204504254366209, 0.00010996826353569, 
                          0.000104442483332336, 0.0110652816548986, 0.00816289086391725, 
                          0.245105662845862, 0.81730002652267, 0.0555732200667039, 0.000539402803479637, 
                          0.00198965732377334, 0.0971789761539614, 0.00376946886456134, 
                          0.737554918428867, 0.000125508191213084, 0.208791529610193, 0.0556605182829735, 
                          0.917799052278001, 0.00801061570581054, 0.00039203668840081, 
                          0.509208927824392, 0.302124508337265, 0.563819574895494, 0.0123414586943736, 
                          0.00116593548435334, 0.00041885051145088, 0.649311954230049, 
                          0.000252723139391976, 0.000266615324056018, 0.00938735098448447, 
                          0.16072035490072, 0.00800554138735826, 0.515711471275756, 0.000132134674886561, 
                          0.000117798547715757, 0.0245544942734344, 0.0195008768018913, 
                          0.587543217406256, 0.951574631606697, 0.147070308231845, 0.000539403543819836, 
                          0.00481023532300308, 0.228635617672199, 0.00394522815487296, 
                          0.917371500589544, 0.000165229720906251, 0.266811636962257, 0.0732121698991489, 
                          0.975447296381243, 0.0135462779721192, 0.000393617133355755, 
                          0.685645602793643, 0.473878051979194, 0.730995327970585, 0.0165510938367965, 
                          0.00160055965017946, 0.000662382558193865, 0.795345176785038, 
                          0.000318058210466938, 0.000331146198012673, 0.0148065722039788, 
                          0.28313194715589, 0.0131038626247472, 0.67427799517504, 0.000148939156075978, 
                          0.000133157588704117, 0.0302928879058856, 0.024885226739424, 
                          0.749317587057568, 0.976362493470362, 0.197319414551394, 0.000539403658225568, 
                          0.00622186531608584, 0.289092646353825, 0.003973901398078, 0.953286389801036, 
                          0.00018589150958259, 0.278784181144195, 0.0769642694186976, 0.985008089139241, 
                          0.0151764346920268, 0.000393862888172669, 1, 1, 1, 1, 1, 1, 1, 
                          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                          1, 1, 1), .Dim = c(31L, 4L)), 
  floodplain = structure(c(0.265448569115345, 
                           0.0903106834397182, 0.227352425068896, 0.00470808360749883, 0.000470730266560436, 
                           0.000183743388259278, 0.322218400009836, 0.000170554194272255, 
                           0.000159348212670315, 0.00284223757262566, 0.0423244710223926, 
                           0.00233316776626047, 0.204504254366209, 0.00010996826353569, 
                           0.000104442483332336, 0.0119492896443239, 0.00816289086391725, 
                           0.245105662845862, 0.358219301766469, 0.0555732200667039, 0.000333834774669483, 
                           0.00198965732377334, 0.115613424611305, 0.00376946886456134, 
                           0.737554918428867, 0.000125508191213084, 0.146919304323206, 0.0556605182829735, 
                           0.568565306560353, 0.00801061570581054, 0.00017101236590705, 
                           0.605347462786082, 0.302124508337265, 0.563819574895494, 0.0123414586943736, 
                           0.00116593548435334, 0.00041885051145088, 0.675366298477467, 
                           0.000282037094435147, 0.000266615324056018, 0.00938735098448447, 
                           0.16072035490072, 0.00800554138735826, 0.515711471275756, 0.000132134674886561, 
                           0.000117798547715757, 0.0254975046740999, 0.0195008768018913, 
                           0.587543217406256, 0.710310060880384, 0.147070308231845, 0.000438761282198813, 
                           0.00481023532300308, 0.249057273861446, 0.00394522815487296, 
                           0.917371500589544, 0.000165229720906251, 0.234304760788797, 0.0732121698991489, 
                           0.827812907860444, 0.0135462779721192, 0.000271487491096558, 
                           0.760556894717273, 0.473878051979194, 0.730995327970585, 0.0165510938367965, 
                           0.00160055965017946, 0.000662382558193865, 0.813480208767092, 
                           0.000343634831813454, 0.000331146198012673, 0.0148065722039788, 
                           0.28313194715589, 0.0131038626247472, 0.67427799517504, 0.000148939156075978, 
                           0.000133157588704117, 0.0309605234793116, 0.024885226739424, 
                           0.749317587057568, 0.837518409515226, 0.197319414551394, 0.00048072028407856, 
                           0.00622186531608584, 0.303820382420823, 0.003973901398078, 0.953286389801036, 
                           0.00018589150958259, 0.260251777600285, 0.0769642694186976, 0.904244024585805, 
                           0.0151764346920268, 0.000319457383263853, 1, 1, 1, 1, 1, 1, 1, 
                           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                           1, 1, 1), .Dim = c(31L, 4L)), 
  sutter = structure(c(0.00148468764845661, 
                       0.00400310810216878, 0.00552798259483219, 1), .Dim = c(1L, 4L
                       ), .Dimnames = list("Yolo Bypass", c("s", "m", "l", "vl"))), 
  yolo = structure(c(0.00148468764845661, 0.00400310810216878, 
                     0.00552798259483219, 1), .Dim = c(1L, 4L), .Dimnames = list(
                       "Yolo Bypass", c("s", "m", "l", "vl"))), delta = structure(c(0.0932457862245425, 
                                                                                    1.09977606108503e-07, 0.0932457862245425, 1.09977606108503e-07, 
                                                                                    0.0932457862245425, 1.09977606108503e-07, 1, 1), .Dim = c(2L, 
                                                                                                                                              4L), .Dimnames = list(c("North Delta", "South Delta"), c("s", 
                                                                                                                                                                                                       "m", "l", "vl"))))

test_that("get_rearing_survival returns the expected result", {
  survival <- get_rearing_survival(year, month,
                                   survival_adjustment = scenario_data$survival_adjustment,
                                   mode = "seed",
                                   avg_temp = params$avg_temp,
                                   avg_temp_delta = params$avg_temp_delta,
                                   prob_strand_early = params$prob_strand_early,
                                   prob_strand_late = params$prob_strand_late,
                                   proportion_diverted = params$proportion_diverted,
                                   total_diverted = params$total_diverted,
                                   delta_proportion_diverted = params$delta_proportion_diverted,
                                   delta_total_diverted = params$delta_total_diverted,
                                   weeks_flooded = params$weeks_flooded,
                                   prop_high_predation = params$prop_high_predation,
                                   contact_points = params$contact_points,
                                   delta_contact_points = params$delta_contact_points,
                                   delta_prop_high_predation = params$delta_prop_high_predation,
                                   ..surv_juv_rear_int= params$..surv_juv_rear_int,
                                   .surv_juv_rear_contact_points = params$.surv_juv_rear_contact_points,
                                   ..surv_juv_rear_contact_points = params$..surv_juv_rear_contact_points,
                                   .surv_juv_rear_prop_diversions = params$.surv_juv_rear_prop_diversions,
                                   ..surv_juv_rear_prop_diversions = params$..surv_juv_rear_prop_diversions,
                                   .surv_juv_rear_total_diversions = params$.surv_juv_rear_total_diversions,
                                   ..surv_juv_rear_total_diversions = params$..surv_juv_rear_total_diversions,
                                   ..surv_juv_bypass_int = params$..surv_juv_bypass_int,
                                   ..surv_juv_delta_int = params$..surv_juv_delta_int,
                                   .surv_juv_delta_contact_points = params$.surv_juv_delta_contact_points,
                                   ..surv_juv_delta_contact_points = params$..surv_juv_delta_contact_points,
                                   .surv_juv_delta_total_diverted = params$.surv_juv_delta_total_diverted,
                                   ..surv_juv_delta_total_diverted = params$..surv_juv_delta_total_diverted,
                                   .surv_juv_rear_avg_temp_thresh = params$.surv_juv_rear_avg_temp_thresh,
                                   .surv_juv_rear_high_predation = params$.surv_juv_rear_high_predation,
                                   .surv_juv_rear_stranded = params$.surv_juv_rear_stranded,
                                   .surv_juv_rear_medium = params$.surv_juv_rear_medium,
                                   .surv_juv_rear_large = params$.surv_juv_rear_large,
                                   .surv_juv_rear_floodplain = params$.surv_juv_rear_floodplain,
                                   .surv_juv_bypass_avg_temp_thresh = params$.surv_juv_bypass_avg_temp_thresh,
                                   .surv_juv_bypass_high_predation = params$.surv_juv_bypass_high_predation,
                                   .surv_juv_bypass_medium = params$.surv_juv_bypass_medium,
                                   .surv_juv_bypass_large = params$.surv_juv_bypass_large,
                                   .surv_juv_bypass_floodplain = params$.surv_juv_bypass_floodplain,
                                   .surv_juv_delta_avg_temp_thresh = params$.surv_juv_delta_avg_temp_thresh,
                                   .surv_juv_delta_high_predation = params$.surv_juv_delta_high_predation,
                                   .surv_juv_delta_prop_diverted = params$.surv_juv_delta_prop_diverted,
                                   .surv_juv_delta_medium = params$.surv_juv_delta_medium,
                                   .surv_juv_delta_large = params$.surv_juv_delta_large,
                                   min_survival_rate = params$min_survival_rate,
                                   stochastic = FALSE)
  expect_equal(survival, expected_survival)
})

expected_migratory_survival <- list(
  uppermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189), 
  lowermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189), 
  lower_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189), 
  sutter = structure(c(0.0385316447670822, 0.0632701201371452, 
                       0.0743504041336172, 1), .Dim = c(1L, 4L), .Dimnames = list(
                         "Yolo Bypass", c("s", "m", "l", "vl"))), yolo = structure(c(0.0385316447670822, 
                                                                                     0.0632701201371452, 0.0743504041336172, 1), .Dim = c(1L, 
                                                                                                                                          4L), .Dimnames = list("Yolo Bypass", c("s", "m", "l", "vl"
                                                                                                                                          ))), san_joaquin = structure(c(0.0426739282499506, 0.163754222436521, 
                                                                                                                                                                         0.291614460825888, 0.291614460825888), .Dim = c(1L, 4L), .Dimnames = list(
                                                                                                                                                                           NULL, c("s", "m", "l", "vl"))), delta = structure(c(0.266668614822945, 
                                                                                                                                                                                                                               0.000123932662831837, 0.000819655793037249, 0.00566155265467863, 
                                                                                                                                                                                                                               0.266668614822945, 0.000123932662831837, 0.000819655793037249, 
                                                                                                                                                                                                                             0.00566155265467863, 0.266668614822945, 0.000123932662831837, 
                                                                                                                                                                                                                             0.000819655793037249, 0.00566155265467863, 0.373914118050784, 
                                                                                                                                                                                                                             0.000245928323835351, 0.00124096914866476, 0.0110614050155086
                                                                                                                                                                         ), .Dim = c(4L, 4L), .Dimnames = list(c("northern_fish", 
                                                                                                                                                                                                                 "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish"
                                                                                                                                                                         ), c("s", "m", "l", "vl"))), bay_delta = 0.358)
test_that("get_migratory_survival returns the expected result", {
  migratory_survival <- get_migratory_survival(year, month,
                                               cc_gates_prop_days_closed = params$cc_gates_prop_days_closed,
                                               freeport_flows = params$freeport_flows,
                                               vernalis_flows = params$vernalis_flows,
                                               stockton_flows = params$stockton_flows,
                                               vernalis_temps = params$vernalis_temps,
                                               prisoners_point_temps = params$prisoners_point_temps,
                                               CVP_exports = params$CVP_exports,
                                               SWP_exports = params$SWP_exports,
                                               upper_sacramento_flows = params$upper_sacramento_flows,
                                               delta_inflow = params$delta_inflow,
                                               avg_temp_delta = params$avg_temp_delta,
                                               avg_temp = params$avg_temp,
                                               delta_proportion_diverted = params$delta_proportion_diverted,
                                               ..surv_juv_outmigration_sj_int = params$..surv_juv_outmigration_sj_int,
                                               .surv_juv_outmigration_san_joaquin_medium = params$.surv_juv_outmigration_san_joaquin_medium,
                                               .surv_juv_outmigration_san_joaquin_large = params$.surv_juv_outmigration_san_joaquin_large,
                                               min_survival_rate = params$min_survival_rate,
                                               stochastic = FALSE)
  expect_equal(migratory_survival, expected_migratory_survival)
})

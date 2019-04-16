context("Function manipulateAllTypeDays")

test_that("manipulateAllTypeDays", {
  library(data.table)
  
  allTypeDay <- readRDS(system.file("testdata/allTypDays.rds", package = "fbClust"))
  # message(getwd())
  expect_error(manipulateAllTypeDays(allTypeDay, output = c("output1", "output2")),
               regexp = "The length of the ouput must be 1")
  expect_error(
    manipulateAllTypeDays(allTypeDay, output = "output"),
    regexp = "You chose a wrong output, possible outputs are vertices, ptdfraw, ptdf and summary")
  
  PLAN <- manipulateAllTypeDays(allTypeDay, "ptdf")
  PLAN_raw <- manipulateAllTypeDays(allTypeDay, "ptdfraw")
  VERT <- manipulateAllTypeDays(allTypeDay, "vertices")
  summary <- manipulateAllTypeDays(allTypeDay, "summary")
  allTypeDay
  
  VERT_target <- rbindlist(sapply(1:length(allTypeDay[, dayIn]), function(X) {
    rbindlist(allTypeDay[, dayIn][[X]][, VERT_details])
  }, simplify = F))
  PLAN_target <- rbindlist(sapply(1:length(allTypeDay[, dayIn]), function(X) {
    rbindlist(allTypeDay[, dayIn][[X]][, PLAN_details])
  }, simplify = F))
  PLAN_raw_target <- rbindlist(sapply(1:length(allTypeDay[, dayIn]), function(X) {
    rbindlist(allTypeDay[, dayIn][[X]][, PLAN_raw_details])
  }, simplify = F))
  
  expect_true(all.equal(PLAN_target, PLAN[, 1:7]))
  expect_true(all.equal(PLAN_raw_target, PLAN_raw[, 1:8]))
  expect_true(all.equal(VERT_target, VERT[, 1:6]))
})
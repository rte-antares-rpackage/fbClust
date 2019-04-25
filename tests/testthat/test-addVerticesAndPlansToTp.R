context("Function .addVerticesAndPlansToTp")

test_that(".addVerticesAndPlansToTp", {
  library(data.table)
  allTypDay_list <- readRDS(system.file("testdata/allTypDay_testAddToTpDays.rds", package = "fbClust"))
  allTypDay <- allTypDay_list$allTypeDay
  PLAN <- allTypDay_list$PLAN
  VERT <- allTypDay_list$VERT
  PLANRaw <- allTypDay_list$PLAN_raw
  
  res <- .addVerticesAndPlansToTp(allTypDay = allTypDay, VERT = VERT, PLAN = PLAN, PLANRaw = PLANRaw)

  expect_true(all(colnames(res) == c("TypicalDay", "Class", "dayIn", "distance", "idDayType")))
  expect_true(all(colnames(res[, .SD, .SDcols = "dayIn"][[1]][[1]]) == c(
    "Date", "Period", "VERT_details", "PLAN_details", "PLANRaw_details")))
  expect_true(all(colnames(
    res[, .SD, .SDcols = "dayIn"][[1]][[1]][, .SD, .SDcols = "VERT_details"][[1]][[1]]) == c(
      "Date", "Period", "ptdfFR", "ptdfDE", "ptdfAT", "ptdfBE")))
  expect_true(nrow(
    res[, .SD, .SDcols = "dayIn"][[1]][[1]][, .SD, .SDcols = "VERT_details"][[1]][[2]]) ==
      1126)
  
  expect_true(all(colnames(
    res[, .SD, .SDcols = "dayIn"][[1]][[1]][, .SD, .SDcols = "PLAN_details"][[1]][[1]]) == c(
      "Date", "Period", "ptdfFR", "ptdfDE", "ptdfAT", "ptdfBE", "ram")))
  expect_true(nrow(
    res[, .SD, .SDcols = "dayIn"][[1]][[1]][, .SD, .SDcols = "PLAN_details"][[1]][[2]]) ==
      205) 
  expect_true(nrow(
    res[, .SD, .SDcols = "dayIn"][[1]][[1]][, .SD, .SDcols = "PLANRaw_details"][[1]][[2]]) ==
      205) 
})
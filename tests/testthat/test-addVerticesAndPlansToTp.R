context("Function .addVerticesAndPlansToTp")

test_that(".addVerticesAndPlansToTp", {
  library(data.table)
  allTypDay_list <- readRDS(system.file("testdata/allTypDay_testAddToTpDays.rds", package = "fbClust"))
  allTypDay <- allTypDay_list[[1]]
  PLAN <- allTypDay_list[[2]]
  VERT <- allTypDay_list[[3]]
  
  res <- .addVerticesAndPlansToTp(allTypDay = allTypDay, VERT = VERT, PLAN = PLAN)
  res[, .SD, .SDcols = "dayIn"][[1]][[1]][, .SD, .SDcols = "PLAN_details"][[1]]
  
  expect_true(all(colnames(res) == c("TypicalDay", "Class", "dayIn", "distance")))
  expect_true(all(colnames(res[, .SD, .SDcols = "dayIn"][[1]][[1]]) == c(
    "Date", "Period", "VERT_details", "PLAN_details")))
  expect_true(all(colnames(
    res[, .SD, .SDcols = "dayIn"][[1]][[1]][, .SD, .SDcols = "VERT_details"][[1]][[1]]) == c(
      "Date", "Period", "ptdfAT", "ptdfBE", "ptdfDE", "ptdfFR")))
  expect_true(nrow(
    res[, .SD, .SDcols = "dayIn"][[1]][[1]][, .SD, .SDcols = "VERT_details"][[1]][[2]]) ==
      1241)
  
  expect_true(all(colnames(
    res[, .SD, .SDcols = "dayIn"][[1]][[1]][, .SD, .SDcols = "PLAN_details"][[1]][[1]]) == c(
      "Date", "Period", "ptdfAT", "ptdfBE", "ptdfDE", "ptdfFR")))
  expect_true(nrow(
    res[, .SD, .SDcols = "dayIn"][[1]][[1]][, .SD, .SDcols = "PLAN_details"][[1]][[2]]) ==
      241) 
})
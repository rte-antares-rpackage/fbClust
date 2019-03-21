context("Function .transformTS")

test_that(".transformTS", {
  library(data.table)
  PLAN <- fread(system.file("testdata/190318_plan.csv", package = "fbClust"), sep = "|")

  
  PLAN_date <- .transformTS(PLAN)
  expect_true(is.null(PLAN_date$timestamp))
  expect_true(all(grepl(pattern = "^[0-9]{4}-[0-1][0-9]-[0-3][0-9]$", PLAN_date$Date)))
  expect_true(all(PLAN_date$Period >= 1 & PLAN_date$Period <= 12))
  expect_true(all(colnames(PLAN_date) == c(
    colnames(PLAN)[-grep("timestamp", colnames(PLAN))], "Period", "Date")))
  
})





context("Function getPreprocPlan")

test_that("getPreprocPlan", {
  library(data.table)
  res <- getPreprocPlan(
    pathPtdfMatrixFactor = system.file(
      "testdata/plan_new_version_factor_AT.rds", package = "fbClust"),
    pathPtdfMatrixConstraint = system.file(
      "testdata/plan_new_version_constraint_AT.rds", package = "fbClust"))
  
  expect_true(all(grepl("ROW_ID|Period|Date|ram|ptdf[A-Z]{2}", colnames(res))))
  expect_true(nrow(res) == 2423)
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", res[['Date']])))
  
})

context("Function .crtlPlan")

test_that(".crtlPlan", {
  library(data.table)
  PLAN <- getPreprocPlan(
    pathPtdfMatrixFactor = system.file(
      "testdata/plan_new_version_factor_AT.rds", package = "fbClust"),
    pathPtdfMatrixConstraint = system.file(
      "testdata/plan_new_version_constraint_AT.rds", package = "fbClust"))
  
  .crtlPlan(PLAN)
  PLAN2 <- copy(PLAN)
  PLAN2[, ram := NULL]
  expect_error(.crtlPlan(PLAN2))
  PLAN3 <- data.table()
  expect_error(.crtlPlan(PLAN3), regexp = "The object PLAN has no row")
  PLAN4 <- c()
  expect_error(.crtlPlan(PLAN4), regexp = "The object PLAN should be a data.table")
})
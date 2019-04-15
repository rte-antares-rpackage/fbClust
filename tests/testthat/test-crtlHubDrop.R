context("Function .ctrlHubDrop")

test_that(".ctrlHubDrop", {
  library(data.table)
  
  PLAN <- getPreprocPlan(
    path_ptdf_matrix_factor = system.file(
      "testdata/plan_new_version_factor_AT.rds", package = "fbClust"),
    path_ptdf_matrix_constraint = system.file(
      "testdata/plan_new_version_constraint_AT.rds", package = "fbClust"))
  hubDrop <- list(NL = c("BE", "DE", "FR", "AT"))
  
  .ctrlHubDrop(hubDrop = hubDrop, PLAN = PLAN)
  
  expect_error(.ctrlHubDrop(list(NL = c("BE", "DE", "FR")), PLAN), fixed = T,
               regexp = "hubDrop does not contain all the ptdf in PLAN")
  
  expect_warning(.ctrlHubDrop(list(NL = c("BE", "DE", "FR", "AT", "UK")), PLAN), fixed = T,
               regexp = "ptdfUK is (are) not in ptdf name")
  
})
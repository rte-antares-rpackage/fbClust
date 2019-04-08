context("Function .ctrlCountryList")

test_that(".ctrlCountryList", {
  library(data.table)
  
  PLAN <- getPreprocPlan(
    path_ptdf_matrix_factor = system.file(
      "testdata/plan_new_version_factor_AT.rds", package = "fbClust"),
    path_ptdf_matrix_constraint = system.file(
      "testdata/plan_new_version_constraint_AT.rds", package = "fbClust"))
  country_list <- list(NL = c("BE", "DE", "FR", "AT"))
  
  .ctrlCountryList(country_list = country_list, PLAN = PLAN)
  
  expect_error(.ctrlCountryList(list(NL = c("BE", "DE", "FR")), PLAN), fixed = T,
               regexp = "country_list does not contain all the ptdf in PLAN")
  
})
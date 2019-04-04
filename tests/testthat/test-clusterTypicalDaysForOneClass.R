context("Function clusterTypicalDaysForOneClass")

test_that("clusterTypicalDaysForOneClass", {
  library(data.table)
  library(quadprog)
  library(vertexenum)

  PLAN <- getPreprocPlan(
    path_ptdf_matrix_factor = system.file(
      "testdata/plan_new_version_factor_AT.rds", package = "fbClust"),
    path_ptdf_matrix_constraint = system.file(
      "testdata/plan_new_version_constraint_AT.rds", package = "fbClust"))
  dates <- seq(as.Date("2018-10-01"), as.Date("2018-10-04"), by = "day")
  nbcluster <- 2
  maxDomainSize <- 20000
  allTypDays <- clusterTypicalDaysForOneClass(
    dates = dates, PLAN = PLAN, VERT = NULL,
    maxDomainSize = maxDomainSize, nbCluster = nbcluster,
    report = F, hourWeight = rep(1, 24), className = NULL,
    reportPath = NULL, id_start = 1)

  maxDomainSize2 <- 2000

  expect_true(nrow(allTypDays) == nbcluster)

  expect_error(clusterTypicalDaysForOneClass(
    dates = dates, PLAN = PLAN, VERT = NULL,
    maxDomainSize = maxDomainSize2, nbCluster = nbcluster,
    report = F))

})

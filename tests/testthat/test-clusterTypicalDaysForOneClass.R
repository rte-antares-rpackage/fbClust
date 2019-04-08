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
  hourWeight <- rep(1, 24)
  allTypDays <- clusterTypicalDaysForOneClass(
    dates = dates, PLAN = PLAN, VERT = VERT,
    maxDomainSize = maxDomainSize, nbCluster = nbcluster,
    report = F, hourWeight = hourWeight, className = NULL,
    reportPath = NULL, id_start = 1)
  expect_true(nrow(allTypDays) == nbcluster)
  
  VERT <- rbindlist(lapply(1:length(allTypDays[, dayIn]), function(X) {
    rbindlist(allTypDays[, dayIn][[X]][, VERT_details])
  }))

  hourWeight[4] <- 0
  allTypDays2 <- clusterTypicalDaysForOneClass(
    dates = dates, PLAN = PLAN, VERT = VERT,
    maxDomainSize = maxDomainSize, nbCluster = nbcluster,
    report = F, hourWeight = hourWeight, className = NULL,
    reportPath = NULL, id_start = 1)
  expect_true(all(allTypDays[, distance][[1]][, Distance][1:2] != 
                    allTypDays2[, distance][[1]][, Distance][1:2]))
  
  VERT2 <- rbindlist(lapply(1:length(allTypDays2[, dayIn]), function(X) {
    rbindlist(allTypDays2[, dayIn][[X]][, VERT_details])
  }))
  expect_true(all.equal(VERT, VERT2))
  
  maxDomainSize2 <- 2000
  expect_error(clusterTypicalDaysForOneClass(
    dates = dates, PLAN = PLAN, VERT = VERT,
    maxDomainSize = maxDomainSize2, nbCluster = nbcluster,
    report = F))

})

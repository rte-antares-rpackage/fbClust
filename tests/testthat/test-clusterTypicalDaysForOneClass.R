context("Function clusterTypicalDaysForOneClass")

test_that("clusterTypicalDaysForOneClass", {
  library(data.table)
  library(quadprog)
  library(fbTools)

  PL_VE <- readRDS(system.file("testdata/plan_not_wanted_ptdf.rds", package = "fbClust"))
  PLAN <- PL_VE[[1]]
  VERT <- PL_VE[[2]]
  dates <- seq(as.Date("2019-02-14"), as.Date("2019-02-17"), by = "day")
  nbcluster <- 2
  maxDomainSize <- 20000
  allTypDays <- clusterTypicalDaysForOneClass(
    dates = dates, PLAN = PLAN, VERT = VERT,
    maxDomainSize = maxDomainSize, nbCluster = nbcluster,
    report = F)

  # maxDomainSize2 <- 2000
  # 
  expect_true(nrow(allTypDays) == nbcluster)
  # 
  # expect_error(clusterTypicalDaysForOneClass(
  #   dates = dates, PLAN = PLAN, VERT = NULL,
  #   maxDomainSize = maxDomainSize2, nbCluster = nbcluster,
  #   report = F))

})

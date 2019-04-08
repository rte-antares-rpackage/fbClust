context("Function clusteringTypicalDays")

test_that("clusteringTypicalDays", {
  library(data.table)
  library(quadprog)
  library(vertexenum)
  
  PLAN <- getPreprocPlan(
    path_ptdf_matrix_factor = system.file(
      "testdata/plan_new_version_factor_AT.rds", package = "fbClust"),
    path_ptdf_matrix_constraint = system.file(
      "testdata/plan_new_version_constraint_AT.rds", package = "fbClust"))
  calendar <- list()
  calendar$interSeasonWe <- c("2018-10-01", "2018-10-02")
  calendar$interSeasonWd <- c("2018-10-03", "2018-10-04")
  country_list <- list(NL = c("BE", "DE", "FR", "AT"))
  hourWeight = rep(1, 24)
  nbClustWeek <- 1
  nbClustWeekend <- 1  
  maxDomainSize <- 20000
  allTypDays <- clusteringTypicalDays(
    calendar = calendar, PLAN = PLAN, VERT = NULL, country_list = country_list,
    maxDomainSize = maxDomainSize, nbClustWeek = nbClustWeek, 
    nbClustWeekend = nbClustWeekend, hourWeight = hourWeight)
  expect_true(nrow(allTypDays) == length(calendar))
  
  VERT <- rbindlist(lapply(1:length(allTypDays[, dayIn]), function(X) {
    rbindlist(allTypDays[, dayIn][[X]][, VERT_details])
  }))
  
  maxDomainSize2 <- 2000

  expect_error(clusteringTypicalDays(
    calendar = calendar, PLAN = PLAN, VERT = VERT, country_list = country_list,
    maxDomainSize = maxDomainSize2, nbClustWeek = nbClustWeek, 
    nbClustWeekend = nbClustWeekend, hourWeight = hourWeight))
  
})



context("Function clusteringTypicalDays")

test_that("clusteringTypicalDays", {
  library(data.table)
  library(quadprog)
  library(vertexenum)
  
  PLAN <- getPreprocPlan(
    pathPtdfMatrixFactor = system.file(
      "testdata/plan_new_version_factor_AT.rds", package = "fbClust"),
    pathPtdfMatrixConstraint = system.file(
      "testdata/plan_new_version_constraint_AT.rds", package = "fbClust"))
  calendar <- list()
  calendar$interSeasonWe <- c("2018-10-01", "2018-10-02")
  calendar$interSeasonWd <- c("2018-10-03", "2018-10-04")
  hubDrop <- list(NL = c("BE", "DE", "FR", "AT"))
  hourWeight = rep(1, 24)
  nbClustWeek <- 1
  nbClustWeekend <- 1  
  maxDomainSize <- 20000
  allTypDays <- clusteringTypicalDays(
    calendar = calendar, PLAN = PLAN, VERT = NULL, hubDrop = hubDrop,
    maxDomainSize = maxDomainSize, nbClustWeek = nbClustWeek, 
    nbClustWeekend = nbClustWeekend, hourWeight = hourWeight)
  
  expect_true(nrow(allTypDays) == length(calendar))
  expect_true(all(colnames(allTypDays) == c(
    "TypicalDay", "Class", "dayIn", "distance", "idDayType")))
  VERT <- rbindlist(lapply(1:length(allTypDays[, dayIn]), function(X) {
    rbindlist(allTypDays[, dayIn][[X]][, VERT_details])
  }))
  
  hourWeight[3] <- 0
  allTypDays2 <- clusteringTypicalDays(
    calendar = calendar, PLAN = PLAN, VERT = VERT, hubDrop = hubDrop,
    maxDomainSize = maxDomainSize, nbClustWeek = nbClustWeek, 
    nbClustWeekend = nbClustWeekend, hourWeight = hourWeight)
  
  expect_true(nrow(allTypDays) == length(calendar))
  VERT2 <- rbindlist(lapply(1:length(allTypDays2[, dayIn]), function(X) {
    rbindlist(allTypDays2[, dayIn][[X]][, VERT_details])
  }))
  expect_true(all.equal(VERT, VERT2))
  
  maxDomainSize2 <- 2000

  expect_error(clusteringTypicalDays(
    calendar = calendar, PLAN = PLAN, VERT = VERT, hubDrop = hubDrop,
    maxDomainSize = maxDomainSize2, nbClustWeek = nbClustWeek, 
    nbClustWeekend = nbClustWeekend, hourWeight = hourWeight))
  
})



context("graphs functions")

test_that("clusterPlot", {
  library(data.table)
  library(rAmCharts)
  library(ggplot2)
  
  # data <- readRDS(system.file("testdata/allTypeDaytest.rds", package = "fbClust"))
  data <- readRDS(system.file("testdata/allTypDays.rds", package = "fbClust"))
  # data <- allTypDay_list$allTypeDay
  country1 <- "NL"
  country2 <- "DE"
  hour <- 1
  dayType <- 1
  
  out <- clusterPlot(data, country1, country2, hour, dayType,
                     typicalDayOnly = FALSE, ggplot = FALSE, width = "420px", height = "410px")
  expect_true("htmlwidget" %in% class(out))
  
  out <- clusterPlot(data, "FR", "NL", hour, dayType,
                     typicalDayOnly = TRUE, ggplot = TRUE, width = "420px", height = "410px")
  expect_true("ggplot" %in% class(out))
  
  expect_error(clusterPlot(
    data, country1 = "FR", country2 = "FR", hour, dayType,
    typicalDayOnly = FALSE, ggplot = TRUE, width = "420px", height = "410px"),
    regexp = "The hubs should be distinct", fixed = T)
  expect_error(clusterPlot(
    data, country1, country2 = "fr", hour, dayType,
    typicalDayOnly = FALSE, ggplot = TRUE, width = "420px", height = "410px"),
    regexp = paste("country1 or country 2 has wrong format. Format should be",
                   "XX (where XX is the abreviation of the hub (ex : FR, DE, BE))"), 
    fixed = T)
  
  ######### pllotFlowbased part
  
  
  
  PLAN <- getPreprocPlan(
    pathPtdfMatrixFactor = system.file(
      "testdata/plan_new_version_factor_AT.rds", package = "fbClust"),
    pathPtdfMatrixConstraint = system.file(
      "testdata/plan_new_version_constraint_AT.rds", package = "fbClust"))

  hubDrop = list(NL = c("BE", "DE", "FR", "AT"))

  #Plot unique polyhedron
  out <- plotFlowbased(PLAN, country1 = "BE", country2 = "DE", hubDrop = hubDrop,
                       hours = c(2), dates = c("2018-10-02"), domainsNames = "2018-10-02", main = "")
  expect_true("htmlwidget" %in% class(out))

  #Plot four polyhedra
  out2 <- plotFlowbased(PLAN, country1 = "BE", country2 = "DE", hubDrop = hubDrop,
                        hours = c(3, 4), dates = c("2018-10-02", "2018-10-04"), domainsNames = NULL,
                        main = NULL)
  expect_true("htmlwidget" %in% class(out2))

  expect_error(plotFlowbased(
    PLAN, country1 = "BE", country2 = "DE", hubDrop = hubDrop, hours = c(3, 4),
    dates = c("2018-10-02", "2018-10-04"), domainsNames = c("one domain"), main = NULL),
    regexp = paste0("You must have one domainsNames specified by combination of hours and time, currently you have ",
                    1, " domainsNames specify for ",
                    4, " PLAN"), fixed = T)

  expect_error(plotFlowbased(
    PLAN, country1 = "BE", country2 = "DE", hubDrop = hubDrop, hours = c(4),
    dates = c("2018-10-04"), domainsNames = c("one domain", "two domains"), main = NULL),
    regexp = "Only one PLAN specified for 2 or more domainsNames", fixed = T)
  
  
  PLAN2 <- copy(PLAN)
  PLAN2 <- PLAN2[Date == "2018-10-04"]
  PLAN2[, ptdfAT := NULL]
  hubDrop2 <- list("NL" = list("BE", "DE", "FR"))
  out3 <- plotFlowbased(PLAN, PLAN2 = PLAN2, country1 = "BE", country2 = "DE", 
                        hubDrop = hubDrop, hubDrop2 = hubDrop2,
                        hours = c(3, 4), dates = c("2018-10-02"),
                        hours2 = c(4), dates2 = c("2018-10-04"),
                        domainsNames = NULL, main = NULL)
  expect_true("htmlwidget" %in% class(out3))
  
  expect_error(plotFlowbased(
    PLAN, country1 = "BE", country2 = "DE", hubDrop = hubDrop, hours = c(4),
    dates = c("2018-10-04"), main = NULL, color = c("green", "yellow")))
  
  out4 <- plotFlowbased(
    PLAN, country1 = "BE", country2 = "DE", hubDrop = hubDrop, hours = c(4),
    dates = c("2018-10-04"), main = NULL, color = c("yellow"))
  expect_true("htmlwidget" %in% class(out4))
})


context("graphs functions")

test_that("clusterPlot", {
  library(data.table)
  library(rAmCharts)
  library(ggplot2)
  
  # data <- readRDS(system.file("testdata/allTypeDaytest.rds", package = "fbClust"))
  allTypDay_list <- readRDS(system.file("testdata/allTypDay_testAddToTpDays.rds", package = "fbClust"))
  data <- allTypDay_list$allTypeDay
  country1 <- "NL"
  country2 <- "ptdfDE"
  hour <- 1
  dayType <- 1
  
  out <- clusterPlot(data, country1, country2, hour, dayType,
                     typicalDayOnly = FALSE, ggplot = FALSE, width = "420px", height = "410px")
  expect_true("htmlwidget" %in% class(out))
  
  out <- clusterPlot(data, "ptdfFR", "ptdfNL", hour, dayType,
                     typicalDayOnly = TRUE, ggplot = TRUE, width = "420px", height = "410px")
  expect_true("ggplot" %in% class(out))
  
  expect_error(clusterPlot(
    data, country1 = "ptdfFR", country2 = "FR", hour, dayType,
    typicalDayOnly = FALSE, ggplot = TRUE, width = "420px", height = "410px"),
    regexp = "The countries should be distinct", fixed = T)
  expect_error(clusterPlot(
    data, country1, country2 = "fr", hour, dayType,
    typicalDayOnly = FALSE, ggplot = TRUE, width = "420px", height = "410px"),
    regexp = paste("country1 or country 2 has wrong format. Format should be",
                   "ptdfXX or XX (where XX is the abreviation of the country (ex : FR, DE, BE))"), 
    fixed = T)
  
})

test_that("plotFlowbased", {
  library(data.table)
  library(rAmCharts)

  PLAN <- getPreprocPlan(
    path_ptdf_matrix_factor = system.file(
      "testdata/plan_new_version_factor_AT.rds", package = "fbClust"),
    path_ptdf_matrix_constraint = system.file(
      "testdata/plan_new_version_constraint_AT.rds", package = "fbClust"))
  
  country_list = list(NL = c("BE", "DE", "FR", "AT"))
  
  #Plot unique polyhedron
  out <- plotFlowbased(PLAN, "BE", "DE", country_list = country_list,
                       hours = c(2), dates = c("2018-10-02"), domainsNames = "2018-10-02", main = "")
  expect_true("htmlwidget" %in% class(out))
  
  #Plot four polyhedra
  out2 <- plotFlowbased(PLAN, "BE", "DE", country_list = country_list,
                        hours = c(3, 4), dates = c("2018-10-02", "2018-10-04"), domainsNames = NULL,
                        main = NULL)
  expect_true("htmlwidget" %in% class(out))
  
  expect_error(plotFlowbased(
    PLAN, "BE", "DE", country_list = country_list, hours = c(3, 4), 
    dates = c("2018-10-02", "2018-10-04"), domainsNames = c("one domain"), main = NULL),
    regexp = paste0("You must have one domainsNames specified by combination of hours and time, currently you have ",
                    1, " domainsNames specify for ", 
                    4, " PLAN"), fixed = T)
  
  expect_error(plotFlowbased(
    PLAN, "BE", "DE", country_list = country_list, hours = c(4), 
    dates = c("2018-10-04"), domainsNames = c("one domain", "two domains"), main = NULL),
    regexp = "Only one PLAN specified for 2 or more domainsNames", fixed = T)
  
})
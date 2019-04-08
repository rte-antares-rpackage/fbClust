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
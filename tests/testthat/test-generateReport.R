# context("Function generateClusteringReport")
# 
# test_that("Function generateClusteringReport works", {
#   
#   library(flexdashboard)
#   library(rAmCharts)
#   library(manipulateWidget)
#   library(flowBasedClustering)
#   library(ggplot2)
#   library(pipeR)
#   library(data.table)
#   library(DT)
#   library(shiny)
#   library(gridExtra)
#   data <- readRDS(system.file("testdata/report_test.rds", package = "fbClust"))
#   data[, Class := "winterSe"]
  # that test does not work with travis
  # clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
  # temp_directory <- tempdir()
  # generateClusteringReport(dayType = 1, data = data, outputFile = temp_directory)
# })
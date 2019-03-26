context("Function getVertices")

test_that("getVertices", {
  library(data.table)
  
  PL_VE <- readRDS(system.file("testdata/plan_not_wanted_ptdf.rds", package = "fbClust"))
  PLAN <- PL_VE[[1]]
  
  VERT <- getVertices(PLAN)
  expect_true(all(colnames(VERT) == c("ptdfAT", "ptdfBE", "ptdfDE", "ptdfFR", "timestamp")))
  expect_true(nrow(VERT) == 3110)


})




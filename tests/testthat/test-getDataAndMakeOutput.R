context("Function .getDataAndMakeOutput")

test_that(".getDataAndMakeOutput", {
  library(data.table)
  testTypDays <- readRDS(system.file("testdata/test_file_get_typical_day.rds", package = "fbClust"))

  distMat <- testTypDays$distMat
  X1 <- testTypDays$X
  X2 <- 2
  vect <- testTypDays$vect
  classe <- testTypDays$classe
  
  typDay1 <- .getDataAndMakeOutput(distMat = distMat, X = X1, vect = vect, className  = classe)
  typDay2 <- .getDataAndMakeOutput(distMat = distMat, X = X2, vect = vect, className  = classe)

  expect_true(all(typDay1[, .SD, .SDcols = "distance"][[1]][[
    1]][, .SD, .SDcols = "Distance"] == distMat[1:3, 1]))
  expect_true(all(typDay2[, .SD, .SDcols = "distance"][[1]][[
    1]][,.SD, .SDcols = "Distance"] == distMat[4, 4]))
  expect_true(all(colnames(typDay1) == c("TypicalDay", "Class", "dayIn", "distance")))
})
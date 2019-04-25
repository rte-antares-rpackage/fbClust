context("Function .ctrlFile")

test_that(".ctrlFile", {
  library(data.table)
  
  f1 <- .ctrlFile(pathFile = system.file("testdata/plan_new_version_factor_AT.rds", package = "fbClust"))
  # message(getwd())
  f2 <- .ctrlFile(pathFile = system.file("testdata/object.rds", package = "fbClust"))
  # f4 <- .ctrlFile(path_data = "testdata", path_file = "test.txt")
  expect_error(.ctrlFile(
    pathFile = system.file("testdata/test.txt")), fixed = T,
    regexp = "Your input data must be a rds or a csv")
    expect_true(all(class(f1) == c("data.table", "data.frame")))
})
  
  
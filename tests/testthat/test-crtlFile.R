# context("Function .ctrlFile")
# 
# test_that(".ctrlFile", {
#   library(data.table)
# 
#   f1 <- .ctrlFile(path_data = NULL, path_file = system.file("testdata/190318_plan.csv", package = "fbClust"))
#   f2 <- .ctrlFile(path_data = "testdata", path_file = "190318_plan.csv")
#   f3 <- .ctrlFile(path_data = "testdata", path_file = "object.rds")
#   f4 <- .ctrlFile(path_data = "testdata", path_file = "test.txt")
#   expect_warning(.ctrlFile(path_data = "testdata", path_file = "test.txt"), fixed = T,
#                  regexp = paste("Your input data must be a rds or a csv"))
#   expect_true(all(f1 == f2))
#   expect_true(class(f3) == "list")
# })
# 

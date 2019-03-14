# context("Function .getDistMatrixV2")
# 
# test_that(".getDistMatrixV2", {
#   library(data.table)
#   library(quadprog)
#   
#   pl_vert <- readRDS(system.file("testdata/pl_vert_test.rds", package = "fbClust"))
#   VERT <- pl_vert[[2]]
#   PLAN <- pl_vert[[1]]
#   col_ptdf <- pl_vert[[3]]
#   hourWeight <- rep(1, 24)
#   res <- .getDistMatrixV2(VERT = VERT, PLAN = PLAN, hourWeight = hourWeight)
#   
#   expect_true(length(unique(res[, dist])) == 1)
#   expect_true(res[1, dist] < 780 & res[1, dist] > 778)
#   expect_true(nrow(res) == 2 & ncol(res) == 4)
# })

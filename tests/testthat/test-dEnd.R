# context("Function .dEnd")
# 
# test_that("dEnd", {
#   library(data.table)
#   library(quadprog)
#   pl_vert <- readRDS(system.file("testdata/pl_vert_test.rds", package = "fbClust"))
#   VERT <- pl_vert[[2]]
#   PLAN <- pl_vert[[1]]
#   col_ptdf <- pl_vert[[3]]
# 
#   res <- .dEnd(VERT = VERT[Date == "2019-02-14"], 
#               PLAN = PLAN[Date == "2019-02-15"], col_ptdf = col_ptdf)
#   expect_true(res < 357 & res > 356)
# })

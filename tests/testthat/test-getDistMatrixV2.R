context("Function .getDistMatrixV2")

test_that(".getDistMatrixV2", {
  library(data.table)
  library(quadprog)

  pl_vert <- readRDS(system.file("testdata/pl_vert_test.rds", package = "fbClust"))
  # PL_VE <- readRDS(system.file("testdata/plan_not_wanted_ptdf.rds", package = "fbClust"))
  VERT <- pl_vert[[2]]
  PLAN <- pl_vert[[1]]
  # VERT <- PL_VE[[2]]
  # PLAN <- PL_VE[[1]]
  # PLAN <- .transformTS(PLAN)
  # VERT <- .transformTS(VERT)
  VERT <- setDT(VERT)
  PLAN <- setDT(PLAN)
  
  hourWeight <- rep(1, 24)
  res <- .getDistMatrixV2(VERT = VERT, PLAN = PLAN, hourWeight = hourWeight, ponderate = FALSE)
  expect_true(is.data.table(res))
  expect_true(length(unique(res[, "dist"])) == 1)
  expect_true(res[1, "dist"] < 782 & res[1, "dist"] > 778)
  expect_true(nrow(res) == 2 & ncol(res) == 4)
  VERT <- .addSignToVertices(VERT)
  res2 <- .getDistMatrixV2(VERT = VERT, PLAN = PLAN, hourWeight = hourWeight, ponderate = TRUE)
  expect_true(is.data.table(res2))
  expect_true(length(unique(res2[, "dist"])) == 1)
  expect_true(res2[1, "dist"] < 1387 & res2[1, "dist"] > 1386)
  expect_true(nrow(res2) == 2 & ncol(res2) == 4)
})

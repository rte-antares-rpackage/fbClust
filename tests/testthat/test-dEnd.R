context("Function .dEnd")

test_that("dEnd", {
  library(quadprog)
  library(data.table)
  pl_vert <- readRDS(system.file("testdata/expl_plan_vert.rds", package = "fbClust"))
  VERT <- pl_vert$VERT
  PLAN <- pl_vert$PLAN
  col_ptdf <- colnames(PLAN)[grep("^ptdf[A-Z]{2}$", colnames(PLAN))]
  col_vert <- colnames(VERT)[!grepl("Date|Period", colnames(VERT))]

  setDT(VERT)
  setDT(PLAN)
  
  res <- .dEnd(VERT = VERT[Date == "2018-10-01"], colVert = col_vert,
              PLAN = PLAN[Date == "2018-10-02"], colPtdf = col_ptdf)
  expect_true(res < 512 & res > 510)
})

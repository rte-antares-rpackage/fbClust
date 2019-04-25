context("Function .dEnd2")

test_that(".dEnd2", {
  library(quadprog)
  library(data.table)
  pl_vert <- readRDS(system.file("testdata/expl_plan_vert.rds", package = "fbClust"))
  VERT <- pl_vert$VERT
  PLAN <- pl_vert$PLAN
  col_ptdf <- colnames(PLAN)[grep("^ptdf[A-Z]{2}$", colnames(PLAN))]
  col_vert <- colnames(VERT)[!grepl("Date|Period", colnames(VERT))]
  VERT <- .addSignToVertices(VERT)
  setDT(VERT)
  setDT(PLAN)
  
  res <- .dEnd2(VERT = VERT[Date == "2018-10-01" & Period == 1], colVert = col_vert,
               PLAN = PLAN[Date == "2018-10-02" & Period == 1], colPtdf = col_ptdf)
  expect_true(res < 1072 & res > 1070)
})


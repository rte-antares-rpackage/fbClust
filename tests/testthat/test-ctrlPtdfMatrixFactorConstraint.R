context("Function .ctrlPtdfMatrixFactorConstraint")

test_that(".ctrlPtdfMatrixFactorConstraint", {
  library(data.table)
  
  ptdfConstraint <- readRDS(system.file("testdata/plan_new_version_constraint_AT_NA.rds", 
                                        package = "fbClust"))
  ptdfFactor <- readRDS(system.file("testdata/plan_new_version_factor_AT_NA.rds", 
                                    package = "fbClust"))
  dtPtdfId <- data.table(
    ptdf_id = c(23, 90, 95, 22, 20, 59, 97, 92, 41, 98, 83, 96, 99),
    ptdf_country = c("ptdfAT", "ptdfBE", "ptdfCZ", "ptdfDE", "ptdfFR", "ptdfHR",
                     "ptdfHU", "ptdfNL", "ptdfPL", "ptdfRO", "ptdfSI", "ptdfSK",
                     "Core"))
  ptdfConstraint2 <- copy(ptdfConstraint)
  ptdfFactor2 <- copy(ptdfFactor)
  ptdfFactor2[, false_col := rep(1, nrow(ptdfFactor2))]
  ptdfConstraint2[, false_col := rep(1, nrow(ptdfConstraint2))]
  
  ptdfFactor3 <- copy(ptdfFactor)
  ptdfFactor3[BIDDINGAREA_ID == 22, BIDDINGAREA_ID := 17]
  
  ptdfFactor4 <- copy(ptdfFactor)
  ptdfFactor4[15, FACTOR := 17]
  
  ptdfFactor5 <- copy(ptdfFactor)
  ptdfFactor5 <- rbindlist(list(ptdfFactor5, data.table("20180104", "1", 25, 20, 0.06728)))
  
  ptdfFactor6 <- copy(ptdfFactor)
  ptdfFactor6[18, SESSION_ID := 9839]
  res_list <- .ctrlPtdfMatrixFactorConstraint(ptdfFactor, ptdfConstraint, dtPtdfId)
  ptdfFactor7 <- copy(ptdfFactor)
  ptdfFactor7 <- ptdfFactor7[-1]
  
  expect_error(
    .ctrlPtdfMatrixFactorConstraint(ptdfFactor2, ptdfConstraint, dtPtdfId), fixed = T,
    regexp = paste("You must have five colnames in PtdfMatrixFactor and these colnames should be",
                   "SESSION_ID, MATRIX_ID, ROW_ID, BIDDINGAREA_ID, FACTOR"))
  
  expect_error(
    .ctrlPtdfMatrixFactorConstraint(ptdfFactor, ptdfConstraint2, dtPtdfId), fixed = T,
    regexp = paste("You must have four colnames in PtdfMatrixFactor and these colnames should be",
                   "SESSION_ID, MATRIX_ID, ROW_ID, REMAININGAVAILABLEMARGIN"))
  
  expect_error(
    .ctrlPtdfMatrixFactorConstraint(ptdfFactor3, ptdfConstraint, dtPtdfId), fixed = T,
    regexp = cat("Your ptdf id in the column BIDDINGAREA_ID must be in",
                  dtPtdfId[["ptdf_id"]]))
  
  expect_error(
    .ctrlPtdfMatrixFactorConstraint(ptdfFactor4, ptdfConstraint, dtPtdfId), fixed = T,
    regexp = "Your ptdf values must be between -1 and 1")
  
  expect_warning(
    .ctrlPtdfMatrixFactorConstraint(ptdfFactor5, ptdfConstraint, dtPtdfId), fixed = T,
    regexp = "You have Period > 24 or < 1, the lines concerned will be deleted")
  
  expect_error(
    .ctrlPtdfMatrixFactorConstraint(ptdfFactor6, ptdfConstraint, dtPtdfId), fixed = T,
    regexp = "The column SESSION_ID has to be in format YYYYMMDD before the pre-processing")
  expect_warning(.ctrlPtdfMatrixFactorConstraint(ptdfFactor7, ptdfConstraint, dtPtdfId))
  
  expect_true(all(res_list$dtPtdfMatrixConstraint == ptdfConstraint))
  expect_true(all(res_list$dtPtdfMatrixFactor == ptdfFactor))
  
  
  
})


library(data.table)
getPreprocPlan <- function(
  path_data = NULL, path_ptdf_matrix_factor = "PtdfMatrixFactors.csv", 
  path_ptdf_matrix_constraint = "PtdfMatrixConstraints.csv") {
  dtPtdfId <- data.table(
    ptdf_id = c(23, 90, 95, 22, 20, 59, 97, 92, 41, 98, 83, 96, 99),
    ptdf_country = c("ptdfAT", "ptdfBE", "ptdfCZ", "ptdfDE", "ptdfFR", "ptdfHR",
                     "ptdfHU", "ptdfNL", "ptdfPL", "ptdfRO", "ptdfSI", "ptdfSK",
                     "Core"))
  
  dtPtdfMatrixFactor <- .ctrlFile(path_file = path_ptdf_matrix_factor)
  dtPtdfMatrixConstraint <- .ctrlFile(path_file = path_ptdf_matrix_constraint)
  
  list_ptdf <- .ctrlPtdfMatrixFactorConstraint(
    dtPtdfMatrixFactor, dtPtdfMatrixConstraint, dtPtdfId)
  dtPtdfMatrixFactor <- list_ptdf$dtPtdfMatrixFactor
  dtPtdfMatrixConstraint <- list_ptdf$dtPtdfMatrixConstraint
  
  dtPtdfMatrixFactorCast <- dcast(dtPtdfMatrixFactor, 
                                  ROW_ID+SESSION_ID+MATRIX_ID~BIDDINGAREA_ID,
                                  value.var = "FACTOR")
  dtAllPtdf <- merge(dtPtdfMatrixFactorCast, dtPtdfMatrixConstraint,
                     by = c("ROW_ID", "MATRIX_ID", "SESSION_ID"))
  
  .crtlNA(dtAllPtdf)
  old_col <- colnames(dtAllPtdf)[colnames(dtAllPtdf) %in% dtPtdfId[, ptdf_id]]
  dtPtdfId[ptdf_id %in% old_col, ptdf_country]
  setnames(dtAllPtdf, old = c(dtPtdfId[ptdf_id %in% old_col, ptdf_id], 
                              "REMAININGAVAILABLEMARGIN", "SESSION_ID", "MATRIX_ID"),
           new = c(dtPtdfId[ptdf_id %in% old_col, ptdf_country], "ram",
                   "Date", "Period"))
  dtAllPtdf
  
}

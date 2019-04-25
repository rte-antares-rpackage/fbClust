#' @title Generate a data.table with the ptdf
#'
#' @description With links to the files PtdfMatrixFactors and PtdfMatrixConstraints,
#' this function computes a data.table containing all columns needed to keep going
#' on the procedure to get the distances between polyhedra.
#' 
#' @param pathPtdfMatrixFactor path of the file containing the ptdf of the
#' flowbased structure
#' \itemize{
#'  \item SESSION_ID : The Date, in numeric format : YYYYMMDD
#'  \item MATRIX_ID : The Period (hour in the day), between 1 and 24
#'  \item ROW_ID : The id of the row
#'  \item REMAININGAVAILABLEMARGIN : The constraint (or ram)
#' }
#' 
#' @param pathPtdfMatrixConstraint path of the file containing the constraints of 
#' the flowbased structure (the ram). Its columns have to be :
#' \itemize{
#'  \item SESSION_ID : The Date, in numeric format : YYYYMMDD
#'  \item MATRIX_ID : The Period (hour in the day), between 1 and 24
#'  \item ROW_ID : The id of the row
#'  \item BIDDINGAREA_ID : The id of the hub, we must have at least two different
#'  and they have to be in this selection: (23, 90, 95, 22, 20, 59, 97, 92, 41, 98, 83, 96, 99)
#'  \item FACTOR : The values of the ptdf by hub
#' }
#'
#' @examples
#' 
#' \dontrun{
#' library(data.table)
#' pathPtdfMatrixFactor = system.file(
#' "testdata/plan_new_version_factor_AT.rds", package = "fbClust")
#' pathPtdfMatrixConstraint = system.file(
#' "testdata/plan_new_version_constraint_AT.rds", package = "fbClust")
#' 
#'  PLAN <- getPreprocPlan(pathPtdfMatrixFactor = pathPtdfMatrixFactor,
#'  pathPtdfMatrixConstraint =  pathPtdfMatrixConstraint)
#' }
#' 
#' @import data.table
#' @export

getPreprocPlan <- function(
  pathPtdfMatrixFactor = "PtdfMatrixFactors.csv", 
  pathPtdfMatrixConstraint = "PtdfMatrixConstraints.csv") {
  
  # remove NOTE data.table
  ptdf_id <- NULL
  ptdf_country <- NULL
  Date <- NULL

  dtPtdfId <- fread(system.file("testdata/matchingIdPtdf.csv", package = "fbClust"))
  
  dtPtdfMatrixFactor <- .ctrlFile(pathFile = pathPtdfMatrixFactor)
  dtPtdfMatrixConstraint <- .ctrlFile(pathFile = pathPtdfMatrixConstraint)
  
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
  dtAllPtdf[, Date := paste(
    substr(Date, 1, 4), substr(Date, 5, 6), substr(Date, 7, 8), sep = "-")]
  dtAllPtdf
  
}


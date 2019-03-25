#' @title Generate a set of flow-based typical days on one time period
#' 
#' @description Run a clustering algorithm for a given historical period. It creates clusters by
#' gathering the most similar days and chooses among them the best
#' representative: it will be a so-called typical day. The metric used to determine the similarity of two days is
#' a weighted sum of 24 hourly distances, meaning the distances between the domains of the two
#' days at the same hour.
#'
#' @param dates \code{character}, vector of date (format YYYY-MM-DD).
#' @param PLAN \code{data.table}, at least ram, timestamp and two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#'  \item ram : line limits
#'  \item timestamp : time in format YYYY-MM-DDTHH:mm:ssZ
#' }
#' If it's not already done, this parameter needs to be standardized with the 
#' function \link{setDiffNotWantedPtdf}
#' 
#' @param VERT \code{data.table}, the same Date, Period and ptdf than we have
#' in PLAN.
#' This parameter can be obtained with the function \link{ptdfToVertices}.
#' @param nbCluster \code{numeric}, number of clusters
#' @param className \code{character}, name of the class characterizing the studied time period
#' @param id_start \code{numeric}, first identifier of the returned typical days. Default value is 1
#' @param reportPath \code{character}, path where the report is written.
#' @param hourWeight \code{numeric}, vector of 24 weights, one for each hour of the day. The clustering algorithm
#' will be more accurate for the flow-based domains of the hours with a relatively higher weight. 
#' @param report \code{boolean}, if TRUE, reports are generated.
#' @param maxDomainSize \code{numeric} limit of domain size in each axis. The function will return an error if one domain
#' or more exceed these limits.
#'
#' @examples
#'
#' \dontrun{
#' library(data.table)
#' library(quadprog)
#' PLAN <- readRDS(system.file("testdata/plan_not_wanted_ptdf.rds", package = "fbClust"))
#' dates <- seq(as.Date("2019-02-14"), as.Date("2019-02-17"), by = "day")
#' hourWeight = rep(1, 24)
#' nbcluster <- 2
#' maxDomainSize <- 20000
#' 
#'  clusterTypicalDaysForOneClass(
#'  dates = dates, PLAN = PLAN, VERT = NULL, maxDomainSize = maxDomainSize, 
#'  nbCluster = nbcluster,report = F, hourWeight = hourWeight)
#' 
#' }
#'
#' @export


clusterTypicalDaysForOneClass <- function(dates,
                                          PLAN,
                                          VERT = NULL,
                                          nbCluster = NULL,
                                          reportPath = NULL,
                                          hourWeight = rep(1, 24),
                                          className = NULL,
                                          report = TRUE,
                                          id_start = 1,
                                          maxDomainSize = 10000) {
  
  # pb <- txtProgressBar(style = 3)
  # setTxtProgressBar(pb, 0)
  
  if(is.null(VERT)) {
    VERT <- fbTools::getVertices(PLAN)
  }
  PLAN <- .ctrlTimestamp(PLAN)
  VERT <- .ctrlTimestamp(VERT)
  
  PLAN <- .transformTS(PLAN)
  VERT <- .transformTS(VERT)
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  Max <- VERT[,max(unlist(.SD)), by = c("Date", "Period"), .SDcols = col_ptdf]
  Max[, isSupLim := V1 > maxDomainSize]
  Max <- Max[Max$isSupLim]
  if(nrow(Max) > 0){
    Max <- Max[,list(list(Period)), by = "Date"]
    
    stop(paste("The following flow-based domains exceed the expected maximum size :",
               paste(Max$Date, "(hour : ",lapply(
                 Max$V1, function(X){paste(X, collapse = ", ")}),")", collapse = ", ")))
  }
  
  dates <- as.character(dates)
  .ctrlDates(dates, unique(VERT$Date))
  
  .ctrlWeight(hourWeight)
  
  .ctrlVertPlanFormat(VERT = VERT, PLAN = PLAN)
  
  dt_dist <- .getDistMatrixV2(VERT = VERT, PLAN = PLAN, hourWeight = hourWeight)
  
  distMat <- dt_dist[, list(dist = sum(dist)), by = c("Date1", "Date2")]
  distMat <- dcast(distMat, Date1~Date2, value.var = "dist")
  distMat[, Date1 := NULL]
  distMat <- as.matrix(distMat)
  rownames(distMat) <- colnames(distMat)
  diag(distMat) <- 0
  
  set.seed(123456)
  vect <- cluster::pam(distMat, nbCluster, diss = TRUE)$clustering
  # setTxtProgressBar(pb, 1)
  
  if(is.null(className)){
    className <- as.character("Class")
  }
  
  allTypDay <- rbindlist(sapply(1:nbCluster, function(X){
    # Found a representative day for each class
    .getDataAndMakeOutput(X, vect, distMat, className)
    
  }, simplify = FALSE))
  
  allTypDay <- .addVerticesAndPlansToTp(allTypDay, VERT, PLAN)
  
  nb <- id_start:(id_start+nrow(allTypDay)-1)
  allTypDay[,idDayType :=nb]
  
  if(report){
    cat("\n")
    cat("Writing report(s)\n")
    # pb <- txtProgressBar(style = 3)
    # setTxtProgressBar(pb, 0)
    outL <- .crtOutFile(allTypDay, reportPath)
    
    sapply(allTypDay$idDayType, function(X){
      setTxtProgressBar(pb, getTxtProgressBar(pb) + 1/(outL$step + 1))
      generateClusteringReport(X, data = allTypDay, outputFile = outL$outputFile)
    })
    
    saveRDS(allTypDay, paste0(outL$outputFile, "/resultClust.RDS"))
    # setTxtProgressBar(pb, 1)
  }
  
  allTypDay
}

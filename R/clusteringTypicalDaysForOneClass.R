#' @title Generate a set of flow-based typical days on one time period
#' 
#' @description Run a clustering algorithm for a given historical period. It creates clusters by
#' gathering the most similar days and chooses among them the best
#' representative: it will be a so-called typical day. The metric used to determine the similarity of two days is
#' a weighted sum of 24 hourly distances, meaning the distances between the domains of the two
#' days at the same hour.
#' 
#' @param dates \code{character}, vector of date (format YYYY-MM-DD).
#' @param PLAN \code{data.table}, at least ram, Date, Period and two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#'  \item ram : line limits
#'  \item Date : date in format YYYY-MM-DD
#'  \item Period : hour in the day, between 1 and 24
#' }
#' PLAN is generated in this format with the function \link{getPreprocPlan}
#' @param VERT \code{data.table}, the same Date, Period and ptdf  we have
#' in PLAN. Default = NULL 
#' This parameter can be obtained with the function \link{getVertices}.
#' @param hubDrop \code{list}, list of hubs in the ptdf, with the ones which should
#' sustracted to the others as the names of the arrays which themself contain the ones which
#' be sustracted
#' @param nbCluster \code{numeric}, number of clusters
#' @param className \code{character}, name of the class characterizing the studied time period
#' @param idStart \code{numeric}, first identifier of the returned typical days. Default value is 1
#' @param hourWeight \code{numeric}, vector of 24 weights, one for each hour of the day. The clustering algorithm
#' will be more accurate for the flow-based domains of the hours with a relatively higher weight.
#' @param maxDomainSize \code{numeric} limit of domain size in each axis. The function will return an error if one domain
#' or more exceed these limits.
#' @param ponderate \code{logical} if TRUE, each angular sector have the same weight 
#' in the clustering metric. E.g. {BE and FR imports, DE and NL exports} is one sector.
#' 
#' @examples
#' 
#' \dontrun{
#' library(data.table)
#' library(quadprog)
#' PLAN <- readRDS(system.file("testdata/plan_test2.rds", package = "fbClust"))
#' dates <- seq(as.Date("2018-10-02"), as.Date("2018-10-04"), by = "day")
#' hourWeight = rep(1, 24)
#' nbcluster <- 2
#' maxDomainSize <- 20000
#' idStart <- 1
#' hubDrop <- list(NL = c("BE", "DE", "FR", "AT"))
#' 
#'  clusterTypicalDaysForOneClass(
#'  dates = dates, PLAN = PLAN, VERT = NULL, maxDomainSize = maxDomainSize,
#'  hubDrop = hubDrop,
#'  nbCluster = nbcluster,report = F, hourWeight = hourWeight, idStart = idStart)
#' 
#' }
#' 
#' @import data.table 
#' @import quadprog
#' @import vertexenum
#' @importFrom stats dist runif
#' @importFrom utils combn timestamp
#' 
#' @export

clusterTypicalDaysForOneClass <- function(dates,
                                          PLAN,
                                          VERT = NULL,
                                          hubDrop = list(NL = c("BE", "DE", "FR", "AT")),
                                          nbCluster = NULL,
                                          hourWeight = rep(1, 24),
                                          className = NULL,
                                          idStart = 1,
                                          maxDomainSize = 20000,
                                          ponderate = FALSE) {

  # pb <- txtProgressBar(style = 3)
  # setTxtProgressBar(pb, 0)
  # remove NOTE data.table
  isSupLim <- NULL
  V1 <- NULL
  Period <- NULL
  dist <- NULL
  Date <- NULL
  PLAN[, Date := as.character(Date)]
  .crtlPlan(PLAN)
  PLANRaw <- copy(PLAN)
  .ctrlHubDrop(hubDrop = hubDrop, PLAN = PLAN)
  PLAN <- setDiffNotWantedPtdf(PLAN = PLAN, hubDrop = hubDrop)
  if(is.null(VERT)) {
    VERT <- getVertices(PLAN)
  }
  

  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  col_vert <- colnames(VERT)[!grepl("Date|Period|sign|N|nbsign", colnames(VERT))]
  Max <- VERT[,max(unlist(.SD)), by = c("Date", "Period"), .SDcols = col_vert]
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
  
  
  VERT <- .addSignToVertices(VERT)
  dt_dist <- .getDistMatrixV2(VERT = VERT[Period %in% which(hourWeight > 0)], 
                              PLAN = PLAN[Period %in% which(hourWeight > 0)], 
                              hourWeight = hourWeight, ponderate = ponderate)
  distMat <- dt_dist[, list(dist = sum(dist)), by = c("Date1", "Date2")]
  distMat <- dcast(distMat, Date1~Date2, value.var = "dist")
  # distMat[, Date1 := NULL]
  distMat[['Date1']] <- NULL
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
    data <- .getDataAndMakeOutput(X, vect, distMat, className)
    data
  }, simplify = FALSE))
  
  setDF(allTypDay)
  allTypDay <- .addVerticesAndPlansToTp(allTypDay, VERT, PLAN, PLANRaw)
  setDT(allTypDay)
  nb <- idStart:(idStart+nrow(allTypDay)-1)
  allTypDay$idDayType <- nb

  allTypDay
}

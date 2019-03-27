#' @import data.table

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
    VERT <- getVertices(PLAN)
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
  print(dt_dist)
  print("distMat data.table is computed")
  distMat <- dt_dist[, list(dist = sum(dist)), by = c("Date1", "Date2")]
  distMat <- dcast(distMat, Date1~Date2, value.var = "dist")
  # distMat[, Date1 := NULL]
  distMat[['Date1']] <- NULL
  distMat <- as.matrix(distMat)
  rownames(distMat) <- colnames(distMat)
  diag(distMat) <- 0
  print("distMat matrix is computed")
  set.seed(123456)
  vect <- cluster::pam(distMat, nbCluster, diss = TRUE)$clustering
  # setTxtProgressBar(pb, 1)
  print("clustering is going well")
  if(is.null(className)){
    className <- as.character("Class")
  }
  print("now a rbindlist")

  allTypDay <- rbindlist(sapply(1:nbCluster, function(X){
    # Found a representative day for each class
    print("go")
    data <- .getDataAndMakeOutput(X, vect, distMat, className)
    print("one iteration")
    data
  }, simplify = FALSE))
  print("rbind done")

  allTypDay <- .addVerticesAndPlansToTp(allTypDay, VERT, PLAN)
  print("vertices added")

  nb <- id_start:(id_start+nrow(allTypDay)-1)
  allTypDay$idDayType <- nb
  print(allTypDay)

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
  print("cool")

  allTypDay
}

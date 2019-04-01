.ctrlFile <- function(path_data = NULL, path_file) {
  if (is.null(path_data)) {
    sep <- ""
  } else {
    sep <- "/"
  }
  if (grepl(pattern = "\\.csv$|\\.CSV$", x = path_file)) {
    data <- fread(paste(
      path_data, path_file, sep = sep))
  } else if (grepl(pattern = "\\.rds$|\\.RDS$", x =path_file)) {
    data <- readRDS(
      paste(path_data, path_file, sep =sep))
  } else {
    stop("Your input data must be a rds or a csv")
  }
  data
}

.ctrlPtdfMatrixFactor_Constraint <- function(
  dtPtdfMatrixFactor, dtPtdfMatrixConstraint, dtPtdfId) {
  
  if (!all(colnames(dtPtdfMatrixFactor) %in% c(
    "SESSION_ID", "MATRIX_ID", "ROW_ID", "BIDDINGAREA_ID", "FACTOR")) |
    ncol(dtPtdfMatrixFactor) != 5) {
    stop(paste("You must have five colnames in PtdfMatrixFactor and these colnames should be",
               "SESSION_ID, MATRIX_ID, ROW_ID, BIDDINGAREA_ID, FACTOR"))
  }
  
  if (!all(colnames(dtPtdfMatrixConstraint) %in% c(
    "SESSION_ID", "MATRIX_ID", "ROW_ID", "REMAININGAVAILABLEMARGIN")) |
    ncol(dtPtdfMatrixConstraint) != 4) {
    stop(paste("You must have four colnames in PtdfMatrixFactor and these colnames should be",
               "SESSION_ID, MATRIX_ID, ROW_ID, REMAININGAVAILABLEMARGIN"))
  }
  lev <- levels(as.factor(dtPtdfMatrixFactor[["BIDDINGAREA_ID"]]))
  if (!all(lev %in% 
           dtPtdfId[["ptdf_id"]])) {
    stop(paste("Your ptdf id in the column BIDDINGAREA_ID must be in",
               dtPtdfId[["ptdf_id"]]))
  }
  if (nrow(dtPtdfMatrixFactor[FACTOR < -1 |
                              FACTOR > 1]) > 0) {
    stop("Your ptdf values must be between -1 and 1")
  }
  if(nrow(dtPtdfMatrixFactor[MATRIX_ID < 1 | MATRIX_ID > 24]) > 0 |
     nrow(dtPtdfMatrixConstraint[MATRIX_ID < 1 | MATRIX_ID > 24]) > 0) {
    warning("You have Period > 24 or < 1, the lines concerned will be deleted")
    dtPtdfMatrixFactor <- dtPtdfMatrixFactor[MATRIX_ID > 0 & MATRIX_ID < 25]
    dtPtdfMatrixConstraint <- dtPtdfMatrixConstraint[MATRIX_ID > 0 & MATRIX_ID < 25]
  }
  
  return(list(dtPtdfMatrixConstraint = dtPtdfMatrixConstraint, 
              dtPtdfMatrixFactor = dtPtdfMatrixFactor))
}


.crtlNA <- function(data) {
  sapply(colnames(data), function(col) {
    if (any(is.na(data[[col]]))) {
      warning(paste("You have na in the column named", col, "be carefull with that"))
    }
  })
}

.ctrlDates <- function(dates, dayInVertices){
  if(!any(dates%in%dayInVertices)){
    stop("One(some) season(s) are not in vertices data.")
  }
  
  if(!all(dates%in%dayInVertices)){
    warning("Somes dates in calendar are not in vertices data.")
  }
  
  if(length(dates) < 2){
    stop("Clustering cannot be performed when class(season/type of day) contains less than 2 days")
  }
  if(all(dates%in%dayInVertices)){
    message("Good, all dates are in vertices data")
    
  }
}

.ctrlTimestamp <- function(data)
{
  if (is.null(data$timestamp)) {
    stop("You need the timestamp column in order to keep going, the format needed is : YYYY-MM-DDTHH:mm:ssZ")
  }
  if (all(grepl("^[0-9]{4}-[0-1][0-2]-[0-3][0-9]T[0-2][0-9]:[0]{2}:[0]{2}", data$timestamp))) {
    message("Good, your timestamp column has the good format")
  } else {
    stop("Your timestamp has ambiguous format, needed is YYYY-MM-DDTHH:mm:ssZ")
  }
  data$timestamp <- as.character(data$timestamp)
  data
}

.ctrlWeight <- function(hourWeight){
  #control Weigth
  if(length(hourWeight)!=24){
    stop("Length of hourWeight must be 24")
  }
  
}

.ctrlVertPlanFormat <- function(VERT, PLAN) {
  col_plan <- colnames(PLAN)
  col_vert <- colnames(VERT)
  col_ptdf_plan <- col_plan[grep("^ptdf[A-Z]{2}$", col_plan)]
  col_ptdf_vert <- col_plan[grep("^ptdf[A-Z]{2}$", col_vert)]
  if(length(col_ptdf_plan) == 0 | length(col_ptdf_vert) == 0) {
    stop("VERT & PLAN must have ptdf colnames in the form ptdfXX (ex : ptdfFR)")
  }
  if(!all(col_ptdf_plan %in% col_ptdf_vert) |
     !all(col_ptdf_vert %in% col_ptdf_plan)) {
    stop(cat("PLAN & VERT must have the same ptdf colnames, \n Currently for PLAN:",
             col_ptdf_plan, " \n Currently for VECT:", col_ptdf_vert))
  }
  if(is.null(PLAN$ram)) {
    stop("PLAN should contains the column named ram")
  }
  message("Good: columns of VERT & PLAN match")
}


.getDataAndMakeOutput <- function(X, vect, distMat, className)
{
  dateIn <- names(vect[which(vect == X)])
  colSel <- row.names(distMat)%in%dateIn
  
  #detect day closed to middle of cluster
  if(length(dateIn) > 1)
  {
    minDay <- which.min(rowSums(distMat[, colSel]))
    distINfo <- distMat[minDay,colSel]
    data.table(TypicalDay = names(minDay),
               Class = className,
               dayIn = list(data.table(Date = rep(dateIn, each = 24), 
                                       Period = rep(1:24, length(dateIn)))),
               distance = list(data.table(Date = dateIn, Distance = distINfo)))
  }
  # case where cluster is of size one :
  else
  {
    minDay <- dateIn
    distINfo <- 0
    data.table(TypicalDay = minDay,
               Class = className,
               dayIn = list(data.table(Date = rep(dateIn, each = 24),
                                       Period = rep(1:24, length(dateIn)))),
               distance = list(data.table(Date = dateIn, Distance = distINfo)))
  }
}

.addVerticesAndPlansToTp <- function(allTypDay, VERT, PLAN)
{
  col_ptdf <- colnames(VERT)[grep("^ptdf[A-Z]{2}", colnames(VERT))]
  # col_ptdf_vert <- paste(col_ptdf, "VERT", sep = "_")
  # col_ptdf_plan <- paste(col_ptdf, "PLAN", sep = "_")
  # colnames(VERT)[colnames(VERT) %in% col_ptdf] <- col_ptdf_vert
  # colnames(PLAN)[colnames(PLAN) %in% col_ptdf] <- col_ptdf_plan
  
  for(i in 1:nrow(allTypDay)){
    allTypDay$dayIn[[i]] <- rbindlist(
      sapply(1:nrow(allTypDay$dayIn[[i]]), function(X){
        date <- allTypDay$dayIn[[i]][X, Date]
        period <- allTypDay$dayIn[[i]][X, Period]
        
        if (nrow(VERT[Date == date & Period == period]) > 0 &
            nrow(PLAN[Date == date & Period == period]) > 0) {
          data.table(Date = date, Period = period, VERT_details = list(VERT[
            Date == date & Period == period, .SD, .SDcols = c("Date", "Period", col_ptdf)]), 
            PLAN_details = list(PLAN[
              Date == date & Period == period, .SD, .SDcols = c("Date", "Period", col_ptdf)
              ]))
        } else {
          data.table()
        }
      }))
  }
  allTypDay
}

.crtOutFile <- function(allTypDay, reportPath = NULL){
  if (is.null(reportPath)) {
    reportPath <- getwd()
  }
  outputFile <- reportPath
  direName <-  as.character(Sys.time())
  direName <- gsub(" ", "", gsub( ":", "",direName))
  reportDir <- paste0(outputFile, "/fb-clustering-", direName)
  suppressWarnings(dir.create(reportDir))
  if (length(list.dirs(reportDir)) == 0) {
    stop(paste("The directory", reportPath, "does not exist"))
  }
  outputFile <- reportDir
  step <- length(allTypDay$idDayType)
  list(outputFile = outputFile, step = step)
}
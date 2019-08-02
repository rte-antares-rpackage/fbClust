.crtlPlan <- function(PLAN) {
  if (!("data.table" %in% class(PLAN))) {
    stop("The object PLAN should be a data.table")
  }
  if (nrow(PLAN) == 0) {
    stop("The object PLAN has no row")
  }
  if (!all(c("Date", "Period", "ram") %in% colnames(PLAN)) |
      !any(grepl("^ptdf", colnames(PLAN)))) {
    stop(paste("PLAN should contains at least, a column named Date, one named Period,",
               "one named ram and one starting with ptdf.", "Currently :",
               paste(colnames(PLAN), collapse = ", ")))
  }
}


.addSignToVertices <- function(VERT) {
  
  # remove NOTE data.table
  Date <- NULL
  Period <- NULL
  
  
  col_vert <- colnames(VERT)[!grepl("Date|Period", colnames(VERT))]
  if (all(!grepl("sign|N|nbsign", col_vert))) {
  vert_sign <- VERT[, lapply(.SD, function(X) {(X > 0)}) , list(Date, Period),
                        .SDcols = col_vert]
  sign <- vert_sign[, get(col_vert[1])]
  for (i in 2:length(col_vert)) {
    sign <- paste(sign, vert_sign[, get(col_vert[i])])
  }
  VERT <- cbind(VERT, sign = sign)
  VERT <- merge(VERT, VERT[, .N, by = c("Date", "Period", "sign")], 
                by = c("Date", "Period", "sign"))
  VERT <- merge(VERT, VERT[, unique(sign), by = c("Date", "Period")][, list(
    nbsign = .N), by = c("Date", "Period")], by = c("Date", "Period"))
  }
  VERT
}

.crtlAllTypeDay <- function(allTypeDay) {
  
  # remove NOTE data.table
  dayIn <- NULL
  
  suppressWarnings(if (!all(colnames(allTypeDay) == c(
    "TypicalDay", "Class", "dayIn", "distance", "idDayType")) |
    length(colnames(allTypeDay)) != 5) {
    stop(paste("The colnames of allTypeDay must be the following, in this order :",
               "TypicalDay,", "Class,", "dayIn,", "distance,", "idDayType"))
  })
  listdt <- lapply(allTypeDay[, dayIn], function(dt) {
    dt
  })
  for (i in 1:length(listdt)) {
    listdt[[i]]$Date <- as.character(listdt[[i]]$Date)
  }
  if (!all(names(rbindlist(listdt)) == c(
    "Date", "Period", "VERT_details", "PLAN_details", "PLANRaw_details"))) {
    stop(paste("The colnames of allTypeDay[, dayIn] must be the following, in this order :",
               "Date,", "Period,", "VERT_details,", "PLAN_details,", "PLANRaw_details"))
  }
  # if (!all(names(rbindlist(allTypeDay[, dayIn])) == c(
  #   "Date", "Period", "VERT_details", "PLAN_details", "PLANRaw_details"))) {
  #   stop(paste("The colnames of allTypeDay[, dayIn] must be the following, in this order :",
  #              "Date,", "Period,", "VERT_details,", "PLAN_details,", "PLANRaw_details"))
  # }
}


.crtlCountriesCombn <- function(countries) {
  if(class(countries) == "list") {
    data <- data.frame(rbindlist(lapply(1:length(countries), function(X) {
      if(length(countries[[X]]) != 2) {
        stop(paste("The combination of countries must all be of length 2, currrently for the",
                   X, "element of the list :", length(countries[[X]])))
      }
      data.frame("X1" = countries[[X]][1], "X2" = countries[[X]][2])
    })))
  } else if(class(countries) == "character") {
    data <- data.frame(t(utils::combn(countries, 2)))
  } else {
    stop("countries type can only be list or character")
  }
  data
}


.ctrlCalendar <- function(calendar){
  
  if(any(!names(calendar)%in% c("interSeasonWe",
                                "interSeasonWd",
                                "winterWe",
                                "winterWd",
                                "summerWe",
                                "summerWd")) | is.null(names(calendar))){
    
    stop("Names of calendar must be 'interSeasonWe',
         'interSeasonWd', 'winterWe',
         'winterWd', 'summerWe', 'summerWd'")
  }
  
}

.ctrlHubDrop <- function(hubDrop, PLAN) {
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  if (!grepl("ptdf", names(hubDrop))) {
    ptdf_hubDrop <- c(paste0("ptdf", names(hubDrop)))
  }
  ptdf_hubDrop <- c(ptdf_hubDrop, sapply(names(hubDrop), function(X) {
    sapply(1:length(hubDrop[[X]]), function(i) {
      paste0("ptdf", hubDrop[[X]][i])
    })
  }))
  
  if (!all(ptdf_hubDrop %in% col_ptdf)) {
    warning(paste(paste(
      ptdf_hubDrop[!(ptdf_hubDrop %in% col_ptdf)], collapse = " "),
      "is (are) not in ptdf name"))
  }

  if (!all(col_ptdf %in% ptdf_hubDrop)) {
    stop("hubDrop does not contain all the ptdf in PLAN")
  }
} 

.ctrlFile <- function(pathFile) {
  # if (is.null(path_data)) {
  #   sep <- ""
  # } else {
  #   sep <- "/"
  # }
  if (grepl(pattern = "\\.csv$|\\.CSV$", x = pathFile)) {
    # data <- fread(paste(
    #   path_data, pathFile, sep = sep))
    data <- fread(pathFile)
  } else if (grepl(pattern = "\\.rds$|\\.RDS$", x =pathFile)) {
    # data <- readRDS(
    #   paste(path_data, pathFile, sep =sep))
    data <- readRDS(pathFile)
  } else {
    stop("Your input data must be a rds or a csv")
  }
  data
}

.ctrlPtdfMatrixFactorConstraint <- function(
  dtPtdfMatrixFactor, dtPtdfMatrixConstraint, dtPtdfId) {
  
  # remove NOTE data.table
  FACTOR <- NULL
  MATRIX_ID <- NULL
  
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
    stop(cat("Your ptdf id in the column BIDDINGAREA_ID must be in",
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
  
  if(any(nchar(dtPtdfMatrixFactor[["SESSION_ID"]]) != "8") |
     any(nchar(dtPtdfMatrixConstraint[["SESSION_ID"]]) != "8")) {
    stop("The column SESSION_ID has to be in format YYYYMMDD before the pre-processing")
  }
  
  if (!all(dtPtdfMatrixFactor$SESSION_ID %in% 
           dtPtdfMatrixConstraint$SESSION_ID &
           dtPtdfMatrixConstraint$SESSION_ID %in% dtPtdfMatrixFactor$SESSION_ID) |
      !all(dtPtdfMatrixFactor$ROW_ID %in% 
           dtPtdfMatrixConstraint$ROW_ID &
           dtPtdfMatrixConstraint$ROW_ID %in% dtPtdfMatrixFactor$ROW_ID) |
      !all(dtPtdfMatrixFactor$MATRIX_ID %in% 
           dtPtdfMatrixConstraint$MATRIX_ID &
           dtPtdfMatrixConstraint$MATRIX_ID %in% dtPtdfMatrixFactor$MATRIX_ID)) {
    warning(paste("Some id(s) are in one of dtPtdfMatrixFactor or dtPtdfMatrixConstraint",
                  "and not in the other"))
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
  
  col_plan_all <- colnames(PLAN)
  col_vert_all <- colnames(VERT)
  col_ptdf <- col_plan_all[grep("^ptdf[A-Z]{2}$", col_plan_all)]
  col_vert <- col_vert_all[!grepl("Date|Period|N|nbsign|sign", col_vert_all)]
  
  if(length(col_ptdf) == 0 | length(col_vert) == 0) {
    stop("PLAN must have ptdf colnames in the form ptdfXX (ex : ptdfFR) & VERT in the form XX (ex : FR)")
  }
  if(!all(gsub("ptdf", "", col_ptdf) %in% col_vert) |
     !all(col_vert %in% gsub("ptdf", "", col_ptdf))) {
    stop(cat(
      "PLAN & VERT must have same hub names, with ptdf for PLAN and not for VECT, 
             \n Currently for PLAN:", col_ptdf, " \n Currently for VECT:", col_vert))
  }
  if(is.null(PLAN$ram)) {
    stop("PLAN should contains the column named ram")
  }
  message("Good: columns of VERT & PLAN match")
}


.getDataAndMakeOutput <- function(X, vect, distMat, className)
{
  # browser()
  dateIn <- names(vect[which(vect == X)])
  colSel <- row.names(distMat)%in%dateIn
  
  #detect day closed to middle of cluster
  if(length(dateIn) > 1)
  {
    #### Modif Date en as.character
    minDay <- which.min(rowSums(distMat[, colSel]))
    distINfo <- distMat[minDay,colSel]
    data.table(TypicalDay = names(minDay),
               Class = className,
               dayIn = list(data.table(Date = as.character(rep(dateIn, each = 24)), 
                                       Period = rep(1:24, length(dateIn)))),
               distance = list(data.table(Date = as.character(dateIn), Distance = distINfo)))
  }
  # case where cluster is of size one :
  else
  {
    minDay <- dateIn
    distINfo <- 0
    #### Modif Date en as.character
    data.table(TypicalDay = minDay,
               Class = className,
               dayIn = list(data.table(Date = as.character(rep(dateIn, each = 24)),
                                       Period = rep(1:24, length(dateIn)))),
               distance = list(data.table(Date = as.character(dateIn), Distance = distINfo)))
  }
}

.addVerticesAndPlansToTp <- function(allTypDay, VERT, PLAN, PLANRaw)
{
  # remove NOTE data.table
  Date <- NULL
  Period <- NULL
  
  
  col_vert <- colnames(VERT)[!grepl("Period|Date", colnames(VERT))]
  col_ptdf <- colnames(PLAN)[grep("^ptdf[A-Z]{2}", colnames(PLAN))]
  col_ptdf_raw <- colnames(PLANRaw)[grep("^ptdf[A-Z]{2}", colnames(PLANRaw))]

  for(i in 1:nrow(allTypDay)){
    allTypDay$dayIn[[i]] <- rbindlist(
      sapply(1:nrow(allTypDay$dayIn[[i]]), function(X){
        date <- allTypDay$dayIn[[i]][X, Date]
        period <- allTypDay$dayIn[[i]][X, Period]
        
        if (nrow(VERT[Date == date & Period == period]) > 0 &
            nrow(PLAN[Date == date & Period == period]) > 0) {
          data.table(Date = date, Period = period, VERT_details = list(VERT[
            Date == date & Period == period, .SD, .SDcols = c("Date", "Period", col_vert)]), 
            PLAN_details = list(PLAN[
              Date == date & Period == period, .SD, .SDcols = c("Date", "Period", col_ptdf, "ram")
              ]),
            PLANRaw_details = list(PLANRaw[
              Date == date & Period == period, .SD, .SDcols = c("Date", "Period", col_ptdf_raw, "ram")
              ]))
        } else {
          data.table()
        }
      }, simplify = FALSE))
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
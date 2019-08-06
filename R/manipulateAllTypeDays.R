#' @title Manipulate the output data.table of the clustering function
#'
#' @description This function has been built in order to help the user to manipulate
#' the output object of the functions \link{clusterTypicalDaysForOneClass} and
#' \link{clusteringTypicalDays}. The user can choose to get a summary of the clustering realised,
#' the vertices of the input data, the raw ptdf of the input data or the ptdf after 
#' differenciation of the input data.
#' 
#' @param allTypeDay \code{data.table} the output data.table of one of the functions 
#' \link{clusterTypicalDaysForOneClass} and \link{clusteringTypicalDays}.
#' \itemize{
#'  \item TypicalDay : The typical day of the cluster defined by the idDayType column.
#'  \item Class : The class of the clustering (typically, one of the classes obtained
#'  with the \link{getCalendar} function).
#'  \item dayIn : The details of the days in the cluster.
#'  \item distance : The distance of each day of the cluster to the typical day.
#'  \item idDayType : The id of the cluster.
#' }
#' 
#' @param output \code{character} The type of output you want to return (either ptdf, ptdfraw, vertices or summary)
#' \itemize{
#'  \item ptdf : The ptdf obtained after differenciation 
#'  \item ptdfraw : The raw ptdf (obtained just after \link{getPreprocPlan})
#'  \item vertices : The vertices obtained with the function \link{getVertices}
#'  \item summary : A summary of the clustering in a data.table with the following column :
#'  \itemize{
#'   \item TypicalDay : The typical day of the cluster
#'   \item idDayType : The id of the cluster
#'   \item Class : The class of the clustering
#'   \item nbDaysCluster : The number of days in the cluster
#'   }
#' }
#' 
#' @examples
#' 
#' \dontrun{
#' library(data.table)
#' allTypeDay <- readRDS(system.file("testdata/allTypDays.rds", package = "fbClust"))
#' 
#' PLAN <- manipulateAllTypeDays(allTypeDay, "ptdf")
#' PLANRaw <- manipulateAllTypeDays(allTypeDay, "ptdfraw")
#' VERT <- manipulateAllTypeDays(allTypeDay, "vertices")
#' summary <- manipulateAllTypeDays(allTypeDay, "summary")
#' 
#' }
#' 
#' @import data.table
#' @export
manipulateAllTypeDays <- function(allTypeDay, output) {
  # browser()
  # remove NOTE data.table
  Class <- NULL
  idDayType <- NULL
  VERT_details <- PLANRaw_details <- PLAN_details <- NULL
  .crtlAllTypeDay(allTypeDay)
  if (length(output) != 1) {
    stop("The length of the ouput must be 1")
  }
  class <- unique(allTypeDay[, Class])
  if (output == "vertices") {
    
    lenDiffCols <- length(unique(sapply(1:nrow(allTypeDay), function(X) {
      colnames(allTypeDay[, dayIn][[X]][, VERT_details][[1]])
    })))
    
    colorder <- colnames(allTypeDay[, dayIn][[1]][, VERT_details][[1]])
    if(lenDiffCols > 1) {
      colorder <- colorder[!grepl("N|nbsign|sign", colorder)]
    }
    dt_output <- rbindlist(sapply(class, function(cl) {
      
      # remove NOTE data.table
      idDayType <- NULL
      dayIn <- NULL
      VERT_details <- NULL
      
      data <- allTypeDay[Class == cl]
      data <- rbindlist(sapply(unique(data[, idDayType]), function(X) {
        
        lst <- data[idDayType == X, dayIn][[1]][, VERT_details]
        # if (!(all(colnames(lst[[1]]) == colorder))) {
        for (i in 1:length(lst)) {
          lst[[i]] <- lst[[i]][, .SD, .SDcols = colorder]
          setcolorder(lst[[i]], colorder)
        }
        # }
        
        if (class(lst[[1]]$Date) == "Date") {
          for (i in 1:length(lst)) {
            lst[[i]]$Date <- as.character(lst[[i]]$Date)
          }
          # lapply(lst, function(dt) {
          #   dt[['Date']] <- as.character(dt[['Date']])
          # })
        }
        
        data.table(rbindlist(lst), Class = cl, idDayType = X)
        
      }, simplify = F))
      data
    }, simplify = F))
    
  } else if (output == "ptdfraw") {
    
    colorder <- colnames(allTypeDay[, dayIn][[1]][, PLANRaw_details][[1]])
    dt_output <- rbindlist(sapply(class, function(cl) {
      # browser()
      # remove NOTE data.table
      idDayType <- NULL
      dayIn <- NULL
      PLANRaw_details <- NULL
      
      data <- allTypeDay[Class == cl]
      data <- rbindlist(sapply(unique(data[, idDayType]), function(X) {
        # browser()
        lst <- data[idDayType == X, dayIn][[1]][, PLANRaw_details]
        if (!(all(colnames(lst[[1]]) == colorder))) {
          for (i in 1:length(lst)) {
            setcolorder(lst[[i]], colorder)
          }
        }
        
        if (class(lst[[1]]$Date) == "Date") {
          for (i in 1:length(lst)) {
            lst[[i]]$Date <- as.character(lst[[i]]$Date)
          }
          # lapply(lst, function(dt) {
          #   dt[['Date']] <- as.character(dt[['Date']])
          # })
        }
        
        data.table(rbindlist(lst), Class = cl, idDayType = X)
      }, simplify = F))
      data
    }, simplify = F))
    
  } else if (output == "ptdf") {
    
    colorder <- colnames(allTypeDay[, dayIn][[1]][, PLAN_details][[1]])
    dt_output <- rbindlist(sapply(class, function(cl) {
      
      # remove NOTE data.table
      idDayType <- NULL
      dayIn <- NULL
      PLAN_details <- NULL
      
      data <- allTypeDay[Class == cl]
      data <- rbindlist(sapply(unique(data[, idDayType]), function(X) {
        
        lst <- data[idDayType == X, dayIn][[1]][, PLAN_details]
        if (!(all(colnames(lst[[1]]) == colorder))) {
          for (i in 1:length(lst)) {
            setcolorder(lst[[i]], colorder)
          }
        }
        
        if (class(lst[[1]]$Date) == "Date") {
          for (i in 1:length(lst)) {
            lst[[i]]$Date <- as.character(lst[[i]]$Date)
          }
          # lapply(lst, function(dt) {
          #   dt[['Date']] <- as.character(dt[['Date']])
          # })
        }
        
        data.table(rbindlist(lst), Class = cl, idDayType = X)
        
        
      }, simplify = F))
      data
    }, simplify = F))
    
  } else if (output == "summary") {
    
    # remove NOTE data.table
    idDayType <- NULL
    TypicalDay <- NULL
    dayIn <- NULL
    Date <- NULL
    
    id <- allTypeDay[, idDayType]
    class <- allTypeDay[, Class]
    typDay <- allTypeDay[, TypicalDay]
    nbDaysCluster <- c(sapply(1:length(allTypeDay[, dayIn]), function(X) {
      length(unique(allTypeDay[, dayIn][[X]][, Date]))
    }))
    dt_output <- data.table(
      TypicalDay = typDay, idDayType = id, Class = class, nbDaysCluster = nbDaysCluster)
  } else {
    stop("You chose a wrong output, possible outputs are vertices, ptdfraw, ptdf and summary")
  }
  return(dt_output)
  
  
}


#' @title Write the output data of a clustering in csv file
#'
#' @description This function has been built to give the possibility to the user
#' to keep the results of the clustering in a csv file using the output object of 
#' the functions \link{clusterTypicalDaysForOneClass} and
#' \link{clusteringTypicalDays}. The user can choose to get 
#' the vertices of the input data, the raw ptdf of the input data or the ptdf after 
#' differenciation of the input data. One of the interests of this function is to 
#' get the ptdf to use easily the package fbAntares
#' 
#' @param allTypeDay \code{data.table} the output data.table of one of the functions 
#' \link{clusterTypicalDaysForOneClass} and \link{clusteringTypicalDays}.
#' \itemize{
#'  \item TypicalDay : The typical day of the cluster defined by the idDayType column.
#'  \item Class : The class of the clustering (typically, one of the classes obtained
#'  with the \link{getCalendar} function).
#'  \item dayIn : The details of the days in the cluster.
#'  \item distance : The distance of each day of the cluster to the typical day.
#'  \item idDayType : The id of the cluster.
#' }
#' 
#' @param output \code{character} The type of output you want to return 
#' (either ptdf, ptdfraw, vertices), default ptdf
#' \itemize{
#'  \item ptdf : The ptdf obtained after differenciation 
#'  \item ptdfraw : The raw ptdf (obtained just after \link{getPreprocPlan})
#'  \item vertices : The vertices obtained with the function \link{getVertices}
#' }
#' 
#' @param outputFile \code{character} The directory where you want to save the file.
#' @param csv \code{logical} If you want to write a csv or a rds, default TRUE
#' @param onlyTypicalDay \code{logical} If you want only the typical days or all 
#' days of the clustering
#' 
#' @examples
#' 
#' \dontrun{
#' library(data.table)
#' allTypeDay <- readRDS(system.file("testdata/allTypDays.rds", package = "fbClust"))
#' 
#' output <- "ptdf
#' outputFile <- tempdir()
#' csv <- T
#' onlyTypicalDay <- T
#' 
#' writeAllTypeDays(allTypeDay, output, outputFile, csv, onlyTypicalDay)
#' }
#' 
#' @import data.table
#' @export
writeAllTypeDays <- function(allTypeDay, output = "ptdf", outputFile, csv = T,
                             onlyTypicalDay = T) {
  
  Date <- TypicalDay <- NULL
  if (!(output %in% c("ptdfraw", "ptdf", "vertices"))) {
    stop("You chose a wrong output, possible outputs are vertices, ptdfraw and ptdf")
  }
  dtOutput <- manipulateAllTypeDays(allTypeDay = allTypeDay, output = output)
  if (onlyTypicalDay) {
    dtOutput <- dtOutput[Date %in% allTypeDay$TypicalDay]
  }
  dtOutput[Date %in% allTypeDay$TypicalDay, TypicalDay := T]
  dtOutput[!(Date %in% allTypeDay$TypicalDay), TypicalDay := F]
  if (csv) {
    fwrite(file = paste(outputFile, paste0(Sys.Date(), output, ".csv"), sep = "/"), 
           x = dtOutput, sep = "|")
  } else {
    saveRDS(dtOutput, paste(outputFile, paste0(Sys.Date(), output, ".rds"), sep = "/"))
  }
  
}



#' @title Manipulate the output data.table of the clustering function
#'
#' @description This function has been built in order to help the user to manipulate
#' the output object of the functions \link{clusterTypicalDaysForOneClass} and
#' \link{clusterTypicalDays}. The user can choose to get a summary of the clustering realised,
#' the vertices of the input data, the raw ptdf of the input data or the ptdf after 
#' differenciation of the input data.
#' 
#' @param allTypeDayr the output data.table of one of the functions 
#' \link{clusterTypicalDaysForOneClass} and \link{clusterTypicalDays}.
#' \itemize{
#'  \item TypicalDay : The typical day of the cluster defined by the idDayType column.
#'  \item Class : The class of the clustering (typically, one of the classes obtained
#'  with the \link{getCalendar} function).
#'  \item dayIn : The details of the days in the cluster.
#'  \item distance : The distance of each day of the cluster to the typical day.
#'  \item idDayType : The id of the cluster.
#' }
#' 
#' @param output The type of output you want to return (either ptdf, ptdfraw, vertices or summary)
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
#' PLAN_raw <- manipulateAllTypeDays(allTypeDay, "ptdfraw")
#' VERT <- manipulateAllTypeDays(allTypeDay, "vertices")
#' summary <- manipulateAllTypeDays(allTypeDay, "summary")
#' 
#' }
#' 
#' @import data.table
#' @export
manipulateAllTypeDays <- function(allTypeDay, output) {
  .crtlAllTypeDay(allTypeDay)
  if (length(output) != 1) {
    stop("The length of the ouput must be 1")
  }
  class <- unique(allTypeDay[, Class])
  if (output == "vertices") {
    dt_output <- rbindlist(sapply(class, function(cl) {
      data <- allTypeDay[Class == cl]
      data <- rbindlist(sapply(unique(data[, idDayType]), function(X) {
        data.table(rbindlist(data[idDayType == X, dayIn][[1]][, VERT_details]),
                   Class = cl, idDayType = X)
      }, simplify = F))
      data
    }, simplify = F))
    
  } else if (output == "ptdfraw") {
    dt_output <- rbindlist(sapply(class, function(cl) {
      data <- allTypeDay[Class == cl]
      data <- rbindlist(sapply(unique(data[, idDayType]), function(X) {
        data.table(rbindlist(data[idDayType == X, dayIn][[1]][, PLAN_raw_details]),
                   Class = cl, idDayType = X)
      }, simplify = F))
      data
    }, simplify = F))
    
  } else if (output == "ptdf") {
    dt_output <- rbindlist(sapply(class, function(cl) {
      data <- allTypeDay[Class == cl]
      data <- rbindlist(sapply(unique(data[, idDayType]), function(X) {
        data.table(rbindlist(data[idDayType == X, dayIn][[1]][, PLAN_details]), 
                   Class = cl, idDayType = X)
      }, simplify = F))
      data
    }, simplify = F))
    
  } else if (output == "summary") {
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

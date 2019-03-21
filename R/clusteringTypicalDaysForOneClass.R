# clusterTypicalDaysForOneClass <- function(dates,
#                                           PLAN,
#                                           VERT = NULL, 
#                                           nbCluster = NULL,
#                                           reportPath = NULL,
#                                           hourWeight = rep(1, 24),
#                                           className = NULL,
#                                           report = TRUE, 
#                                           id_start = 1,
#                                           maxDomainSize = 10000) {
#   
#   if(is.null(VERT)) {
#     VERT <- fbTools::getVertices(PLAN)
#   }
#   PLAN <- .ctrlTimestamp(PLAN)
#   VERT <- .ctrlTimestamp(VERT)
#   
#   PLAN <- .transformTS(PLAN)
#   VERT <- .transformTS(VERT)
#   col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
#   Max <- VERT[,max(unlist(.SD)), by = c("Date", "Period"), .SDcols = col_ptdf]
#   Max[, isSupLim := V1 > maxDomainSize]
#   Max <- Max[Max$isSupLim]
#   if(nrow(Max) > 0){
#     Max <- Max[,list(list(Period)), by = "Date"]
#     
#     stop( paste("The following flow-based domains exceed the expected maximum size :", 
#                 paste(Max$Date, "(hour : ",lapply(
#                   Max$V1, function(X){paste(X, collapse = ", ")}),")", collapse = ", ")))
#   }
#   
#   dates <- as.character(dates)
#   .ctrlDates(dates, unique(VERT$Date))
# }



# toto <- data.table(plop = c((1:8), rep(NA, 10), 15, NA))
# toto  
# shift(toto[, plop], n = 1, )

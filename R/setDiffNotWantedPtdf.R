#' Compute differance for remove contry
#' 
#' @description Remove country in not_wanted_col
#' 
#' @param PLAN {data.table}
#' @param not_wanted_col {character}
#' 
setDiffNotWantedPtdf <- function(PLAN, not_wanted_col = NULL)
{
  # PLAN2 <- copy(PLAN)
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  if (is.null(not_wanted_col)) {
    not_wanted_col <-  col_ptdf[length(col_ptdf)]
  }
  col_ptdf <- col_ptdf[-which(col_ptdf == not_wanted_col)]

  PLAN[, col_ptdf] <- sapply(col_ptdf, function(ptdf) {
    PLAN[[ptdf]] <- PLAN[[ptdf]] - PLAN[[not_wanted_col]]
  })
  # PLAN2[, c(col_ptdf) := lapply(col_ptdf, function(ptdf) {
  #   PLAN2[[ptdf]] - PLAN2[[not_wanted_col]]
  # })]
  PLAN[[not_wanted_col]] <- NULL
  return(PLAN)
}



######### COmpute the Ax0 matrix ####
##### matrix needed for the projection & check if interior point #####
##### The function also return if a point is interior or external


# .computeAx0_checkIntExt <- function(VERT, PLAN) {
#   Ax0_list <- lapply(1:nrow(VERT), function(X) {
#     V1 <- VERT[X]
#     Ax0 <- (as.matrix(
#       PLAN[, .SD, .SDcols = col_ptdf])%*%t(as.matrix(V1[, .SD, .SDcols = col_ptdf])))
#     if (sum(Ax0 > PLAN[, ram])) {
#       VERT[X, INTERIOR := F]
#       NA
#     } else {
#       Ax0
#     }
#   })
#   Ax0_list <- Ax0_list[!is.na(Ax0_list)]
# }





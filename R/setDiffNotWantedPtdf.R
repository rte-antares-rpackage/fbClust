#' @title Compute facets equations
#' 
#' @description Subtract one ptdf to the others and remove it to compute facets equations
#' 
#' @param PLAN {data.table}
#' @param not_wanted_col {character}, if NULL it takes the last ptdf. This paramater
#' has to be in the form ptdfXX (where XX is FR, DE, BE or NL for example).
#' 
#' @import data.table
#' 
#' @export

setDiffNotWantedPtdf <- function(PLAN, not_wanted_col = NULL)
{
  # PLAN2 <- copy(PLAN)
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  if (is.null(not_wanted_col)) {
    not_wanted_col <-  col_ptdf[length(col_ptdf)]
  }
  col_ptdf <- col_ptdf[-which(col_ptdf == not_wanted_col)]

  for(i in col_ptdf){
    PLAN[[i]] <- PLAN[[i]] - PLAN[[not_wanted_col]]
  }
  PLAN[[not_wanted_col]] <- NULL
  return(PLAN)
}







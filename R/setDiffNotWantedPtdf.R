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
  
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  
  if (is.null(not_wanted_col)) {
    not_wanted_col <-  col_ptdf[length(col_ptdf)]
  }
  if (!grepl("ptdf", not_wanted_col)) {
    not_wanted_col <- c(paste0("ptdf", not_wanted_col))
  }
  
  col_ptdf <- col_ptdf[-which(col_ptdf == not_wanted_col)]
  
  for(i in col_ptdf){
    PLAN[[i]] <- PLAN[[i]] - PLAN[[not_wanted_col]]
  }
  PLAN[[not_wanted_col]] <- NULL
  return(PLAN)
}

.setDiffNotWantedPtdf2 <- function(PLAN, hubDrop = list(
  NL = c("BE", "DE", "FR", "AT")))
{
  # browser()
  .ctrlHubDrop(PLAN = PLAN, hubDrop = hubDrop)
  for (X in names(hubDrop)) {
    vec_ptdf <- hubDrop[[X]]
    col_X <- paste0("ptdf", X)
    # if (!all(col_X %in% colnames(PLAN))) {
    #   stop("All hubDrop have to be in your PLAN")
    # }
    
    for(i in vec_ptdf){
      col_i <- paste0("ptdf", i)
      if (col_i %in% colnames(PLAN)) {
        PLAN[[col_i]] <- PLAN[[col_i]] - PLAN[[col_X]]
      }
    }
    PLAN[[col_X]] <- NULL
  }
  return(PLAN)
}

#' @title Compute facets equations
#' 
#' @description This function computes the facets equation by droping one or more hubs
#' and sustracting them to the others (with a named list).
#' 
#' @param PLAN {data.table}
#' @param hubDrop \code{list}, list of hubs in the ptdf, with the ones which should
#' sustracted to the others as the names of the arrays which themself contain the ones which
#' be sustracted
#' 
#' @import data.table
#' 
#' @export

setDiffNotWantedPtdf <- function(PLAN, hubDrop = list(
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

# setDiffNotWantedPtdf <- function(PLAN, not_wanted_col = NULL)
# {
#   
#   col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
#   
#   if (is.null(not_wanted_col)) {
#     not_wanted_col <-  col_ptdf[length(col_ptdf)]
#   }
#   if (!grepl("ptdf", not_wanted_col)) {
#     not_wanted_col <- c(paste0("ptdf", not_wanted_col))
#   }
#   
#   col_ptdf <- col_ptdf[-which(col_ptdf == not_wanted_col)]
#   
#   for(i in col_ptdf){
#     PLAN[[i]] <- PLAN[[i]] - PLAN[[not_wanted_col]]
#   }
#   PLAN[[not_wanted_col]] <- NULL
#   return(PLAN)
# }
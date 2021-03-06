#' @title Give the vertices from PTDF data
#' 
#' @description This function generates the vertices of the polyhedra from the facets
#' equation. It can only be computed after using \link{setDiffNotWantedPtdf} which
#' itself compute the plans equations.
#' Otherwise, it will crash because the ptdf won't correspond to facets equations.
#'
#' @param PTDF {data.frame | data.table} PLAN, at least Date, Period and two ptdf columns :
#' 
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#'  \item ram : line limits
#'  \item Date : date in format YYYY-MM-DD
#'  \item Period : hour in the day, between 1 and 24
#' }
#' @param ctrdel {character} name of country deleted (two maj letters,
#' ex : FR for France or NL for Nederlands), can be NULL

#'
#' @examples
#' \dontrun{
#' PLAN <- readRDS(system.file("testdata/plan_test.rds", package = "fbClust"))
#' 
#' VERT <- getVertices(PLAN)
#' }
#' @import vertexenum
#' @import data.table
#' 
#' @export
getVertices <- function(PTDF,  ctrdel = NULL){
  
  
  .crtlPlan(PTDF)
  
  PTDF <- data.table(PTDF)
  # browser()
  PTDF$timestamp <- paste(PTDF$Date, PTDF$Period, sep = "-")
  DDout <- sapply(unique(PTDF$timestamp), function(X){
    # remove NOTE data.table
    timestamp <- NULL
    
    DD <- .foundVertices(PTDF[timestamp == X], ctrdel = ctrdel)
    DD$timestamp <- X
    DD
  }, simplify = FALSE)
  end <- rbindlist(DDout)
  
  end$Date <- substr(end$timestamp, 1, 10)
  end$Period <- substr(end$timestamp, 12, 13)
  PTDF$timestamp <- NULL
  end$timestamp <- NULL
  colnames(end) <- gsub("ptdf", "", colnames(end))
  end
}


.foundVertices <- function(PTDF, ctrdel = NULL){
  # browser()
  ctry <- names(PTDF)[grep("ptdf", names(PTDF))]
  if(!is.null(ctrdel))
  {
    
    ctrdel <- paste0("ptdf", ctrdel)
    ctrnodel <- ctry[ctry!=ctrdel]
    
    for(i in ctrnodel){
      PTDF[[i]] <- PTDF[[i]] - PTDF[[ctrdel]]
    }
  }else{
    ctrnodel = ctry
  }
  vertices <- vertexenum::enumerate.vertices(
    as.matrix(PTDF[,.SD, .SDcols = ctrnodel]), PTDF$ram)
  if(all(vertices == rep(0,length(ctrnodel)))){
    vertices <- matrix(0, nrow = 1, ncol = length(ctrnodel))
  }
  
  vertices <- data.table(vertices)

  names(vertices) <- ctrnodel
  vertices
}





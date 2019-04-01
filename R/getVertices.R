#' vertices from PTDF data
#'
#' @param PTDF {data.frame | data.table}
#' @param ctrdel {character} name of country deleted
#'
#'
#' @examples
#' \dontrun{
#'   PTDFv <- getVertices(PTDF, "NL")
#' }
#' @import vertexenum
#' @export
getVertices <- function(PTDF,  ctrdel = NULL){
  PTDF <- data.table(PTDF)
  PTDF$timestamp <- paste(PTDF$Date, PTDF$Period, sep = "-")
  DDout <- sapply(unique(PTDF$timestamp), function(X){
    DD <- .foundVertices(PTDF[timestamp == X], ctrdel = ctrdel)
    DD$timestamp <- X
    DD
  }, simplify = FALSE)
  end <- rbindlist(DDout)
  
  end$Date <- substr(end$timestamp, 1, 10)
  end$Period <- substr(end$timestamp, 12, 12)
  PTDF$timestamp <- NULL
  end$timestamp <- NULL
  end
}


.foundVertices <- function(PTDF, ctrdel = NULL){
  
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
  vertices <- vertexenum::enumerate.vertices(as.matrix(PTDF[,.SD, .SDcols = ctrnodel]), PTDF$ram)
  vertices <- data.table(vertices)
  names(vertices) <- ctrnodel
  vertices
}





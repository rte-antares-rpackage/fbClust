#' @import data.table
#' 
######### Création data.table avec toutes les distances résultantes #####


.getDistMatrixV2 <- function(
  VERT, PLAN, hourWeight){
  # browser()
  set.seed(1234)
  PLAN[['ram']] <- PLAN[['ram']] + runif(nrow(PLAN))/10000
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  col_vert <- colnames(VERT)[!grepl("Date|Period", colnames(VERT))]
  
  res_hour <- data.frame(t(combn(unique(VERT[['Date']]), 2)))
  colnames(res_hour) <- c("V1", "V2")
  data <- rbindlist(sapply(1:nrow(res_hour), function(comb){

    date_1 <- as.character(res_hour[comb, "V1"])
    date_2 <- as.character(res_hour[comb, "V2"])
  
    v_hours <- intersect(unlist(VERT[Date%in% date_1, .SD, .SDcols = "Period"]), 
                         unlist(VERT[Date%in% date_2, .SD, .SDcols = "Period"]))

    rbindlist(sapply(v_hours, function(h){

      DD <- .dEnd(VERT[Date == date_1 & Period == h], col_vert = col_vert,
                  PLAN[Date == date_2 & Period == h], col_ptdf = col_ptdf)
      DD2 <- .dEnd(VERT[Date == date_2 & Period == h], col_vert = col_vert,
                   PLAN[Date == date_1 & Period == h], col_ptdf = col_ptdf)
      d <- DD + DD2
      weigthPond <- hourWeight[as.numeric(h)]
      d <- weigthPond * d
      # print(data.table(Date1 = c(date_1, date_2),
      #                  Date2 = c(date_2, date_1), Period = h, dist = d))
      data.table(Date1 = c(date_1, date_2),
                 Date2 = c(date_2, date_1), Period = h, dist = d)

    }, simplify = FALSE))
    
  }, simplify = FALSE))
  
}

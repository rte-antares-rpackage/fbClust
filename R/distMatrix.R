
######### Création data.table avec toutes les distances résultantes #####


.getDistMatrixV2 <- function(
  VERT, PLAN, hourWeight){
  set.seed(1234)
  PLAN[['ram']] <- PLAN[['ram']] + runif(nrow(PLAN))/10000
  # PLAN[, ram := ram + runif(nrow(PLAN))/10000]
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  print(col_ptdf)
  # res_hour <- data.table(t(combn(unique(VERT[, Date]), 2)))
  print(unique(VERT[['Date']]))
  res_hour <- data.frame(t(combn(unique(VERT[['Date']]), 2)))
  colnames(res_hour) <- c("V1", "V2")
  print(res_hour)
  data <- rbindlist(sapply(1:nrow(res_hour), function(comb){
    
    ##To sapply
    date_1 <- as.character(res_hour[comb, "V1"])
    date_2 <- as.character(res_hour[comb, "V2"])
    print(date_1)
    # v_hours <- intersect(VERT[Date%in% date_1, Period], VERT[Date%in% date_2, Period])
    # v_hours <- intersect(unlist(VERT[VERT$Date%in% date_1, "Period"]), unlist(VERT[VERT$Date%in% date_2, "Period"]))
    v_hours <- intersect(unlist(VERT[Date%in% date_1, .SD, .SDcols = "Period"]), 
                         unlist(VERT[Date%in% date_2, .SD, .SDcols = "Period"]))
    
    ##To sapply
    # h <- v_hours[1]
    rbindlist(sapply(v_hours, function(h){
      print(h)
      # print(paste("begin", date_1, date_2, Sys.time()))
      
      # DD <- .dEnd(VERT[Date == date_1 & Period == h],
      #             PLAN[Date == date_2 & Period == h], col_ptdf = col_ptdf)
      DD <- .dEnd(VERT[Date == date_1 & Period == h],
                  PLAN[Date == date_2 & Period == h], col_ptdf = col_ptdf)
      # print(paste("end", date_1, date_2, Sys.time()))
      # print(paste("begin", date_2, date_1, Sys.time()))
      
      # DD2 <- .dEnd(VERT[Date == date_2 & Period == h],
      #              PLAN[Date == date_1 & Period == h], col_ptdf = col_ptdf)
      DD2 <- .dEnd(VERT[Date == date_2 & Period == h],
                   PLAN[Date == date_1 & Period == h], col_ptdf = col_ptdf)
      # print(paste("end", date_2, date_1, Sys.time()))
      d <- DD + DD2
      print("it's ending")
      weigthPond <- hourWeight[h]
      d <- weigthPond * d
      print(data.table(Date1 = c(date_1, date_2),
                       Date2 = c(date_2, date_1), Period = h, dist = d))
      data.table(Date1 = c(date_1, date_2),
                 Date2 = c(date_2, date_1), Period = h, dist = d)
    }, simplify = FALSE))
    
  }, simplify = FALSE))
}

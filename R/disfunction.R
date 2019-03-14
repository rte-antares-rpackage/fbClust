setDiffNotWantedPtdf <- function(PLAN, not_wanted_col = NULL)
{
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  if (is.null(not_wanted_col)) {
    not_wanted_col <-  col_ptdf[length(col_ptdf)]
  }
  col_ptdf <- col_ptdf[-which(col_ptdf == not_wanted_col)]

  PLAN[, c(col_ptdf) := lapply(col_ptdf, function(ptdf) {
    PLAN[[ptdf]] - PLAN[[not_wanted_col]]
  })]
  PLAN[[not_wanted_col]] <- NULL
  return(PLAN)
}

############### transformTS ###############
#### Permet de jouer sur les dates et passer à l'heure française

.transformTS <- function(dt)
{
  dt$Period <- hour(dt$timestamp) + 1
  dt$Date <- as.character(as.Date(dt$timestamp))
  dt$timestamp <- NULL
  dt
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

.flagExtOrInt <- function(Ax0, b) {
  flag <- (sum(Ax0 > b) > 0)
  # vec <- (Ax0 > b)
  # flag <- any(vec)
  # flag <- any((Ax0 > b))
}

######### Compute the distance if external point ####
.getDistExt <- function(V1, PL , ram, Dmat)
{
  # sapply(1:nrow(VERT), function(X){
  # V1 <- VERT[X]
  ## dvec correspond aux coordonnées du sommet d'étude *2
  ## Dmat est créé dans la fonction dEnd, c'est une matrice diagonale,
  ## de diagonale = 2 et de taille le nombre de coordonnées du sommet
  ## (donc la dimension dans laquelle il est défini)
  ## Amat prend l'opposé de l'ensemble des coordonnées des hyperplans
  ## (une colonne = un hyperplan), sans prendre en compte la dernière dimension
  ## (ici NL)
  ## bvec permet de constuire la contrainte, il prend l'opposé de la variable
  ## ram qui correspond soit à la valeur max de transfert, soit au nb de lignes,
  ## à vérifier..
  ## PL2 <- -t(PL)
  re <- solve.QP(Dmat = Dmat,
                 dvec = V1,
                 Amat = PL,
                 bvec = -ram,meq = 0)
  # settings <- osqpSettings(verbose = FALSE)
  #    re = solve_osqp(P = Dmat, q = V1, A = t(PL), u = -ram, pars = settings)$x


  ## Distance euclidienne entre solution et point en entrée
  # print(re)
  val <- sqrt(sum((abs(re$solution-re$unconstrained.solution))^2))
  # val <- sqrt(sum((abs(V1-re))^2))
  # print(val)
  val
  # })
}
######### Compute the distance if interior point ####


.getDistInt <- function(V1, PL, ram, Ax0)
{

  norm <- rowSums(PL^2)
  val <- min(sqrt((abs(ram^2 + Ax0^2 - 2*ram*Ax0)/norm)))
  # print(val)
  val
}


######### dEnd : Récupération du calcul de distance entre polyèdres #####

dEnd <- function(VERT, PLAN, col_ptdf)
{
  Dmat <- diag(1, nrow = dim(VERT[, .SD, .SDcols = col_ptdf])[2])
  PL <- as.matrix(
    PLAN[, .SD, .SDcols = col_ptdf])
  TPL <- -t(PL)
  ram <- PLAN[, ram]
  VERT <- as.matrix(VERT[, .SD, .SDcols = col_ptdf])
  mean(sapply(1:nrow(VERT), function(X){
    # print(X)
    V1 <- matrix(VERT[X,])
    Ax0 <- PL%*%V1
    flag <- .flagExtOrInt(Ax0, ram)
    if (flag) {
      # print("external")
      # saveRDS(list(V1, flag, Ax0, PL, X, ram), "object.rds")
      .getDistExt(V1 = V1, PL = TPL, ram = ram, Dmat = Dmat)
    } else {
      # print("interior")
      .getDistInt(V1 = V1, PL = PL, ram = ram, Ax0 = Ax0)
    }
  }))
  # Y_X <- get_dist_poly( VERT, PLAN, col_ptdf = col_ptdf)
  # X_Y + Y_X
}

######### Création data.table avec toutes les distances résultantes #####


.getDistMatrixV2 <- function(
  VERT, PLAN, hourWeight){
  set.seed(1234)
  PLAN[, ram := ram + runif(nrow(PLAN))/10000]
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  res_hour <- data.table(t(combn(unique(VERT[, Date]), 2)))

  rbindlist(sapply(1:nrow(res_hour), function(comb){

    ##To sapply
    date_1 <- res_hour[comb, V1]
    date_2 <- res_hour[comb, V2]
    v_hours <- intersect(VERT[Date%in% date_1, Period], VERT[Date%in% date_2, Period])

    ##To sapply
    # h <- v_hours[1]
    rbindlist(sapply(v_hours, function(h){
      print(h)
      # print(paste("begin", date_1, date_2, Sys.time()))

      DD <- dEnd(VERT[Date == date_1 & Period == h],
                 PLAN[Date == date_2 & Period == h], col_ptdf = col_ptdf)

      # print(paste("end", date_1, date_2, Sys.time()))
      # print(paste("begin", date_2, date_1, Sys.time()))

      DD2 <- dEnd(VERT[Date == date_2 & Period == h],
                  PLAN[Date == date_1 & Period == h], col_ptdf = col_ptdf)

      # print(paste("end", date_2, date_1, Sys.time()))
      d <- DD + DD2
      weigthPond <- hourWeight[h]
      d <- weigthPond * d
      data.table(Date1 = c(date_1, date_2),
                 Date2 = c(date_2, date_1), Period = h, dist = d)
    }, simplify = FALSE))

  }, simplify = FALSE))
}

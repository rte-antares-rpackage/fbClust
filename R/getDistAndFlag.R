######### .dEnd : Récupération du calcul de distance entre polyèdres #####
.dEnd <- function(VERT, PLAN, colPtdf, colVert)
{

  Dmat <- diag(1, nrow = dim(VERT[, .SD, .SDcols = colVert])[2])
  PL <- as.matrix(PLAN[, .SD, .SDcols = colPtdf])
  
  TPL <- -t(PL)
  ram <- PLAN[['ram']]
  VERT <- as.matrix(VERT[, .SD, .SDcols = colVert])
  
  mean(sapply(1:nrow(VERT), function(X){
    # print(X)
    V1 <- matrix(VERT[X,])
    Ax0 <- PL%*%V1
    flag <- .flagExtOrInt(Ax0, ram)
    if (flag) {
      .getDistExt(V1 = V1, PL = TPL, ram = ram, Dmat = Dmat)
    } else {
      .getDistInt(V1 = V1, PL = PL, ram = ram, Ax0 = Ax0)
    }
  }))
}

.dEnd2 <- function(VERT, PLAN, colPtdf, colVert)
{
  
  # remove NOTE data.table
  N <- NULL
  
  Dmat <- diag(1, nrow = dim(VERT[, .SD, .SDcols = colVert])[2])
  PL <- as.matrix(PLAN[, .SD, .SDcols = colPtdf])
  nbsign <- 1/unique(VERT[, nbsign])
  vect_sign <- 1/VERT[, N]
  TPL <- -t(PL)
  ram <- PLAN[['ram']]
  VERT <- as.matrix(VERT[, .SD, .SDcols = colVert])
  
  nbsign*sum(vect_sign*sapply(1:nrow(VERT), function(X){
    # print(X)
    V1 <- matrix(VERT[X,])
    Ax0 <- PL%*%V1
    flag <- .flagExtOrInt(Ax0, ram)
    if (flag) {
      .getDistExt(V1 = V1, PL = TPL, ram = ram, Dmat = Dmat)
    } else {
      .getDistInt(V1 = V1, PL = PL, ram = ram, Ax0 = Ax0)
    }
  }))
}

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

############### transformTS ###############
#### Permet de jouer sur les dates et passer à l'heure française

.transformTS <- function(dt)
{
  dt$Period <- hour(dt$timestamp) + 1
  dt$Date <- as.character(as.Date(dt$timestamp))
  dt$timestamp <- NULL
  dt
}

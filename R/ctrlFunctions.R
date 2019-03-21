.ctrlDates <- function(dates, dayInVertices){
  if(!any(dates%in%dayInVertices)){
    stop("One(some) season(s) are not in vertices data.")
  }
  
  if(!all(dates%in%dayInVertices)){
    warning("Somes dates in calendar are not in vertices data.")
  }
  
  if(length(dates) < 2){
    stop("Clustering cannot be performed when class(season/type of day) contains less than 2 days")
  }
  if(all(dates%in%dayInVertices)){
    message("Good, all dates are in vertices data")
    
  }
}

.ctrlTimestamp <- function(data)
{
  if (is.null(data$timestamp)) {
    stop("You need the timestamp column in order to keep going, the format needed is : YYYY-MM-DDTHH:mm:ssZ")
  }
  if (all(grepl("^[0-9]{4}-[0-1][0-2]-[0-3][0-9]T[0-2][0-9]:[0]{2}:[0]{2}", data$timestamp))) {
    message("Good, your timestamp column has the good format")
  } else {
    stop("Your timestamp has ambiguous format, needed is YYYY-MM-DDTHH:mm:ssZ")
  }
  data$timestamp <- as.character(data$timestamp)
  data
}

#' @title Plot a flow-based domain of a typical day
#' 
#' @description For a given hour, plot the flow-based domain of one typical day along with the other
#' flow-based domains it represents (i.e. other domains of the same cluster).
#'
#' @param data \code{data.table} results from the clustering, output data from \link{clusterTypicalDaysForOneClass}
#' @param country1 \code{character}, name of the country whose net position is in the x axis (BE, FR, DE or NL)
#' @param country2 \code{character}, name of the country whose net position is in the y axis (BE, FR, DE or NL)
#' @param hour \code{numeric}, hour of the plotted domain
#' @param dayType  \code{numeric}, typical flow-based day identifier
#' @param typicalDayOnly \code{logical} if TRUE, plot only the domain of the typical day 
#' and not the other domains of the cluster
#' @param ggplot \code{logical} should ggplot package be used (static graph) instead of rAmCharts (dynamic graph)
#' @param width \code{character}, for rAmCharts only. Default to "420px" (set to "100/100" for dynamic resize)
#' @param height \code{character}, for rAmCharts only. Default to "410px" (set to "100/100" for dynamic resize)
#' 
#' @import rAmCharts ggplot2 pipeR
#'
#' @examples
#'
#' \dontrun{
#'
#' # classification result
#' clusterTD <- readRDS(system.file("testdata/allTypeDaytest.rds",package = "fbClust"))
#'
#' clusterPlot(clusterTD, "FR", "DE", 3, 1, FALSE, FALSE)
#' clusterPlot(clusterTD, "ptdfFR", "DE", 3, 1, FALSE, TRUE)
#' clusterPlot(clusterTD, "FR", "ptdfDE", 3, 1, TRUE, TRUE)
#' clusterPlot(clusterTD, "ptdfFR", "ptdfDE", 3, 1, TRUE, FALSE)
#'
#' }
#' 
#' @export

clusterPlot <- function(data, country1, country2, hour, dayType,
                        typicalDayOnly = FALSE, ggplot = FALSE, width = "420px", height = "410px"){
  dataPlot <- .getDataPlotClustering(data[idDayType==dayType],  country1, country2, hour)
  .makeGraph(dataPlot, data[idDayType==dayType]$TypicalDay,
             typicalDayOnly = typicalDayOnly, ggplot = ggplot, width = width, height = height)
}



### Compute list of data.frame with convex hull for two countries on defined hour and day

.getDataPlotClustering <- function(allTypeDay, country1, country2, hour)
{
  if (!grepl("ptdf", country1)) {
    ctry1 <- paste0("ptdf", country1)
  } else {
    ctry1 <- country1
  }
  if (!grepl("ptdf", country2)) {
    ctry2 <- paste0("ptdf", country2)
  } else {
    ctry2 <- country2
  }
  if(country1 == country2) {
    stop("The countries should be distinct")
  }
  if (!grepl("^ptdf[A-Z]{2}$", ctry1) |
      !grepl("^ptdf[A-Z]{2}$", ctry2)) {
    stop(paste("country1 or country 2 has wrong format. Format should be",
               "ptdfXX or XX (where XX is the abreviation of the country (ex : FR, DE, BE))"))
  }
  data_stud <- allTypeDay$dayIn[[1]][Period == hour]
  data_plot <- lapply(1:nrow(data_stud), function(X) {
    dataChull <- .getChull(data_stud[X, VERT_details][[1]], ctry1, ctry2)
    dataChull <- data.frame(dataChull)
    names(dataChull) <- c(
      paste(unique(data_stud[X, VERT_details][[1]]$Date), ctry1, sep = "_"), 
      paste(unique(data_stud[X, VERT_details][[1]]$Date), ctry2, sep = "_"))
    dataChull
  })
  
  maxRow <- max(unlist(lapply(data_plot, nrow)))
  data_plot <- lapply(data_plot, function(X){
    rbind(X, data.table(rep(NA, maxRow-nrow(X)), rep(NA, maxRow-nrow(X))), use.names=FALSE)
  })
  data_plot <- cbind.data.frame(data_plot)
  data_plot
}

## Compute the convex hull from two countries in a data.frame

.getChull <- function(data, country1, country2){
  data <- data.frame(data)
  if(country1 == "ptdfNL"){
    ptctry <- -rowSums(data[grep("ptdf", colnames(data))])
  }else{
    ptctry <- data[[country1]]
  }
  if(country2 == "ptdfNL"){
    ptctry2 <- -rowSums(data[grep("ptdf", colnames(data))])
  }else{
    ptctry2 <- data[[country2]]
  }
  res <- cbind(ptctry, ptctry2)
  res <- res[chull(res),]
  res <- rbind(res, res[1,])
  res
}

##### Render plots, ggplot2 or rAmcharts

.makeGraph <- function(data, typicalDayDate, typicalDayOnly = FALSE, 
                       ggplot = FALSE, width = "420px", height = "410px"){
  ctry <- unique(substr(names(data), 16, 17))
  if(typicalDayOnly){
    dates <- typicalDayDate
  } else {
    dates <- unique(substr(names(data), 1, 10))
  }
  
  xlim = c(-round(max(data, na.rm = TRUE) + 500, -3), round(max(data, na.rm = TRUE) + 500, -3))
  ylim = xlim
  
  # xlim = c(-10000, 10000)
  # ylim = c(-10000, 10000)
  # 
  # if(max(data, na.rm = TRUE) <= 8000 & min(data, na.rm = TRUE) >= -8000){
  #   xlim = c(-8000, 8000)
  #   ylim = c(-8000, 8000)
  # }
  
  if(!ggplot){
    graphs <- sapply(dates, function(X){
      columns <- names(data)[grep(X,names(data))]
      if(X == typicalDayDate){
        graph <- amGraph(
          title = X, balloonText =
            paste0('<b>', X, '<br>', ctry[1], '</b> :[[x]] <br><b>', ctry[2], '</b> :[[y]]'),
          bullet = 'circle', xField = columns[1], yField = columns[2],
          lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#FF0000",
          lineThickness = 3)
      }else{
        graph <-  amGraph(
          title = X, balloonText =
            paste0('<b>', X, '<br>', ctry[1], '</b> :[[x]] <br><b>', ctry[2], '</b> :[[y]]'),
          bullet = 'circle', xField = columns[1], yField = columns[2],
          lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#D3D3D3",
          lineThickness = 1)
      }
      graph
    }, USE.NAMES = FALSE, simplify = FALSE)
    pipeR::pipeline(
      amXYChart(dataProvider = data),
      addTitle(text = paste0("Flow-based  clustering ", ctry[1], "/", ctry[2])),
      setGraphs(graphs),
      setChartCursor(),
      addValueAxes(title = paste(ctry[1], "(MW)"), position = "bottom", minimum = xlim[1], 
                   maximum = xlim[2], minHorizontalGap = 35, minVerticalGap = 35),
      addValueAxes(title =  paste(ctry[2], "(MW)"), minimum = ylim[1], 
                   maximum = ylim[2], minHorizontalGap = 35, minVerticalGap = 35),
      setExport(enabled = TRUE),
      plot(width = width, height = height)
    )
  } else {
    
    gg_data <- do.call("rbind.data.frame", lapply(dates, function(X){
      columns <- names(data)[grep(X, names(data))]
      tmp_data <- data[, columns]
      colnames(tmp_data) <- gsub(paste0(X, "_ptdf"), "", colnames(tmp_data))
      tmp_data <- tmp_data[!is.na(tmp_data[, 1]), ]
      tmp_data$date  <- X
      if(X == typicalDayDate){
        tmp_data$col <- "0"
        tmp_data$size <- 1
      }else {
        tmp_data$col <- "1"
        tmp_data$size <- 0.5
      }
      tmp_data
    }))
    
    ggplot(data=gg_data, aes(
      x = get(ctry[1]), y = get(ctry[2]), 
      group = date, colour = col, size = size, linetype = as.character(col))) + 
      geom_path() +
      geom_point()+ 
      scale_size(range=c(0.1, 2), guide=FALSE) + 
      theme(legend.position= "none") +
      # xlim(xlim[1], xlim[2]) + ylim(ylim[1], ylim[2]) + 
      ggtitle(paste0("Flow-based  clustering ", ctry[1], "/", ctry[2])) +
      theme(plot.title = element_text(hjust = 0.5)) + 
      ylab(paste(ctry[2], "(MW)")) +
      xlab(paste(ctry[1], "(MW)")) + 
      theme(panel.background = element_rect(fill = 'white', colour = 'black'),
            panel.grid.major = element_line(size = 0.5, linetype = 'dashed',
                                            colour = "grey")) +
      scale_y_continuous(breaks = seq(ylim[1], ylim[2], 2000), 
                         limits = ylim, expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(xlim[1], xlim[2], 2000), 
                         limits = xlim, expand = c(0, 0))
    
    
  }
}

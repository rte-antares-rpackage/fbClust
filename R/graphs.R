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
#' @param xlim \code{numeric}, limits of x-axis (default = c(-10000, 10000))
#' @param ylim \code{numeric}, limits of x-axis (default = c(-10000, 10000))
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

clusterPlot <- function(data, 
                        country1, 
                        country2,
                        hour,
                        dayType,
                        typicalDayOnly = FALSE, 
                        ggplot = FALSE, 
                        width = "420px", 
                        height = "410px",
                        xlim = c(-10000, 10000),
                        ylim = c(-10000, 10000)){
  .crtlAllTypeDay(data)
  dataPlot <- .getDataPlotClustering(data[idDayType==dayType],  country1, country2, hour)
  .makeGraph(dataPlot, data[idDayType==dayType]$TypicalDay, xlim = xlim, ylim = ylim,
             typicalDayOnly = typicalDayOnly, ggplot = ggplot, width = width, height = height)
}



### Compute list of data.frame with convex hull for two countries on defined hour and day

.getDataPlotClustering <- function(allTypeDay, country1, country2, hour)
{
  if (grepl("ptdf", country1)) {
    ctry1 <- gsub("ptdf", "", country1)
  } else {
    ctry1 <- country1
  }
  if (grepl("ptdf", country2)) {
    ctry2 <- gsub("ptdf", "", country2)
  } else {
    ctry2 <- country2
  }
  if(ctry1 == ctry2) {
    stop("The hubs should be distinct")
  }
  
  hubnames <- colnames(allTypeDay$dayIn[[1]][Period == hour, PLAN_raw_details][[1]])
  hubnames <- gsub("ptdf", "", hubnames[grepl("ptdf", hubnames)])
  print(hubnames)
  if (!(ctry1 %in% hubnames) |
      !(ctry2 %in% hubnames)) {
    stop(paste("country1 or country 2 has wrong format. Format should be",
               "XX (where XX is the abreviation of the hub (ex : FR, DE, BE))"))
  }
  hubnames_vert <- colnames(allTypeDay$dayIn[[1]][Period == hour, PLAN_details][[1]])
  hubnames_vert <- gsub("ptdf", "", hubnames_vert[grepl("ptdf", hubnames_vert)])
  hubname_diff <- hubnames[!(hubnames %in% hubnames_vert)]
  
  data_stud <- allTypeDay$dayIn[[1]][Period == hour]
  data_plot <- lapply(1:nrow(data_stud), function(X) {
    dataChull <- .getChull(data_stud[X, VERT_details][[1]], ctry1, ctry2, hubname_diff)
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

.getChull <- function(data, country1, country2, hubname_diff){
  data <- data.frame(data)
  if(country1 == hubname_diff){
    ptctry <- -rowSums(data[!grepl("Period|Date", colnames(data))])
  }else{
    ptctry <- data[[country1]]
  }
  if(country2 == hubname_diff){
    ptctry2 <- -rowSums(data[!grepl("Period|Date", colnames(data))])
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
                       ggplot = FALSE, width = "420px", height = "410px",
                       xlim, ylim){
  ctry <- unique(substr(names(data), 12, 13))
  if(typicalDayOnly){
    dates <- typicalDayDate
  } else {
    dates <- unique(substr(names(data), 1, 10))
  }
  
  # xlim = c(-round(max(data, na.rm = TRUE) + 500, -3), round(max(data, na.rm = TRUE) + 500, -3))
  # ylim = xlim
  
  # xlim = c(-10000, 10000)
  # ylim = c(-10000, 10000)
  # 
  # if(max(data, na.rm = TRUE) <= 8000 & min(data, na.rm = TRUE) >= -8000){
  #   xlim = c(-8000, 8000)
  #   ylim = c(-8000, 8000)
  # }
  
  if(!ggplot){
    graphs <- sapply(dates, function(X){
      columns <- names(data)[grep(X, names(data))]
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



#' Plot flow-based domain(s)
#'
#' @param PLAN \code{data.table}, at least ram, Date, Period and two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#'  \item ram : line limits
#'  \item Date : date in format YYYY-MM-DD
#'  \item Period : hour in the day, between 1 and 24
#' }
#' PLAN is generated in this format with the function \link{getPreprocPlan}
#' @param country1 \code{character}, name of the country whose net position is in the x axis (BE, FR, DE, AT or NL)
#' @param country2 \code{character}, name of the country whose net position is in the y axis (BE, FR, DE, AT or NL)
#' @param hours \code{character}, hours of interest for the graphics.
#' @param dates \code{character}, dates of interest for the graphics. (All the combinations of dates and hours are )
#' @param domainsNames \code{character} names of the domain(s), used as legend of the graphics.
#' The length of domainsNames has to be the same of the number of combinations of hours and dates
#' @param main \code{character} title of the graph, if NULL, the title will be "Domains country1 - country2"
#' @param hubDrop \code{list}, list of hubs in the ptdf, with the ones which should
#' sustracted to the others as the names of the arrays which themself contain the ones which
#' be sustracted
#' @param xlim \code{numeric}, limits of x-axis (default = c(-10000, 10000))
#' @param ylim \code{numeric}, limits of x-axis (default = c(-10000, 10000))
#' @param width \code{character}, for rAmCharts only. Default to "420px" (set to "100/100" for dynamic resize)
#' @param height \code{character}, for rAmCharts only. Default to "410px" (set to "100/100" for dynamic resize)
#' 
#' @examples
#'
#' \dontrun{
#' library(data.table)
#' library(rAmCharts)
#' PLAN <- getPreprocPlan(
#' path_ptdf_matrix_factor = system.file(
#'   "testdata/plan_new_version_factor_AT.rds", package = "fbClust"),
#' path_ptdf_matrix_constraint = system.file(
#'  "testdata/plan_new_version_constraint_AT.rds", package = "fbClust"))
#'  
#'  hubDrop = list(NL = c("BE", "DE", "FR", "AT"))
#' #Plot unique polyhedron
#' plotFlowbased(PLAN, "BE", "DE", hubDrop = hubDrop, 
#' hours = c(2), dates = c("2018-10-02"), domainsNames = "2018-10-02", main = "")
#'
#' #Plot four polyhedra
#' plotFlowbased(PLAN, "BE", "DE", hubDrop = hubDrop, 
#' hours = c(3, 4), dates = c("2018-10-02", "2018-10-04"), domainsNames = NULL,
#' main = NULL)
#'
#' }
#'
#' @export

plotFlowbased <- function(PLAN,
                          country1,
                          country2,
                          hours,
                          dates,
                          domainsNames = NULL,
                          hubDrop = list(NL = c("BE", "DE", "FR", "AT")),
                          xlim = c(-10000, 10000),
                          ylim = c(-10000, 10000),
                          main = NULL,
                          width = "420px", height = "410px"){

  #Generate data for plot
  if (grepl("ptdf", country1)) {
    ctry1 <- gsub("ptdf", "", country1)
  } else {
    ctry1 <- country1
  }
  if (grepl("ptdf", country2)) {
    ctry2 <- gsub("ptdf", "", country2)
  } else {
    ctry2 <- country2
  }
  if(ctry1 == ctry2) {
    stop("The countries should be distinct")
  }
  PLAN
  hubnames <- gsub("ptdf", "", colnames(PLAN)[grep("ptdf", colnames(PLAN))])
  PLAN <- copy(PLAN)
  PLAN <- PLAN[Period %in% hours & Date %in% dates]
  .ctrlHubDrop(hubDrop = hubDrop, PLAN = PLAN)
  PLAN <- .setDiffNotWantedPtdf2(PLAN = PLAN, hubDrop = hubDrop)
  comb <- unique(PLAN[, list(Period, Date)])

  #Control arguments
  multiPDTF <- (nrow(comb) > 1)
  if(!is.null(domainsNames)){
    if(!multiPDTF){
      if(length(domainsNames) != 1){
        stop("Only one PLAN specified for 2 or more domainsNames")
      }
    }else{
      if(length(domainsNames) != nrow(comb)){
        stop(paste0("You must have one domainsNames specified by combination of hours and time, currently you have ",
                    length(domainsNames), " domainsNames specify for ",
                    nrow(comb), " PLAN"))
      }
    }
  }
  if(is.null(domainsNames)){
    domainsNames <- paste("Date :", comb[, Date], "Hour :", comb[, Period])
  }

  VERT <- getVertices(PLAN)
  hubnames_vert <- gsub("ptdf", "", colnames(VERT)[grep("ptdf", colnames(VERT))])
  hubname_diff <- hubnames[!(hubnames %in% hubnames_vert)]
  # lim <- round(max(VERT[, list(get(ctry1), get(ctry2))])+500, -3)
  # xlim <- c(-lim, lim)
  # ylim <- c(-lim, lim)

  dataToGraph <- .givePlotData(VERT, ctry1, ctry2, comb, domainsNames, hubname_diff)
  rowMax <- max(unlist(lapply(dataToGraph, nrow)))
  dataToGraph <- lapply(dataToGraph, function(dta){
    if(nrow(dta)<rowMax){
      Na <-  data.frame(rep(NA,rowMax - nrow(dta)),
                        rep(NA,rowMax - nrow(dta)))
      names(Na) <- names(dta)
      rbind(dta,Na)
    }else{
      dta
    }
  })
  dataToGraph <- do.call(cbind, dataToGraph)
  if (is.null(main)) {
    main <- paste("Domains", gsub("ptdf", "", ctry1), "-", gsub("ptdf", " ", ctry2))
  }


  #Graph creation for more exmples see rAmCharts::runExamples()
  graphs <- sapply(1:length(domainsNames), function(X){
    amGraph(title = domainsNames[X], balloonText =
              paste0('<b>',domainsNames[X],'<br>',
                     paste0(domainsNames[X], gsub("ptdf", " ", ctry1)),
                     '</b> :[[x]] <br><b>',
                     paste0(domainsNames[X], gsub("ptdf", " ", ctry2)), '</b> :[[y]]'),
            bullet = 'circle', xField = paste0(domainsNames[X], " ", ctry1),
            yField = paste0(domainsNames[X], " ", ctry2),
            lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineThickness = 3)

  }, USE.NAMES = FALSE)
  pipeR::pipeline(
    amXYChart(dataProvider = dataToGraph),
    addTitle(text = main),
    setGraphs(graphs),
    setChartCursor(),
    addValueAxes(title = paste(gsub("ptdf", "", ctry1), "(MW)"), position = "bottom", minimum = xlim[1],
                 maximum = xlim[2], minHorizontalGap = 35, minVerticalGap = 35),
    addValueAxes(title =  paste(gsub("ptdf", "", ctry2), "(MW)"), minimum = ylim[1],
                 maximum = ylim[2], minHorizontalGap = 35, minVerticalGap = 35),
    setExport(enabled = TRUE),
    setLegend(enabled = TRUE),
    plot(width = width, height = height)
  )
}

.givePlotData <- function(VERT, ctry1, ctry2, comb, domainsNames, hubname_diff){
  
  res <- lapply(1:nrow(comb), function(X) {
    period <- comb[X, Period]
    date <- comb[X, Date]
    data <- data.table(.getChull(VERT[Period == period & Date == date], 
                                 ctry1, ctry2, hubname_diff))
    setnames(data, old = c("ptctry", "ptctry2"),
             new = paste(domainsNames[X], c(gsub("ptdf", "", ctry1), gsub("ptdf", "", ctry2))))
    # names(dataToGraph)[X:(X+1)]))
    data
  })
}


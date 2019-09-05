## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fbClust)

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Define dates for the clustering
#  dates <- seq(as.Date("2018-10-01"), as.Date("2018-10-04"), by = "day")
#  
#  
#  # Generate ptdf data from a constraint file and a factor file
#  PLAN <- getPreprocPlan(
#    pathPtdfMatrixFactor = system.file(
#      "ptdfFactor.csv", package = "fbClust"),
#    pathPtdfMatrixConstraint = system.file(
#      "ptdfConstraint.csv", package = "fbClust"))
#  
#  
#  # Clustering on the days
#  allTypeDay <- clusterTypicalDaysForOneClass(
#    VERT = NULL, PLAN = PLAN, nbCluster = 2, className = "interSaisonSe",
#    dates = dates)
#  
#  # Plot the clusters
#  clusterPlot(allTypeDay, "FR", "BE", hour = 1, dayType = 1)

## ------------------------------------------------------------------------
# read ptdf data from rds or csv (the input is two files, one with the ptdf and one with the rams)
PLAN <- getPreprocPlan(
  pathPtdfMatrixFactor = system.file(
    "ptdfFactor.csv", package = "fbClust"),
  pathPtdfMatrixConstraint = system.file(
    "ptdfConstraint.csv", package = "fbClust"))
DT::datatable(PLAN[1:100])

## ------------------------------------------------------------------------
# Define a calendar (period on which the clustering will be made + distinction of each season)
dates <- getSequence("2015-11-01", "2017-01-20")
interSeasonBegin <- c("2016-03-01", "2016-10-01")
interSeasonEnd <- c("2016-05-15", "2016-10-31")
calendar <- getCalendar(dates, interSeasonBegin, interSeasonEnd)
str(calendar)

## ----results = "hide"----------------------------------------------------
list(NL = c("BE", "DE", "FR", "AT"))

## ----results = "hide"----------------------------------------------------
list(NL = c("BE", "DE", "FR", "AT"), AL1 = c("AL2"))


## ------------------------------------------------------------------------
PLAN2 <- setDiffNotWantedPtdf(PLAN, list("NL" = c("BE", "DE", "FR", "AT")))
DT::datatable(PLAN2[1:100])

## ------------------------------------------------------------------------
# Transform the ptdf data to vertices (after substracting one column the the others)
VERT <- getVertices(PLAN2)

DT::datatable(VERT[1:100, list(Date, Period, FR = round(FR, 2), DE = round(DE, 2),
                               BE = round(BE, 2), AT = round(AT, 2))])

## ------------------------------------------------------------------------
# Example with one plot
plotFlowbased(PLAN = PLAN, country1 = "FR", country2 = "DE", hubDrop = list(NL = c("BE", "DE", "FR", "AT")),
hours = 2, dates = "2018-10-03", domainsNames = NULL, main = NULL, width = "100%", height = "640px", export = F)

## ------------------------------------------------------------------------
# Example with four plots
plotFlowbased(PLAN = PLAN, country1 = "BE", country2 = "NL", hubDrop = list(NL = c("BE", "DE", "FR", "AT")),
hours = c(3, 4), dates = c("2018-10-02", "2018-10-04"), domainsNames = NULL,
main = NULL, width = "100%", height = "640px", xlim = c(-12000, 12000), ylim = c(-12000, 12000), export = F)

## ----message = FALSE, results = "hide"-----------------------------------
# Cluster the typical day for one class
allTypeDay <- clusterTypicalDaysForOneClass(
  VERT = VERT, PLAN = PLAN, dates = seq(as.Date("2018-10-01"), as.Date("2018-10-04"), by = "day"),
  hourWeight = rep(1, 24), nbCluster = 2, maxDomainSize = 20000, className = "interSaisonSe",
  list(NL = c("BE", "DE", "FR", "AT")))



## ----eval = FALSE--------------------------------------------------------
#  
#  # Or for many classes
#  dates <- getSequence("2018-09-01", "2019-08-31")
#  interSeasonBegin <- c("2018-10-01", "2019-03-01")
#  interSeasonEnd <- c("2018-11-30", "2019-05-31")
#  calendar <- getCalendar(dates, interSeasonBegin, interSeasonEnd)
#  dtClust <- clusteringTypicalDays(PLAN = PLAN, calendar = calendar,
#                        hubDrop = list(NL = c("BE", "DE", "FR", "AT")), nbClustWeek = 3,
#                        nbClustWeekend = 1, hourWeight = rep(1, 24), maxDomainSize = 20000,
#                        VERT = NULL)
#  

## ------------------------------------------------------------------------
ptdfraw <- manipulateAllTypeDays(allTypeDay, output = "ptdfraw")
DT::datatable(head(ptdfraw))

## ------------------------------------------------------------------------
ptdf <- manipulateAllTypeDays(allTypeDay, output = "ptdf")
DT::datatable(head(ptdf))

## ------------------------------------------------------------------------
vertices <- manipulateAllTypeDays(allTypeDay, output = "vertices")
DT::datatable(head(vertices[, list(Date, Period, FR = round(FR, 2), 
                                   DE = round(DE, 2), AT = round(AT, 2), BE = round(BE, 2),
                                   Class, idDayType)]))

## ------------------------------------------------------------------------
summary <- manipulateAllTypeDays(allTypeDay, output = "summary")
DT::datatable(summary)

## ---- fig.align = "center", fig.width= 7, fig.height = 6-----------------
clusterPlot(allTypeDay, "BE", "FR", hour = 1, dayType = 1, ggplot = TRUE, xlim = c(-12000, 12000), ylim = c(-12000, 12000), typicalDayOnly = TRUE)

## ------------------------------------------------------------------------
clusterPlot(allTypeDay, "AT", "DE", hour = 1, dayType = 1, ggplot = FALSE, width = "100%", height = "640px", xlim = c(-10000, 10000), ylim = c(-10000, 10000), typicalDayOnly = TRUE)

## ----eval = FALSE--------------------------------------------------------
#  ### An example of the report generation
#  
#  
#  allTypDay <- readRDS(system.file("testdata/allTypDays.rds", package = "fbClust"))
#  generateClusteringReport(dayType = 1, data = allTypDay, outputFile = tempdir(),
#  countries = list(c("BE", "FR"), c("BE", "NL")), xlim = c(-12000, 12000),
#  ylim = c(-12000, 12000))
#  
#  generateClusteringReport(dayType = 1, data = allTypDay, outputFile = tempdir(),
#  countries = c("AT", "FR", "NL", "DE"), xlim = c(-12000, 12000),
#  ylim = c(-12000, 12000))
#  

## ---- eval = TRUE, echo = FALSE------------------------------------------
climate <- data.table::fread(system.file("testdata/climate_example.txt",package = "fbClust"))
head(climate)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  # same quantiles for each variables
#  MatProb <- getProbability(climate, dtClust, levelsProba = c(1/3, 2/3))
#  
#  # diffents quantiles for each class and variables
#  levelsProba <- list(summerWd = list(FR_load = c(0.5), DE_wind = c(1/3, 2/3), DE_solar = .5),
#                      winterWd = list(FR_load = c(0.5, 0.7), DE_wind = c(.5)))
#  MatProb <- getProbability(dtClust, allTypDay, levelsProba = levelsProba)


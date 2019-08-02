#' Generate html report
#'
#' Write and save a report with the results of the clustering of the flow-based domains. For one typical day,
#' the report contains the representation of all the 24 hourly flow-based domains and their comparison
#' with the domains of the other historical days it represents.
#'
#' @param dayType \code{numeric} Typical day identifier
#' @param outputFile \code{character} a folder where the html report is saved
#' @param data \code{data.table} results from the clustering, output data from \link{clusteringTypicalDays}
#' @param countries \code{list, character} a list of couples of countries to choose the axises for the projection
#' of the flowbased domains (ex : list(c("BE", "FR"), c("BE", "NL"))) or an array of countries
#' (ex : c("FR", "NL", "AT")) to project the domains on all the countries combination 
#' (here FR+NL, FR+AT and NL+AT)
#' @param xlim \code{numeric}, limits of x-axis (default = c(-10000, 10000))
#' @param ylim \code{numeric}, limits of x-axis (default = c(-10000, 10000))
#' 
#' @examples
#'
#' \dontrun{
#' 
#' allTypDay <- readRDS(system.file("testdata/allTypDays.rds", package = "fbClust"))
#' generateClusteringReport(dayType = 1, data = allTypDay, outputFile = tempdir(),
#' countries = list(c("BE", "FR"), c("BE", "NL")), xlim = c(-12000, 12000),
#' ylim = c(-12000, 12000))
#' 
#' generateClusteringReport(dayType = 1, data = allTypDay, outputFile = tempdir(),
#' countries = c("AT", "FR", "NL", "DE"), xlim = c(-12000, 12000),
#' ylim = c(-12000, 12000))
#' }
#' @export
#' 
#' @import rmarkdown flexdashboard manipulateWidget gridExtra DT
#' @importFrom shiny tags
generateClusteringReport <- function(
  dayType, 
  outputFile, 
  data, 
  countries = list(c("BE", "FR"), c("BE", "NL"), c("DE", "FR"), c("DE", "AT")),
  xlim = c(-10000, 10000),
  ylim = c(-10000, 10000)){
  
  # remove NOTE data.table
  idDayType <- NULL
  
  output_Dir <- outputFile
  outputFile <- paste0(outputFile, "/", gsub(":", "", gsub( " ", "_",as.character(Sys.time()))), 
                       "_flowBased_",dayType, "_",data[idDayType == dayType]$Class, ".html")
  combi <- .crtlCountriesCombn(countries)
  e <- environment()
  e$dayType <- dayType
  e$data <- data
  e$countries <- countries
  e$combi <- combi
  e$xlim <- xlim
  e$ylim <- ylim
  matchingNameTable <- data.table(Class = c("interSaisonWe", "interSaisonSe", "winterWe", "winterSe",
                                            "summerWe", "summerSe"),
                                  title = c("inter-season weekend day", "inter-season working day",
                                            "winter weekend day", "winter working day",
                                            "summer weekend day", "summer working day"))
  
  
  CompTitle <- matchingNameTable$title[which(data[dayType]$Class == matchingNameTable$Class)]
  # print(CompTitle)
  e$CompTitle <- CompTitle
  
  rmarkdown::render(system.file("/report/resumeclustflex.Rmd", package = "fbClust"),
                    output_file = outputFile,
                    params = list(set_title = paste0(
                      "Typical Day ", dayType,
                      " : ", CompTitle,  " (generated on ", Sys.Date(), ")")),
                    intermediates_dir = output_Dir, envir = e,
                    quiet = TRUE)
  outputFile
}

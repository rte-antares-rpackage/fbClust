context("Function pdtfToVertices")
library(data.table)

test_that(".ctrlvlPb works", {
  
  library(data.table)
  
  climate <- fread(system.file("testdata/climate_example.txt",package = "fbClust"))
  
  # load clustering results (or build them with clusteringTypicalDays function())
  clusterTD <- readRDS(system.file("testdata/allTypDays.rds", package = "fbClust"))
  
  
  class <- unique(clusterTD$Class)
  clVar <- names(climate)[!names(climate)%in%"Date"]
  
  
  
  #Test if levelsProba not between 0 and 1
  expect_error(.ctrlvlPb(2, clVar, class))
  
  levelsProba <- list(
    winterWd = list(FR_load = c(1/3, 2/3),
                    DE_wind = c(1/3, 2/3)),
    interSeasonWd2 = list(FR_load = c(1/3, 2/3),
                          DE_wind = c(1/3, 2/3), 
                          DE_solar = c(1/2))
  )
  # expect_error(.ctrlvlPb(levelsProba, clVar, class),
  #              "Names of the list levelsProba should be classes of the clustering 
  #              :summerWd;summerWe;winterWd;winterWe;interSeasonWd;interSeasonWe")
  expect_error(.ctrlvlPb(levelsProba, clVar, class),
               "Names of the list levelsProba should be classes of the clustering :Class")
  
  # levelsProba <- list(
  #   winterWd = list(FR_load = c(1/3, 2/3),
  #                   DE_wind = c(1/3, 2/3)),
  #   interSeasonWd = list(FR_load = c(1/3, 2/3),
  #                         DE_wind2 = c(1/3, 2/3), 
  #                         DE_solar = c(1/2))
  # )
  levelsProba <- list(
    Class = list(FR_load = c(1/3, 2/3),
                         DE_wind2 = c(1/3, 2/3), 
                         DE_solar = c(1/2)))
  expect_error(.ctrlvlPb(levelsProba, clVar, class),
               "DE_wind2is not a variable of the climate input.")
  
  
  
  
  levelsProba <- list(
    FR_load = c(1/3, 2/3), DE_wind = c(1/3, 2/3)
  )
  
  expect_error(.ctrlvlPb(levelsProba, clVar, class),
               "Names of the list levelsProba should be classes of the clustering :Class")
  
  
  levelsProba <- list(
    summerWd = c(1/3, 2/3)
  )
  expect_error(.ctrlvlPb(levelsProba, clVar[1], class))
  
  levelsProba <- list(
    winterWd = list(FR_LAOD = c(1/3, 2/3), DE_WIND = c(1/3, 2/3))
  )
  expect_error(.ctrlvlPb(levelsProba, clVar, class))
  
  
  
  
})

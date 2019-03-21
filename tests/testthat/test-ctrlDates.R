context("Function .ctrlDates")

test_that(".ctrlDates", {
  library(data.table)
  pl_vert <- readRDS(system.file("testdata/pl_vert_test.rds", package = "fbClust"))
  VERT <- pl_vert[[2]]
  dates1 <- as.character(seq(as.Date("2019-02-14"), as.Date("2019-02-15"), by = "day"))
  dates2 <- as.character(seq(as.Date("2019-02-14"), as.Date("2019-02-18"), by = "day"))
  dates3 <- as.character(seq(as.Date("2019-02-20"), as.Date("2019-02-25"), by = "day"))
  dates4 <- "2019-02-14" 
  setDT(VERT)

  expect_message(.ctrlDates(dates = dates1, dayInVertices = VERT[, Date]), 
                 regexp = "Good, all dates are in vertices data", fixed = T)
  expect_warning(.ctrlDates(dates = dates2, dayInVertices = VERT[, Date]), 
                 regexp = "Somes dates in calendar are not in vertices data.", fixed = T)
  expect_error(.ctrlDates(dates = dates3, dayInVertices = VERT[, Date]), 
               regexp = "One(some) season(s) are not in vertices data.", fixed = T)
  expect_error(.ctrlDates(dates = dates4, dayInVertices = VERT[, Date]), fixed = T,
               regexp = "Clustering cannot be performed when class(season/type of day) contains less than 2 days")
  
})

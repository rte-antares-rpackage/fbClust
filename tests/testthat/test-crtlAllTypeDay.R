context("Function .crtlAllTypeDay")

test_that(".crtlAllTypeDay", {
  library(data.table)
  data <- readRDS(system.file("testdata/allTypDays.rds", package = "fbClust"))
  .crtlAllTypeDay(data)
  
  expect_error(.crtlAllTypeDay(data[, list(TypicalDay, Class, dayIn, distance)]),
               regexp = paste("The colnames of allTypeDay must be the following, in this order :",
                              "TypicalDay,", "Class,", "dayIn,", "distance,", "idDayType"))
  data2 <- copy(data)
  data2[, dayIn][[1]][, Date := NULL]
  data2[, dayIn][[2]][, Date := NULL]
  expect_error(
    .crtlAllTypeDay(data2, regexp = paste(
      "The colnames of allTypeDay[, dayIn] must be the following, in this order :",
      "Date,", "Period,", "VERT_details,", "PLAN_details,", "PLANRaw_details")))
})


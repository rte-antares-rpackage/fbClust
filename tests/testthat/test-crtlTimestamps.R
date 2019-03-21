context("Function .ctrlTimestamp")

test_that(".ctrlTimestamp", {
  library(data.table)
  PLAN <- fread(system.file("testdata/190318_plan.csv", package = "fbClust"), sep = "|")
  PLAN2 <- copy(PLAN)
  PLAN2[, timestamp := NULL]
  PLAN3 <- copy(PLAN)
  PLAN3[, timestamp := as.Date(timestamp)]
  
  expect_error(.ctrlTimestamp(PLAN2), fixed = T,
               regexp = "You need the timestamp column in order to keep going, the format needed is : YYYY-MM-DDTHH:mm:ssZ")
  expect_message(.ctrlTimestamp(PLAN), 
                 regexp = "Good, your timestamp column has the good format", fixed = T)
  expect_error(.ctrlTimestamp(PLAN3), fixed = T,
               regexp = "Your timestamp has ambiguous format, needed is YYYY-MM-DDTHH:mm:ssZ")
})
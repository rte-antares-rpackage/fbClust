context("Function .crtOutFile")

test_that(".crtOutFile", {
  library(data.table)

  dt_test <- data.table(V1 = rep("hello", 10), idDayType = rep(1:10))
  
  res <- .crtOutFile(dt_test, reportPath = NULL)
  grepl(getwd(), res1$outputFile)
  grepl(Sys.Date(), res1$outputFile)
  expect_true(grepl(getwd(), res$outputFile) &
                grepl(Sys.Date(), res$outputFile) &
                res$step == 10)
  
  reportPath_failed <- "here"
  expect_error(.crtOutFile(dt_test, reportPath = reportPath_failed), 
               regexp = paste("The directory", reportPath_failed, "does not exist"),
               fixed = T)
 
})
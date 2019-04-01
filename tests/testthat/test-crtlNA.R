context("Function .crtlNA")

test_that(".crtlNA", {
  library(data.table)

  data <- data.table(V1 = c(rep(1:10), NA, rep(1:5)), V2 = rep("coucou", 16))
  
  expect_warning(.crtlNA(data), fixed = T,
               regexp = paste("You have na in the column named", "V1", "be carefull with that"))

})
  
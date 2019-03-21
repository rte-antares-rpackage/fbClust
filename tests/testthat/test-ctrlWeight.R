context("Function .ctrlWeight")

test_that(".ctrlWeight", {

  hourweight <- rep(1, 24)
  hourweight2 <- rep(1, 34)
  expect_silent(.ctrlWeight(hourweight))
  expect_error(.ctrlWeight(hourweight2), fixed = T, regexp = "Length of hourWeight must be 24")

})
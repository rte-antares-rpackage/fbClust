context("Function .ctrlCalendar")

test_that(".ctrlCalendar", {
  calendar <- list()
  calendar$interSeasonWe <- c("2018-10-01", "2018-10-02")
  calendar$interSeasonWd <- c("2018-10-03", "2018-10-04")
  
  calendar2 <- list()
  calendar2$interSeasonWe <- c("2018-10-01", "2018-10-02")
  calendar2$interSeasonWd <- c("2018-10-03", "2018-10-04")
  calendar2$fakeSeason <- seq(as.Date("2038-10-02"), as.Date("2038-12-04"), by = "day")
  .ctrlCalendar(calendar)

  expect_error(.ctrlCalendar(calendar2), 
               regexp = "Names of calendar must be 'interSeasonWe',
         'interSeasonWd', 'winterWe',
         'winterWd', 'summerWe', 'summerWd'", fixed = T)
})
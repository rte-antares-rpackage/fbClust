context("Function setDiffNotWantedPtdf")

test_that("setDiffNotWantedPtdf", {
  library(data.table)
  PLAN <- data.table("ptdfXX" = seq(900,1000, 4), "ptdfYY" = seq(600,650, 2),
                     "ptdfZZ" = seq(300, 400, 4), "bla" = seq(500, 525, 1))
  plan_res <- setDiffNotWantedPtdf(PLAN = PLAN, not_wanted_col = "ptdfYY")

  expect_true(all(plan_res[, bla] == PLAN[, bla]))
  expect_true(all(plan_res[, ptdfXX] == PLAN[, ptdfXX - ptdfYY]))
  expect_true(all(plan_res[, ptdfZZ] == PLAN[, ptdfZZ - ptdfYY]))
  expect_true(ncol(plan_res) == (ncol(PLAN) - 1))
})

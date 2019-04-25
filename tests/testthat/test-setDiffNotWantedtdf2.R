# context("Function setDiffNotWantedPtdf")
# 
# test_that("setDiffNotWantedPtdf", {
#   library(data.table)
#   PLAN <- data.table("ptdfXX" = seq(900,1000, 4), "ptdfYY" = seq(600,650, 2),
#                      "ptdfZZ" = seq(300, 400, 4), "bla" = seq(500, 525, 1))
#   plan_res <- setDiffNotWantedPtdf(PLAN = PLAN, not_wanted_col = "ptdfYY")
# 
#   expect_true(all(plan_res[['bla']] == PLAN[['bla']]))
#   expect_true(all(plan_res[['ptdfXX']] == PLAN[['ptdfXX']] - PLAN[['ptdfYY']]))
#   expect_true(all(plan_res[['ptdfZZ']] == PLAN[['ptdfZZ']] - PLAN[['ptdfYY']]))
#   expect_true(is.null(plan_res$ptdfYY))
#   
#   plan_res2 <- setDiffNotWantedPtdf(PLAN = PLAN, not_wanted_col = NULL)
#   
#   expect_true(all(plan_res2[['bla']] == PLAN[['bla']]))
#   expect_true(all(plan_res2[['ptdfXX']] == PLAN[['ptdfXX']] - PLAN[['ptdfZZ']]))
#   expect_true(all(plan_res2[['ptdfYY']] == PLAN[['ptdfYY']] - PLAN[['ptdfZZ']]))
#   expect_true(is.null(plan_res2$ptdfZZ))
# })

context("Function .ctrlVertPlanFormat")

test_that(".ctrlVertPlanFormat", {
  
  pl_vert <- readRDS(system.file("testdata/expl_plan_vert.rds", package = "fbClust"))
  PLAN <-pl_vert[[1]]
  VERT <- pl_vert[[2]]
  PLAN2 <- copy(PLAN)
  PLAN2[, ram := NULL]
  PLAN3 <- PLAN[, list(Period, Date)]
  VERT2 <- copy(VERT)
  VERT2[, FR := NULL]
  col_plan <- colnames(PLAN)
  col_vert2 <- colnames(VERT2)
  col_ptdf_plan <- col_plan[grep("^ptdf[A-Z]{2}$", col_plan)]
  col_ptdf_vert2 <- col_plan[!grepl("Date|Period", col_vert2)]
  
  expect_message(.ctrlVertPlanFormat(VERT, PLAN), fixed = T,
                 regexp = "Good: columns of VERT & PLAN match")
  expect_error(.ctrlVertPlanFormat(VERT, PLAN2), fixed = T,
               regexp = "PLAN should contains the column named ram")
  expect_error(.ctrlVertPlanFormat(VERT, PLAN3), fixed = T,
               regexp = "PLAN must have ptdf colnames in the form ptdfXX (ex : ptdfFR) & VERT in the form XX (ex : FR)")
  expect_error(
    .ctrlVertPlanFormat(VERT2, PLAN), fixed = T,
    regexp = cat("PLAN & VERT must have same hub names, with ptdf for PLAN and not for VECT, \n Currently for PLAN:",
                 col_ptdf_plan, " \n Currently for VECT:", col_ptdf_vert2)
  )
  
})


library(testthat)
test_that("validate_and_clean parses dates and numeric columns for a valid sample", {
  df_good <- data.frame(
    id = 1:3,
    site = c("AAKB","AAKB","GECE"),
    date = c("01-09-2025", "02-09-2025", "02-09-2025"), # dmy format
    type = c("Electricity","Electricity","Gas"),
    value = c(100, 150, 200),
    `carbon.emission.in.kgco2e` = c(10, 15, 20),
    stringsAsFactors = FALSE
  )

  res <- validate_and_clean(df_good)
  expect_true(res$ok)
})





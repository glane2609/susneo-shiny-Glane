library(testthat)

test_that("BusinessLogic KPIs are computed correctly", {
  df_good <- data.frame(
    id = 1:3,
    site = c("AAKB","AAKB","GECE"),
    date = c("01-09-2025", "02-09-2025", "02-09-2025"), # dmy format
    type = c("Electricity","Electricity","Gas"),
    value = c(100, 150, 200),
    `carbon.emission.in.kgco2e` = c(10, 15, 20),
    stringsAsFactors = FALSE
  )

  bl <- BusinessLogic$new(df_good)
  expect_equal(bl$total_consumption(), sum(df_good$value))
  expect_equal(bl$total_emissions(), sum(df_good$carbon.emission.in.kgco2e))
  expect_equal(bl$facility_count(), 2)
  daily <- df_good %>% dplyr::group_by(site, date) %>% dplyr::summarise(daily = sum(value, na.rm = TRUE), .groups = "drop")
  expected_peak <- max(daily$daily, na.rm = TRUE)
  expect_equal(bl$peak_daily_consumption(), expected_peak)
  expected_avg <- mean(daily$daily, na.rm = TRUE)
  expect_equal(bl$average_consumption(), expected_avg)
})





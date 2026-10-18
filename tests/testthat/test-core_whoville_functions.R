library(dplyr)

test_that("countries argument options work", {
  countries_test <- whoville::countries %>%
    filter(iso3 == "AFG") %>%
    mutate(who_region = "MANUAL who_region")

  # test with countries object argument
  result <- iso3_to_regions(iso3 = "AFG", region = "who_region", countries_manual = countries_test)
  expect_identical(result, "MANUAL who_region")

  # test with option specified
  countries_test <- countries_test %>% mutate(who_region = "OPTION who_region")
  options(whoville.option.countries = countries_test)
  result <- iso3_to_regions(iso3 = "AFG", region = "who_region")
  expect_identical(result, "OPTION who_region")
  options(whoville.option.countries = NULL)

  # test with built in countries object
  result <- iso3_to_regions(iso3 = "AFG", region = "who_region")
  expect_identical(result, "EMR")

})

test_that("GBD high-income functions use the GBD 'High-income' region", {
  result <- gbd_high_income_states()
  expect_length(result, 36)
  expect_true("USA" %in% result)
  expect_false("IND" %in% result)

  expect_identical(
    is_gbd_high_income(c("USA", "IND", "not a code")),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("World Bank income group check expects the new fiscal year after July 1st", {
  # FY27 ('wb_ig_2025') is released on 2026-07-01
  expect_warning(
    warn_if_wb_ig_outdated(1987:2024, source = "test", date = as.Date("2026-10-18")),
    "FY27"
  )
  expect_no_warning(warn_if_wb_ig_outdated(1987:2025, source = "test", date = as.Date("2026-10-18")))
  expect_no_warning(warn_if_wb_ig_outdated(1987:2024, source = "test", date = as.Date("2026-06-30")))
  expect_identical(wb_ig_fiscal_year(c(1998, 2025)), c("FY00", "FY27"))
})

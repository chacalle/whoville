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

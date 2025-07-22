library(dplyr)

test_that("`get_who_public_xmart` works", {
  testthat::skip_if_offline()

  # https://extranet.who.int/xmart4/data/DATA_/REF_SEX
  expected <- dplyr::tibble(
    REF_TERM = c("FEMALE", "MALE"),
    TERM_NAME = c("Female", "Male")
  )
  result <- get_who_public_xmart(
    url = "https://xmart-api-public.who.int/DATA_/REF_SEX?$select=REF_TERM,TERM_NAME & $filter=REF_TERM in ('MALE','FEMALE')"
  )

  testthat::expect_identical(expected, result)
})

test_that("`format_who_xmart_ref_country` works", {
  testthat::skip_if_offline()

  # https://extranet.who.int/xmart4/data/REFMART/REF_COUNTRY
  result <- get_who_public_xmart(
    url = "https://xmart-api-public.who.int/REFMART/REF_COUNTRY"
  ) %>%
    format_who_xmart_ref_country() %>%
    arrange(iso3)

  testthat::expect_true(nrow(result) > 0)

  expected <- whoville::countries %>%
    select(names(result)) %>%
    arrange(iso3)
  # testthat::expect_identical(expected, result)
})

test_that("`get_oecd_countries` works", {
  testthat::skip_if_offline()

  result <- get_oecd_countries()
  testthat::expect_true(nrow(result) > 0)

  expected <- whoville::countries %>%
    select(names(result))
  # testthat::expect_identical(expected, result)
})

test_that("`get_wb_ig` works", {
  testthat::skip_if_offline()

  result <- get_wb_ig() %>%
    format_wb_ig() %>%
    # drop former countries that are not stored in the countries object
    filter(iso3 %in% whoville::countries$iso3) %>%
    arrange(iso3)
  testthat::expect_true(nrow(result) > 0)

  expected <- whoville::countries %>%
    dplyr::select(iso3, starts_with("wb_ig"))
  expected_iso3 <- expected$iso3[rowSums(!is.na(expected)) > 1]
  expected <- expected %>%
    filter(iso3 %in% expected_iso3) %>%
    arrange(iso3)

  # result %>%
  #   select(all_of(names(expected))) %>%
  #   testthat::expect_identical(expected)
})

test_that("`get_wb_reg` works", {
  testthat::skip_if_offline()

  result <-  get_wb_reg() %>%
    format_wb_reg() %>%
    # drop countries (Channel Islands) that are not stored in the countries object
    filter(iso3 %in% whoville::countries$iso3) %>%
    arrange(iso3)
  testthat::expect_true(nrow(result) > 0)

  expected <- whoville::countries %>%
    dplyr::select(iso3, wb_region, wb_region_name_en) %>%
    filter(!is.na(wb_region)) %>%
    arrange(iso3)
  # testthat::expect_identical(expected, result)
})

test_that("`get_un_m49` works", {
  testthat::skip_if_offline()

  result <- get_un_m49() %>%
      format_un_m49() %>%
    arrange(iso3)
  testthat::expect_true(nrow(result) > 0)

  expected <- whoville::countries %>%
    arrange(iso3) %>%
    select(all_of(names(result)))
  # testthat::expect_identical(expected, result)
})


test_that("`get_undesa_sdg` works", {
  testthat::skip_if_offline()

  result <- get_undesa_sdg() %>%
    format_undesa_sdg() %>%
    arrange(iso3)
  testthat::expect_true(nrow(result) > 0)

  expected <- whoville::countries %>%
    arrange(iso3) %>%
    select(all_of(names(result)))
  # testthat::expect_identical(expected, result)
})


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
    format_who_xmart_ref_country()

  expected <- whoville::countries %>%
    select(names(result))

  testthat::expect_identical(expected, result)
})

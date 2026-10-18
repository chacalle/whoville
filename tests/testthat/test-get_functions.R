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

test_that("`make_countries` works with 'xmart' args set to 'TRUE'", {
  testthat::skip_if_offline()

  # only the logical arguments (e.g. not `gbd_path`)
  args <- names(Filter(is.logical, formals(make_countries)))
  args <- rep(TRUE, length(args)) %>%
    as.list() %>%
    setNames(args)

  result <- do.call(make_countries, args)

  testthat::expect_true(nrow(result) > 0)

  # all sources are included in the log of sources used
  source_log <- attr(result, "source_log")
  testthat::expect_named(source_log, c("data", "source", "version"))
  testthat::expect_equal(nrow(source_log), 9)
})

test_that("`make_countries` works with 'xmart' args set to 'FALSE'", {
  testthat::skip_if_offline()

  # only the logical arguments (e.g. not `gbd_path`)
  args <- names(Filter(is.logical, formals(make_countries)))
  args <- rep(FALSE, length(args)) %>%
    as.list() %>%
    setNames(args)
  args[grepl("include", names(args))] <- TRUE

  result <- do.call(make_countries, args)

  testthat::expect_true(nrow(result) > 0)
})

test_that("`make_countries` works with 'xmart' args set to 'TRUE' and 'include' arguments set to 'FALSE'", {
  testthat::skip_if_offline()

  # only the logical arguments (e.g. not `gbd_path`)
  args <- names(Filter(is.logical, formals(make_countries)))
  args <- rep(TRUE, length(args)) %>%
    as.list() %>%
    setNames(args)
  args[grepl("include", names(args))] <- FALSE

  result <- do.call(make_countries, args)

  testthat::expect_true(nrow(result) > 0)
})

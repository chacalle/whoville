#' Get WHO membership status from ISO3 codes.
#'
#' `is_who_member()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is a WHO member state or not.
#'
#' @param iso3 Character vector of ISO3 codes.
#' @inheritParams resolve_countries_arg
#'
#' @return Logical vector.
#'
#' @export
is_who_member <- function(iso3, countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3", "who_member"))

  members <- countries_use[["who_member"]]
  idx <- match(iso3, countries_use[["iso3"]])
  members[idx] %in% TRUE
}


#' Get WHO small state membership status from ISO3 codes.
#'
#' `is_who_member_small()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is a small WHO member state or not. In this instance,
#' small member states are defined as member states with a total population <=
#' 90,000 in mid-year 2018, according to the World Population Prospects.
#'
#' @inheritParams is_who_member
#'
#' @return Logical vector.
#'
#' @export
is_who_member_small <- function(iso3, countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3", "who_member_small"))

  members <- countries_use[["who_member_small"]]
  idx <- match(iso3, countries_use[["iso3"]])
  members[idx] %in% TRUE
}

#' Get large state membership status from ISO3 codes.
#'
#' `is_who_member_large()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is a large WHO member state or not. In this instance,
#' large member states are defined as member states with a total population >
#' 90,000 in mid-year 2018, according to the World Population Prospects.
#'
#' @inheritParams is_who_member
#'
#' @return Logical vector.
#'
#' @export
is_who_member_large <- function(iso3, countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)

  is_who_member(iso3, countries_manual = countries_use) & !is_who_member_small(iso3, countries_manual = countries_use)
}

#' Get ISO3 codes for WHO member states.
#'
#' `who_member_states()` returns ISO3 codes for WHO member states. By default,
#' it returns all countries, but can be subset to only small (less than 90,000
#' population) or large states. Useful to expand data frames to explicitly include
#' missing data for countries.
#'
#' @param include Subset of member states to return.
#'     * "all": All member states (the default)
#'     * "small": Small member states (2018 population < 90,000)
#'     * "large": Large member states (2018 population >= 90,000)
#' @inheritParams resolve_countries_arg
#'
#' @return A character vector of ISO3 codes.
#'
#' @export
who_member_states <- function(include = c("all", "small", "large"),
                              countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3"))
  include <- rlang::arg_match(include)
  x <- countries_use[["iso3"]]
  if (include == "small") {
    x <- x[is_who_member_small(x, countries_manual = countries_use)]
  } else if (include == "large") {
    x <- x[is_who_member_large(x, countries_manual = countries_use)]
  } else {
    x <- x[is_who_member(x, countries_manual = countries_use)]
  }
  x
}

#' Get OECD membership status from ISO3 codes.
#'
#' `is_oecd_member()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is an OECD member state or not.
#'
#' @inheritParams is_who_member
#'
#' @return Logical vector.
#'
#' @export
is_oecd_member <- function(iso3, countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3", "oecd_member"))
  members <- countries_use[["oecd_member"]]
  idx <- match(iso3, countries_use[["iso3"]])
  members[idx] %in% TRUE
}

#' Get ISO3 codes for OECD member states.
#'
#' `oecd_member_states()` returns ISO3 codes for OECD member states. Useful to
#' expand data frames to explicitly include missing data for countries.
#'
#' @inheritParams resolve_countries_arg
#' @inherit who_member_states return
#'
#' @export
oecd_member_states <- function(countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3"))
  x <- countries_use[["iso3"]]
  x[is_oecd_member(x, countries_manual = countries_use)]
}

#' Check GBD high-income classification status from ISO3 codes.
#'
#' `is_gbd_high_income()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is classified as high-income in the 2019 Global Burden
#' of Disease released by the Institute for Health Metrics and Evaluation.
#'
#' @inheritParams is_who_member
#'
#' @return Logical vector.
#'
#' @export
is_gbd_high_income <- function(iso3, countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3", "gbd_high_income"))
  members <- countries_use[["gbd_high_income"]]
  idx <- match(iso3, countries_use[["iso3"]])
  members[idx] %in% TRUE
}

#' Get ISO3 codes for GBD high-income states.
#'
#' `gbd_high_income_states()` returns ISO3 codes for states classified as high
#' income in the 2019 Global Burden of Disease released by the Institute for
#' Health Metrics and Evaluation. Useful to expand data frames to explicitly
#' include missing data for countries.
#'
#' @inheritParams resolve_countries_arg
#' @inherit who_member_states return
#'
#' @export
gbd_high_income_states <- function(countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3"))
  x <- countries_use[["iso3"]]
  x[is_gbd_high_income(x, countries_manual = countries_use)]
}

#' Get ISO3 codes for least-developed countries.
#'
#' `un_ldcs()` returns ISO3 codes for countries classified as least
#' developed by the United Nations. Useful to expand data frames to explicitly
#' include missing data for countries.
#'
#' @inheritParams resolve_countries_arg
#' @inherit who_member_states return
#'
#' @export
un_ldcs <- function(countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3"))
  x <- countries_use[["iso3"]]
  x[is_un_ldc(x, countries_manual = countries_use)]
}

#' Check least-developed country classification status from ISO3 codes.
#'
#' `is_un_ldc()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is classified as a least-developed country by the
#' United Nations.
#'
#' @inheritParams is_who_member
#'
#' @return Logical vector.
#'
#' @export
is_un_ldc <- function(iso3, countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3", "un_ldc"))
  members <- countries_use[["un_ldc"]]
  idx <- match(iso3, countries_use[["iso3"]])
  members[idx] %in% TRUE
}

#' Get ISO3 codes for land-locked developing countries.
#'
#' `un_lldcs()` returns ISO3 codes for countries classified as land-locked
#' developing countries by the United Nations. Useful to expand data frames to explicitly
#' include missing data for countries.
#'
#' @inheritParams resolve_countries_arg
#' @inherit who_member_states return
#'
#' @export
un_lldcs <- function(countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3"))
  x <- countries_use[["iso3"]]
  x[is_un_lldc(x, countries_manual = countries_use)]
}

#' Check land-locked developing country classification status from ISO3 codes.
#'
#' `is_un_lldc()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is classified as a land-locked developing country by the
#' United Nations.
#'
#' @inheritParams is_who_member
#'
#' @return Logical vector.
#'
#' @export
is_un_lldc <- function(iso3, countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3", "un_lldc"))
  members <- countries_use[["un_lldc"]]
  idx <- match(iso3, countries_use[["iso3"]])
  members[idx] %in% TRUE
}

#' Get ISO3 codes for small island developing states.
#'
#' `un_sids()` returns ISO3 codes for countries classified as small island
#' developing states by the United Nations. Useful to expand data frames to explicitly
#' include missing data for countries.
#'
#' @inheritParams resolve_countries_arg
#' @inherit who_member_states return
#'
#' @export
un_sids <- function(countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3"))
  x <- countries_use[["iso3"]]
  x[is_un_sid(x, countries_manual = countries_use)]
}

#' Check small island developing state status from ISO3 codes.
#'
#' `is_un_sid()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is classified as a small island developing state by the
#' United Nations.
#'
#' @inheritParams is_who_member
#'
#' @return Logical vector.
#'
#' @export
is_un_sid <- function(iso3, countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  check_cols_exists(countries_use, cols = c("iso3", "un_sids"))
  members <- countries_use[["un_sids"]]
  idx <- match(iso3, countries_use[["iso3"]])
  members[idx] %in% TRUE
}

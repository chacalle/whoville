#' @noRd
name_cols <- function(countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  names(dplyr::select(countries_use, dplyr::contains("name") & !dplyr::contains("region")))
}

#' Country code types available
#'
#' `country_code_types()` provides a character vector of all country code types
#' available in the `countries` data frame.
#'
#' @inheritParams resolve_countries_arg
#'
#' @return Character vector of country code types.
#' @examples
#' country_code_types()
#' @export
country_code_types <- function(countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  nms <- names(countries_use)
  codes <- grep("iso|code|m49", names(countries_use), value = TRUE) %>%
    setdiff("sovereign_iso3")
  return(codes)
}

#' @noRd
assert_no_nas <- function(df) {
  df_nas <- df[!(stats::complete.cases(df)),]
  if (nrow(df_nas) > 0) {
    print(df_nas)
    stop("NAs detected")
  }
  return(df)
}

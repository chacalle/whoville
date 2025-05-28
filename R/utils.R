#' @noRd
name_cols <- function() {
  names(dplyr::select(whoville::countries, dplyr::contains("name") & !dplyr::contains("region")))
}

#' Country code types available
#'
#' `country_code_types()` provides a character vector of all country code types
#' available in the `countries` data frame.
#'
#' @return Character vector of country code types.
#' @examples
#' country_code_types()
#' @export
country_code_types <- function() {
  nms <- names(whoville::countries)
  nms[1:match("gbd_code", nms)]
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

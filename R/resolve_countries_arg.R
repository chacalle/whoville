#' @title Helper function to resolve the countries object used in basic whoville
#'   functions
#'
#' @param countries_manual Optional argument to directly pass in the countries
#'     object to use. If specified the object is used, otherwise the
#'     'whoville.updated.countries' option is used if it is set, and finally
#'     otherwise the built in `whoville::countries` object is used.
#'
#' @examples
#' \dontrun{
#' countries_updated <- make_countries()
#' options(whoville.option.countries = countries_updated)
#' iso3_to_regions(iso3 = 'JPN', region = 'who_region')
#' }
resolve_countries_arg <- function(countries_manual) {

  # use the manually specified countries object if it is passed in
  if (!is.null(countries_manual)) {
    check_data_frame(countries_manual)
    return(countries_manual)
  }

  # otherwise use the option object if it is specified
  countries_option <- getOption("whoville.option.countries", default = NULL)
  if (!is.null(countries_option)) {
    check_data_frame(countries_option)
    return(countries_option)
  }

  # otherwise use the built in countries object
  return(whoville::countries)
}

#' @title Check that the countries object has the specified columns
#'
#' @param countries_object `countries` object to check
#' @param cols columns to ensure exist in `countries_object`
check_cols_exists <- function(countries_object, cols) {
  check_data_frame(countries_object)
  cols_exists <- cols %in% names(countries_object)

  if (any(!cols_exists)) {
    cols_missing <- cols[!cols_exists]
    cols_missing <- paste(cols_missing, collapse = "', '")
    msg <- stringr::str_glue("Columns missing in the specified 'countries' object: '{cols_missing}'")
    stop(msg)
  }
  return(invisible(NULL))
}

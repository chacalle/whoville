
#' @title Get data from a public WHO xMart table.
#'
#' @param url \[`character(1)`\]\cr
#'   Url to xMart api, see the xmart documentation for how to modify the url to
#'   filter rows and select columns.
#'
#' @details
#' This helper function is only set up to get publicly available data from xMart.
#' If you need more general access, see the [`xmart4`](https://github.com/gpw13/xmart4)  R package.
#' - [xMart Documentation](https://extranet.who.int/xmart4/docs/)
#' - [See all available marts](https://extranet.who.int/xmart4/data)
#' - [See example REFMART](https://extranet.who.int/xmart4/REFMART):
#' (Reference database of WHO. For example, standard country and area lists, country population estimates, dimensions, etc.)
#' - [See example REFMART/REF_COUNTRY](https://extranet.who.int/xmart4/REFMART):
#' (COMPREHENSIVE COUNTRY/AREA LIST)
#'
#' @returns
#' \[`tibble()`\] with data from the specified mart and table.
#'
#' @seealso [whoville::format_who_xmart_ref_country()]
#' @seealso [xmart4::xmart4()]
#'
#' @examples
#' \dontrun{
#' url <- "https://xmart-api-public.who.int/REFMART/REF_COUNTRY"
#' get_who_public_xmart(url = url)
#' url <- "https://xmart-api-public.who.int/DATA_/REF_SEX"
#' url <- paste0(url, "?$select=REF_TERM,TERM_NAME & $filter=REF_TERM in ('MALE','FEMALE')")
#' get_who_public_xmart(url = url)
#' }
#'
#' @export
get_who_public_xmart <- function(url = "https://xmart-api-public.who.int/REFMART/REF_COUNTRY") {

  rlang::check_installed(
    pkg = "httr2",
    reason = "to download data from the xmart api"
  )

  check_string(url, allow_empty = FALSE)

  response <- get_xmart_response(url)
  result_data <- get_xmart_table(response)

  if ("@odata.nextLink" %in% names(response)) {
    url_next <- response[["@odata.nextLink"]]
    response_next <- get_xmart_response(url_next)
    result_data_next <- get_xmart_table(response_next)
    result_data <- dplyr::bind_rows(result_data, result_data_next)
  }
  return(result_data)
}

#' @noRd
get_xmart_response <- function(url) {

  check_string(url, allow_empty = FALSE)
  url <- gsub(" ", "%20", url)

  req <- httr2::request(url)
  resp <- req %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  return(resp)
}

#' @noRd
get_xmart_table <- function(response) {
  tbl <- response$value %>%
    dplyr::bind_rows()
  return(tbl)
}

#' @title Format the country list from the WHO xMart REFMART/REF_COUNTRY table
#'
#' @description
#' Format the country list from the WHO xMart REFMART/REF_COUNTRY table for
#' inclusion in the `whoville::countries` object
#'
#' @param dat_who_xmart_ref_country \[`tibble(1)`\]\cr
#'   data returned from `get_who_public_xmart`
#'
#' @returns
#' \[`tibble()`\] with formatted WHO country list with data for the
#' `whoville::countries` object.
#'
#' @seealso [whoville::get_who_public_xmart()]
format_who_xmart_ref_country <- function(dat_who_xmart_ref_country) {
  check_data_frame(dat_who_xmart_ref_country)
  dat_who_xmart_ref_country <- dat_who_xmart_ref_country %>%
    dplyr::mutate(
      m49 = .data$GEO_M49_CODE %>%
        as.numeric() %>%
        as.character(),
      iso3 = .data$CODE_ISO_3,
      iso2 = .data$CODE_ISO_2 %>%
        dplyr::na_if("NA"),
      iso_numeric = .data$CODE_ISO_NUMERIC %>%
        as.numeric(),
      who_code = .data$CODE_WHO,
      who_short_name_en = .data$NAME_SHORT_EN,
      who_formal_name_en = .data$NAME_FORMAL_EN,
      who_short_name_ar = .data$NAME_SHORT_AR,
      who_formal_name_ar = .data$NAME_FORMAL_AR,
      who_short_name_es = .data$NAME_SHORT_ES,
      who_formal_name_es = .data$NAME_FORMAL_ES,
      who_short_name_fr = .data$NAME_SHORT_FR,
      who_formal_name_fr = .data$NAME_FORMAL_FR,
      who_short_name_ru = .data$NAME_SHORT_RU,
      who_formal_name_ru = .data$NAME_FORMAL_RU,
      who_short_name_zh = .data$NAME_SHORT_ZH,
      who_formal_name_zh = .data$NAME_FORMAL_ZH,
      who_member = .data$WHO_LEGAL_STATUS %in% "M",
      who_member_small = .data$GEO_SMALL_POP_FLAG == "TRUE",
      sovereign_iso3 = .data$SOVEREIGN_ISO_3,
      who_region = .data$GRP_WHO_REGION,
      .keep = "none"
    )
  return(dat_who_xmart_ref_country)
}


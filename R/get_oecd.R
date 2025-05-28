
#' @title Get/Format Organisation for Economic Co-operation and Development (OECD)
#'   member countries
#'
#' @param url \[`character(1)`\]\cr
#'   OECD member country webpage url
#' @param oecd_country_names \[`character()`\]\cr Scraped OECD member country names
#' @param map_iso3_manual \[`data.frame()`\]\cr
#'   Mapping between `oecd_country_name` and `iso3` for OECD country names that
#'   do not exactly match one of the `whoville::countries` name columns.
#' @param allow_na \[`logical(1)`\]\cr
#'   Whether to allow NA values in the result. Switching this to `TRUE` can be
#'   helpful to figure out which `oecd_country_names` are not automatically
#'   mapping to a `iso3`.
#'
#' @returns
#' `get_oecd_countries()` returns a \[`character()`\] vector with member country
#' names as presented on the `url`. `format_oecd_countries()` returns a \[`data.frame()`\]
#' with a row for each `oecd_country_names` and the mapped `iso3` code.
#'
#' @examples
#' \dontrun{
#' oecd <- get_oecd_countries() %>%
#'   format_oecd_countries()
#' }
#'
#' @rdname oecd_countries
get_oecd_countries <- function(url = "https://www.oecd.org/en/about/members-partners.html") {

  check_string(url, allow_empty = FALSE)
  rlang::check_installed(
    pkg = "rvest",
    reason = "to get oecd countries because no file is currently available for download so we must scrape the website with the 'rvest' R package"
  )
  rlang::check_installed(
    pkg = c("rvest", "chromote", "R6"),
    reason = "because the oecd website uses javascript to generate HTML so we require live web scraping (with rvest/chromote)"
  )

  sess <- rvest::read_html_live(url)

  oecd_country_names <- sess %>%
    rvest::html_elements("#container-85cd66efb5 .cmp-tabs-list__item-link") %>%
    rvest::html_text2()
  check_character(oecd_country_names, allow_na = FALSE)

  return(oecd_country_names)
}

#' @rdname oecd_countries
format_oecd_countries <- function(oecd_country_names,
                                  map_iso3_manual = data.frame(
                                    oecd_country_name = c("Korea", "Slovak Republic", "Netherlands"),
                                    iso3 = c("KOR", "SVK", "NLD")
                                  ),
                                  allow_na = FALSE) {

  check_character(oecd_country_names, allow_na = FALSE)
  check_data_frame(map_iso3_manual)

  oecd <- dplyr::tibble(
    oecd_country_name = oecd_country_names
  ) %>%
    dplyr::mutate(iso3 = names_to_iso3(names = .data$oecd_country_name, fuzzy_matching = "no")) %>%
    dplyr::left_join(map_iso3_manual, by = "oecd_country_name", suffix = c("", "_manual")) %>%
    dplyr::mutate(iso3 = dplyr::if_else(is.na(.data$iso3), .data$iso3_manual, .data$iso3)) %>%
    dplyr::select(-c("iso3_manual"))

  if (!allow_na) {
    assert_no_nas(oecd)
  }

  na_row <- oecd %>%
    dplyr::filter(is.na(.data$iso3))
  if (nrow(na_row) > 0) {
    na_row_countries <- paste0("'", paste(na_row$oecd_country_name, collapse = "', '"), "'")
    warning("- OECD: some country names were not matched with an iso3 code (", na_row_countries, "). Rerun with modified `map_iso3_manual` argument")
  }

  oecd <- whoville::countries %>%
    select(iso3) %>%
    mutate(oecd_member = iso3 %in% oecd$iso3)

  return(oecd)
}

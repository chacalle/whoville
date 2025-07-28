
#' @title Get/Format the regions defined by the United Nations Department of
#'   Economic and Social Affairs (UN DESA) and the Sustainable Development Goals (SDG)
#'
#' @description
#' Get and format the UN DESA and SDG regions defined in the World Population Prospects (WPP)
#' locations [documentation](https://population.un.org/wpp/downloads?folder=Documentation&group=Documentation).
#' The [WPP 'Definition of Regions' page](https://population.un.org/wpp/definition-of-regions)
#' also describes this in more detail.
#'
#' @param url_file \[`character(1)`\]\cr
#'   Url to download the 'Locations (XLSX)' file from, the 'DB' sheet is read in.
#'   This link changes for every WPP revision year.
#' @param url_wpp \[`character(1)`\]\cr
#'   Url to the WPP home page. This is used to double check whether a more
#'   recent `url_file` link is likely available.
#' @param wpp_year \[`numeric(1)`\]\cr
#'   The WPP year that should be expected to be found on the WPP home page.
#'   This is used to double check whether a more recent `url_file` link is
#'   likely available.
#' @param dat_undesa_sdg \[`data.frame(1)`\]\cr
#'   The returned object from `get_undesa_sdg` to be formatted to match the
#'   `whoville::countries` object formatting.
#'
#' @returns
#' `format_sdg_xmart` returns a \[`tibble()`\] with the SDG regions definied in
#' xMart 'REFMART/REF_GROUPS_CURRENT' formatted to match the
#' `whoville::countries` object.
#'
#' `get_undesa_sdg_direct` returns a \[`tibble()`\] with the raw data found
#' in the 'Locations (XLSX)' document 'DB' sheet. `format_undesa_sdg_direct` returns a
#' \[`tibble()`\] with the same data formatted to match the
#' `whoville::countries` object.
#'
#' @examples
#' \dontrun{
#' dat_sdg <- format_sdg_xmart(
#'   dat_ref_groups_current =  get_who_public_xmart(
#'     url = "https://xmart-api-public.who.int/REFMART/REF_GROUPS_CURRENT"
#'   )
#' )
#'
#' undesa_sdg <- get_undesa_sdg_direct() %>%
#'   format_undesa_sdg_direct()
#' }
#'
#' @rdname undesa_sdg
get_undesa_sdg_direct <- function(url_file = "https://population.un.org/wpp/assets/Excel%20Files/4_Metadata/WPP2024_F01_LOCATIONS.xlsx",
                                  url_wpp = "https://population.un.org/wpp/",
                                  wpp_year = 2024) {

  message("Getting location metadata from UN DESA & SDG")
  check_string(url_file, allow_empty = FALSE)
  check_string(url_wpp, allow_empty = FALSE)
  check_number_whole(wpp_year, allow_empty = FALSE)

  # check that a more recent WPP version is not available
  rlang::check_installed(
    pkg = "rvest",
    reason = "to scrape the WPP webpage"
  )
  webpage <- rvest::read_html(url_wpp)
  header <- rvest::html_elements(webpage, ".header") %>%
    rvest::html_text2()
  wpp_year_found <- header %>%
    stringr::str_extract(pattern = "\\d\\d\\d\\d") %>%
    as.numeric()
  if (!identical(wpp_year, wpp_year_found)) {
    stop(stringr::str_glue("The WPP year found in the `url_wpp` header ('{wpp_year_found}') does not match the expected `wpp_year` ('{wpp_year}'). May need to update the `url_file` link."))
  }

  message("- downloading 'Locations (XLSX)' file from '", url_file, "'")
  temp <- tempfile(fileext = ".xlsx")
  utils::download.file(url_file, temp, quiet = TRUE)

  message("- reading 'DB' sheet")
  dat_undesa_sdg <- readxl::read_xlsx(path = temp, sheet = "DB")

  return(dat_undesa_sdg)
}

#' @rdname undesa_sdg
format_undesa_sdg_direct <- function(dat_undesa_sdg) {

  message("Formatting UN DESA & SDG location metadata to match the `whoville::countries` object")

  dat_undesa_sdg <- dat_undesa_sdg %>%
    dplyr::mutate(
      iso3 = .data$ISO3_Code,
      un_desa_region = .data$GeoRegID %>%
        as.character(),
      un_desa_region_name_en = .data$GeoRegName,
      un_desa_subregion = .data$SubRegID %>%
        as.character(),
      un_desa_subregion_name_en = .data$SubRegName,
      sdg_region = .data$SDGRegID %>%
        as.character(),
      sdg_region_name_en = .data$SDGRegName,
      sdg_subregion = .data$SDGSubRegID %>%
        as.character(),
      sdg_subregion_name_en = .data$SDGSubRegName,
      .keep = "none"
    ) %>%
    dplyr::filter(!is.na(.data$iso3)) %>%
    dplyr::mutate(
      sdg_subregion = dplyr::if_else(
        is.na(.data$sdg_subregion),
        .data$sdg_region,
        .data$sdg_subregion
      ),
      sdg_subregion_name_en = dplyr::if_else(
        is.na(.data$sdg_subregion_name_en),
        .data$sdg_region_name_en,
        .data$sdg_subregion_name_en
      ),

      # preserve the spelling in the original package version
      # also matches current 'un_subregion_name_en'and
      # https://unstats.un.org/sdgs/indicators/regional-groups/
      un_desa_subregion_name_en = dplyr::if_else(
        .data$un_desa_subregion_name_en == "Australia/New Zealand",
        "Australia and New Zealand",
        .data$un_desa_subregion_name_en
      ),
      sdg_region_name_en = dplyr::if_else(
        .data$sdg_region_name_en == "Australia/New Zealand",
        "Australia and New Zealand",
        .data$sdg_region_name_en
      ),
      sdg_subregion_name_en = dplyr::if_else(
        .data$sdg_subregion_name_en == "Australia/New Zealand",
        "Australia and New Zealand",
        .data$sdg_subregion_name_en
      )
    )
  return(dat_undesa_sdg)
}

#' @rdname undesa_sdg
#' @inheritParams format_unsd_special_xmart
format_sdg_xmart <- function(dat_ref_groups_current) {
  check_data_frame(dat_ref_groups_current)

  dat_sdg <- dat_ref_groups_current %>%
    dplyr::filter(.data$GROUP_TYPE_CODE == "UNSD_REGION_SDG") %>%
    dplyr::filter(.data$GROUP_NAME != "Europe, Northern America, Australia and New Zealand") %>%
    dplyr::select(m49 = "GEO_CODE_M49", "GROUP_CODE", "GROUP_NAME", "GROUP_LEVEL") %>%
    dplyr::mutate(
      m49 = .data$m49 %>%
        as.numeric() %>%
        as.character(),
      GROUP_LEVEL = dplyr::case_when(
        .data$GROUP_LEVEL == 2 ~ "sdg_region",
        .data$GROUP_LEVEL == 3 ~ "sdg_subregion"
      )
    ) %>%
    tidyr::pivot_longer(cols = c("GROUP_CODE", "GROUP_NAME")) %>%
    dplyr::mutate(
      name = dplyr::case_match(
        .data$name,
        "GROUP_CODE" ~ "",
        "GROUP_NAME" ~ "_name_en")
    ) %>%
    tidyr::pivot_wider(names_from = c("GROUP_LEVEL", "name"), names_sep = "", values_from = "value")
  return(dat_sdg)
}

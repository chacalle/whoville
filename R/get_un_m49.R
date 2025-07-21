#' @title Get/Format the United Nations Statistics Division (UNSD) m49 regions
#' and location name translations.
#'
#' @param url \[`character(1)`\]\cr
#'   Url to scrape the UNSD table (in html format) from.
#' @param dat_unsd_m49 \[`data.frame(1)`\]\cr
#'   The returned object from `get_un_m49` to be formatted to match the
#'   `whoville::countries` object formatting.
#' @param language_codes \[`character()`\]\cr
#'   Named character vector with the values corresponding to the language names
#'   used in `dat_unsd_m49` and names corresponding to the language code to use
#'   in the returned formatted data frame.
#'
#' @returns
#' `get_un_m49` returns a named list of \[`data.frame()`\]s with the list names
#' corresponding to the labeled language. `format_un_m49` returns a \[`tibble()`\]
#' with the same data formatted to match the `whoville::countries` object.
#'
#' @examples
#' \dontrun{
#' un_m49 <- get_un_m49() %>%
#'   format_un_m49()
#' }
#'
#' @rdname un_m49
get_un_m49 <- function(url = "https://unstats.un.org/unsd/methodology/m49/overview/") {

  rlang::check_installed(
    pkg = "rvest",
    reason = "to scrape the UN M49 webpage"
  )
  check_string(url)

  webpage <- rvest::read_html(url)

  # get languages available in table
  languages_unsd_m49 <- webpage %>%
    rvest::html_element(".nav-tabs") %>%
    rvest::html_elements("a") %>%
    rvest::html_text() %>%
    tolower()

  dat_unsd_m49 <- webpage %>%
    rvest::html_table(header = TRUE) %>%
    stats::setNames(languages_unsd_m49)

  return(dat_unsd_m49)
}

#' @rdname un_m49
format_un_m49 <- function(dat_unsd_m49,
                          language_codes = c(
                            "ru" = "russian",
                            "fr" = "french",
                            "es" = "spanish",
                            "ar" = "arabic",
                            "zh" = "chinese"
                          )) {

  stopifnot(rlang::is_character(language_codes))
  stopifnot(rlang::is_list(dat_unsd_m49))
  stopifnot(all(sapply(dat_unsd_m49, is.data.frame)))

  # Pulling in names in different languages
  languages_unsd_m49 <- names(dat_unsd_m49)
  ln_codes <- names(language_codes)
  ln_names <- unname(language_codes)
  stopifnot(identical(setdiff(languages_unsd_m49, ln_names), "english"))

  # Prep full codes and names from the English table, only does not include the
  # "Global Code" and "Global Name" columns
  unsd_m49_english <- dat_unsd_m49[["english"]] %>%
    dplyr::mutate(
      m49 = .data$`M49 Code` %>%
        as.character(),
      iso2 = .data$`ISO-alpha2 Code`,
      iso3 = .data$`ISO-alpha3 Code`,
      un_ldc = !.data$`Least Developed Countries (LDC)` %in% c(NA, ""),
      un_lldc = !.data$`Land Locked Developing Countries (LLDC)` %in% c(NA, ""),
      un_sids = !.data$`Small Island Developing States (SIDS)` %in% c(NA, ""),
      un_name_en = .data$`Country or Area`,
      un_region = .data$`Region Code` %>%
        as.character(),
      un_region_name_en = .data$`Region Name`,
      un_subregion = .data$`Sub-region Code` %>%
        as.character(),
      un_subregion_name_en = .data$`Sub-region Name`,
      un_intermediate_region = .data$`Intermediate Region Code` %>%
        as.character(),
      un_intermediate_region_name_en = .data$`Intermediate Region Name`,
      .keep = "none"
    ) %>%
    dplyr::mutate(
      un_intermediate_region = dplyr::if_else(
        .data$un_intermediate_region %in% c(NA, ""),
        .data$un_subregion,
        .data$un_intermediate_region
      ),
      un_intermediate_region_name_en = dplyr::if_else(
        .data$un_intermediate_region_name_en %in% c(NA, ""),
        .data$un_subregion_name_en,
        .data$un_intermediate_region_name_en
      )
    )

  # For other languages prep the country and region names, all other codes and
  # metadata will be the same
  unsd_m49_other <- lapply(1:length(ln_codes), function(i) {
    dat_unsd_m49[[ln_names[i]]] %>%
      dplyr::mutate(
        `Intermediate Region Name` = dplyr::if_else(
          .data$`Intermediate Region Name` %in% c(NA, ""),
          .data$`Sub-region Name`,
          .data$`Intermediate Region Name`
        )
      ) %>%
      dplyr::mutate(
        m49 = .data$`M49 Code` %>%
          as.character(),
        !!sym(paste0("un_name_", ln_codes[i])) := .data$`Country or Area`,
        !!sym(paste0("un_region_name_", ln_codes[i])) := .data$`Region Name`,
        !!sym(paste0("un_subregion_name_", ln_codes[i])) := .data$`Sub-region Name`,
        !!sym(paste0("un_intermediate_region_name_", ln_codes[i])) := .data$`Intermediate Region Name`,
        .keep = "none"
      )
  })
  unsd_m49_other <- Reduce(f = function(x1, x2) dplyr::full_join(x1, x2, by = "m49"), x = unsd_m49_other)

  unsd_m49 <- unsd_m49_english %>%
    dplyr::full_join(unsd_m49_other, by = "m49") %>%
    # drop Antarctica which was not included previously
    dplyr::filter(.data$iso3 != "ATA")
    # mutate(across(starts_with("un_intermediate_region_name"), ~ na_if(x = .x, y = "")))

  return(unsd_m49)
}

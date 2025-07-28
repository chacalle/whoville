#' @title Years of WB IG classifications available
#'
#' @description
#' `wb_ig_years()` provides a numeric vector of all years of World Bank Income
#' Group classifications available in the `whoville::countries` data frame.
#'
#' @examples
#' wb_ig_years()
#'
#' @export
wb_ig_years <- function() {
  nms <- names(whoville::countries)
  nms <- nms[grepl("wb_ig_", nms)]
  as.numeric(gsub("[^0-9]", "", nms))
}

#' @title Get/Format the World Bank Income Group and Region country classifications.
#'
#' @description
#' Each fiscal year, the World Bank updates their income group country
#' classifications and releases the files
#' [here](https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups).
#'
#' The links to the "historical classification by income in XLSX format" and
#' the "current classification by income in XLSX format" include the World Bank
#' Income Group and World Bank Region classifications respectively. The
#' `get_wb_ig_direct` & `get_wb_reg_direct` download these files and read in the
#' corresponding sheet from the XLSX document. `format_wb_ig_direct` and
#' `format_wb_reg_direct` then format the data to match the `whoville::countries`
#' object formatting.
#'
#' Alternatively (preferably), this same data is extracted and uploaded to the
#' xMart4 REFMART 'REF_GROUPS_CURRENT' & 'REF_GROUPS_FULL' tables.
#' `format_wb_ig_xmart` & `format_wb_ref_xmart` format data from these tables
#' to match the `whoville::countries` object formatting.
#'
#' @param url \[`character(1)`\]\cr
#'   Url to download the country classifications directly from the WB website.
#' @param sheet \[`character(1)`\]\cr
#'   The name of the sheet within the xlsx document where the country
#'   classifications are stored.
#' @param skip_years \[`integer(1)`\]\cr
#'   The number of rows to skip when reading the XLSX document before reaching
#'   the row listing the years available in the historical classification by income group document.
#' @param skip_data \[`integer(1)`\]\cr
#'   The number of rows to skip when reading the XLSX document before reaching
#'   the data rows with the country classifications.
#' @param dat_raw \[`data.frame(1)`\]\cr
#'   The original country classification data from the World Bank or xMart4
#'   REFMART tables to be formatted to match the `whoville::countries` object
#'   formatting.
#'
#' @returns
#' `get_wb_ig_xmart` & `get_wb_reg_xmart` return a \[`tibble()`\] with data
#' formatted to match the `whoville::countries` object formatting.
#'
#' `get_wb_ig_direct` and `get_wb_reg_direct` return a \[`tibble()`\] with data
#' from the specified World Bank XLSX document. `format_wb_ig_direct` and
#' `format_wb_reg_direct` return a \[`tibble()`\] with the same data formatted
#' to match the `whoville::countries` object formatting.
#'
#' @examples
#' \dontrun{
#' wb_ig_xmart <- get_who_public_xmart(
#'   url = "https://xmart-api-public.who.int/REFMART/REF_GROUPS_FULL?$filter=GROUP_TYPE_CODE eq 'WB_INCOME'"
#' ) %>%
#'   format_wb_ig_xmart()
#' wb_ig_direct <- get_wb_ig_direct() %>%
#'   format_wb_ig_direct()
#'
#' wb_reg_xmart <- get_who_public_xmart(
#'   url = "https://xmart-api-public.who.int/REFMART/REF_GROUPS_CURRENT?$filter=GROUP_TYPE_CODE eq 'WB_REGION'"
#' ) %>%
#'   format_wb_reg_xmart()
#' wb_reg_direct <- get_wb_reg_direct() %>%
#'   format_wb_reg_direct()
#' }
#'
#' @rdname wb
get_wb_ig_direct <- function(url = "https://ddh-openapi.worldbank.org/resources/DR0095334/download",
                      sheet = "Country Analytical History",
                      skip_years = 5,
                      skip_data = 10) {

  check_string(url)
  check_string(sheet)
  check_number_whole(skip_years)
  check_number_whole(skip_data)

  temp <- tempfile(fileext = ".xlsx")
  utils::download.file(url, temp, quiet = TRUE)

  # identify years available in current sheet to use for column names
  wb_ig_years_available <- readxl::read_xlsx(
    path = temp,
    sheet = sheet,
    skip = skip_years,
    n_max = 1
  ) %>%
    names()
  wb_ig_years_available <- grep(pattern = "\\d+", x = wb_ig_years_available, value = TRUE)
  wb_ig_years_available <- as.numeric(wb_ig_years_available)
  stopifnot(wb_ig_years_available[1] == 1987)

  year_current <- Sys.Date() %>%
    substr(1, 4) %>%
    as.integer()
  if (Sys.Date() > as.Date(paste0(year_current, "-06-01"))) {
    wb_year_expected <- year_current - 1
  } else {
    wb_year_expected <- year_current - 2
  }

  if (!wb_year_expected %in% wb_ig_years_available) {
    warning("Updated World Bank Income Group classifications should be available after July 1st of each year, check for a new link")
  }

  wb_ig <- readxl::read_xlsx(
    path = temp,
    sheet = sheet,
    skip = skip_data,
    col_names = c("iso3", "name", wb_ig_years_available),
    na = ".."
  )
  return(wb_ig)
}

#' @rdname wb
get_wb_reg_direct <- function(url = "https://ddh-openapi.worldbank.org/resources/DR0095333/download",
                              sheet = "List of economies") {

  temp <- tempfile(fileext = ".xlsx")
  utils::download.file(url, temp, quiet = TRUE)

  wb_reg <- readxl::read_xlsx(
    path = temp,
    sheet = "List of economies",
    na = ".."
  )
  return(wb_reg)
}

#' @rdname wb
format_wb_ig_direct <- function(dat_raw) {
  check_data_frame(dat_raw)

  dat_formatted <- dat_raw %>%
    # blank space in sheet before former countries ("Czechoslovakia (former)", etc.)
    dplyr::filter(!is.na(.data$iso3)) %>%
    dplyr::mutate(
      iso3 = .data$iso3,
      dplyr::across(
        .cols = dplyr::matches("[0-9]{4}"),
        .fns = ~ dplyr::case_when(
          .x %in% c("L", "H") ~ paste0(.x, "IC"),
          .x %in% c("LM", "UM") ~ paste0(.x, "C")
        ),
        .names = "wb_ig_{.col}"
      ),
      .keep = "none"
    ) %>%
    return()
  return(dat_formatted)
}

#' @rdname wb
format_wb_reg_direct <- function(dat_raw) {
  check_data_frame(dat_raw)

  dat_formatted <- dat_raw %>%
    dplyr::filter(!is.na(.data$Region)) %>%
    dplyr::rename(iso3 = "Code", wb_region = "Region") %>%
    dplyr::select("iso3", "wb_region") %>%
    dplyr::mutate(wb_region_name_en = .data$wb_region) %>%
    assert_no_nas()
  return(dat_formatted)
}

#' @rdname wb
format_wb_ig_xmart <- function(dat_raw) {
  check_data_frame(dat_raw)

  dat_formatted <- dat_raw %>%
    dplyr::filter(!is.na(.data$GROUP_CODE)) %>%
    dplyr::select(m49 = "GEO_CODE_M49", "GRP_RELEASE_CODE", "GROUP_NAME") %>%
    dplyr::mutate(
      m49 = .data$m49 %>%
        as.numeric() %>%
        as.character(),

      GRP_RELEASE_CODE = stringr::str_replace(.data$GRP_RELEASE_CODE, "FY", "") %>%
        as.numeric(),
      GRP_RELEASE_CODE = dplyr::case_when(
        GRP_RELEASE_CODE >= 80 ~ 1900 + .data$GRP_RELEASE_CODE,
        .default = 2000 + .data$GRP_RELEASE_CODE
      ),
      # convert from fiscal year to calendar year
      GRP_RELEASE_CODE = .data$GRP_RELEASE_CODE - 2,

      GROUP_NAME = dplyr::case_match(
        .data$GROUP_NAME,
        "Low income" ~ "LIC",
        "Lower middle income" ~ "LMC",
        "Upper middle income" ~ "UMC",
        "High income" ~ "HIC"
      )
    ) %>%
    assert_no_nas() %>%
    dplyr::arrange(.data$m49, .data$GRP_RELEASE_CODE) %>%
    tidyr::pivot_wider(
      names_from = "GRP_RELEASE_CODE",
      names_prefix = "wb_ig_",
      values_from = "GROUP_NAME"
    )

  # arrange correctly by year
  nms <- names(dat_formatted) %>%
    setdiff("m49") %>%
    sort()
  dat_formatted <- dat_formatted %>%
    dplyr::select("m49", dplyr::all_of(nms))

  return(dat_formatted)
}

#' @rdname wb
format_wb_reg_xmart <- function(dat_raw) {
  check_data_frame(dat_raw)

  dat_formatted <- dat_raw %>%
    dplyr::select(m49 = "GEO_CODE_M49", wb_region = "GROUP_CODE", wb_region_name_en = "GROUP_NAME") %>%
    dplyr::mutate(
      m49 = .data$m49 %>%
        as.numeric() %>%
        as.character(),
    )
  return(dat_formatted)
}

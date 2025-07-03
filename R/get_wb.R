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
#' The links to the "historical classification by income in XLSX format" and
#' the "current classification by income in XLSX format" include the World Bank
#' Income Group and World Bank Region classifications respectively.
#'
#' @param url \[`character(1)`\]\cr
#'   Url to download the country classifications if xlsx format.
#' @param sheet \[`character(1)`\]\cr
#'   The name of the sheet within the xlsx document where the country
#'   classifications are stored.
#' @param skip_years \[`integer(1)`\]\cr
#'   The number of rows to skip when reading the XLSX document before reaching
#'   the row listing the years available in the historical classification by income group document.
#' @param skip_data \[`integer(1)`\]\cr
#'   The number of rows to skip when reading the XLSX document before reaching
#'   the data rows with the country classifications.
#' @param dat \[`data.frame(1)`\]\cr
#'   The original country classification data from the World Bank to be formatted
#'   to match the `whoville::countries` object formatting.
#'
#' @returns
#' `get_wb_ig` and `get_wb_reg` return a \[`tibble()`\] with data from the
#' specified XLSX document. `format_wb_ig` and `format_wb_reg` return a
#' \[`tibble()`\] with the same data formatted to match the `whoville::countries`
#' object formatting.
#'
#' @examples
#' \dontrun{
#' wb_ig <- get_wb_ig() %>%
#'   format_wb_ig()
#' wb_reg <- get_wb_reg() %>%
#'   format_wb_reg()
#' }
#'
#' @rdname wb
get_wb_ig <- function(url = "https://ddh-openapi.worldbank.org/resources/DR0095334/download",
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
get_wb_reg <- function(url = "https://ddh-openapi.worldbank.org/resources/DR0095333/download",
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
format_wb_ig <- function(dat) {
  dat_formatted <- dat %>%
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
format_wb_reg <- function(dat) {
  dat_formatted <- dat %>%
    dplyr::filter(!is.na(.data$Region)) %>%
    dplyr::rename(iso3 = "Code", wb_region = "Region") %>%
    dplyr::select("iso3", "wb_region") %>%
    dplyr::mutate(wb_region_name_en = .data$wb_region) %>%
    assert_no_nas()
  return(dat_formatted)
}

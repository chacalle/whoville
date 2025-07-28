#' @title Get/Format Institute for Health Metrics and Evaluation (IHME) Global
#' Burden of Disease (GBD) location hierarchy information
#'
#' @param gbd_year \[`character(1)`\]\cr
#'   String specifying the GBD year/version to get
#' @param url \[`character()`\]\cr
#'   Named vector specifying GBD year/version specific urls to download zip
#'   files from
#' @param zip_fname \[`character()`\]\cr
#'   Named vector specifying GBD year/version specific file name to read from
#'   the downloaded zip file
#' @param dat \[`data.frame(1)`\]\cr
#'   The original GBD location hierarchy to be formatted to match the
#'   `whoville::countries` object formatting
#' @param map_iso3_manual \[`data.frame()`\]\cr
#'   Mapping between `Location Name` and `iso3` for GBD location names that do
#'   not exactly match one of the `whoville::countries` name columns.
#'
#' @returns
#' `get_gbd()_direct` returns a \[`tibble()`\] with the location hierarchy
#' from the specified zip/xlsx file. `format_gbd()_direct` returns a \[`tibble()`\]
#' with a row for each GBD level 3 location and the mapped `iso3` code.
#'
#' @examples
#' \dontrun{
#' dat_gbd <- get_gbd_direct("2021") %>%
#'   format_gbd_direct()
#' }
#'
#' @rdname gbd
get_gbd_direct <- function(gbd_year = "2021",
                           url = c(
                             "2021" = "http://ghdx.healthdata.org/sites/default/files/ihme_query_tool/IHME_GBD_2021_CODEBOOK.zip",
                             "2019" = "http://ghdx.healthdata.org/sites/default/files/ihme_query_tool/IHME_GBD_2019_CODEBOOK.zip"
                           ),
                           zip_fname = c(
                             "2021" = "IHME_GBD_2021_HIERARCHIES_Y2024M05D16.XLSX",
                             "2019" = "IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2022M06D29.XLSX"
                           )) {

  message("Getting location metadata from GBD ", gbd_year)

  check_string(gbd_year, allow_empty = FALSE)
  url <- url[gbd_year]
  check_string(url, allow_empty = FALSE)
  zip_fname <- zip_fname[gbd_year]
  check_string(zip_fname, allow_empty = FALSE)

  # Hierarchies are also stored as individual ghdx records including GBD 2016 & 2017, these require logging into the GHDX
  # https://ghdx.healthdata.org/record/global-burden-disease-study-2021-gbd-2021-cause-rei-and-location-hierarchies
  # https://ghdx.healthdata.org/record/ihme-data/gbd-2019-cause-rei-and-location-hierarchies
  # https://ghdx.healthdata.org/record/ihme-data/gbd-2017-cause-rei-and-location-hierarchies
  # https://ghdx.healthdata.org/record/ihme-data/gbd-2016-location-hierarchies

  temp_z <- tempfile()
  on.exit(file.remove(temp_z))
  temp_d <- tempdir()
  on.exit(unlink(temp_d, recursive = TRUE))

  utils::download.file(url, destfile = temp_z, quiet = TRUE)

  zip_fnames <- zip::zip_list(temp_z)
  if (!zip_fname %in% zip_fnames$filename) {
    zip_fnames <- paste(zip_fnames$filename, collapse = "', '")
    stop("- '", zip_fname, "' is not available in the downloaded zip file\n- available files include: '", zip_fnames, "'")
  }

  dat <- utils::unzip(
    zipfile = temp_z,
    files = zip_fname,
    exdir = temp_d
  ) %>%
    readxl::read_excel()

  return(dat)
}

#' @rdname gbd
format_gbd_direct <- function(dat,
                              map_iso3_manual = dplyr::tibble(
                                `Location Name` = c("Taiwan (Province of China)", "Netherlands", "C\\u00f4te d\\'Ivoire", "Palestine", "United Kingdom"),
                                iso3 = c("TWN", "NLD", "CIV", "PSE", "GBR")
                              )) {

  message("Formatting GBD location metadata to match the `whoville::countries` object")
  check_data_frame(dat)

  map_iso3_manual <- map_iso3_manual %>%
    dplyr::mutate(`Location Name` = stringi::stri_unescape_unicode(.data$`Location Name`))

  dat_gbd_iso3 <- dat %>%
    dplyr::filter(.data$Level == 3) %>%
    dplyr::mutate(iso3 = names_to_iso3(names = .data$`Location Name`, fuzzy_matching = "no")) %>%
    dplyr::left_join(map_iso3_manual, by = "Location Name", suffix = c("", "_manual")) %>%
    dplyr::mutate(iso3 = dplyr::if_else(is.na(.data$iso3), .data$iso3_manual, .data$iso3)) %>%
    dplyr::select(-"iso3_manual") %>%
    assert_no_nas() %>%
    dplyr::select("iso3", gbd_code = "Location ID", gbd_subregion = "Parent ID")

  dat_gbd_subregion <- dat %>%
    dplyr::filter(.data$Level == 2) %>%
    dplyr::select(
      gbd_subregion = "Location ID",
      gbd_subregion_name_en = "Location Name",
      gbd_region = "Parent ID"
    )

  dat_gbd_region <- dat %>%
    dplyr::filter(.data$Level == 1) %>%
    dplyr::select(
      gbd_region = "Location ID",
      gbd_region_name_en = "Location Name",
    )

  dat <- dat_gbd_iso3 %>%
    dplyr::left_join(dat_gbd_subregion, by = "gbd_subregion") %>%
    dplyr::left_join(dat_gbd_region, by = "gbd_region") %>%
    assert_no_nas()

  return(dat)
}

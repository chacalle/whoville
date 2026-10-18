#' @title Get/Format Institute for Health Metrics and Evaluation (IHME) Global
#' Burden of Disease (GBD) location hierarchy information
#'
#' @description
#' Location hierarchies are only available as a direct download for some GBD
#' versions (2021 and 2019). Other versions need to be downloaded from the
#' GHDx record page after logging in, and the file passed with `path`:
#' - GBD 2023: <https://ghdx.healthdata.org/record/gbd-2023-cause-rei-and-location-hierarchies>
#'
#' `get_gbd_direct()` warns if a GHDx page exists for a more recent GBD version
#' than `gbd_year`.
#'
#' @param gbd_year \[`character(1)`\]\cr
#'   String specifying the GBD year/version to get
#' @param path \[`character(1)`\]\cr
#'   Path to a manually downloaded location hierarchy xlsx file for `gbd_year`,
#'   the first sheet is read in. Required for GBD versions without a direct
#'   download in `url`. Default is NULL to download the file from `url`.
#' @param url \[`character()`\]\cr
#'   Named vector specifying GBD year/version specific urls to download zip
#'   files from
#' @param zip_fname \[`character()`\]\cr
#'   Named vector specifying GBD year/version specific file name to read from
#'   the downloaded zip file
#' @param location_set_version_id \[`numeric()`\]\cr
#'   Named vector specifying the GBD year/version specific 'Location Set Version
#'   ID' of the GBD reporting hierarchy, e.g. 1511 is 'GBD 2023 Reporting'. Some
#'   hierarchy files include many location sets, only this one is kept. If
#'   `gbd_year` is not included, all location sets are returned.
#' @param url_ghdx \[`character(1)`\]\cr
#'   Url to the GHDx website, used to check whether a page exists for a more
#'   recent GBD version (`{url_ghdx}/gbd-{year}`).
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
#' dat_gbd <- get_gbd_direct(gbd_year = "2021") %>%
#'   format_gbd_direct()
#'
#' dat_gbd <- get_gbd_direct(
#'   gbd_year = "2023",
#'   path = "IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX"
#' ) %>%
#'   format_gbd_direct()
#' }
#'
#' @rdname gbd
get_gbd_direct <- function(gbd_year = "2023",
                           path = NULL,
                           url = c(
                             "2021" = "http://ghdx.healthdata.org/sites/default/files/ihme_query_tool/IHME_GBD_2021_CODEBOOK.zip",
                             "2019" = "http://ghdx.healthdata.org/sites/default/files/ihme_query_tool/IHME_GBD_2019_CODEBOOK.zip"
                           ),
                           zip_fname = c(
                             "2021" = "IHME_GBD_2021_HIERARCHIES_Y2024M05D16.XLSX",
                             "2019" = "IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2022M06D29.XLSX"
                           ),
                           location_set_version_id = c(
                             "2023" = 1511,
                             "2021" = 1188
                           ),
                           url_ghdx = "https://ghdx.healthdata.org") {

  message("Getting location metadata from GBD ", gbd_year)

  check_string(gbd_year, allow_empty = FALSE)
  check_string(path, allow_empty = FALSE, allow_null = TRUE)
  check_string(url_ghdx, allow_empty = FALSE)

  warn_if_newer_gbd(gbd_year = gbd_year, url_ghdx = url_ghdx)

  # Hierarchies are also stored as individual ghdx records including GBD 2016 & 2017, these require logging into the GHDX
  # https://ghdx.healthdata.org/record/gbd-2023-cause-rei-and-location-hierarchies
  # https://ghdx.healthdata.org/record/global-burden-disease-study-2021-gbd-2021-cause-rei-and-location-hierarchies
  # https://ghdx.healthdata.org/record/ihme-data/gbd-2019-cause-rei-and-location-hierarchies
  # https://ghdx.healthdata.org/record/ihme-data/gbd-2017-cause-rei-and-location-hierarchies
  # https://ghdx.healthdata.org/record/ihme-data/gbd-2016-location-hierarchies

  # read a manually downloaded file
  if (!is.null(path)) {
    if (!file.exists(path)) {
      stop("The GBD location hierarchy file does not exist: '", path, "'")
    }
    if (!stringr::str_detect(basename(path), gbd_year)) {
      warning("The `path` file name does not include the `gbd_year` ('", gbd_year, "'): '", path, "'")
    }
    message("- reading '", path, "'")
    dat <- readxl::read_excel(path) %>%
      filter_gbd_location_set(gbd_year = gbd_year, location_set_version_id = location_set_version_id)
    return(dat)
  }

  if (!gbd_year %in% names(url)) {
    stop(
      "- GBD ",
      gbd_year,
      " location hierarchies can not be downloaded directly\n",
      "- download the hierarchy file from the GHDx record page (requires logging in) and supply it with `path`\n",
      "- see `?get_gbd_direct` for the record pages"
    )
  }
  url <- url[gbd_year]
  zip_fname <- zip_fname[gbd_year]
  check_string(zip_fname, allow_empty = FALSE)

  temp_z <- tempfile()
  temp_d <- tempfile()
  dir.create(temp_d, recursive = TRUE)
  on.exit(unlink(c(temp_z, temp_d), recursive = TRUE), add = TRUE)

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
    readxl::read_excel() %>%
    filter_gbd_location_set(gbd_year = gbd_year, location_set_version_id = location_set_version_id)

  return(dat)
}

#' @rdname gbd
format_gbd_direct <- function(dat,
                              map_iso3_manual = dplyr::tibble(
                                `Location Name` = c("Taiwan (Province of China)", "Taiwan", "Netherlands", "C\\u00f4te d\\'Ivoire", "Palestine", "United Kingdom"),
                                iso3 = c("TWN", "TWN", "NLD", "CIV", "PSE", "GBR")
                              )) {

  message("Formatting GBD location metadata to match the `whoville::countries` object")
  check_data_frame(dat)

  map_iso3_manual <- map_iso3_manual %>%
    dplyr::mutate(`Location Name` = stringi::stri_unescape_unicode(.data$`Location Name`))

  # some hierarchy files (e.g. GBD 2023) include many location sets
  # - `get_gbd_direct()` keeps only the reporting hierarchy when its id is known
  # - otherwise keep the location set that includes the countries (level 3)
  # - other location sets reuse the same location ids with different parents
  if ("Location Set Version ID" %in% names(dat)) {
    location_set_countries <- dat %>%
      dplyr::filter(.data$Level == 3) %>%
      dplyr::distinct(.data$`Location Set Version ID`) %>%
      dplyr::pull()
    if (length(location_set_countries) != 1) {
      stop("Expected the countries (level 3) to be in one location set, found: ", length(location_set_countries))
    }
    dat <- dat %>%
      dplyr::filter(.data$`Location Set Version ID` == location_set_countries)
  }

  dat <- dat %>%
    dplyr::mutate(`Location Name` = repair_double_encoding(.data$`Location Name`))

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
    dplyr::left_join(dat_gbd_subregion, by = "gbd_subregion", relationship = "many-to-one") %>%
    dplyr::left_join(dat_gbd_region, by = "gbd_region", relationship = "many-to-one") %>%
    assert_no_nas()

  return(dat)
}

# Warn if GHDx has a page for a more recent GBD version than `gbd_year`
# - each GBD version has a GHDx page at `{url_ghdx}/gbd-{year}` (true since GBD 2016)
# - only a warning so scripts using an older version keep running
#' @noRd
warn_if_newer_gbd <- function(gbd_year, url_ghdx) {
  if (!rlang::is_installed("httr2")) {
    message("- skipping the check for a more recent GBD version, requires the 'httr2' package")
    return(invisible(NULL))
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  later_years <- seq_len(max(current_year - as.numeric(gbd_year), 0)) + as.numeric(gbd_year)

  for (later_year in later_years) {
    url_page <- stringr::str_glue("{url_ghdx}/gbd-{later_year}")
    status <- tryCatch(
      httr2::request(url_page) %>%
        httr2::req_error(is_error = function(resp) FALSE) %>%
        httr2::req_perform() %>%
        httr2::resp_status(),
      error = function(e) NA_integer_
    )

    if (is.na(status)) {
      message("- could not reach GHDx to check for a more recent GBD version")
      return(invisible(NULL))
    }
    if (status == 200) {
      warning(stringr::str_glue("A more recent GBD version ('{later_year}') than `gbd_year` ('{gbd_year}') may be available, see '{url_page}'."))
    }
  }

  return(invisible(NULL))
}

# Keep only the GBD reporting hierarchy location set for `gbd_year`
# - returns `dat` unchanged if the location set for `gbd_year` is not known
#' @noRd
filter_gbd_location_set <- function(dat, gbd_year, location_set_version_id) {
  has_location_sets <- "Location Set Version ID" %in% names(dat)
  if (!has_location_sets || !gbd_year %in% names(location_set_version_id)) {
    return(dat)
  }

  id_keep <- location_set_version_id[[gbd_year]]
  dat <- dat %>%
    dplyr::filter(.data$`Location Set Version ID` == id_keep)
  if (nrow(dat) == 0) {
    stop("Location set version '", id_keep, "' is not included in the GBD ", gbd_year, " file")
  }

  return(dat)
}

# Repair names that were encoded as UTF-8 twice (e.g. 'TÃ¼rkiye' to 'Türkiye')
#' @noRd
repair_double_encoding <- function(x) {
  is_double_encoded <- !is.na(x) & stringr::str_detect(x, "\u00c3")

  x_repaired <- iconv(x[is_double_encoded], from = "UTF-8", to = "latin1")
  Encoding(x_repaired) <- "UTF-8"
  x[is_double_encoded] <- x_repaired

  return(x)
}

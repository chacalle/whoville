#' @title Make the `whoville::countries` dataset
#'
#' @param wb_use_xmart \[`logical(1)`\]\cr
#'   Whether to get the world bank income groups and regions from xMart or
#'   directly from the world bank website. Default is TRUE.
#' @param unsd_special_use_xmart \[`logical(1)`\]\cr
#'   Whether to get the special unsd classifications (ldc, lldc, sids) from xMart
#'   or directly from the unsd website. Default is TRUE.
#' @param sdg_use_xmart \[`logical(1)`\]\cr
#'   Whether to get the sdg regions from xMart or directly from the undesa wpp
#'   files. Default is TRUE.
#' @param include_undesa \[`logical(1)`\]\cr
#'   Whether to include the undesa regions when making the countries object.
#'   Default is FALSE to enable quicker and less buggy creation of the object
#'   for when users are attempting to make their own updated object.
#' @param include_gbd \[`logical(1)`\]\cr
#'   Whether to include the gbd regions when making the countries object.
#'   Default is FALSE to enable quicker and less buggy creation of the object
#'   for when users are attempting to make their own updated object.
#' @param include_oecd \[`logical(1)`\]\cr
#'   Whether to include oecd membership when making the countries object.
#'   Default is FALSE to enable quicker and less buggy creation of the object
#'   for when users are attempting to make their own updated object.
#'
#' @details
#' - Basic location codes (m49, iso3, iso2, etc)
#' - WHO regions and location name translations
#' - World Bank income groups
#' - World Bank regions
#' - United Nations Statistics Division (UNSD) m49 regions
#' and location name translations
#' - United Nations Department of Economic and Social Affairs (UN DESA) and the
#' Sustainable Development Goals (SDG) regions
#' - Institute for Health Metrics and Evaluation (IHME) Global Burden of
#' Disease (GBD) regions
#' - Organisation for Economic Co-operation and Development (OECD) membership
#'
#' @returns \[`tibble()`\] with the updated data for the `whoville::countries`
#' object.
#'
#' @examples
#' \dontrun{
#' countries_new <- make_countries()
#' }
#'
#' @export
make_countries <- function(wb_use_xmart = TRUE,
                           unsd_special_use_xmart = TRUE,
                           sdg_use_xmart = TRUE,
                           include_undesa = FALSE,
                           include_gbd = FALSE,
                           include_oecd = FALSE) {

  check_bool(wb_use_xmart)
  check_bool(unsd_special_use_xmart)
  check_bool(sdg_use_xmart)

  check_bool(include_undesa)
  check_bool(include_gbd)
  check_bool(include_oecd)

  dat_who_ref_country <- get_who_public_xmart(
    url = "https://xmart-api-public.who.int/REFMART/REF_COUNTRY"
  ) %>%
    format_who_xmart_ref_country()

  dat_who_ref_groups_current <- get_who_public_xmart(
    url = "https://xmart-api-public.who.int/REFMART/REF_GROUPS_CURRENT"
  )

  # get the world bank income groups and regions
  if (wb_use_xmart) {
    dat_wb_ig <- get_who_public_xmart(
      url = "https://xmart-api-public.who.int/REFMART/REF_GROUPS_FULL?$filter=GROUP_TYPE_CODE eq 'WB_INCOME'"
    ) %>%
      format_wb_ig_xmart()

    dat_wb_reg <- dat_who_ref_groups_current %>%
      dplyr::filter(.data$GROUP_TYPE_CODE == "WB_REGION") %>%
      format_wb_reg_xmart()
  } else {
    dat_wb_ig <- get_wb_ig_direct() %>%
      format_wb_ig_direct()
    dat_wb_reg <- get_wb_reg_direct() %>%
      format_wb_reg_direct()
  }

  # get the unsd regions and special groups
  dat_unsd_m49 <- get_un_m49_direct() %>%
    format_un_m49_direct()
  if (unsd_special_use_xmart) {
    dat_unsd_special <- format_unsd_special_xmart(
      dat_ref_groups_current =  dat_who_ref_groups_current,
      unsd_m49s = dat_unsd_m49
    )
    dat_unsd_m49 <- dat_unsd_m49 %>%
      dplyr::select(-c("un_ldc", "un_lldc", "un_sids")) %>%
      dplyr::full_join(dat_unsd_special, by = "m49")
  }

  # get the undesa and sdg regions
  if (include_undesa | !sdg_use_xmart) {
    dat_undesa_sdg <- get_undesa_sdg_direct() %>%
      format_undesa_sdg_direct()

    if (!include_undesa) {
      dat_undesa_sdg <- dat_undesa_sdg %>%
        dplyr::select(-dplyr::starts_with("un_desa"))
    }
  }
  if (sdg_use_xmart) {
    dat_sdg <- format_sdg_xmart(dat_who_ref_groups_current) %>%
      dplyr::left_join(
        y = dat_who_ref_country %>%
          dplyr::select("iso3", "m49"),
        by = "m49"
      ) %>%
      dplyr::select(-"m49")

    if (include_undesa) {
      dat_undesa_sdg <- dat_undesa_sdg %>%
        dplyr::select(-dplyr::starts_with("sdg")) %>%
        dplyr::full_join(dat_sdg, by = "iso3")
    } else {
      dat_undesa_sdg <- dat_sdg
    }
  }

  dat_gbd <- NULL
  if (include_gbd) {
    # TODO: automatically check if a more recent GBD year is available
    dat_gbd <- get_gbd_direct() %>%
      format_gbd_direct()
  }

  oecd <- NULL
  if (include_oecd) {
    oecd <- get_oecd_countries_direct()
  }

  languages <- c("en", "ru", "fr", "es", "ar", "zh")
  who_names <- as.vector(outer(c("who_short_name_", "who_formal_name_"), languages, paste0))
  un_regions <- c("un_region", "un_subregion", "un_intermediate_region")
  un_region_names <- as.vector(outer(paste0(un_regions, "_name_"), languages, paste0))
  regions_others <- c(
    "un_desa_region", "un_desa_subregion",
    "sdg_region", "sdg_subregion",
    "gbd_region", "gbd_subregion",
    "wb_region"
  )
  regions_others <- as.vector(t(outer(regions_others, c("", "_name_en"), paste0)))
  wb_igs <- grep("wb_ig_", names(dat_wb_ig), value = TRUE)

  col_order <- c(
    "iso3",
    "iso2",
    "iso_numeric",
    "who_code",
    "m49",
    "gbd_code",
    "sovereign_iso3",
    "who_member",
    "who_member_small",
    "oecd_member",
    "un_ldc",
    "un_lldc",
    "un_sids",
    who_names,
    paste0("un_name_", languages),
    "who_region",
    un_regions,
    un_region_names,
    regions_others,
    wb_igs
  )
  if (!include_undesa) {
    col_order <- grep("^un_desa_", col_order, value = TRUE, invert = TRUE)
  }
  if (!include_gbd) {
    col_order <- grep("^gbd_", col_order, value = TRUE, invert = TRUE)
  }
  if (!include_oecd) {
    col_order <- grep("^oecd_", col_order, value = TRUE, invert = TRUE)
  }

  countries <- dat_who_ref_country %>%
    dplyr::left_join(dat_wb_ig, by = ifelse(wb_use_xmart, "m49", "iso3")) %>%
    dplyr::left_join(dat_wb_reg, by = ifelse(wb_use_xmart, "m49", "iso3")) %>%
    dplyr::left_join(dat_unsd_m49, by = c("m49", "iso3", "iso2")) %>%
    dplyr::left_join(dat_undesa_sdg, by = "iso3")

  if (include_gbd) {
    countries <- dplyr::left_join(countries, dat_gbd, by = "iso3")
  }
  if (include_oecd) {
    countries <- dplyr::left_join(countries, oecd, by = "iso3")
  }

  countries <- countries %>%
    dplyr::select(!!!rlang::syms(col_order))

  return(countries)
}

#' @title Make the `whoville::countries` dataset
#'
#' @param wb_use_xmart \[`logical(1)`\]\cr
#'   Whether to get the world bank income groups and regions from xMart or
#'   directly from the world bank website.
#' @param unsd_special_use_xmart \[`logical(1)`\]\cr
#'   Whether to get the special unsd classifications (ldc, lldc, sids) from xMart
#'   or directly from the unsd website.
#' @param sdg_use_xmart \[`logical(1)`\]\cr
#'   Whether to get the sdg regions from xMart or directly from the undesa wpp
#'   files.
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
                           sdg_use_xmart = TRUE) {

  check_bool(wb_use_xmart)
  check_bool(unsd_special_use_xmart)
  check_bool(sdg_use_xmart)

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
  dat_undesa_sdg <- get_undesa_sdg_direct() %>%
    format_undesa_sdg_direct()
  if (sdg_use_xmart) {
    dat_sdg <- format_sdg_xmart(dat_who_ref_groups_current)

    dat_undesa_sdg <- dat_undesa_sdg %>%
      dplyr::select(-dplyr::starts_with("sdg")) %>%
      dplyr::left_join(
        y = dat_who_ref_country %>%
          dplyr::select("iso3", "m49"),
        by = "iso3"
      ) %>%
      dplyr::full_join(dat_sdg, by = "m49") %>%
      dplyr::select(-"m49")
  }

  # TODO: automatically check if a more recent GBD year is available
  dat_gbd <- get_gbd_direct("2021") %>%
    format_gbd_direct()

  oecd <- get_oecd_countries_direct()

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

  countries <- dat_who_ref_country %>%
    dplyr::left_join(dat_wb_ig, by = ifelse(wb_use_xmart, "m49", "iso3")) %>%
    dplyr::left_join(dat_wb_reg, by = ifelse(wb_use_xmart, "m49", "iso3")) %>%
    dplyr::left_join(dat_unsd_m49, by = c("m49", "iso3", "iso2")) %>%
    dplyr::left_join(dat_undesa_sdg, by = "iso3") %>%
    dplyr::left_join(dat_gbd, by = "iso3") %>%
    dplyr::left_join(oecd, by = "iso3") %>%
    dplyr::select(
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
      dplyr::all_of(who_names),
      dplyr::all_of(paste0("un_name_", languages)),
      "who_region",
      dplyr::all_of(un_regions),
      dplyr::all_of(un_region_names),
      dplyr::all_of(regions_others),
      dplyr::starts_with("wb_ig_")
    )

  return(countries)
}

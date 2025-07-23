#' @title Make the `whoville::countries` dataset
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
#' - Alternative and former location names
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
make_countries <- function() {

  dat_who_ref_country <- get_who_public_xmart(
    url = "https://xmart-api-public.who.int/REFMART/REF_COUNTRY"
  ) %>%
    format_who_xmart_ref_country()

  dat_wb_ig <- get_wb_ig() %>%
    format_wb_ig()
  dat_wb_reg <- get_wb_reg() %>%
    format_wb_reg()

  dat_unsd_m49 <- get_un_m49() %>%
    format_un_m49()

  dat_undesa_sdg <- get_undesa_sdg() %>%
    format_undesa_sdg()

  # TODO: automatically check if a more recent GBD year is available
  dat_gbd <- get_gbd("2021") %>%
    format_gbd()

  oecd <- get_oecd_countries()

  # TODO: where did this file come from?
  alt_c <- readxl::read_excel("data-raw/alt_countries.xlsx") %>%
    dplyr::select(
      "iso3",
      alt_name_en = "altname",
      alt_name_2_en = "altname2",
      alt_name_3_en = "altname3",
      alt_name_4_en = "altname4",
      alt_name_5_en = "altname5",
      former_name_en = "formername",
      former_name_2_en = "formername2"
    )

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
    dplyr::left_join(dat_wb_ig, by = "iso3") %>%
    dplyr::left_join(dat_wb_reg, by = "iso3") %>%
    dplyr::left_join(dat_unsd_m49, by = c("m49", "iso3", "iso2")) %>%
    dplyr::left_join(dat_undesa_sdg, by = "iso3") %>%
    dplyr::left_join(dat_gbd, by = "iso3") %>%
    dplyr::left_join(oecd, by = "iso3") %>%
    dplyr::left_join(alt_c, by = "iso3") %>%
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
      dplyr::starts_with("alt_name"),
      dplyr::starts_with("former_name"),
      "who_region",
      dplyr::all_of(un_regions),
      dplyr::all_of(un_region_names),
      dplyr::all_of(regions_others),
      dplyr::starts_with("wb_ig_")
    )

  return(countries)
}

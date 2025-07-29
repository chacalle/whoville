#' Get regions from ISO3 country codes.
#'
#' `iso3_to_regions()` takes in a vector of ISO3 codes and returns a vector of
#' specified regions. Currently, this can be codes or English names for
#' UN regions/subregions/intermediate regions, UN DESA regions/subregions,
#' SDG regions/subregions, WHO regions, and World Bank geographical regions and
#' Income Groups.
#'
#' @param iso3 Character vector of ISO3 codes.
#' @param region Type of region to return. Regional classifications available from
#'     the WHO, UN, UNSD SDGs, UN DESA, the World Bank (geographic and income based),
#'     and IHME's GBD. See the [whoville::countries] documentation for more source
#'     details.
#' @param year For World Bank Income Group only, specify the year to return.
#' @param name For UN, UNSD SDG, UN DESA, WB and IHME GBD regions only. If `TRUE`,
#'     return official regional name instead of code.
#' @param language For `"un_region"`, `"un_subregion"`, and `"un_intermediate_region"`
#'     only if returning name. A character value specifying the language of the
#'     country names. Should be specified using the ISO2 language code.
#' @inheritParams resolve_countries_arg
#'
#' @return Character vector.
#'
#' @export
iso3_to_regions <- function(iso3,
                            region = c("who_region", "un_region", "un_subregion", "un_intermediate_region", "sdg_region", "sdg_subregion", "gbd_region", "gbd_subregion", "un_desa_region", "un_desa_subregion", "wb_region", "wb_ig"),
                            year = max(wb_ig_years()),
                            name = FALSE,
                            language = c("en", "es", "ru", "ar", "zh", "fr"),
                            countries_manual = NULL) {

  countries_use <- resolve_countries_arg(countries_manual)
  region <- rlang::arg_match(region)

  if (region == "wb_ig") {
    check_number_whole(year, min = min(wb_ig_years()), max = max(wb_ig_years()))
    mtch <- paste(region, year, sep = "_")
  } else if (region %in% c("un_region", "un_subregion", "un_intermediate_region", "sdg_region", "sdg_subregion", "gbd_region", "gbd_subregion", "un_desa_region", "un_desa_subregion", "wb_region") && name) {
    language <- rlang::arg_match(language)
    mtch <- paste(region, "name", language, sep = "_")
  } else {
    mtch <- region
  }

  check_cols_exists(countries_use, cols = c("iso3", mtch))
  regions <- countries_use[[mtch]]
  idx <- match(iso3, countries_use[["iso3"]])
  regions[idx]
}

#' Check if country codes are present in `countries` data frame.
#'
#' `valid_codes()` takes in a vector of country codes and returns a logical vector
#' on whether that country code is present in `countries[["iso3"]]`
#'
#' @param codes Character vector of country codes.
#' @param type A character value specifying the type of country code supplied.
#' All possible values available through `country_code_types()`.
#' @inheritParams resolve_countries_arg
#'
#' @return Logical vector.
#'
#' @export
valid_codes <- function(codes, type = "iso3", countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  rlang::arg_match(type, country_code_types())

  check_cols_exists(countries_use, cols = type)
  codes %in% countries_use[[type]]
}

#' Get country names from ISO3 country codes.
#'
#' `iso3_to_names()` takes in a vector of ISO3 codes and returns names as
#' specified by the user. Currently, this can be World Health Organization names,
#' both short and formal, in all 6 official languages and United Nations names in
#' all 6 official languages.
#'
#' @param iso3 Character vector of ISO3 codes.
#' @param org Official names of which organization to return. Can be
#' either "who" (default) or "un".
#' @param type "short" (default) or "formal" name to return. Only used when org is "who".
#' @param language A character value specifying the language of the country names.
#' Should be specified using the ISO2 language code. Defaults to "en", but matching
#' available for all 6 official WHO languages. Possible values are "en", "es",
#' "ar", "fr", "ru", and "zh".
#' @inheritParams resolve_countries_arg
#'
#' @return Character vector.
#'
#' @export
iso3_to_names <- function(iso3,
                          org = c("who", "un"),
                          type = c("short", "formal"),
                          language = c("en", "es", "ru", "ar", "zh", "fr"),
                          countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  org <- rlang::arg_match(org)
  language <- rlang::arg_match(language)
  if (org == "who") {
    type <- rlang::arg_match(type)
    org <- paste(org, type, sep = "_")
  }

  check_cols_exists(countries_use, cols = "iso3")
  rgx <- sprintf("^%s_name_%s$", org, language)
  names <- countries_use[[which(grepl(rgx, names(countries_use)))]]
  idx <- match(iso3, countries_use[["iso3"]])
  names[idx]
}

#' Get country codes from ISO3 country codes.
#'
#' `iso3_to_codes()` takes in a vector of ISO3 codes and returns a country code
#' vector specified by the user.
#'
#' @param iso3 Character vector of ISO3 codes.
#' @param type A character value specifying the type of country code to return.
#' All possible values available through `country_code_types()`.
#' @inheritParams resolve_countries_arg
#'
#' @return Character vector.
#'
#' @export
iso3_to_codes <- function(iso3, type, countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  rlang::arg_match(type, country_code_types())

  check_cols_exists(countries_use, cols = c("iso3", type))

  codes <- countries_use[[type]]
  idx <- match(iso3, countries_use[["iso3"]])
  codes[idx]
}

#' Get ISO3 codes from other country codes.
#'
#' `codes_to_iso3()` takes in a vector of country codes and returns ISO3 codes.
#'
#' @param codes Character vector of country codes.
#' @inheritParams iso3_to_codes
#'
#' @return Character vector.
#'
#' @export
codes_to_iso3 <- function(codes, type, countries_manual = NULL) {
  countries_use <- resolve_countries_arg(countries_manual)
  rlang::arg_match(type, country_code_types())

  check_cols_exists(countries_use, cols = c("iso3", type))

  iso3 <- countries_use[["iso3"]]
  idx <- match(codes, countries_use[[type]])
  iso3[idx]
}

# Description: Create the internal `source_versions` object documenting the
# version of each external source included in the `whoville::countries` object.
#
# Notes:
# - sourced by `data-raw/countries.R` when the new `countries` object is saved,
#   so the access dates match when the sources were downloaded
# - `version` is the release/revision included, or the access date for sources
#   without versions
# - `version_check` describes how a more recent version is detected, if at all
# - `make_countries_latest` is whether `make_countries()` automatically gets the
#   latest version, otherwise the version below is used until updated here
# - `gbd_year` and `wpp_year` must match the versions used in `make_countries()`
#
# Input:
# - `countries`: the new `countries` object created in `data-raw/countries.R`
#
# Output:
# - R/sysdata.rda (`source_versions`)

library(dplyr)

access_date <- paste("accessed", Sys.Date())
gbd_year <- "2023"
wpp_year <- "2024"

# the most recent World Bank fiscal year included
# - `wb_ig_{year}` columns are named by the year of the GNI data, two years before the fiscal year
wb_ig_year_latest <- grep("^wb_ig_", names(countries), value = TRUE) %>%
  stringr::str_remove("^wb_ig_") %>%
  as.numeric() %>%
  max()
wb_fiscal_year_latest <- as.character(stringr::str_glue(
  "FY{(wb_ig_year_latest + 2) %% 100}"
))

gbd_source <- as.character(stringr::str_glue(
  "IHME GBD location hierarchy ('GBD {gbd_year} Reporting' location set)"
))

# fmt: skip
source_versions <- tribble(
  ~source, ~columns, ~version, ~make_countries_latest, ~version_check,
  "WHO xMart REFMART 'REF_COUNTRY'", "location codes, WHO names, WHO membership and regions", access_date, TRUE, "none, always the current version",
  "World Bank income groups (WHO xMart REFMART 'REF_GROUPS_FULL')", "wb_ig_*", wb_fiscal_year_latest, TRUE, "none, all available fiscal years are included",
  "World Bank regions (WHO xMart REFMART 'REF_GROUPS_CURRENT')", "wb_region*", access_date, TRUE, "none, always the current version",
  "UNSD M49 overview webpage", "un_name_*, un_region*, un_subregion*, un_intermediate_region*", access_date, TRUE, "none, always the current version",
  "UNSD special groups (WHO xMart REFMART 'REF_GROUPS_CURRENT')", "un_ldc, un_lldc, un_sids", access_date, TRUE, "none, always the current version",
  "UN SDG regions (WHO xMart REFMART 'REF_GROUPS_CURRENT')", "sdg_region*, sdg_subregion*", access_date, TRUE, "none, always the current version",
  "UN DESA World Population Prospects 'Locations (XLSX)' file", "un_desa_*", wpp_year, FALSE, "error if the UN Data Portal API lists a more recent WPP revision",
  gbd_source, "gbd_*", gbd_year, FALSE, "warning if GHDx has a page for a more recent GBD version",
  "OECD SDMX API (CPI country weights dataflow)", "oecd_member", access_date, TRUE, "none, always the current version"
)

usethis::use_data(source_versions, internal = TRUE, overwrite = TRUE)

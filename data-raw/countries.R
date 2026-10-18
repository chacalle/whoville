# Description: Create the `whoville::countries` object and compare to the
# previous version. The final saving section must be manually run after
# reviewing the summarized changes.

library(daff)
library(openxlsx2)
library(tidyverse)

devtools::load_all()

# save current version of the countries object for comparison
countries_previous <- countries

# GBD location hierarchies can not be downloaded without logging in
# - download from the GHDx record page listed in `?get_gbd_direct`
# - the GBD version is set in `data-raw/source_versions.R`
gbd_path <- here::here("data-raw/gbd/IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX")

# make a new `countries` object using xmart when possible
countries_new_xmart <- make_countries(
  wb_use_xmart = TRUE,
  unsd_special_use_xmart = TRUE,
  sdg_use_xmart = TRUE,
  include_undesa = TRUE,
  include_gbd = TRUE,
  gbd_path = gbd_path,
  include_oecd = TRUE
)

# make a new `countries` object using direct sources when possible (WB website, unsd website etc.)
countries_new_direct <- make_countries(
  wb_use_xmart = FALSE,
  unsd_special_use_xmart = FALSE,
  sdg_use_xmart = FALSE,
  include_undesa = TRUE,
  include_gbd = TRUE,
  gbd_path = gbd_path,
  include_oecd = TRUE
)


# Confirm changes and differences between `countries` objects -------------

# make two comparisons assuming we will use the xmart data source if available
# - compare to the previous dataset
# - compare to a new version with all data collected directly from source websites

diff_against_previous <- compare_countries(
  new = countries_new_xmart,
  old = countries_previous
)
diff_against_direct <- compare_countries(
  new = countries_new_xmart,
  old = countries_new_direct
)

# Render the data diffs to html
if (FALSE) {
  daff::render_diff(
    diff = diff_against_previous$daff,
    title = "Comparison of previous and new `whoville::countries` object"
  )
  daff::render_diff(
    diff = diff_against_direct$daff,
    title = "Comparison of new `whoville::countries` objects derived from online sources and xmart"
  )
}

# save a manual xlsx version of the diffs with color highlighting
openxlsx2::wb_save(
  wb = diff_against_previous$wb_daff,
  file = stringr::str_glue("{here::here()}/data-raw/countries_diff_against_previous.xlsx")
)
openxlsx2::wb_save(
  wb = diff_against_direct$wb_daff,
  file = stringr::str_glue("{here::here()}/data-raw/countries_diff_against_direct.xlsx")
)


# Compare to key tagged versions ------------------------------------------

# permanent comparisons against the `countries` object in key tagged versions
# - add new tags here to keep comparing against them in future updates
# - v0.1.7: final version in the original 'gpw13/whoville' repository
# - v0.1.7.9000: first updated version (2025-07-29), used in analyses after the
#   IDN WHO region change
tags_compare <- c("v0.1.7", "v0.1.7.9000")

for (tag in tags_compare) {
  # read the `countries` object saved in the tagged version
  path_tag <- tempfile(fileext = ".rda")
  system2(
    command = "git",
    args = c("-C", here::here(), "show", stringr::str_glue("{tag}:data/countries.rda")),
    stdout = path_tag
  )
  env_tag <- new.env()
  load(path_tag, envir = env_tag)

  diff_against_tag <- compare_countries(
    new = countries_new_xmart,
    old = env_tag$countries
  )
  openxlsx2::wb_save(
    wb = diff_against_tag$wb_daff,
    file = stringr::str_glue("{here::here()}/data-raw/countries_diff_against_{tag}.xlsx")
  )
}


# Save the new `countries` object -----------------------------------------

# Manually run these lines when satisfied with the changes summarized above
# - the `source_log` attribute from `make_countries()` is saved with the object
# - also updates the internal `source_versions` object documenting each source version
if (FALSE) {
  countries <- countries_new_xmart
  usethis::use_data(countries, overwrite = TRUE)
  source(here::here("data-raw/source_versions.R"))
}

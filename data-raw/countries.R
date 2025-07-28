# Description: Create the `whoville::countries` object and compare to the
# previous version. The final saving section must be manually run after
# reviewing the summarized changes.

library(daff)
library(openxlsx2)
library(tidyverse)

devtools::load_all()

# save current version of the countries object for comparison
countries_previous <- countries

# make a new `countries` object using xmart when possible
countries_new_xmart <- make_countries(
  wb_use_xmart = TRUE,
  unsd_special_use_xmart = TRUE,
  sdg_use_xmart = TRUE,
  include_undesa = TRUE,
  include_gbd = TRUE,
  include_oecd = TRUE
)

# make a new `countries` object using direct sources when possible (WB website, unsd website etc.)
countries_new_direct <- make_countries(
  wb_use_xmart = FALSE,
  unsd_special_use_xmart = FALSE,
  sdg_use_xmart = FALSE,
  include_undesa = TRUE,
  include_gbd = TRUE,
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


# Save the new `countries` object -----------------------------------------

# Manually run the final two lines when satisfied with the changes summarized above
if (FALSE) {
  countries <- countries_new_xmart
  usethis::use_data(countries, overwrite = TRUE)
}

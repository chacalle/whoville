# Description: Create the `whoville::countries` object and compare to the
# previous version. The final saving section must be manually run after
# reviewing the summarized changes.

library(daff)
library(openxlsx2)
library(readxl)
library(tidyverse)

# save current version of the countries object before loading the
# potentially updated package contents
countries_current <- whoville::countries

devtools::load_all()


# Get WHO Country List ----------------------------------------------------

dat_who_ref_country <- get_who_public_xmart(
  url = "https://xmart-api-public.who.int/REFMART/REF_COUNTRY"
) %>%
  format_who_xmart_ref_country()


# Get World Bank Definitions ----------------------------------------------

dat_wb_ig <- get_wb_ig() %>%
  format_wb_ig()
dat_wb_reg <- get_wb_reg() %>%
  format_wb_reg()


# Get UNSD M49 Definitions ------------------------------------------------

dat_unsd_m49 <- get_un_m49() %>%
  format_un_m49()


# Get UN DESA & SDG Regions -----------------------------------------------

dat_undesa_sdg <- get_undesa_sdg() %>%
  format_undesa_sdg()


# Get IHME GBD Definitions ------------------------------------------------

# TODO: automatically check if a more recent GBD year is available
dat_gbd <- get_gbd("2021") %>%
  format_gbd()


# Get OECD Members --------------------------------------------------------

oecd <- get_oecd_countries()


# Get alternate and former country names ----------------------------------

# TODO: where did this file come from?
alt_c <- readxl::read_excel("data-raw/alt_countries.xlsx") %>%
  select(iso3,
    alt_name_en = altname,
    alt_name_2_en = altname2,
    alt_name_3_en = altname3,
    alt_name_4_en = altname4,
    alt_name_5_en = altname5,
    former_name_en = formername,
    former_name_2_en = formername2
  )


# Merge all together ------------------------------------------------------

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

countries_new <- dat_who_ref_country %>%
  left_join(dat_wb_ig, by = "iso3") %>%
  left_join(dat_wb_reg, by = "iso3") %>%
  left_join(dat_unsd_m49, by = c("m49", "iso3", "iso2")) %>%
  left_join(dat_undesa_sdg, by = "iso3") %>%
  left_join(dat_gbd, by = "iso3") %>%
  left_join(oecd, by = "iso3") %>%
  left_join(alt_c, by = "iso3") %>%
  select(
    iso3,
    iso2,
    iso_numeric,
    who_code,
    m49,
    gbd_code,
    sovereign_iso3,
    who_member,
    who_member_small,
    oecd_member,
    un_ldc,
    un_lldc,
    un_sids,
    all_of(who_names),
    all_of(paste0("un_name_", languages)),
    starts_with("alt_name"),
    starts_with("former_name"),
    who_region,
    all_of(un_regions),
    all_of(un_region_names),
    all_of(regions_others),
    starts_with("wb_ig_")
  )


# Compare with currently saved `countries` object -------------------------

cols_intersect <- intersect(names(countries_current), names(countries_new))
cols_dropped <- setdiff(names(countries_current), cols_intersect)
cols_added <- setdiff(names(countries_new), cols_intersect)

check_countries <- countries_current %>%
  arrange(iso3)
check_countries_new <- countries_new %>%
  arrange(iso3)

cols_identical <- map_lgl(names(check_countries), function(col) {
  identical(check_countries[[col]], check_countries_new[[col]])
})
cols_changed <- names(check_countries)[!cols_identical]
cols_identical <- names(check_countries)[cols_identical]

countries_diff <- daff::diff_data(
  data_ref = check_countries,
  data = check_countries_new,
  ids = c("iso3"),
  show_unchanged = FALSE
)


# Render the data diff to html
# daff::render_diff(
#   diff = countries_diff,
#   title = "Comparison of current and new `whoville::countries` object"
# )

dat_diff <- countries_diff$get_data()

header_row <- dat_diff[2, ] %>%
  as.character()
highlight_changed_cols <- which(header_row %in% setdiff(cols_changed, c(cols_added, cols_dropped)))
dat_diff[1, highlight_changed_cols] <- "->"

wb_diff <- openxlsx2::wb_workbook() %>%
  openxlsx2::wb_add_worksheet(sheet = "`countries` diff") %>%
  openxlsx2::wb_add_data(sheet = "`countries` diff", dat_diff) %>%
  # bold header row
  openxlsx2::wb_add_font(bold = TRUE, dims = openxlsx2::wb_dims(rows = 3, cols = 1:ncol(dat_diff))) %>%
  openxlsx2::wb_set_col_widths(cols = 1:ncol(dat_diff), widths = 15) %>%
  openxlsx2::wb_add_ignore_error(number_stored_as_text = TRUE, dims = openxlsx2::wb_dims(x = dat_diff)) %>%
  openxlsx2::wb_add_dxfs_style(name = "modifiedStyle", bg_fill = openxlsx2::wb_color(hex = "#6b64ff")) %>%
  openxlsx2::wb_add_conditional_formatting(
    "`countries` diff",
    dims = openxlsx2::wb_dims(x = dat_diff),
    rule = "->",
    type = "containsText",
    style = "modifiedStyle"
  )

highlight_rows_columns <- function(wb,
                                   x,
                                   dimension = c("rows", "columns"),
                                   change_type = c("added", "removed")) {

  search_string <- if_else(change_type == "added", "\\+\\+\\+", "\\-\\-\\-")
  hex_color <- if_else(change_type == "added", "#72ff6d", "#fd676c")

  check_vector <- if (dimension == "rows") {
    check_vector <- dat_diff[, 2]
  } else {
    check_vector <- dat_diff[1, ]
  }

  # identify the indices matching the search string
  i <- check_vector %>%
    as.vector() %>%
    grepl(pattern = search_string) %>%
    which()
  if (length(i) == 0) {
    return(wb)
  }

  # identify workbook dimensions to highlight
  if (dimension == "rows") {
    dims_highlight <- openxlsx2::wb_dims(rows = i, cols = 1:(ncol(x) + 1))
  } else {
    dims_highlight <- openxlsx2::wb_dims(rows = 1:(nrow(x) + 1), cols = i)
  }

  wb <- wb %>%
    # highlight rows/columns added/removed
    openxlsx2::wb_add_fill(
      dims = dims_highlight,
      color = openxlsx2::wb_color(hex = hex_color)
    )
  return(wb)
}

wb_diff <- highlight_rows_columns(wb_diff, dat_diff, "rows", "added") %>%
  highlight_rows_columns(dat_diff, "rows", "removed") %>%
  highlight_rows_columns(dat_diff, "columns", "added") %>%
  highlight_rows_columns(dat_diff, "columns", "removed")

wb_diff <- wb_diff %>%
  openxlsx2::wb_freeze_pane(first_active_row = 4, first_active_col = 4) %>%
  openxlsx2::wb_add_filter(rows = 3, cols = 1:ncol(dat_diff))

dir.create(tempdir())
openxlsx2::wb_save(wb_diff, stringr::str_glue("{here::here()}/data-raw/countries_diff.xlsx"))


# Save new `countries` object ---------------------------------------------

# Manually run the final two lines when satisfied with the changes summarized above
if (FALSE) {
  countries <- countries_new
  usethis::use_data(countries, overwrite = TRUE)
}

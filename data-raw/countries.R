# Description: Create the `whoville::countries` object and compare to the
# previous version. The final saving section must be manually run after
# reviewing the summarized changes.

library(daff)
library(openxlsx2)
library(tidyverse)

devtools::load_all()

# save current version of the countries object
countries_current <- countries

countries_new <- make_countries()

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

dir.create(tempdir())

# Render the data diff to html
daff::render_diff(
  diff = countries_diff,
  title = "Comparison of current and new `whoville::countries` object"
)

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

openxlsx2::wb_save(wb_diff, stringr::str_glue("{here::here()}/data-raw/countries_diff.xlsx"))


# Save new `countries` object ---------------------------------------------

# Manually run the final two lines when satisfied with the changes summarized above
if (FALSE) {
  countries <- countries_new
  usethis::use_data(countries, overwrite = TRUE)
}

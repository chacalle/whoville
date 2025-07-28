
#' @title Use the `daff` R package to compare versions of the countries object
#'
#' @param new \[`data.frame(1)`\]\cr
#'   The new data frame to check for new changes compared to the old data frame.
#' @param old \[`data.frame(1)`\]\cr
#'   The reference (old) data frame to compare changes against.
#' @param col_order \[`character()`\]\cr
#'   Column order for the returned workbook object.
#'
#' @returns a named \[`list(2)`\] with 'daff' storing the difference object and
#' 'wb_daff' storing the `openxlsx2::wbWorkbook` object.
#'
#' @examples
#' \dontrun{
#' countries_new_xmart <- make_countries(
#'   wb_use_xmart = TRUE,
#'   unsd_special_use_xmart = TRUE,
#'   sdg_use_xmart = TRUE
#' )
#' diff_against_previous <- compare_countries(
#'   new = countries_new_xmart,
#'   old = countries
#' )
#' daff::render_diff(
#'   diff = diff_against_previous$daff,
#'   title = "Comparison of previous and new `whoville::countries` object"
#' )
#' openxlsx2::wb_save(
#'   wb = diff_against_previous$wb_daff,
#'   file = stringr::str_glue("{here::here()}/data-raw/countries_diff_against_previous.xlsx")
#' )
#' }
#' @export
compare_countries <- function(new,
                              old,
                              col_order = c("iso3", "who_member", "who_short_name_en")) {

  rlang::check_installed(
    pkg = "daff",
    reason = "to calculate differences between two `data.frame`s"
  )
  rlang::check_installed(
    pkg = "openxlsx2",
    reason = "to save the data differences in an xlsx file"
  )

  check_data_frame(new)
  check_data_frame(old)

  cols_intersect <- intersect(names(old), names(new))
  cols_dropped <- setdiff(names(old), cols_intersect)
  cols_added <- setdiff(names(new), cols_intersect)

  stopifnot(all(col_order %in% cols_intersect))

  old <- old %>%
    dplyr::arrange(.data$iso3)
  new <- new %>%
    dplyr::arrange(.data$iso3)

  cols_identical <- sapply(names(old), function(col) {
    identical(old[[col]], new[[col]])
  })
  cols_changed <- names(old)[!cols_identical]
  cols_identical <- names(old)[cols_identical]

  countries_diff <- daff::diff_data(
    data_ref = old,
    data = new,
    ids = c("m49"),
    always_show_order = TRUE,
    show_unchanged = TRUE,
    show_unchanged_columns = TRUE,
    show_unchanged_meta = TRUE
  )

  # get a tabular representation of the diff
  # https://paulfitz.github.io/daff-doc/spec.html
  dat_diff <- countries_diff$get_data()

  # determine the actual row with the column names
  header_row_i <- which(dat_diff[, 2] == "@@")
  header_row <- dat_diff[header_row_i, ] %>%
    as.character()
  header_row[1] <- "order_column"
  header_row[2] <- "action_column"

  # change the column names from the column orders to the actual column names
  order_row <- names(dat_diff)
  names(dat_diff) <- header_row
  dat_diff <- dat_diff %>%
    dplyr::filter(.data$action_column != "@@")
  # put the column order as the new first row
  order_row <- as.list(order_row)
  names(order_row) <- header_row
  order_row <- as.data.frame(order_row)
  dat_diff <- rbind(order_row, dat_diff)

  # when no columns have been deleted/added the schema row is missing
  # manually add a blank row in these cases
  if (length(c(cols_dropped, cols_added)) == 0) {
    dat_schema <- dat_diff[1, ]
    dat_schema[1, 1:ncol(dat_schema)] <- rep("", ncol(dat_schema))
    dat_schema[1, "action_column"] <- "!"

    dat_diff <- rbind(
      dat_diff[1, ],
      dat_schema,
      dat_diff[2:nrow(dat_diff), ]
    )
  }

  # add '->' to the schema row to highlight changed columns
  highlight_changed_cols <- which(header_row %in% setdiff(cols_changed, c(cols_added, cols_dropped)))
  dat_diff[2, highlight_changed_cols] <- "->"

  col_order <- c("order_column", "action_column", col_order) %>%
    unique()
  dat_diff <- dat_diff %>%
    dplyr::relocate(!!!rlang::syms(col_order))

  wb_diff <- openxlsx2::wb_workbook() %>%
    openxlsx2::wb_add_worksheet(sheet = "`countries` diff") %>%
    openxlsx2::wb_add_data(sheet = "`countries` diff", dat_diff) %>%
    # bold header, order, and schema rows
    openxlsx2::wb_add_font(bold = TRUE, dims = openxlsx2::wb_dims(rows = 1:3, cols = 1:ncol(dat_diff))) %>%
    openxlsx2::wb_set_col_widths(cols = 1:ncol(dat_diff), widths = 15) %>%
    openxlsx2::wb_add_ignore_error(number_stored_as_text = TRUE, dims = openxlsx2::wb_dims(x = dat_diff)) %>%
    # highlight blue if the cell contains the text '->'
    openxlsx2::wb_add_dxfs_style(name = "modifiedStyle", bg_fill = openxlsx2::wb_color(hex = "#6b64ff")) %>%
    openxlsx2::wb_add_conditional_formatting(
      "`countries` diff",
      dims = openxlsx2::wb_dims(x = dat_diff),
      rule = "->",
      type = "containsText",
      style = "modifiedStyle"
    )

  wb_diff <- highlight_rows_columns(wb_diff, dat_diff, "rows", "added") %>%
    highlight_rows_columns(dat_diff, "rows", "removed") %>%
    highlight_rows_columns(dat_diff, "columns", "added") %>%
    highlight_rows_columns(dat_diff, "columns", "removed")

  wb_diff <- wb_diff %>%
    openxlsx2::wb_freeze_pane(first_active_row = 4, first_active_col = length(col_order) + 1) %>%
    openxlsx2::wb_add_filter(rows = 1, cols = 1:ncol(dat_diff))

  result <- list(
    daff = countries_diff,
    wb_daff = wb_diff
  )
  return(result)
}

#' @title Helper function to convert the difference object to an
#' openxlsx2::wbWorkbook object with cell highlighting and other light formatting.
#'
#' @param wb \[`wbWorkbook(1)`\]\cr
#'   `openxlsx2` workbook to format with highlighted cells, rows and columns.
#' @param dat \[`data.frame(1)`\]\cr
#'   differences data frame returned by the `get_data` method from the returned
#'   differences object.
#' @param dimension \[`character(1)`\]\cr
#'   whether to highlight 'rows' or 'columns'.
#' @param change_type \[`character(1)`\]\cr
#'   whether to highlight `dimensions` that were 'added' or 'removed'.

highlight_rows_columns <- function(wb,
                                   dat,
                                   dimension = c("rows", "columns"),
                                   change_type = c("added", "removed")) {

  check_data_frame(dat)
  rlang::arg_match(dimension)
  rlang::arg_match(change_type)

  search_string <- dplyr::if_else(change_type == "added", "\\+\\+\\+", "\\-\\-\\-")
  hex_color <- dplyr::if_else(change_type == "added", "#72ff6d", "#fd676c")

  # get the schema row/column
  check_vector <- if (dimension == "rows") {
    check_vector <- dat[, 2]
  } else {
    check_vector <- dat[2, ]
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
    dims_highlight <- openxlsx2::wb_dims(rows = i, cols = 1:(ncol(dat) + 1))
  } else {
    dims_highlight <- openxlsx2::wb_dims(rows = 1:(nrow(dat) + 1), cols = i)
  }

  wb <- wb %>%
    # highlight rows/columns added/removed
    openxlsx2::wb_add_fill(
      dims = dims_highlight,
      color = openxlsx2::wb_color(hex = hex_color)
    )
  return(wb)
}

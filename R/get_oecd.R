
#' @title Get Organisation for Economic Co-operation and Development (OECD)
#'   member countries
#'
#' @param url \[`character(1)`\]\cr
#'   URL to an OECD data-explorer dataset for OECD only countries (currently
#'   the OECD consumer price index country weights)
#'
#' @returns
#' \[`data.frame()`\] with a row for each 'iso3' in `whoville::countries` and
#' a logical column called 'oecd_member' indicating membership in OECD.
#'
#' @examples
#' \dontrun{
#' oecd <- get_oecd_countries_direct()
#' }
get_oecd_countries_direct <- function(url = "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_CPI_COU_WEIGHTS@DF_CPI_CTRY_WEIGHTS,1.0/.A..?dimensionAtObservation=AllDimensions&format=csvfilewithlabels") {

  # https://www.oecd.org/en/about/members-partners.html
  # CPI country weights - OECD composition
  # This dataset contains country weights used for calculation of Consumer
  # Prices aggregates for OECD total and G7 area. The weights are based on
  # relative share of individual final consumption expenditure of households
  # and non-profit institutions serving households expressed in Purchasing Power
  # Parities (PPPs).

  check_string(url, allow_empty = FALSE)
  oecd_iso3s <- utils::read.csv(url) %>%
    dplyr::pull("REF_AREA") %>%
    unique()

  oecd <- whoville::countries %>%
    dplyr::select("iso3") %>%
    dplyr:: mutate(oecd_member = .data$iso3 %in% oecd_iso3s)

  return(oecd)
}

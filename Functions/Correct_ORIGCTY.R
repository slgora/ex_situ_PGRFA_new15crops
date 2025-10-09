#' Convert country names to ISO 3-letter codes using a lookup table.
#'
#' This function converts a vector of country names (e.g., from ORIGCTY) to ISO 3-letter country codes
#' using a lookup table such as geo_names (with columns ctyFullName and country3).
#'
#' @param origcty_vec A character vector of country names to be converted (e.g., your ORIGCTY column).
#' @param lookup_df   A data frame for country name to code conversion (default: geo_names).
#' @param name_col    The column in lookup_df that contains country names (default: "ctyFullName").
#' @param code_col    The column in lookup_df that contains the 3-letter ISO codes (default: "country3").
#'
#' @return A character vector of ISO 3-letter country codes (with NAs for unmatched names).
#' @examples
#' # Example: geo_names loaded and a data frame df with ORIGCTY to convert:
#' df$ORIGCTY <- correct_origcty(df$ORIGCTY)
correct_origcty <- function(origcty_vec,
                            lookup_df = geo_names,
                            name_col = "ctyFullName",
                            code_col = "country3") {
  # Ensure the lookup columns exist
  if (!(name_col %in% names(lookup_df)) || !(code_col %in% names(lookup_df))) {
    stop("Lookup data frame must have columns: ", name_col, " and ", code_col)
  }
  # Create a named vector for fast lookup
  country_to_iso <- setNames(lookup_df[[code_col]], lookup_df[[name_col]])
  # Convert
  iso_codes <- country_to_iso[origcty_vec]
  return(iso_codes)
}


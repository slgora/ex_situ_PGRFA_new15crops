#' Assign Diversity Regions Based on Crop Strategy
#'
#' This function assigns primary and secondary regions of diversity to each accession in a dataframe,
#' using the Crop_strategy field to match with regions specified in a reference crops table. It also
#' identifies whether the accession's origin country corresponds to its primary or secondary region
#' of diversity, and applies filtering for breeding/other material based on SAMPSTAT.
#'
#' @param df A dataframe of accessions. Must contain columns: Crop_strategy, ORIGCTY (country code),
#'           GENUS, SAMPSTAT (sample status).
#' @param crops A dataframe with crop strategy and regions of diversity. Must contain columns:
#'              CropStrategy, PrimaryRegionsofDiversity, SecondaryRegionsofDiversity,
#'              Genera_primary, Genera_synonyms.
#' @param countries_in_regions A dataframe mapping country codes to regions. Must contain columns:
#'              Country_code, PlantsThatFeedTheWorld_Region_new.
#' @return The input dataframe (`df`) with the following added/updated columns:
#'         - Primary_diversity_region: Assigned primary region of diversity (character)
#'         - secondary_diversity_region: Assigned secondary region of diversity (character)
#'         - ORIGCTY_region: Region assigned based on ORIGCTY country code
#'         - fromPrimary_diversity_region: TRUE if ORIGCTY_region matches Primary_diversity_region, else FALSE/NA
#'         - fromSecondary_diversity_region: TRUE if ORIGCTY_region matches secondary_diversity_region, else FALSE/NA
#'         - Diversity region flags set to NA if SAMPSTAT > 399 and not 999
#' @details
#'   - The function uses the Crop_strategy field in `df` to assign regions by matching to the
#'     CropStrategy field in `crops`.
#'   - If SAMPSTAT > 399 and not 999, diversity region flags are set to NA.
#'   - Relies on dplyr and tidyverse for dataframe manipulation.
#' @examples
#'   result <- assign_diversity_regions(df, crops, countries_in_regions)
#' @import dplyr
#' @export
assign_diversity_regions = function(df, crops, countries_in_regions) {

  # Combine 'Genera_primary' and 'Genera_synonyms' columns while avoiding NA values
  crops$genera <- trimws(ifelse(is.na(crops$Genera_primary), "", crops$Genera_primary)) %>%
    paste(trimws(ifelse(is.na(crops$Genera_synonyms), "", crops$Genera_synonyms)), sep = ",")

  # Create named vectors with CropStrategy -> regions of diversity
  primary_regions <- setNames(crops$PrimaryRegionsofDiversity, crops$CropStrategy)
  secondary_regions <- setNames(crops$SecondaryRegionsofDiversity, crops$CropStrategy)

  # Assign values to column of primary and secondary diversity regions based on Crop_strategy in df
  df <- df %>%
    mutate(
      Primary_diversity_region = primary_regions[Crop_strategy],
      secondary_diversity_region = secondary_regions[Crop_strategy]
    )

  # Create dictionary Country code -> region (following formatting in regions of diversity)
  country_regions_dictionary <- setNames(countries_in_regions$PlantsThatFeedTheWorld_Region_new, countries_in_regions$Country_code )

  # Use ORIGCTY to determine if accession is from primary or secondary region of diversity
  df <- df %>% mutate(ORIGCTY_region = ifelse(!is.na(Primary_diversity_region), country_regions_dictionary[ORIGCTY], NA))

  # Create True/False value for Primary_diversity_region, works by matching strings
  df <- df %>%
    rowwise() %>%
    mutate(fromPrimary_diversity_region = ifelse(
      (!is.na(ORIGCTY) & !is.na(Primary_diversity_region)),
      grepl(ORIGCTY_region, Primary_diversity_region),
      NA
    )) %>%
    ungroup()

  # Create True/False value for secondary_diversity_region, works by matching strings
  df <- df %>%
    rowwise() %>%
    mutate(fromSecondary_diversity_region = ifelse(
      (!is.na(ORIGCTY) & !is.na(secondary_diversity_region)),
      grepl(ORIGCTY_region, secondary_diversity_region),
      NA
    )) %>%
    ungroup()

  # Exclude breeding/other material from diversity region flags unless SAMPSTAT = 999
  df <- df %>%
    mutate(across(
      c(fromPrimary_diversity_region, fromSecondary_diversity_region),
      ~ if_else(SAMPSTAT > 399 & SAMPSTAT != 999, NA_real_, .)
    ))
  return(df)
}

#' Assign Crop Strategy Categorical Variable
#'
#' Assigns a categorical variable `Crop_strategy` to each row of a data frame based on plant taxonomic information and custom mappings.
#'
#' Assignment rules:
#'   1. If the binomial (first two words, case-insensitive, trimmed) is "Solanum torvum", assign "Turkey berry".
#'   2. If the binomial (first two words, as above) matches any in `tomato_species`, assign "Tomato".
#'   3. If the genus (first word, case-insensitive, trimmed) is "Lycopersicon", assign "Tomato".
#'   4. Otherwise, if the genus matches an entry in the `crops` mapping and is NOT "Solanum", assign the corresponding `CropStrategy`.
#'   5. If none of the above, assign NA.
#'
#' @param df Data frame. Must contain a column with species/binomial names.
#' @param crops Data frame. Must contain columns Genera_primary, Genera_synonyms, and CropStrategy.
#' @param col_name Character. Name of the column in `df` containing the species/binomial names.
#' @param tomato_species Character vector. List of binomials that should be classified as "Tomato".
#'
#' @return `df` with an added/modified column `Crop_strategy`.
#' @examples
#' # df <- data.frame(Species = c("Solanum lycopersicum", "Lycopersicon esculentum", "Solanum torvum"))
#' # crops <- data.frame(Genera_primary = "Capsicum", Genera_synonyms = NA, CropStrategy = "Pepper")
#' # assign_crop_strategy(df, crops, "Species", tomato_species = c("Solanum lycopersicum"))
assign_crop_strategy <- function(df, crops, col_name, tomato_species) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  # Combine primary and synonym genera, separate to rows, and clean
  crops <- crops %>%
    mutate(
      genera_combined = paste(
        trimws(coalesce(Genera_primary, "")),
        trimws(coalesce(Genera_synonyms, "")),
        sep = ","
      )
    ) %>%
    mutate(genera_combined = trimws(genera_combined)) %>%
    separate_rows(genera_combined, sep = ",") %>%
    mutate(genera_combined = tolower(trimws(genera_combined))) %>%
    filter(genera_combined != "") %>%
    distinct(genera_combined, .keep_all = TRUE)
  
  crop_strategy_dict <- setNames(as.character(crops$CropStrategy), crops$genera_combined)
  
  # Helper: clean binomial (trim, remove trailing punctuation, lower)
  clean_binomial <- function(x) {
    x <- trimws(x)
    x <- sub("[[:punct:]]+$", "", x)
    tolower(x)
  }
  
  # Split to get binomial (first two words) and genus (first word)
  name_parts <- strsplit(trimws(df[[col_name]]), "\\s+")
  binomial <- sapply(name_parts, function(x) paste(x[1:2], collapse = " "))
  genus <- sapply(name_parts, function(x) x[1])
  
  binomial_clean <- clean_binomial(binomial)
  tomato_species_clean <- clean_binomial(tomato_species)
  genus_clean <- tolower(trimws(genus))
  
  # Assign strategy with explicit Lycopersicon handling
  df$Crop_strategy <- dplyr::case_when(
    binomial_clean == clean_binomial("Solanum torvum") ~ "Turkey berry",
    binomial_clean %in% tomato_species_clean ~ "Tomato",
    genus_clean == "lycopersicon" ~ "Tomato",
    genus_clean %in% names(crop_strategy_dict) & genus_clean != "solanum" ~ crop_strategy_dict[genus_clean],
    TRUE ~ NA_character_
  )
  
  return(df)
}

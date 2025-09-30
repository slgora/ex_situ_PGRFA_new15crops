#' Assign Crop Strategy Categorical Variable
#'
#' - Assigns 'Turkey berry' only if the binomial is 'Solanum torvum'.
#' - Assigns 'Tomato' if the binomial (first two words, exact match after trimming/case) is in tomato_species.
#' - Otherwise, assigns strategy using the genus (first word) and crops mapping, EXCEPT for Solanum, which gets NA.
# 
# # Read the Excel files for the croplist and the sheet that has tomato species
# crops <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/GCCS_Selected_crops/croplist_new15crops.xlsx")
# tomato_species <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/GCCS_Selected_crops/croplist_new15crops.xlsx", sheet = "tomato genepool")$Species
# ---------------------------------------------------------------------------------------------------------------
assign_crop_strategy <- function(df, crops, col_name, tomato_species) {
  library(dplyr)
  library(tidyr)
  library(stringr)

  crops$genera <- trimws(ifelse(is.na(crops$Genera_primary), "", crops$Genera_primary)) %>%
    paste(trimws(ifelse(is.na(crops$Genera_synonyms), "", crops$Genera_synonyms)), sep = ",")

  crops <- crops %>%
    mutate(genera_list = strsplit(trimws(genera), ",")) %>%
    unnest(genera_list) %>%
    mutate(genera_list = trimws(genera_list)) %>%
    filter(genera_list != "") %>%
    distinct(genera_list, .keep_all = TRUE)

  crop_strategy_dict <- setNames(as.character(crops$CropStrategy), crops$genera_list)

  clean_binomial <- function(x) {
    x <- trimws(x)
    x <- sub("[[:punct:]]+$", "", x)
    tolower(x)
  }

  # Get binomial (first two words), genus (first word)
  binomial <- sapply(strsplit(trimws(df[[col_name]]), "\\s+"), function(x) paste(x[1:2], collapse = " "))
  genus <- sapply(strsplit(trimws(df[[col_name]]), "\\s+"), `[`, 1)

  binomial_clean <- clean_binomial(binomial)
  tomato_species_clean <- clean_binomial(tomato_species)
  genus_clean <- tolower(trimws(genus))
  crop_strategy_names_clean <- tolower(trimws(names(crop_strategy_dict)))

  # Assign Crop_strategy
  df$Crop_strategy <- dplyr::case_when(
    binomial_clean == clean_binomial("Solanum torvum") ~ "Turkey berry",
    binomial_clean %in% tomato_species_clean ~ "Tomato",
    # Do NOT assign "Tomato" by genus match for Solanum or Lycopersicon
    genus_clean %in% crop_strategy_names_clean & !(genus_clean %in% c("solanum", "lycopersicon")) ~ crop_strategy_dict[names(crop_strategy_dict)[match(genus_clean, crop_strategy_names_clean)]],
    TRUE ~ NA_character_
  )

  return(df)
}

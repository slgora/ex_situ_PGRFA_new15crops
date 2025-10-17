######## use conversion table with validated taxa for taxa standardisation,
## do the processing that comes after standardization,as these depend on the correct genus and species #########

###### 1. Load datasets with taxa to be standardized using conversion table  #######################
crops <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/GCCS_Selected_crops/croplist_new15crops.xlsx")
tomato_species <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/GCCS_Selected_crops/croplist_new15crops.xlsx", sheet = "tomato genepool")$Species
combined_new15_df <- read_csv("../../GCCSmetricsII/Data_processing/1_merge_data/2025_10_01/gen_wiews_new15_df.csv")
SGSV_allcrops <- read_csv("../../GCCSmetricsII/Data_processing/1_merge_data/2025_10_01/SGSV_new15crops_processed.csv")
GLIS_processed <- read_csv("../../GCCSmetricsII/Data_processing/1_merge_data/2025_10_17/GLIS_new15_processed.csv")
BGCI_processed <- read_csv("../../GCCSmetricsII/Data_processing/1_merge_data/2025_10_01/BGCI_new15crops_processed.csv")


# create a copy of fullTaxa without some characters that gave problems when using the conversion table
combined_new15_df$fullTaxa2 <- trimws(
  gsub("\\s+", " ",
       gsub("\\?", "",
            gsub("\\+", " ",
                 na.omit(combined_new15_df$fullTaxa)
            )
       )
  )
)

# For GLIS, combine GENUS + SPECIES + SUBTAXA to make fullTaxa field
GLIS_processed <- GLIS_processed %>%
  tidyr::unite("fullTaxa", GENUS, SPECIES, SUBTAXA, sep = " ", na.rm = TRUE, remove = FALSE)


#### add standardized_taxa column here when the manual vetting is complete and standardization_table becomes available, code to be tested#######
#load standardization table 
# testing 
table_standardization <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/Taxa_standardization/standardization_table_new15_2025_10_03.xlsx")

# Replace tabs with space in input_name
table_standardization$input_name <- gsub("\t", " ", table_standardization$input_name)

#Replace multiple spaces with one space
table_standardization$input_name <- gsub("\\s+", " ", table_standardization$input_name)

#Add a space after period, "." between words in SG_suggestion (use PG_recommendation in final run)
table_standardization$SG_suggestion <- gsub("(?<=[a-zA-Z])\\.(?=[a-zA-Z])", ". ", table_standardization$SG_suggestion, perl = TRUE)


# Create named vector: input_name -> SG_suggestion (use PG_recommendation in final run)
standardization_table <- setNames(table_standardization$SG_suggestion, table_standardization$input_name)

combined_new15_df$Standardized_taxa = NA
combined_new15_df <- combined_new15_df %>%
  mutate(Standardized_taxa = ifelse(is.na(Standardized_taxa), standardization_table[fullTaxa], Standardized_taxa))

BGCI_processed$Standardized_taxa = NA
BGCI_processed <- BGCI_processed %>%
  mutate(Standardized_taxa = ifelse(is.na(Standardized_taxa), standardization_table[fullTaxa], Standardized_taxa))

# NOTE in SGSV dataset and GLIS dataset taxa were not yet standardized

###### 2. Assign crop strategy categorical variable #####################
# use Assign_crop_strategy.R function in Functions folder
source("Functions/Assign_crop_strategy.R")

combined_new15_df <- assign_crop_strategy(
  combined_new15_df,
  crops,
  "Standardized_taxa",
  tomato_species) %>%
  filter(!is.na(Crop_strategy))  # remove records where Crop_strategy is NA

SGSV_allcrops <- assign_crop_strategy(
  SGSV_allcrops,
  crops,
  "fullTaxa",
  tomato_species) %>%
  filter(!is.na(Crop_strategy))

GLIS_processed <- assign_crop_strategy(
  GLIS_processed,
  crops,
  "fullTaxa",
  tomato_species) %>%
  filter(!is.na(Crop_strategy))

BGCI_processed <- assign_crop_strategy(
  BGCI_processed,
  crops,
  "Standardized_taxa",
  tomato_species) %>%
  filter(!is.na(Crop_strategy))


###### 3. Assign diversity region               ######################
# It requires crops dataframe (croplist_new15crops.xlsx) and countries in regions (countries_in_regions.xlsx)
source("Functions/Assign_diversity_regions.R")   #needs update with new 15 crops
countries_in_regions <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/Geographical/countries_in_regions.xlsx")
combined_new15_df = assign_diversity_regions(combined_new15_df, crops = crops, countries_in_regions = countries_in_regions)

###### 4. Assign Annex 1 status                 ######################
# assign annex I status for new 15 crops according to the new 15 croplist
combined_new15_df <- combined_new15_df %>%
  left_join(crops %>% select(CropStrategy, Annex1), by = c("Crop_strategy" = "CropStrategy")) %>%
  mutate(Annex1 = ifelse(Annex1 == "Y", TRUE,
                         ifelse(Annex1 == "N", FALSE, Annex1)))

##### save resulting datasets in folder: ../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/
write.csv(combined_new15_df, '../../GCCSmetricsII/Data_processing/3_post_taxa_standardization_processing/2025_10_07/combined_new15_df.csv', row.names = FALSE)
write.csv(SGSV_allcrops, '../../GCCSmetricsII/Data_processing/3_post_taxa_standardization_processing/2025_10_07/SGSV_new15_processed.csv', row.names = FALSE)
write.csv(GLIS_processed, '../../GCCSmetricsII/Data_processing/3_post_taxa_standardization_processing/2025_10_17/GLIS_new15_processed.csv', row.names = FALSE)
write.csv(BGCI_processed, '../../GCCSmetricsII/Data_processing/3_post_taxa_standardization_processing/2025_10_07/BGCI_new15_processed.csv', row.names = FALSE)

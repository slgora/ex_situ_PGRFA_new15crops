### Standardize Taxa with Global Name Verifier Tool 
library(readxl)
library(readr)
library(tidyr)
library(httr)
library(jsonlite)

# read dataset
df = read.csv("../../GCCSmetricsII/Data_processing/1_merge_data/2025_09_30/gen_wiews_new15_df.csv", header = TRUE )

# combine GENUS + SPECIES + SPAUTHOR to account for the taxa author in the input name
df <- df %>% unite("fullTaxa2", GENUS, SPECIES, SPAUTHOR, sep = " ", na.rm = TRUE)

####################
# load functions
source('Functions/Query_taxa_resolver.R')   # import function query_taxa_resolver
source('Functions/Process_taxa_resolver_results.R') # import function extract_best_result

# taxa list to be standardised
taxa_list <- unique(trimws(na.omit(df$fullTaxa2)))

# loop trough taxa list and query the API
result_queries_WFO <- list()
result_queries_GRIN <- list()
counter <- 0
for (i in taxa_list){
  print(paste(round(counter / length(taxa_list) * 100, 2), "%", i))
  best_result_WFO <- query_taxa_resolver(i, c('196'))
  best_result_GRIN <- query_taxa_resolver(i, c('6'))
  result_queries_WFO <- append(result_queries_WFO, list(best_result_WFO))
  result_queries_GRIN <- append(result_queries_GRIN, list(best_result_GRIN))
  counter <- counter + 1
}

# extract best result from the result of the queries
res_WFO <- extract_best_result(result_queries_WFO)
res_GRIN <- extract_best_result(result_queries_GRIN)
# create a taxa dictionary to be used to map the standardised taxa to the taxa in the dataset
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
taxa_standardized_df_GRIN <- as.data.frame(do.call(rbind, res_GRIN))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
colnames(taxa_standardized_df_GRIN) <- c('input_name', 'matched_name_GRIN', 'match_type_GRIN', 'status_GRIN', 'output_name_GRIN')

taxa_standardized_df_WFO <- cbind(taxa_standardized_df_WFO, data_source = "WFO")
taxa_standardized_df_GRIN <- cbind(taxa_standardized_df_GRIN, data_source = "GRIN")

# Join results from GRIN with results from WFO
taxa_standardized_df <- taxa_standardized_df_GRIN %>%
  full_join(taxa_standardized_df_WFO, by = c("input_name" = "input_name"))

# add outputs_comparison field for review
taxa_standardized_df <- taxa_standardized_df %>%
  mutate(
    output_name_GRIN_chr = sapply(output_name_GRIN, as.character),
    output_name_WFO_chr  = sapply(output_name_WFO, as.character),
    outputs_comparison = ifelse(
      is.na(output_name_GRIN_chr) | is.na(output_name_WFO_chr) |
        output_name_GRIN_chr != output_name_WFO_chr,
      "outputs_differ",
      "outputs_match"))

# save table with results from both WFO and GRIN
df_save_results <- apply(taxa_standardized_df,2,as.character)
write.csv(df_save_results, '../../GCCSmetricsII/Data_processing/2_standardize_taxa/2025_09_29/gen_wiews_new15crops_standardized_taxa2025_09_30.csv', row.names = FALSE)


######### OPTIONAL: ASSIGN CROP STRATEGY to taxa names standardized to WFO ############
source("Functions/Assign_crop_strategy.R") # import fucntion, assign_crop_strategy

# Read in the crop list for new 15 crops Excel file
crops <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/GCCS_Selected_crops/croplist_new15crops.xlsx")
# Read in standardized taxa table
gen_wiews_new15_standardized_taxa <- read_csv("../../GCCSmetricsII/Data_processing/2_standardize_taxa/2025_09_30/gen_wiews_new15crops_standardized_taxa2025_09_30.csv")
# read in tomato species sheet from the croplist
tomato_species <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/GCCS_Selected_crops/croplist_new15crops.xlsx", sheet = "tomato genepool")$Species

# Keep only genus and species from standardized WFO output name
gen_wiews_new15_standardized_taxa$output_name_WFO_base <-
  sapply(strsplit(gen_wiews_new15_standardized_taxa$output_name_WFO, " "), function(x) paste(x[1:2], collapse = " "))

# Assign crop strategies using the 'output_name_WFO' column
gen_wiews_new15_standardized_taxa2 <- assign_crop_strategy(
  gen_wiews_new15_standardized_taxa,
  crops,
  "output_name_WFO",
  tomato_species)
#save
write.csv(gen_wiews_new15_standardized_taxa2, '../../GCCSmetricsII/Data_processing/2_standardize_taxa/2025_09_30/gen_wiews_new15crops_standardized_taxa2025_09_30_cropstrategy.csv', row.names = FALSE)

############ END SCRIPT ##############

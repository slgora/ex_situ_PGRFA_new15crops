#' Calculate Treaty germplasm distribution metrics for selected crops
#'
#' This function processes ITPGRFA transfer records from 2015–2021 and computes:
#' - Average number of distributed samples per year (mean of yearly totals)
#' - Average number of recipient countries per year
#' - Gini index of sample distribution across recipient regions
#'
#' The function reads and processes all necessary files internally.
#'
#' @param croplist_file Path to Excel file containing selected crops and strategy metadata
#' @param out_path Path where the output Excel file should be saved
#' @param transfers_2012_2019_file Path to ITPGRFA transfer records from 2012–2019 (Excel)
#' @param transfers_2019_2021_file Path to ITPGRFA transfer records from 2019–2021 (Excel)
#' @param countries_regions_file Path to Excel file mapping ISO3 country codes to recipient regions
#' @param unique_crop_dirty_file Path to Excel file mapping dirty crop names to crop strategies (for 2019–2021)
#'
#' @return A data frame with Treaty germplasm distribution metrics per crop strategy, also written to Excel
#'
#' @examples
#' transfers_metrics(
#'   croplist_file = "input/crop_list.xlsx",
#'   out_path = "output/table4_metrics.xlsx",
#'   transfers_2012_2019_file = "input/ITPGRFA_MLSDataStore2022_7_1.xlsx",
#'   transfers_2019_2021_file = "input/Updated_transfers_retrieved2025_09_18.xlsx",
#'   countries_regions_file = "input/countries_in_regions.xlsx",
#'   unique_crop_dirty_file = "input/Transfers_2019_2021_unique_crop_dirty.xlsx"
#' )
transfers_metrics <- function(
    croplist_file,
    out_path,
    transfers_2012_2019_file,
    transfers_2019_2021_file,
    countries_regions_file,
    unique_crop_dirty_file
) {
  # Load required libraries
  library(dplyr)
  library(readxl)
  library(stringr)
  library(tidyr)
  library(writexl)
  library(ineq)
  
  # Read in data files
  transfers_2012_2019 <- read_excel(transfers_2012_2019_file)
  transfers_2019_2021 <- read_excel(transfers_2019_2021_file, skip = 2)
  countries_regions <- read_excel(countries_regions_file)
  unique_crop_dirty <- read_excel(unique_crop_dirty_file)
  
  # Clean 2015–2018 data
  transfers_2015_2018 <- transfers_2012_2019 %>%
    filter(Year >= 2015 & Year <= 2018 & Dataset != "CGIAR only") %>%
    select(`ISO3...2`, `Provider country`, Crop_cleaned, taxa, Year, Samples, `ISO3...15`, `Recipient country`) %>%
    rename(
      provider_ISO3 = `ISO3...2`,
      provider_country_name = `Provider country`,
      PlantsthatFeedtheWorld_name = Crop_cleaned,
      Taxonomic_name = taxa,
      year = Year,
      number_of_samples = Samples,
      recipient_ISO3 = `ISO3...15`,
      recipient_country_name = `Recipient country`
    )
  
  # Load and filter croplist
  croplist <- read_excel(croplist_file) %>%
    select(PlantsthatFeedtheWorld_name, CropStrategy, Genera_primary, Taxa_main) %>%
    filter(!is.na(PlantsthatFeedtheWorld_name), PlantsthatFeedtheWorld_name != "NA") %>%
    distinct(PlantsthatFeedtheWorld_name, .keep_all = TRUE)
  
  # Join croplist to transfers
  transfers_2015_2018 <- left_join(croplist, transfers_2015_2018, by = "PlantsthatFeedtheWorld_name") %>%
    select(CropStrategy, year, provider_ISO3, provider_country_name, recipient_ISO3, recipient_country_name, number_of_samples) %>%
    filter(!is.na(number_of_samples))
  
  # Rename for consistency
  transfers_2015_2018 <- transfers_2015_2018 %>%
    rename(Crop_strategy = CropStrategy)
  
  # Clean 2019–2021 transfers
  transfers_2019_2021 <- transfers_2019_2021  %>%
    rename(provider_ISO3         = `ISO3...1`,
           provider_country_name = `Provider country`,
           crop_dirty            = Crop,
           year                  = Year,
           number_of_samples     = `# Samples`,
           recipient_ISO3        = `ISO3...6`,
           recipient_country_name= `Recipient country`) %>%
    left_join(unique_crop_dirty %>% select(crop_dirty, crop_strategy), by = "crop_dirty") %>%
    filter(!is.na(crop_strategy)) %>%
    select(crop_strategy, year, provider_ISO3, provider_country_name, recipient_ISO3, recipient_country_name, number_of_samples) %>%
    rename(Crop_strategy = crop_strategy)
  
  # Combine all transfers
  transfers_all <- bind_rows(transfers_2019_2021, transfers_2015_2018)
  
  # Ensure number_of_samples is numeric
  transfers_all <- transfers_all %>%
    mutate(number_of_samples = as.numeric(number_of_samples))
  
  # Metric 1: Average (mean of yearly sums) samples per year
  avg_samples <- transfers_all %>%
    group_by(Crop_strategy, year) %>%
    summarise(total_samples = sum(number_of_samples, na.rm = TRUE), .groups = "drop") %>%
    group_by(Crop_strategy) %>%
    summarise(avg_number_of_samples_per_year = mean(total_samples, na.rm = TRUE), .groups = "drop")
  
  # Metric 2: Average recipient country count
  avg_recipients <- transfers_all %>%
    group_by(Crop_strategy, year) %>%
    summarise(recipient_count = n_distinct(recipient_ISO3), .groups = "drop") %>%
    group_by(Crop_strategy) %>%
    summarise(avg_number_of_recipient_countries = mean(recipient_count, na.rm = TRUE), .groups = "drop")
  
  # Metric 3: Gini index for regional distribution
  transfers_with_regions <- left_join(transfers_all, countries_regions, by = c("recipient_ISO3" = "Country_code")) %>%
    separate_rows(PlantsThatFeedTheWorld_Region_new, sep = ",") %>%
    mutate(regions = str_trim(PlantsThatFeedTheWorld_Region_new))
  
  gini_results <- transfers_with_regions %>%
    group_by(Crop_strategy, regions) %>%
    summarise(total_samples = sum(number_of_samples, na.rm = TRUE), .groups = "drop") %>%
    group_by(Crop_strategy) %>%
    summarise(gini_index = Gini(total_samples), .groups = "drop")
  
  # Combine all metrics
  treaty_metrics <- avg_samples %>%
    left_join(avg_recipients, by = "Crop_strategy") %>%
    left_join(gini_results, by = "Crop_strategy") %>%
    rename(
      "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty" = avg_number_of_samples_per_year,
      "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-count_of_countries_recipients_distributions_treaty" = avg_number_of_recipient_countries,
      "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-gini_recipients_distributions_treaty" = gini_index
    )
  
  write_xlsx(treaty_metrics, out_path)
  return(treaty_metrics)
}

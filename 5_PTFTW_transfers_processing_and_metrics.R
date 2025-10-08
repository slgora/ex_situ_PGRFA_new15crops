# -------------------------------------------------------------------------------------------
# PTFTW and Treaty Transfer Indicators
#
# This script processes and summarizes two key metric blocks:
# 1. Plants That Feed the World (PTFTW) indicator metrics — combining digital, trade, research,
#    and use indicators for selected crops.
# 2. Germplasm transfer metrics under the ITPGRFA Treaty — calculating distribution statistics
#    and equity indicators (sample averages, country counts, and Gini inequality).
# Outputs: Two Excel files with crop strategy-level metrics, ready for reporting.
####  working directory: working directory should be Code/R_code , following shared folder structure ####
# -------------------------------------------------------------------------------------------

# load libraries
install.packages("writexl")
library(writexl)


### 19. PTFTW metrics (56 metrics)
# source function to process PTFTW dataset and metrics
# note: will need to update local paths within function to work properly
source("Functions/Load_PTFTW_dataset.R")

## Data read in: Plants That Feed the World dataset and croplist
indicator_file <- read_csv("../../GCCSmetricsII/Data/Plants_that_feed_the_world/Indicators/indicator_average.csv")
croplist <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/GCCS_Selected_crops/croplist_new15crops.xlsx")

# Run function, specify indicator file to process and croplist, output file is saved
# add folder with date of ouput run
PTFTW_metrics_processed <- process_PTFTW_metrics(
  indicator_file = "../../GCCSmetricsII/Data/Plants_that_feed_the_world/Indicators/indicator_average.csv",
  croplist = "../../GCCSmetricsII/Data_processing/Support_files/GCCS_Selected_crops/croplist_new15crops.xlsx",
  out_path = "../../GCCSmetricsII/Data_processing/5_PTFTW_processing_and_metrics/2025_10_08/PTFTW_new15_metrics.xlsx"
)


## 20. Transfer metrics calculations (3 metrics)
# source function to process transfers datasets
# note: will need to update local paths within function to work properly
source("Functions/Process_transfers.R")

# Data read in, transfers (2012-2019), transfers (2019-2021), country regions, croplist already read in above
# note: function filters the older transfers dataset to include only records from 2015–2018,
#       excluding earlier years and post-2019 entries to avoid duplication with newer 2019–2021 dataset
transfers_2012_2019 <- read_excel("../../GCCSmetricsII/Data/Plant_Treaty/Data_store/ITPGRFA_MLSDataStore2022_7_1.xlsx")
transfers_2019_2021 <- read_excel("../../GCCSmetricsII/Data/Plant_Treaty/Data_store/Updated_transfers_retrieved2025_09_18.xlsx", skip = 2)
countries_regions <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/Geographical/countries_in_regions.xlsx")

# process transfers_2019_2021 updated file, SG note: need to add to function
transfers_2019_2021 <- transfers_2019_2021  %>%
  rename(provider_ISO3         = `ISO3...1`,
        provider_country_name = `Provider country`,
        crop_dirty            = Crop,
        year                  = Year,
        number_of_samples     = `# Samples`,
        recipient_ISO3        = `ISO3...6`,
        recipient_country_name= `Recipient country`)

# add crop strategy assignment, SG and CK assigned 2025_10_08, move file to processing docs folder
unique_crop_dirty <- read_excel("../../GCCSmetricsII/Data/Plant_Treaty/Data_store/Transfers_2019_2021_unique_crop_dirty.xlsx")
transfers_2019_2021 <- transfers_2019_2021 %>%
  left_join(unique_crop_dirty %>% select(crop_dirty, crop_strategy), by = "crop_dirty") %>%
  filter(!is.na(crop_strategy)) #drop rows where crop strategy is NA

# Run function, specify croplist and output file is saved
# add folder with date of ouput run
transfer_metrics <- transfers_metrics(
  croplist = "../../GCCSmetricsII/Data_processing/Support_files/GCCS_Selected_crops/croplist_new15crops.xlsx",
  out_path = "../../GCCSmetricsII/Data_processing/5_PTFTW_processing_and_metrics/2025_10_08/transfers_new15_metrics_2015_2021.xlsx",
  transfers_2012_2019 = transfers_2012_2019,
  transfers_2019_2021 = transfers_2019_2021,
  countries_regions = countries_regions
  )


#' Generate Table 6: Crop-Specific Number Metrics (custom decimal places, no unnecessary .0)
#'
#' Builds Table 6 for each crop. This function:
#'   1. Reads your `metrics_guide` to identify metrics flagged for Table 6
#'      where Role = "Number"
#'   2. Joins each metric to its corresponding summary data frame in `metric_dfs`
#'   3. Extracts the numeric or character value for the current crop (first non-NA)
#'   4. Formats numbers:
#'       - Passport data completeness index: always 2 decimals
#'       - All others: no decimals for integers, 1 decimal for non-integers
#'       - Always use comma as thousands separator, never scientific notation
#'   5. Returns a named list of tibbles, one per Crop_strategy, each with:
#'      - Metric: the label for the row
#'      - Number: the formatted value for that crop (or blank if missing)
#'
#' Details / behavior notes:
#' - The function reads `metrics_guide` and selects rows where
#'   `Pertains to Table` == 6 and `Metric Role` == "Number".
#' - The columns used from `metrics_guide` are:
#
generate_table6 <- function(metrics_guide, metric_dfs) {
  library(dplyr)
  library(purrr)
  library(stringr)
  # .data pronoun used by dplyr
  crop_column <- "Crop_strategy"

  # 1) Identify only the Number metrics for Table 6
  guide_tbl <- metrics_guide %>%
    filter(`Pertains to Table` == 6,
           `Metric Role` == "Number") %>%
    mutate(
      df_name  = str_trim(`Name of Summary Variable (if summarized)`),
      var_name = str_trim(`Name of Individual Metric Variable`),
      Metric   = `Metric Name in Results Table (or summaries text)`
    ) %>%
    # Drop truly missing/empty df or var names
    filter(!is.na(df_name), df_name != "",
           !is.na(var_name), var_name != "")

  # 2) Get full list of crops from metric data frames (remove NA)
  if (length(metric_dfs) == 0) {
    stop("metric_dfs is empty")
  }
  all_crops <- reduce(metric_dfs, full_join, by = crop_column) %>%
    pull(!!sym(crop_column)) %>%
    unique() %>%
    discard(is.na) %>%
    as.character()

  # Helper: test near-integer
  is_integer_like <- function(x, tol = .Machine$double.eps^0.5) {
    if (is.na(x) || !is.finite(x)) return(FALSE)
    abs(x - round(x)) <= tol
  }

  # 3) Build each crop's table
  crop_tables <- set_names(all_crops) %>%
    map(function(crop) {
      result <- guide_tbl %>%
        mutate(
          Number = map_chr(seq_len(n()), function(i) {
            df_name_i <- df_name[i]
            varname_i <- var_name[i]
            metric_label <- Metric[i]

            # Missing data-frame or column check
            if (!df_name_i %in% names(metric_dfs)) {
              message("Missing data frame: ", df_name_i)
              return("")
            }
            df <- metric_dfs[[df_name_i]]
            if (!all(c(crop_column, varname_i) %in% names(df))) {
              message("Missing column '", varname_i, "' in ", df_name_i)
              return("")
            }

            # Extract first non-NA value for this crop
            vals <- df %>%
              filter(.data[[crop_column]] == crop) %>%
              pull(varname_i)
            if (length(vals) == 0) return("")
            # pick first non-NA element
            val <- vals[which(!is.na(vals))[1]]
            if (length(val) == 0 || is.na(val)) return("")

            # Normalize and detect numeric content
            num_val <- NA_real_
            is_num <- FALSE
            if (is.numeric(val)) {
              num_val <- as.numeric(val)
              is_num <- !is.na(num_val)
            } else {
              # remove spaces, commas (thousands separators), and non-breaking spaces
              v_chr <- as.character(val) %>% str_replace_all("[,\\s\\u00A0]", "")
              # Allow leading +/-, decimals, exponent
              if (str_detect(v_chr, "^[+-]?(?:\\d+\\.?\\d*|\\.?\\d+)(?:[eE][+-]?\\d+)?$")) {
                num_val <- as.numeric(v_chr)
                is_num <- !is.na(num_val)
              } else {
                is_num <- FALSE
              }
            }

            if (is_num && !is.na(num_val) && is.finite(num_val)) {
              # Passport data completeness index -> always 2 decimals
              if (str_detect(metric_label, regex("Passport data completeness index", ignore_case = TRUE))) {
                formatC(num_val, format = "f", digits = 2, big.mark = ",")
              } else {
                # Integers: no decimals; otherwise 1 decimal
                if (is_integer_like(num_val)) {
                  formatC(round(num_val), format = "f", digits = 0, big.mark = ",")
                } else {
                  # Round to 1 decimal before formatting to avoid long tails
                  num_1 <- round(num_val, 1)
                  formatC(num_1, format = "f", digits = 1, big.mark = ",")
                }
              }
            } else {
              # Non-numeric, return as-is (trimmed)
              str_trim(as.character(val))
            }
          })
        ) %>%
        select(Metric, Number)

      # If no rows, return placeholders from guide_tbl
      if (nrow(result) == 0) {
        result <- tibble::tibble(
          Metric = guide_tbl$Metric,
          Number = rep("", nrow(guide_tbl))
        )
      }
      result
    })

  crop_tables
}

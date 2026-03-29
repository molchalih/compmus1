validate_tempogram <- function(path) {
  message("Checking: ", path)
  
  df <- read_csv(path, show_col_types = FALSE)
  
  probs <- problems(df)
  
  issues <- c()
  
  # --- check TIME ---
  if (!"TIME" %in% names(df)) {
    issues <- c(issues, "Missing TIME column")
  } else {
    time_num <- suppressWarnings(as.numeric(df$TIME))
    
    if (any(is.na(time_num))) {
      issues <- c(issues, "TIME contains non-numeric values")
    }
  }
  
  # --- check tempo columns ---
  tempo_cols <- setdiff(names(df), "TIME")
  
  if (length(tempo_cols) == 0) {
    issues <- c(issues, "No tempo columns found")
  } else {
    # check if at least some columns are numeric
    numeric_check <- map_lgl(df[tempo_cols], is.numeric)
    
    if (!any(numeric_check)) {
      issues <- c(issues, "No numeric tempo columns")
    }
  }
  
  # --- parsing issues ---
  if (nrow(probs) > 0) {
    issues <- c(issues, paste("Parsing issues:", nrow(probs)))
  }
  
  # --- result ---
  if (length(issues) == 0) {
    tibble(file = basename(path), status = "OK")
  } else {
    tibble(file = basename(path), status = "ERROR", issues = paste(issues, collapse = " | "))
  }
}

act_files <- list.files("datasets/final/tempo_act", full.names = TRUE)

act_results <- map_dfr(act_files, validate_tempogram)

act_results


dft_files <- list.files("datasets/final/tempo_dft", full.names = TRUE)

dft_results <- map_dfr(dft_files, validate_tempogram)

dft_results


act_results |> filter(status == "ERROR")
dft_results |> filter(status == "ERROR")
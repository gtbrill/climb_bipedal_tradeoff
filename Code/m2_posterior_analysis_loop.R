library(here)

m2_all_posteriors <- readRDS(here("RDS", "m2_all_posteriors.rds"))

# --------------------------------------------------------------------------
### POSTERIOR SPECIFICATION DATAFRAME

posterior_specs_df <- read.csv(here("CSV", "m2_posterior_specifications.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))

# --------------------------------------------------------------------------
### STORAGE DATAFRAMES

m2_posterior_samples <- list(
  Female = list(),
  Male = list()
)
m2_posterior_summaries <- data.frame()

# --------------------------------------------------------------------------
### EXTRACT POSTERIORS LOOP - BY SEX

for (sex in c("Female", "Male")) {
  
  cat("\n########################################\n")
  cat("### EXTRACTING POSTERIORS FOR", toupper(sex), "###\n")
  cat("########################################\n")
  
  for (i in 1:nrow(posterior_specs_df)) {
    
    spec <- posterior_specs_df[i, ]
    
    cat("\n========================================\n")
    cat("Sex:", sex, "| Extracting:", spec$Extraction_name, "\n")
    cat("========================================\n")
    
    ## Get the fitted model posteriors
    if (!spec$Model_name %in% names(m2_all_posteriors[[sex]])) {
      cat("WARNING: Model", spec$Model_name, "not found for", sex, ". Skipping.\n")
      next
    }
    
    post <- m2_all_posteriors[[sex]][[spec$Model_name]]
    
    
    ## Extract Parameter 1
    
    # Check if it's a vector parameter (has an index)
    if (!is.na(spec$Parameter1_index)) {
      # Vector parameter like b[2]
      param1_samples <- post[[spec$Parameter1]][, spec$Parameter1_index]
    } else {
      # Simple parameter like d
      param1_samples <- post[[spec$Parameter1]]
    }
    
    ## Extract Parameter 2
    
    if (!is.na(spec$Parameter2)) {
      
      # Check if it's a vector parameter
      if (!is.na(spec$Parameter2_index)) {
        # Vector parameter
        param2_samples <- post[[spec$Parameter2]][, spec$Parameter2_index]
      } else {
        # Simple parameter
        param2_samples <- post[[spec$Parameter2]]
      }
      
      # Compute difference
      posterior_dist <- param1_samples - param2_samples
      cat("Computed difference:", spec$Parameter1, 
          ifelse(!is.na(spec$Parameter1_index), paste0("[", spec$Parameter1_index, "]"), ""),
          "-", spec$Parameter2,
          ifelse(!is.na(spec$Parameter2_index), paste0("[", spec$Parameter2_index, "]"), ""),
          "\n")
      
    } else {
      # Single parameter
      posterior_dist <- param1_samples
      cat("Extracted single parameter:", spec$Parameter1,
          ifelse(!is.na(spec$Parameter1_index), paste0("[", spec$Parameter1_index, "]"), ""),
          "\n")
    }
    
    
    ## Store full posterior distribution
    
    m2_posterior_samples[[sex]][[spec$Extraction_name]] <- posterior_dist
    
    
    # Calculate summary statistics
    # --------------------------------------------------------------------------
    
    summary_row <- data.frame(
      Sex = sex,
      Extraction_name = spec$Extraction_name,
      Model_name = spec$Model_name,
      Parameter1 = spec$Parameter1,
      Parameter1_index = spec$Parameter1_index,
      Parameter2 = ifelse(is.na(spec$Parameter2), NA, spec$Parameter2),
      Parameter2_index = ifelse(is.na(spec$Parameter2_index), NA, spec$Parameter2_index),
      Comparison_type = spec$Comparison_type,
      mean = mean(posterior_dist),
      sd = sd(posterior_dist),
      ci_lower_89 = quantile(posterior_dist, 0.055),
      ci_upper_89 = quantile(posterior_dist, 0.945),
      ci_lower_95 = quantile(posterior_dist, 0.025),
      ci_upper_95 = quantile(posterior_dist, 0.975),
      prob_positive = mean(posterior_dist > 0),
      n_samples = length(posterior_dist),
      stringsAsFactors = FALSE
    )
    
    # Append to summary dataframe
    m2_posterior_summaries <- rbind(m2_posterior_summaries, summary_row)
    
    cat("Mean:", round(summary_row$mean, 3), 
        "| 89% CI: [", round(summary_row$ci_lower_89, 3), ",", 
        round(summary_row$ci_upper_89, 3), "]\n")
    cat("Probability positive:", round(summary_row$prob_positive, 3), "\n")
  }
}

# --------------------------------------------------------------------------
### SAVE OUTPUTS

# Save posterior summaries
write.csv(m2_posterior_summaries,
          here("CSV", "m2_posterior_summaries.csv"),
          row.names = FALSE)
# Save full posterior samples
saveRDS(m2_posterior_samples,
        here("RDS", "m2_posterior_samples.rds"))

cat("\n========================================\n")
cat("POSTERIOR EXTRACTION COMPLETE!\n")
cat("========================================\n")
cat("Summaries saved to: m2_posterior_summaries.csv\n")
cat("Full distributions saved to: m2_posterior_samples.rds\n")
cat("\nExtracted", nrow(m2_posterior_summaries), "posterior distributions\n")
cat("(", sum(m2_posterior_summaries$Sex == "Female"), "Female,",
    sum(m2_posterior_summaries$Sex == "Male"), "Male )\n")
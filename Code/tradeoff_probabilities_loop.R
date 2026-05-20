library(here)

# Load the posterior samples
m1_posterior_samples <- readRDS(here("RDS", "m1_posterior_samples.rds"))
m2_posterior_samples <- readRDS(here("RDS", "m2_posterior_samples.rds"))

# Parse m1 extraction names
parse_m1_name <- function(name) {
  parts <- strsplit(name, "___")[[1]]
  
  focal_control_eth <- parts[1]
  posterior_type <- parts[2]
  
  # Remove "m1_" prefix
  focal_control_eth <- sub("^m1_", "", focal_control_eth)
  
  # Check if .Eth is present
  has_eth <- grepl("\\.Eth$", focal_control_eth)
  if (!has_eth) return(NULL)
  
  # Accept ClimbF, ClimbM, RunF, RunM, WalkF, WalkM, SprintF, SprintM
  valid_types <- c("ClimbF", "ClimbM", "RunF", "RunM", "WalkF", "WalkM", "SprintF", "SprintM")
  if(!posterior_type %in% valid_types) return(NULL)
  
  # Remove .Eth suffix
  focal_control <- sub("\\.Eth$", "", focal_control_eth)
  
  # Split by dots to get focal and control
  fc_parts <- strsplit(focal_control, "\\.")[[1]]
  focal <- fc_parts[1]
  control <- if(length(fc_parts) > 1) paste(fc_parts[-1], collapse=".") else NA
  
  # Determine sport and sex
  sport <- tolower(gsub("[FM]$", "", posterior_type))  # Remove F or M, convert to lowercase
  sex <- ifelse(grepl("F$", posterior_type), "F", "M")
  
  return(list(
    focal = focal,
    control = control,
    proportion_id = focal_control,
    sport = sport,
    sex = sex,
    extraction_name = name
  ))
}

# Parse m2 extraction names
parse_m2_name <- function(name, sex_label) {
  parts <- strsplit(name, "___")[[1]]
  
  focal_control_eth <- parts[1]
  posterior_type <- parts[2]
  
  # Remove "m2_" prefix
  focal_control_eth <- sub("^m2_", "", focal_control_eth)
  
  # Check if .Eth is present
  has_eth <- grepl("\\.Eth$", focal_control_eth)
  if (!has_eth) return(NULL)
  
  # Remove .Eth suffix
  focal_control <- sub("\\.Eth$", "", focal_control_eth)
  
  # Split by dots to get focal and control
  fc_parts <- strsplit(focal_control, "\\.")[[1]]
  focal <- fc_parts[1]
  control <- if(length(fc_parts) > 1) paste(fc_parts[-1], collapse=".") else NA
  
  # ONLY sport.Control effects
  if(!grepl("\\.Control$", posterior_type)) return(NULL)
  
  # Get sport part
  sport_part <- sub("\\.Control$", "", posterior_type)
  
  # Accept Climb, Run, Walk, and Sprint
  if(!sport_part %in% c("Climb", "Run", "Walk", "Sprint")) return(NULL)
  
  sport <- tolower(sport_part)
  
  return(list(
    focal = focal,
    control = control,
    proportion_id = focal_control,
    sport = sport,
    sex = sex_label,
    extraction_name = name
  ))
}

# Organize m1 posteriors
cat("Organizing m1 posteriors...\n")
m1_organized <- data.frame()
for(name in names(m1_posterior_samples)) {
  parsed <- parse_m1_name(name)
  if(!is.null(parsed)) {
    m1_organized <- rbind(m1_organized, data.frame(
      extraction_name = name,
      dataset = "m1",
      focal = parsed$focal,
      control = ifelse(is.na(parsed$control), "absolute", parsed$control),
      proportion_id = parsed$proportion_id,
      sport = parsed$sport,
      sex = parsed$sex,
      stringsAsFactors = FALSE
    ))
  }
}

# Organize m2 posteriors (Female)
cat("Organizing m2 female posteriors...\n")
m2_female_organized <- data.frame()
for(name in names(m2_posterior_samples$Female)) {
  parsed <- parse_m2_name(name, "F")
  if(!is.null(parsed)) {
    m2_female_organized <- rbind(m2_female_organized, data.frame(
      extraction_name = name,
      dataset = "m2",
      focal = parsed$focal,
      control = ifelse(is.na(parsed$control), "absolute", parsed$control),
      proportion_id = parsed$proportion_id,
      sport = parsed$sport,
      sex = "F",
      stringsAsFactors = FALSE
    ))
  }
}

# Organize m2 posteriors (Male)
cat("Organizing m2 male posteriors...\n")
m2_male_organized <- data.frame()
for(name in names(m2_posterior_samples$Male)) {
  parsed <- parse_m2_name(name, "M")
  if(!is.null(parsed)) {
    m2_male_organized <- rbind(m2_male_organized, data.frame(
      extraction_name = name,
      dataset = "m2",
      focal = parsed$focal,
      control = ifelse(is.na(parsed$control), "absolute", parsed$control),
      proportion_id = parsed$proportion_id,
      sport = parsed$sport,
      sex = "M",
      stringsAsFactors = FALSE
    ))
  }
}

# Combine m2
m2_organized <- rbind(m2_female_organized, m2_male_organized)

cat("\nFound", nrow(m1_organized), "m1 posteriors\n")
cat("Found", nrow(m2_organized), "m2 posteriors\n")

# ============================================================================
# FUNCTION TO CALCULATE TRADE-OFF PROBABILITIES
# ============================================================================

calculate_tradeoffs <- function(organized_data, posterior_list, dataset_name, sport_pairs) {
  
  unique_proportions <- unique(organized_data$proportion_id)
  results <- data.frame()
  
  cat("\n========================================\n")
  cat("Processing", dataset_name, "\n")
  cat("========================================\n")
  cat("Unique proportions:", length(unique_proportions), "\n\n")
  
  for(prop in unique_proportions) {
    for(s in c("F", "M")) {
      
      # Loop through each sport pair
      for(pair in sport_pairs) {
        sport1 <- pair[1]
        sport2 <- pair[2]
        
        # Get rows for both sports
        sport1_row <- organized_data[organized_data$proportion_id == prop & 
                                       organized_data$sport == sport1 & 
                                       organized_data$sex == s, ]
        
        sport2_row <- organized_data[organized_data$proportion_id == prop & 
                                       organized_data$sport == sport2 & 
                                       organized_data$sex == s, ]
        
        # Check if both exist and are unique
        if(nrow(sport1_row) != 1 || nrow(sport2_row) != 1) {
          next
        }
        
        # Extract posterior samples
        if(dataset_name == "m1") {
          sport1_samples <- posterior_list[[sport1_row$extraction_name]]
          sport2_samples <- posterior_list[[sport2_row$extraction_name]]
        } else {
          # m2 - need to access by sex list
          sex_list <- ifelse(s == "F", "Female", "Male")
          sport1_samples <- posterior_list[[sex_list]][[sport1_row$extraction_name]]
          sport2_samples <- posterior_list[[sex_list]][[sport2_row$extraction_name]]
        }
        
        # Calculate probabilities
        p_same_sign <- mean(sign(sport1_samples) == sign(sport2_samples))
        p_tradeoff <- 1 - p_same_sign
        
        # Store results
        results <- rbind(results, data.frame(
          proportion_id = prop,
          focal = sport1_row$focal,
          control = sport1_row$control,
          sex = s,
          dataset = dataset_name,
          sport1 = sport1,
          sport2 = sport2,
          comparison = paste0(sport1, "_vs_", sport2),
          p_same_sign = p_same_sign,
          p_tradeoff = p_tradeoff,
          sport1_mean = mean(sport1_samples),
          sport2_mean = mean(sport2_samples),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  cat("Completed:", nrow(results), "proportion-sex-sportpair combinations\n")
  return(results)
}

# ============================================================================
# DEFINE SPORT PAIRS FOR EACH DATASET
# ============================================================================

# M1: Simple effects (climb vs run, climb vs walk, climb vs sprint)
m1_sport_pairs <- list(
  c("climb", "run"),
  c("climb", "walk"),
  c("climb", "sprint")
)

# M2: Control effects (climb-control vs run-control, climb-control vs walk-control, climb-control vs sprint-control)
m2_sport_pairs <- list(
  c("climb", "run"),
  c("climb", "walk"),
  c("climb", "sprint")
)

# ============================================================================
# RUN ANALYSIS FOR BOTH DATASETS
# ============================================================================

# Calculate for m1
m1_tradeoff_results <- calculate_tradeoffs(m1_organized, m1_posterior_samples, "m1", m1_sport_pairs)

# Calculate for m2
m2_tradeoff_results <- calculate_tradeoffs(m2_organized, m2_posterior_samples, "m2", m2_sport_pairs)

# Combine results
all_tradeoff_results <- rbind(m1_tradeoff_results, m2_tradeoff_results)

# ============================================================================
# SAVE RESULTS
# ============================================================================

write.csv(all_tradeoff_results,
          here("CSV", "tradeoff_probabilities.csv"),
          row.names = FALSE)

cat("\n========================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("========================================\n")
cat("M1 results:", nrow(m1_tradeoff_results), "rows\n")
cat("M2 results:", nrow(m2_tradeoff_results), "rows\n")
cat("Total:", nrow(all_tradeoff_results), "rows\n\n")
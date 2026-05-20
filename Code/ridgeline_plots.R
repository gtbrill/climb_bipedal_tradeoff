library(here)
library(tidyverse)
library(ggridges)

# =============================================================
# DEFINE MODELS: input files and their scale/xlim settings
# =============================================================
models <- list(
  m1 = list(
    file     = here("RDS", "m1_posterior_samples.rds"),
    scale    = 1.8,
    xlim_lower = -1.2,
    xlim_upper = 1.2
  ),
  m2 = list(
    file     = here("RDS", "m2_posterior_samples.rds"),
    scale    = 3.6,
    xlim_lower = -1.9,
    xlim_upper = 1.9
  )
)

# =============================================================
# LABELS (unchanged from your original)
# =============================================================
distribution_labels <- c(
  "Run" = "Run",
  "Climb" = "Climb",
  "Control" = "Control",
  "Sprint" = "Sprint",
  "Walk" = "Walk",
  "RunM" = "Run (Male)",
  "ClimbM" = "Climb (Male)",
  "RunF" = "Run (Female)",
  "ClimbF" = "Climb (Female)",
  "RunM.RunF" = "Run (Male - Female)",
  "ClimbM.ClimbF" = "Climb (Male - Female)",
  "ClimbF.ControlF" = "Climb - Control (Female)",
  "RunF.ControlF" = "Run - Control (Female)",
  "SprintF.ControlF" = "Sprint - Control (Female)",
  "WalkF.ControlF" = "Walk - Control (Female)",
  "ClimbF.RunF" = "Climb - Run (Female)",
  "ClimbF.SprintF" = "Climb - Sprint (Female)",
  "ClimbF.WalkF" = "Climb - Walk (Female)",
  "RunF.WalkF" = "Run - Walk (Female)",
  "RunF.SprintF" = "Run - Sprint (Female)",
  "SprintF.WalkF" = "Sprint - Walk (Female)",
  "ClimbM.ControlM" = "Climb - Control (Male)",
  "RunM.ControlM" = "Run - Control (Male)",
  "SprintM.ControlM" = "Sprint - Control (Male)",
  "WalkM.ControlM" = "Walk - Control (Male)",
  "ClimbM.RunM" = "Climb - Run (Male)",
  "ClimbM.SprintM" = "Climb - Sprint (Male)",
  "ClimbM.WalkM" = "Climb - Walk (Male)",
  "RunM.WalkM" = "Run - Walk (Male)",
  "RunM.SprintM" = "Run - Sprint (Male)",
  "SprintM.WalkM" = "Sprint - Walk (Male)",
  "Climb.Control" = "Climb - Control",
  "Run.Control" = "Run - Control",
  "Sprint.Control" = "Sprint - Control",
  "Walk.Control" = "Walk - Control"
)

row_labels <- c(
  "m1_PC1" = "PC1",
  "m1_Stature" = "Stature",
  "m1_SitHeight" = "Sitting height",
  "m1_ArmSpan" = "Arm span",
  "m1_Leg" = "Leg length",
  "m1_Femur" = "Femur length",
  "m1_Arm" = "Arm length",
  "m1_Tibia" = "Tibia length",
  "m1_Humerus" = "Humerus length",
  "m1_ForearmL" = "Forearm length",
  "m1_FootLength" = "Foot length",
  "m1_FootBreadth" = "Foot breadth",
  "m1_HandLength" = "Hand length",
  "m1_HandBreadth" = "Hand breadth",
  "m1_Finger" = "Finger length",
  "m1_Biiliac" = "Biiliac breadth",
  "m1_Biacrom" = "Biacromial breadth",
  "m1_Mass" = "Body mass",
  "m1_Triceps" = "Triceps skinfold",
  "m1_ForearmC" = "Forearm circumference",
  "m1_Hrs_Sport" = "Sport-specific training",
  "m1_Exp_yrs" = "Sport experience",
  "m2_PC1" = "PC1",
  "m2_Stature" = "Stature",
  "m2_SitHeight" = "Sitting height",
  "m2_ArmSpan" = "Arm span",
  "m2_Leg" = "Leg length",
  "m2_Femur" = "Femur length",
  "m2_Arm" = "Arm length",
  "m2_Tibia" = "Tibia length",
  "m2_Humerus" = "Humerus length",
  "m2_ForearmL" = "Forearm length",
  "m2_FootLength" = "Foot length",
  "m2_FootBreadth" = "Foot breadth",
  "m2_HandLength" = "Hand length",
  "m2_HandBreadth" = "Hand breadth",
  "m2_Finger" = "Finger length",
  "m2_Biiliac" = "Biiliac breadth",
  "m2_Biacrom" = "Biacromial breadth",
  "m2_Mass" = "Body mass",
  "m2_Triceps" = "Triceps skinfold",
  "m2_ForearmC" = "Forearm circumference"
)

condition_labels <- c(
  "m1_SitHeight.Stature" = "Stature",
  "m1_ArmSpan.Stature" = "Stature",
  "m1_Leg.Stature" = "Stature",
  "m1_Femur.Stature" = "Stature",
  "m1_Arm.Stature" = "Stature",
  "m1_Arm.Leg" = "Leg length",
  "m1_Tibia.Stature" = "Stature",
  "m1_Tibia.Femur" = "Femur length",
  "m1_Humerus.Stature" = "Stature",
  "m1_Humerus.Femur" = "Femur length",
  "m1_ForearmL.Stature" = "Stature",
  "m1_ForearmL.Humerus" = "Humerus length",
  "m1_FootLength.Stature" = "Stature",
  "m1_FootBreadth.Stature" = "Stature",
  "m1_HandLength.Stature" = "Stature",
  "m1_HandBreadth.Stature" = "Stature",
  "m1_Finger.Stature" = "Stature",
  "m1_Finger.HandLength" = "Hand length",
  "m1_Biiliac.Stature" = "Stature",
  "m1_Biacrom.Stature" = "Stature",
  "m1_Biacrom.Biiliac" = "Biiliac breadth",
  "m1_Mass.Stature" = "Stature",
  "m1_Triceps.Hrs_Total" = "Total training",
  "m1_ForearmC.ForearmL" = "Forearm length",
  "m1_ForearmC.Hrs_Total" = "Total training",
  "m1_ForearmC.ForearmL.Hrs_Total" = "Fl. + Tt.",
  "m2_SitHeight.Stature" = "Stature",
  "m2_ArmSpan.Stature" = "Stature",
  "m2_Leg.Stature" = "Stature",
  "m2_Femur.Stature" = "Stature",
  "m2_Arm.Stature" = "Stature",
  "m2_Arm.Leg" = "Leg length",
  "m2_Tibia.Stature" = "Stature",
  "m2_Tibia.Femur" = "Femur length",
  "m2_Humerus.Stature" = "Stature",
  "m2_Humerus.Femur" = "Femur length",
  "m2_ForearmL.Stature" = "Stature",
  "m2_ForearmL.Humerus" = "Humerus length",
  "m2_FootLength.Stature" = "Stature",
  "m2_FootBreadth.Stature" = "Stature",
  "m2_HandLength.Stature" = "Stature",
  "m2_HandBreadth.Stature" = "Stature",
  "m2_Finger.Stature" = "Stature",
  "m2_Finger.HandLength" = "Hand length",
  "m2_Biiliac.Stature" = "Stature",
  "m2_Biacrom.Stature" = "Stature",
  "m2_Biacrom.Biiliac" = "Biiliac breadth",
  "m2_Mass.Stature" = "Stature",
  "m2_Triceps.Hrs_Total" = "Total training",
  "m2_ForearmC.ForearmL" = "Forearm length",
  "m2_ForearmC.Hrs_Total" = "Total training",
  "m2_ForearmC.ForearmL.Hrs_Total" = "Fl. + Tt."
)

# =============================================================
# OUTER LOOP: iterate over each model
# =============================================================
for (model_name in names(models)) {
  
  cat("\n\n##############################################\n")
  cat(paste0("## Processing: ", model_name, "\n"))
  cat("##############################################\n")
  
  # Pull model-specific settings
  scale      <- models[[model_name]]$scale
  xlim_lower <- models[[model_name]]$xlim_lower
  xlim_upper <- models[[model_name]]$xlim_upper
  post_raw   <- readRDS(models[[model_name]]$file)
  
  # Detect sex-nested structure
  is_sex_nested <- all(c("Female", "Male") %in% names(post_raw))
  
  if (is_sex_nested) {
    cat("Detected sex-nested structure. Will create plots for both Female and Male.\n")
    sexes_to_plot <- c("Female", "Male")
  } else {
    cat("Using non-sex-nested posteriors (Model 1 format)\n")
    sexes_to_plot <- c(NA)
  }
  
  # ---- everything below is your original per-sex loop, unchanged ----
  for (current_sex in sexes_to_plot) {
    
    if (is.na(current_sex)) {
      cat("\n========================================\n")
      cat("Processing Model 1 (non-sex-nested) data\n")
      cat("========================================\n")
      post <- post_raw
      selected_sex <- NULL
    } else {
      cat("\n========================================\n")
      cat("Processing", current_sex, "data\n")
      cat("========================================\n")
      post <- post_raw[[current_sex]]
      selected_sex <- current_sex
    }
    
    extraction_names <- names(post)
    
    parse_extraction_name <- function(name) {
      parts <- str_split(name, "___", simplify = TRUE)
      if (length(parts) < 2) return(NULL)
      condition_part   <- parts[1]
      distribution_part <- parts[2]
      has_ethnicity    <- str_detect(condition_part, "\\.Eth$")
      condition_clean  <- str_remove(condition_part, "\\.Eth$")
      components       <- str_split(condition_clean, "\\.", simplify = TRUE)
      n_components     <- length(components[components != ""])
      dist_components  <- str_split(distribution_part, "\\.", simplify = TRUE)
      return(list(
        full_name     = name,
        condition     = condition_clean,
        has_ethnicity = has_ethnicity,
        n_components  = n_components,
        distribution  = distribution_part,
        dist_components = dist_components
      ))
    }
    
    parsed_list <- map(extraction_names, parse_extraction_name) %>% compact()
    
    parsed_df <- map_df(parsed_list, function(x) {
      tibble(
        full_name     = x$full_name,
        condition     = x$condition,
        has_ethnicity = x$has_ethnicity,
        n_components  = x$n_components,
        distribution  = x$distribution
      )
    })
    
    unique_distributions <- unique(parsed_df$distribution)
    
    get_first_component <- function(condition) {
      str_split(condition, "\\.", simplify = TRUE)[1]
    }
    
    all_conditions   <- parsed_df %>% pull(condition) %>% unique()
    first_components <- map_chr(all_conditions, get_first_component) %>% unique() %>% rev()
    
    get_family_conditions <- function(first_comp) {
      all_conditions[map_chr(all_conditions, get_first_component) == first_comp]
    }
    
    condition_families <- map(first_components, get_family_conditions)
    names(condition_families) <- first_components
    
    max_columns <- max(map_int(condition_families, length))
    cat(paste0("Maximum number of columns needed: ", max_columns, "\n"))
    
    prepare_plot_data <- function(condition, distribution, include_ethnicity = TRUE) {
      name_with_eth    <- paste0(condition, ".Eth___", distribution)
      name_without_eth <- paste0(condition, "___", distribution)
      data_list <- list()
      if (name_without_eth %in% names(post)) {
        data_list$without_eth <- tibble(
          value     = post[[name_without_eth]],
          ethnicity = "Without Ethnicity",
          label     = condition
        )
      }
      if (include_ethnicity && name_with_eth %in% names(post)) {
        data_list$with_eth <- tibble(
          value     = post[[name_with_eth]],
          ethnicity = "With Ethnicity",
          label     = condition
        )
      }
      if (length(data_list) > 0) return(bind_rows(data_list)) else return(NULL)
    }
    
    calculate_pd <- function(samples) {
      prop_positive <- mean(samples > 0)
      max(prop_positive, 1 - prop_positive)
    }
    
    get_distribution_display_name <- function(distribution) {
      display_name <- distribution_labels[distribution]
      if (is.na(display_name)) display_name <- distribution
      if (!is.null(selected_sex)) {
        display_name <- paste0(unname(display_name), " (", selected_sex, ")")
      } else {
        display_name <- unname(display_name)
      }
      return(display_name)
    }
    
    create_ridgeline_plot <- function(distribution) {
      cat(paste0("\nCreating plot for distribution: ", distribution, "\n"))
      
      plot_data_list <- list()
      y_labels <- c()
      y_positions <- c()
      current_y <- 0
      gap_after <- c("m1_Mass", "m1_Hrs_Sport", "m2_Mass")
      positions_with_data <- c()
      
      for (i in seq_along(first_components)) {
        first_comp         <- first_components[i]
        current_y          <- current_y + 1
        family_conditions  <- condition_families[[first_comp]]
        
        for (col_idx in seq_along(family_conditions)) {
          condition <- family_conditions[col_idx]
          data <- prepare_plot_data(condition, distribution)
          if (!is.null(data)) {
            data$y_position <- current_y
            data$column     <- paste0("Column_", col_idx)
            plot_data_list  <- c(plot_data_list, list(data))
          }
        }
        
        label_to_use <- row_labels[first_comp]
        if (is.na(label_to_use)) label_to_use <- first_comp
        label_to_use <- unname(label_to_use)
        
        y_labels            <- c(y_labels, label_to_use)
        y_positions         <- c(y_positions, current_y)
        positions_with_data <- c(positions_with_data, current_y)
        
        if (first_comp %in% gap_after) current_y <- current_y + 1
      }
      
      if (length(plot_data_list) == 0) {
        cat("No data found for this distribution.\n")
        return(NULL)
      }
      
      plot_data <- bind_rows(plot_data_list)
      data_positions <- plot_data %>% distinct(y_position, column)
      
      star_data <- plot_data %>%
        filter(ethnicity == "With Ethnicity") %>%
        group_by(label, y_position, column) %>%
        summarise(pd = calculate_pd(value), .groups = "drop") %>%
        mutate(stars = case_when(
          pd > 0.95 ~ "**",
          pd > 0.90 ~ "*",
          TRUE ~ ""
        )) %>%
        filter(stars != "")
      
      x_limits <- c(xlim_lower, xlim_upper)
      distribution_display_name <- get_distribution_display_name(distribution)
      
      p <- ggplot(plot_data, aes(x = value, y = factor(y_position), fill = ethnicity)) +
        annotate("rect", xmin = -1, xmax = 1, ymin = -Inf, ymax = Inf,
                 fill = "grey85", alpha = 0.3) +
        geom_segment(
          data = data_positions,
          aes(x = -Inf, xend = Inf, y = y_position, yend = y_position),
          colour = "black", linewidth = 0, alpha = 0.9, inherit.aes = FALSE
        ) +
        geom_vline(xintercept = 1,  colour = "black", linewidth = 0.0, alpha = 0.3, linetype = "dashed") +
        geom_vline(xintercept = -1, colour = "black", linewidth = 0.0, alpha = 0.3, linetype = "dashed") +
        geom_density_ridges(alpha = 0.5, scale = scale, rel_min_height = 0,
                            panel_scaling = FALSE, from = xlim_lower, to = xlim_upper) +
        geom_vline(xintercept = 0, colour = "black", linewidth = 0.4, alpha = 0.9, linetype = "dashed") +
        geom_text(
          data = plot_data %>%
            group_by(column, y_position, label) %>%
            slice(1) %>% ungroup() %>%
            filter(column != "Column_1") %>%
            mutate(display_label = {
              custom <- condition_labels[label]
              ifelse(is.na(custom), label, custom)
            }),
          aes(label = display_label, x = Inf, y = y_position),
          hjust = 1, vjust = -0.5, size = 2.5, inherit.aes = FALSE
        ) +
        geom_text(
          data = star_data,
          aes(label = stars, x = -Inf, y = y_position),
          hjust = -0.2, vjust = 0.25, size = 4, inherit.aes = FALSE, color = "black"
        ) +
        facet_wrap(~ column, ncol = max_columns, scales = "fixed") +
        scale_x_continuous(
          limits = x_limits, expand = c(0, 0),
          breaks = seq(floor(min(x_limits)), ceiling(max(x_limits)), by = 1),
          minor_breaks = seq(floor(min(x_limits)), ceiling(max(x_limits)), by = 0.5)
        ) +
        scale_y_discrete(
          breaks = y_positions,
          labels = y_labels,
          limits = factor(1:max(y_positions))
        ) +
        scale_fill_manual(
          values = c("With Ethnicity" = "sienna3", "Without Ethnicity" = "khaki2"),
          breaks = c("Without Ethnicity", "With Ethnicity"),
          labels = c("With Ethnicity" = "Yes", "Without Ethnicity" = "No")
        ) +
        labs(title = distribution_display_name, x = "Effect Size (SD)", y = NULL, fill = "Ethnicity Control") +
        theme_minimal() +
        theme(
          plot.title       = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
          axis.text.y      = element_text(size = 8, color = "black", vjust = 0),
          axis.text.x      = element_text(angle = 0, vjust = 0.5, hjust = 0.5, color = "black"),
          legend.position  = "bottom",
          strip.text       = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank()
        )
      
      return(p)
    }
    
    # Save plots
    for (dist in unique_distributions) {
      plot_obj <- create_ridgeline_plot(dist)
      if (!is.null(plot_obj)) {
        # Filename now includes model name prefix
        if (!is.null(selected_sex)) {
          filename <- paste0(model_name, "_ridgeline_", str_replace_all(dist, "\\.", "_"), "_", selected_sex, ".png")
        } else {
          filename <- paste0(model_name, "_ridgeline_", str_replace_all(dist, "\\.", "_"), ".png")
        }
        
        ggsave(
          here("Figures", "Figures S10-11", filename),
          plot   = plot_obj,
          width  = 12,
          height = 6.270833,
          units  = "in",
          dpi    = 300
        )
        cat(paste0("Saved: ", filename, "\n"))
      }
    }
    
    cat(paste0("\n", ifelse(is.null(selected_sex), model_name, paste(model_name, selected_sex)), " plots completed!\n"))
  }
}

cat("\nAll models and plots completed\n")
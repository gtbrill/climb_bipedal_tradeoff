library(rethinking)
library(here)
dir.create(here("RDS"), showWarnings = FALSE)

### DATA

data <- read.csv(here("CSV", "anthropometric_data_PC1.csv"))
  
  #Extract run and climb
  run=subset(data,data$Sport=="Run")
  climb=subset(data,data$Sport=="Climb")
  
  # Standardise performance within each sex
  run$StdPerformance <- ave(run$IAAF, run$Sex, 
                            FUN = function(x) (x - mean(x)) / sd(x))
  climb$StdPerformance <- ave(climb$Grade_IRCRA, climb$Sex, 
                              FUN = function(x) (x - mean(x)) / sd(x))
  
  # Bind datasets
  data_to_use <- rbind(run, climb)
  
  # Define Group and Ethnicity
  data_to_use$Group <- as.integer(factor(data_to_use$Group, 
                             levels = c("RunF","RunM", "ClimbF", "ClimbM")))
  data_to_use$Ethnicity <- as.integer(factor(data_to_use$Ethnicity, 
                                 levels = c("White","Asian", "Mixed", "Other")))


# --------------------------------------------------------------------------
### MODEL SPECIFICATION DATAFRAME

model_specs_df <- read.csv(here("CSV", "m1_model_specifications.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))

# --------------------------------------------------------------------------
### STORAGE DATAFRAMES

m1_fitted_models <- list()
m1_all_posteriors <- list()
m1_convergence_diagnostics <- data.frame()

# --------------------------------------------------------------------------
### FIT MODELS LOOP

for (i in 1:nrow(model_specs_df)) {
  
  spec <- model_specs_df[i, ]
  
  cat("\n========================================\n")
  cat("Fitting model:", spec$Model_name, "\n")
  cat("========================================\n")
  
  ## MODEL DATA
  
  m_data <- list(
    N = nrow(data_to_use),
    Metric = ave(data_to_use[[spec$Focal]], data_to_use$Sex, 
                 FUN = function(x) (x - mean(x)) / sd(x)),
    Performance = data_to_use$StdPerformance,
    Group = data_to_use$Group
  )
  
  # Add Control1 if specified
  if (!is.na(spec$Control1)) {
    m_data$Metric2 <- ave(data_to_use[[spec$Control1]], data_to_use$Sex, 
                          FUN = function(x) (x - mean(x)) / sd(x))
  }
  
  # Add Control2 if specified
  if (!is.na(spec$Control2)) {
    m_data$Metric3 <- ave(data_to_use[[spec$Control2]], data_to_use$Sex, 
                          FUN = function(x) (x - mean(x)) / sd(x))
  }
  
  # Add Ethnicity control if specified
  if (spec$Ethnicity == "Y") {
    m_data$Ethnicity <- as.integer(factor(data_to_use$Ethnicity))
  }
  
  
  # --------------------------------------------------------------------------
  # MODEL FORMULAS BASED ON MODEL_TYPE
  # --------------------------------------------------------------------------
  
  if (spec$Model_type == "m1.1M") {
    model_formula <- alist(
          # Model
          Performance ~ dnorm(mu, sigma),
          mu <- a + b[Group]*Metric,
          # Priors
          a ~ dnorm(0,0.5),
          vector[4]: b ~ dnorm(0,1),
          sigma ~ dexp(1)
        )
    
  } else if (spec$Model_type == "m1.1M_Eth") {
    model_formula <- alist(
      # Model
      Performance ~ dnorm(mu, sigma),
      mu <- a + b[Group]*Metric + sigma_y*z[Ethnicity],
      # Non-centred ethnicity effects
      vector[4]: z ~ dnorm(0, 1),
      transpars> vector[4]: y <<- z * sigma_y,    
      # Priors
      a ~ dnorm(0,0.5),
      vector[4]: b ~ dnorm(0,1),
      sigma_y ~ dexp(1),
      sigma ~ dexp(1)
    )
    
  } else if (spec$Model_type == "m1.2M") {
    model_formula <- alist(
      # Model
      Performance ~ dnorm(mu, sigma),
      mu <- a + b[Group]*Metric + d*Metric2,
      # Priors
      a ~ dnorm(0,0.5),
      vector[4]: b ~ dnorm(0,1),
      d ~ dnorm(0,1),
      sigma ~ dexp(1)
    )
    
  } else if (spec$Model_type == "m1.2M_Eth") {
    model_formula <- alist(
      # Model
      Performance ~ dnorm(mu, sigma),
      mu <- a + b[Group]*Metric + sigma_y*z[Ethnicity] + d*Metric2,
      # Non-centred ethnicity effects
      vector[4]: z ~ dnorm(0, 1),
      transpars> vector[4]: y <<- z * sigma_y,    
      # Priors
      a ~ dnorm(0,0.5),
      vector[4]: b ~ dnorm(0,1),
      sigma_y ~ dexp(2),
      d ~ dnorm(0,1),
      sigma ~ dexp(1)
    )
    
  } else if (spec$Model_type == "m1.3M") {
    model_formula <- alist(
      # Model
      Performance ~ dnorm(mu, sigma),
      mu <- a + b[Group]*Metric + d*Metric2 + e*Metric3,
      # Priors
      a ~ dnorm(0,0.5),
      vector[4]: b ~ dnorm(0,1),
      d ~ dnorm(0,1),
      e ~ dnorm(0,1),
      sigma ~ dexp(1)
    )
    
  } else if (spec$Model_type == "m1.3M_Eth") {
    model_formula <- alist(
      # Model
      Performance ~ dnorm(mu, sigma),
      mu <- a + b[Group]*Metric + sigma_y*z[Ethnicity] + d*Metric2 + e*Metric3,
      # Non-centred ethnicity effects
      vector[4]: z ~ dnorm(0, 1),
      transpars> vector[4]: y <<- z * sigma_y,    
      # Priors
      a ~ dnorm(0,0.5),
      vector[4]: b ~ dnorm(0,1),
      sigma_y ~ dexp(2),
      d ~ dnorm(0,1),
      e ~ dnorm(0,1),
      sigma ~ dexp(1)
    )
    
    
  } else {
    stop("Unknown model_type: ", spec$Model_type)
  }
  
  # --------------------------------------------------------------------------
  # FIT THE MODEL
  # --------------------------------------------------------------------------
  
  fit <- ulam(
    model_formula,
    data = m_data,
    chains = 4,
    cores = 4,
    iter = 8000
  )
  
  # --------------------------------------------------------------------------
  # STORE FITTED MODEL AND POSTERIORS
  # --------------------------------------------------------------------------
  
  m1_fitted_models[[spec$Model_name]] <- fit
  m1_all_posteriors[[spec$Model_name]] <- extract.samples(fit)
  
  cat("Model fitted successfully!\n")
  
  # --------------------------------------------------------------------------
  # EXTRACT DIAGNOSTICS FROM PRECIS
  # --------------------------------------------------------------------------
  
  # Get precis output with all parameters
  param_summary <- precis(fit, depth = 2)
  
  # Loop through each parameter
  for (param_name in rownames(param_summary)) {
    
    diagnostic_row <- data.frame(
      Model_name = spec$Model_name,
      Parameter = param_name,
      rhat = param_summary[param_name, "rhat"],
      ess_bulk = param_summary[param_name, "ess_bulk"],
      stringsAsFactors = FALSE
    )
    
    # Append to diagnostics dataframe
    m1_convergence_diagnostics <- rbind(m1_convergence_diagnostics, diagnostic_row)
  }
  
  cat("Diagnostics extracted\n")
  cat("Model", i, "of", nrow(model_specs_df), "complete\n")
}


# ============================================================================
### SAVE OUTPUTS

# Save diagnostics
write.csv(m1_convergence_diagnostics,
          here("CSV", "m1_convergence_diagnostics.csv"),
          row.names = FALSE)
# Save fitted models
saveRDS(m1_fitted_models,
        here("RDS", "m1_fitted_models.rds"))
# Save posteriors
saveRDS(m1_all_posteriors,
        here("RDS", "m1_all_posteriors.rds"))


cat("\n========================================\n")
cat("ALL MODELS FITTED!\n")
cat("========================================\n")
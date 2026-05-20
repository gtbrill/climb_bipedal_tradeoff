library(here)
library(rethinking)
dir.create(here("RDS"), showWarnings = FALSE)

### DATA

data <- read.csv(here("CSV", "anthropometric_data_PC1.csv"))

# Extract elite groups + controls
Model2 <- data[data$Model2_data == "Yes", ]

# --------------------------------------------------------------------------
### MODEL SPECIFICATION DATAFRAME

model_specs_df <- read.csv(here("CSV", "m2_model_specifications.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))

# --------------------------------------------------------------------------
### STORAGE DATAFRAMES

m2_fitted_models <- list(
  Female = list(),
  Male = list()
)
m2_all_posteriors <- list(
  Female = list(),
  Male = list()
)
m2_convergence_diagnostics <- data.frame()

# --------------------------------------------------------------------------
### FIT MODELS LOOP - BY SEX

for (sex in c("Female", "Male")) {
  
  cat("\n########################################\n")
  cat("### FITTING MODELS FOR", toupper(sex), "###\n")
  cat("########################################\n")
  
  # Filter data by sex
  data_to_use <- Model2[Model2$Sex == sex, ]
  
  # Define Sport and Ethnicity for this sex
  data_to_use$Group <- as.integer(factor(data_to_use$Sport, 
                                         levels = c("Control", "Climb", "Run", "Sprint", "Walk")))
  data_to_use$Ethnicity <- as.integer(factor(data_to_use$Ethnicity, 
                                             levels = c("White","Black","Asian", "Mixed", "Other")))
  
  
  for (i in 1:nrow(model_specs_df)) {
    
    spec <- model_specs_df[i, ]
    
    cat("\n========================================\n")
    cat("Sex:", sex, "| Model:", spec$Model_name, "\n")
    cat("========================================\n")
    
    ## MODEL DATA
    
    m_data <- list(
      N = nrow(data_to_use),
      Metric = standardize(data_to_use[[spec$Focal]]),
      Group = data_to_use$Group
    )
    
    # Add Control1 if specified
    if (!is.na(spec$Control1)) {
      m_data$Metric2 <- standardize(data_to_use[[spec$Control1]])
    }
    
    # Add Control2 if specified
    if (!is.na(spec$Control2)) {
      m_data$Metric3 <- standardize(data_to_use[[spec$Control2]])
    }
    
    # Add Ethnicity control if specified
    if (spec$Ethnicity == "Y") {
      m_data$Ethnicity <- data_to_use$Ethnicity
    }
    
    
    # --------------------------------------------------------------------------
    # MODEL FORMULAS BASED ON MODEL_TYPE
    # --------------------------------------------------------------------------
    
    
    if (spec$Model_type == "m2.1M") {
      model_formula <- alist(
        # Model
        Metric ~ dnorm(mu, sigma),
        mu <- a + sigma_b*x[Group],
        # Non-centered group effects
        vector[5]: x ~ dnorm(0, 1),
        transpars> vector[5]: b <<- x * sigma_b,   
        # Priors
        a ~ dnorm(0,0.5),
        sigma_b ~ dexp(1),
        sigma ~ dexp(1)
      )
      
    } else if (spec$Model_type == "m2.1M_Eth") {
      model_formula <- alist(
        # Model
        Metric ~ dnorm(mu, sigma),
        mu <- a + sigma_b*x[Group] + sigma_y*z[Ethnicity],
        # Non-centered group effects
        vector[5]: x ~ dnorm(0, 1),
        transpars> vector[5]: b <<- x * sigma_b,   
        # Non-centered ethnicity effects
        vector[5]: z ~ dnorm(0, 1),
        transpars> vector[5]: y <<- z * sigma_y,
        # Priors
        a ~ dnorm(0,0.5),
        sigma_b ~ dexp(1),
        sigma_y ~ dexp(1),
        sigma ~ dexp(1)
      )
      
    } else if (spec$Model_type == "m2.2M") {
      model_formula <- alist(
        # Model
        Metric ~ dnorm(mu, sigma),
        mu <- a + sigma_b*x[Group] + d*Metric2,
        # Non-centered group effects
        vector[5]: x ~ dnorm(0, 1),
        transpars> vector[5]: b <<- x * sigma_b,   
        # Priors
        a ~ dnorm(0,1),
        d ~ dnorm(0,1),
        sigma_b ~ dexp(0.5),
        sigma ~ dexp(1)
      )
      
    } else if (spec$Model_type == "m2.2M_Eth") {
      model_formula <- alist(
        # Model
        Metric ~ dnorm(mu, sigma),
        mu <- a + sigma_b*x[Group] + sigma_y*z[Ethnicity] + d*Metric2,
        # Non-centered group effects
        vector[5]: x ~ dnorm(0, 1),
        transpars> vector[5]: b <<- x * sigma_b,   
        # Non-centered ethnicity effects
        vector[5]: z ~ dnorm(0, 1),
        transpars> vector[5]: y <<- z * sigma_y,
        # Priors
        a ~ dnorm(0,1),
        d ~ dnorm(0,1),
        sigma_b ~ dexp(0.5),
        sigma_y ~ dexp(1),
        sigma ~ dexp(1)
      )
      
    } else if (spec$Model_type == "m2.3M") {
      model_formula <- alist(
        # Model
        Metric ~ dnorm(mu, sigma),
        mu <- a + sigma_b*x[Group] + d*Metric2 + e*Metric3,
        # Non-centered group effects
        vector[5]: x ~ dnorm(0, 1),
        transpars> vector[5]: b <<- x * sigma_b,   
        # Priors
        a ~ dnorm(0,1),
        d ~ dnorm(0,1),
        e ~ dnorm(0,1),
        sigma_b ~ dexp(0.5),
        sigma ~ dexp(1)
      )
      
    } else if (spec$Model_type == "m2.3M_Eth") {
      model_formula <- alist(
        # Model
        Metric ~ dnorm(mu, sigma),
        mu <- a + sigma_b*x[Group] + sigma_y*z[Ethnicity] + d*Metric2 + e*Metric3,
        # Non-centered group effects
        vector[5]: x ~ dnorm(0, 1),
        transpars> vector[5]: b <<- x * sigma_b,   
        # Non-centered ethnicity effects
        vector[5]: z ~ dnorm(0, 1),
        transpars> vector[5]: y <<- z * sigma_y,
        # Priors
        a ~ dnorm(0,1),
        d ~ dnorm(0,1),
        e ~ dnorm(0,1),
        sigma_b ~ dexp(0.5),
        sigma_y ~ dexp(1),
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
    
    m2_fitted_models[[sex]][[spec$Model_name]] <- fit
    m2_all_posteriors[[sex]][[spec$Model_name]] <- extract.samples(fit)
    
    cat("Model fitted successfully!\n")
    
    # --------------------------------------------------------------------------
    # EXTRACT DIAGNOSTICS FROM PRECIS
    # --------------------------------------------------------------------------
    
    # Get precis output with all parameters
    param_summary <- precis(fit, depth = 2)
    
    # Loop through each parameter
    for (param_name in rownames(param_summary)) {
      
      diagnostic_row <- data.frame(
        Sex = sex,
        Model_name = spec$Model_name,
        Parameter = param_name,
        rhat = param_summary[param_name, "rhat"],
        ess_bulk = param_summary[param_name, "ess_bulk"],
        stringsAsFactors = FALSE
      )
      
      # Append to diagnostics dataframe
      m2_convergence_diagnostics <- rbind(m2_convergence_diagnostics, diagnostic_row)
    }
    
    cat("Diagnostics extracted\n")
    cat("Sex:", sex, "| Model", i, "of", nrow(model_specs_df), "complete\n")
  }
}


# ============================================================================
### SAVE OUTPUTS

# Save diagnostics
write.csv(m2_convergence_diagnostics,
          here("CSV", "m2_convergence_diagnostics.csv"),
          row.names = FALSE)
# Save fitted models
saveRDS(m2_fitted_models,
        here("RDS", "m2_fitted_models.rds"))
# Save posteriors
saveRDS(m2_all_posteriors,
        here("RDS", "m2_all_posteriors.rds"))


cat("\n========================================\n")
cat("ALL MODELS FITTED!\n")
cat("========================================\n")
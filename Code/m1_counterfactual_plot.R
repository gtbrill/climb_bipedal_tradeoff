library(here)
library(rethinking)
dev.off()

Models1 <- readRDS(here("RDS", "m1_fitted_models.rds"))
fit1 <- Models1[["m1_ForearmC.ForearmL.Hrs_Total.Eth"]]

# Extract run and climb
run <- subset(data, data$Sport == "Run")
climb <- subset(data, data$Sport == "Climb")

# Standardise performance within each sex
run$StdPerformance <- ave(run$IAAF, run$Sex, 
                          FUN = function(x) (x - mean(x)) / sd(x))
climb$StdPerformance <- ave(climb$Grade_IRCRA, climb$Sex, 
                            FUN = function(x) (x - mean(x)) / sd(x))

# Bind datasets
data_to_use <- rbind(run, climb)

# Standardise metrics within sex
data_to_use$Metric <- ave(data_to_use$ForearmC, data_to_use$Sex, 
                          FUN = function(x) (x - mean(x)) / sd(x))
data_to_use$Metric2 <- ave(data_to_use$ForearmL, data_to_use$Sex, 
                           FUN = function(x) (x - mean(x)) / sd(x))
data_to_use$Metric3 <- ave(data_to_use$Hrs_Total, data_to_use$Sex, 
                           FUN = function(x) (x - mean(x)) / sd(x))
data_to_use$GroupNum <- match(data_to_use$Group, c("RunF", "RunM", "ClimbF", "ClimbM"))

# Store standardisation parameters for back-transformation
std_params <- data.frame(Sex = c("F", "M"))

for(s in c("Female", "Male")) {
  sex_label <- ifelse(s == "Female", "F", "M")
  data_sex_temp <- subset(data_to_use, Sex == s)
  
  std_params[std_params$Sex == sex_label, "mean_forearm"] <- mean(data_sex_temp$ForearmC, na.rm = TRUE)
  std_params[std_params$Sex == sex_label, "sd_forearm"] <- sd(data_sex_temp$ForearmC, na.rm = TRUE)
  
  run_sex <- subset(data_sex_temp, Sport == "Run")
  climb_sex <- subset(data_sex_temp, Sport == "Climb")
  
  std_params[std_params$Sex == sex_label, "mean_perf_run"] <- mean(run_sex$IAAF, na.rm = TRUE)
  std_params[std_params$Sex == sex_label, "sd_perf_run"] <- sd(run_sex$IAAF, na.rm = TRUE)
  std_params[std_params$Sex == sex_label, "mean_perf_climb"] <- mean(climb_sex$Grade_IRCRA, na.rm = TRUE)
  std_params[std_params$Sex == sex_label, "sd_perf_climb"] <- sd(climb_sex$Grade_IRCRA, na.rm = TRUE)
}

# CONSISTENT STYLING:
# Run = steelblue, Climb = black
# Female = solid (pch=16, lty=1), Male = hollow (pch=1, lty=2)
group_cols <- c("steelblue", "steelblue", "black", "black")  # RunF, RunM, ClimbF, ClimbM
group_labels <- c("RunF", "RunM", "ClimbF", "ClimbM")
group_pch <- c(16, 1, 16, 1)  # Filled female, hollow male, filled female, hollow male
group_lty <- c(1, 2, 1, 2)    # Solid female, dashed male, solid female, dashed male

# Extract posterior samples
post <- extract.samples(fit1)



# Set up 3-panel plot (1 row, 3 columns)
par(mfrow = c(2, 2))

plot(precis(fit1, depth = 2))

# ===== PANEL 1: STANDARDISED PLOT (all 4 groups) =====
M_seq <- seq(from = min(data_to_use$Metric), to = max(data_to_use$Metric), length.out = 100)

plot(data_to_use$Metric, data_to_use$StdPerformance, 
     type = "n",
     xlab = "Forearm Circumference (standardised)", 
     ylab = "Performance (standardised)",
     main = "Standardised")

for(g in 1:4) {
  mu <- sapply(M_seq, function(m) {
    post$a + post$b[, g] * m + post$d * 0 + post$e * 0
  })
  
  mu_mean <- apply(mu, 2, mean)
  mu_PI <- apply(mu, 2, PI, prob = 0.89)
  
  shade(mu_PI, M_seq, col = col.alpha(group_cols[g], 0.2))
  lines(M_seq, mu_mean, lwd = 2, col = group_cols[g], lty = group_lty[g])
}

points(data_to_use$Metric, data_to_use$StdPerformance, 
       col = group_cols[data_to_use$GroupNum], 
       pch = group_pch[data_to_use$GroupNum])

# ===== PANEL 2: RUN (unstandardised, both sexes) =====
sport <- "Run"
data_sport <- subset(data_to_use, Sport == sport)
groups_sport <- c(1, 2)  # RunF, RunM
perf_var <- "IAAF"

plot(data_sport$ForearmC, 
     data_sport[[perf_var]],
     type = "n",
     xlab = "Forearm Circumference (cm)",
     ylab = "IAAF Score",
     main = "Run")

for(i in 1:2) {
  g <- groups_sport[i]
  sex_full <- if(i == 1) "Female" else "Male"
  sex <- if(i == 1) "F" else "M"
  
  params <- std_params[std_params$Sex == sex, ]
  data_sex <- subset(data_sport, Sex == sex_full)
  
  M_seq_raw <- seq(from = min(data_sex$ForearmC, na.rm = TRUE), 
                   to = max(data_sex$ForearmC, na.rm = TRUE), 
                   length.out = 100)
  M_seq_std <- (M_seq_raw - params$mean_forearm) / params$sd_forearm
  
  mu <- sapply(M_seq_std, function(m) {
    post$a + post$b[, g] * m + post$d * 0 + post$e * 0
  })
  
  mu_mean_std <- apply(mu, 2, mean)
  mu_PI_std <- apply(mu, 2, PI, prob = 0.89)
  
  mu_mean_raw <- mu_mean_std * params$sd_perf_run + params$mean_perf_run
  mu_PI_raw <- mu_PI_std * params$sd_perf_run + params$mean_perf_run
  
  shade(mu_PI_raw, M_seq_raw, col = col.alpha(group_cols[g], 0.2))
  lines(M_seq_raw, mu_mean_raw, lwd = 2, col = group_cols[g], lty = group_lty[g])
}

data_female <- subset(data_sport, Sex == "Female")
data_male <- subset(data_sport, Sex == "Male")

points(data_female$ForearmC, data_female[[perf_var]], 
       col = group_cols[1], pch = group_pch[1])
points(data_male$ForearmC, data_male[[perf_var]], 
       col = group_cols[2], pch = group_pch[2])

# Add legend to middle plot
legend("topright", 
       legend = group_labels, 
       col = group_cols, 
       pch = group_pch, 
       lty = group_lty,
       lwd = 2,
       bty = "n")

# ===== PANEL 3: CLIMB (unstandardised, both sexes) =====
sport <- "Climb"
data_sport <- subset(data_to_use, Sport == sport)
groups_sport <- c(3, 4)  # ClimbF, ClimbM
perf_var <- "Grade_IRCRA"

plot(data_sport$ForearmC, 
     data_sport[[perf_var]],
     type = "n",
     xlab = "Forearm Circumference (cm)",
     ylab = "IRCRA Grade",
     main = "Climb")

for(i in 1:2) {
  g <- groups_sport[i]
  sex_full <- if(i == 1) "Female" else "Male"
  sex <- if(i == 1) "F" else "M"
  
  params <- std_params[std_params$Sex == sex, ]
  data_sex <- subset(data_sport, Sex == sex_full)
  
  M_seq_raw <- seq(from = min(data_sex$ForearmC, na.rm = TRUE), 
                   to = max(data_sex$ForearmC, na.rm = TRUE), 
                   length.out = 100)
  M_seq_std <- (M_seq_raw - params$mean_forearm) / params$sd_forearm
  
  mu <- sapply(M_seq_std, function(m) {
    post$a + post$b[, g] * m + post$d * 0 + post$e * 0
  })
  
  mu_mean_std <- apply(mu, 2, mean)
  mu_PI_std <- apply(mu, 2, PI, prob = 0.89)
  
  mu_mean_raw <- mu_mean_std * params$sd_perf_climb + params$mean_perf_climb
  mu_PI_raw <- mu_PI_std * params$sd_perf_climb + params$mean_perf_climb
  
  shade(mu_PI_raw, M_seq_raw, col = col.alpha(group_cols[g], 0.2))
  lines(M_seq_raw, mu_mean_raw, lwd = 2, col = group_cols[g], lty = group_lty[g])
}

data_female <- subset(data_sport, Sex == "Female")
data_male <- subset(data_sport, Sex == "Male")

points(data_female$ForearmC, data_female[[perf_var]], 
       col = group_cols[3], pch = group_pch[3])
points(data_male$ForearmC, data_male[[perf_var]], 
       col = group_cols[4], pch = group_pch[4])



# Reset plotting parameters
par(mfrow = c(1, 1))

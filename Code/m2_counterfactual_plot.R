library(here)
library(rethinking)
dev.off()

Models2 <- readRDS(here("RDS", "m2_fitted_models.rds"))
fit2 <- Models2[["Male"]][["m2_ForearmC.ForearmL.Hrs_Total.Eth"]]

par(mfrow=c(1,2))

plot(precis(fit2, depth = 2),main="Parameters")

# Get the right sample
male=data[data$Sex=="Male",]
Model2 <- male[male$Model2_data == "Yes", ]
# Extract posterior samples
post <- extract.samples(fit2)
# Define groups in order matching the b parameters
groups <- c("ControlM", "ClimbM", "RunM", 
             "SprintM",  "WalkM")
n_groups <- length(groups)
# Get the standardization parameters
mean_ForearmC <- mean(Model2$ForearmC, na.rm = TRUE)
sd_ForearmC <- sd(Model2$ForearmC, na.rm = TRUE)
# Unstandardize the b parameters
post$b_unstand <- post$b * sd_ForearmC + mean_ForearmC
# Define colors for each exercise type
colors <- c("grey80", "black", "steelblue", "orange", "#66CC66")       
# Create the plot
plot(NULL, xlim = c(0.5, n_groups + 0.5), 
     ylim = range(Model2$ForearmC, na.rm = TRUE),
     xlab = "", ylab = "Forearm Circumference (cm)", main="Group Effects", xaxt = "n")

# Group labels
axis(1, at = 1:n_groups, labels = c("Control", "Climb", "Run", 
                                    "Sprint",  "Walk"), las=2, line = 0)

# For each group
for(i in 1:n_groups) {
  # Get actual data points for this group
  group_data <- Model2$ForearmC[Model2$Group == groups[i]]
  group_sex <- Model2$Sex[Model2$Group == groups[i]]
  
  # Determine point type: filled for Female (16), hollow for Male (1)
  pch_vec <- ifelse(group_sex == "Female", 16, 1)
  
  # Plot raw data points (with jitter for visibility) - NO transparency
  points(rep(i, length(group_data)) + rnorm(length(group_data), 0, 0.05), 
         group_data, pch = pch_vec, col = colors[i])
  
  # Get UNSTANDARDIZED posterior for this group's b parameter
  group_post <- post$b_unstand[, i]
  
  # Calculate posterior mean and 89% CI
  post_mean <- mean(group_post)
  post_ci <- PI(group_post, prob = 0.89)
  
  # Draw shaded box for 89% CI
  rect(i - 0.2, post_ci[1], i + 0.2, post_ci[2], 
       col = col.alpha(colors[i], 0.4), border = NA)
  
  # Draw line for posterior mean
  segments(i - 0.2, post_mean, i + 0.2, post_mean, 
           col = colors[i], lwd = 3)
}

# Reset plotting parameters
par(mfrow = c(1, 1))
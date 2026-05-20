library(here)

m1_convergence_diagnostics <- read.csv(here("CSV", "m1_convergence_diagnostics.csv"))
m2_convergence_diagnostics <- read.csv(here("CSV", "m2_convergence_diagnostics.csv"))

par(mfrow = c(1, 2))
x_axis <- c(0.9995, 1.0045)
y_axis <- c(1000, 28000)

# Model 1
plot(m1_convergence_diagnostics$rhat, m1_convergence_diagnostics$ess_bulk, 
     xlim = x_axis, ylim = y_axis,
     main = "Model 1", ylab = "ESS", xlab = "R-hat")

min_ess_m1 <- min(m1_convergence_diagnostics$ess_bulk)
max_rhat_m1 <- max(m1_convergence_diagnostics$rhat)

abline(h = min_ess_m1, lty = 2)
abline(v = max_rhat_m1, lty = 2)

# Add labels (adjust pos and offset to keep inside plot)
text(x = x_axis[1] + diff(x_axis) * 0.05,  # 5% from left edge
     y = min_ess_m1, 
     labels = round(min_ess_m1, 0),  # 0 decimal places for ESS
     pos = 1, cex = 0.7, col = "black")

text(x = max_rhat_m1, 
     y = y_axis[2] - diff(y_axis) * 0.05,  # 5% from top edge
     labels = sprintf("%.4f", max_rhat_m1),  # 4 decimal places for R-hat
     pos = 4, cex = 0.7, col = "black")  # pos=2 puts text to the LEFT

# Model 2
plot(m2_convergence_diagnostics$rhat, m2_convergence_diagnostics$ess_bulk, 
     xlim = x_axis, ylim = y_axis,
     main = "Model 2", ylab = "ESS", xlab = "R-hat")

min_ess_m2 <- min(m2_convergence_diagnostics$ess_bulk)
max_rhat_m2 <- max(m2_convergence_diagnostics$rhat)

abline(h = min_ess_m2, lty = 2)
abline(v = max_rhat_m2, lty = 2)

# Add labels
text(x = x_axis[1] + diff(x_axis) * 0.05,
     y = min_ess_m2, 
     labels = round(min_ess_m2, 0),
     pos = 3, cex = 0.7, col = "black")

text(x = max_rhat_m2, 
     y = y_axis[2] - diff(y_axis) * 0.05,
     labels = round(max_rhat_m2, 4),
     pos = 2, cex = 0.7, col = "black")
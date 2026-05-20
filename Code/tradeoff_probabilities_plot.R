library(ggplot2)
library(dplyr)
library(tidyr)
library(here)

# LOAD DATA
trade <- read.csv(here("CSV", "tradeoff_probabilities.csv"))

plot_data <- trade %>%
  filter(!proportion_id %in% c("Exp_yrs", "Hrs_Sport", "Mass", "Mass.Stature", 
                               "ForearmC", "ForearmC.ForearmL", "ForearmC.Hrs_Total", 
                               "ForearmC.ForearmL.Hrs_Total",
                               "Triceps", "Triceps.Hrs_Total")) %>%
  mutate(dataset_comp = paste(dataset, comparison, sep = "_"),
         control_type = ifelse(control == "absolute", "absolute", "relative"))

female_data <- plot_data %>% filter(sex == "F")
male_data   <- plot_data %>% filter(sex == "M")

groups       <- c("m2_climb_vs_walk", "m2_climb_vs_sprint", "m2_climb_vs_run", "m1_climb_vs_run")
group_labels <- c("M2: Walk", "M2: Sprint", "M2: Run", "M1: Run")
n_groups     <- length(groups)

col_abs <- "#E41A1C"
col_rel <- "#377EB8"

add_mean_triangle <- function(vals, y_pos, color) {
  if (length(vals) > 0) {
    m <- mean(vals, na.rm = TRUE)
    points(m, y_pos, pch = 24, col = color, bg = NA, cex = 1.4, lwd = 1.5)
  }
}

# Helper to add corner labels to the current plot
add_corner_labels <- function() {
  usr <- par("usr")
  text(x = usr[1] + 0.02, y = usr[3] + 0.08, labels = "synergy",
       adj = c(0, 0), cex = 0.8, font = 3, col = "gray30")
  text(x = usr[2] - 0.02, y = usr[3] + 0.08, labels = "trade-off",
       adj = c(1, 0), cex = 0.8, font = 3, col = "gray30")
}

layout(matrix(c(1, 2, 3,
                4, 4, 4), nrow = 2, byrow = TRUE),
       widths  = c(4, 1, 4),
       heights = c(4, 0.5))

# ============ FEMALE PLOT (left) ============
par(mar = c(4, 1, 3, 0.5))

plot(NULL, ylim = c(0.5, n_groups + 0.5),
     xlim = c(0, 1),
     ylab = "", xlab = "p(trade-off)",
     main = "Female", yaxt = "n", xaxt = "n")

usr <- par("usr")
rect(usr[1], usr[3], 0.5, usr[4], col = col.alpha("green", 0.1), border = NA, xpd = FALSE)
rect(0.5,    usr[3], usr[2], usr[4], col = col.alpha("red",   0.1), border = NA, xpd = FALSE)
box()
abline(v = 0.5, lty = 2, col = "gray50")
axis(1, at = seq(0, 1, 0.25), labels = seq(0, 1, 0.25))
axis(4, at = 1:n_groups, labels = FALSE, tcl = -0.3)
add_corner_labels()

for (i in 1:n_groups) {
  abs_data <- female_data$p_tradeoff[female_data$dataset_comp == groups[i] &
                                       female_data$control_type == "absolute"]
  rel_data <- female_data$p_tradeoff[female_data$dataset_comp == groups[i] &
                                       female_data$control_type == "relative"]
  
  if (length(abs_data) > 0)
    points(abs_data, rep(i + 0.15, length(abs_data)) + rnorm(length(abs_data), 0, 0.02),
           pch = 16, col = col.alpha(col_abs, 0.6), cex = 0.8)
  if (length(rel_data) > 0)
    points(rel_data, rep(i - 0.15, length(rel_data)) + rnorm(length(rel_data), 0, 0.02),
           pch = 16, col = col.alpha(col_rel, 0.6), cex = 0.8)
  
  add_mean_triangle(abs_data, i + 0.15, col_abs)
  add_mean_triangle(rel_data, i - 0.15, col_rel)
}

# ============ CENTRE PANEL — shared y-axis labels ============
par(mar = c(4, 0, 3, 0))

plot(NULL, xlim = c(0, 1), ylim = c(0.5, n_groups + 0.5),
     xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")

text(x = 0.5, y = 1:n_groups, labels = group_labels,
     cex = 0.95, adj = 0.5, xpd = TRUE)

# ============ MALE PLOT (right) ============
par(mar = c(4, 0.5, 3, 1))

plot(NULL, ylim = c(0.5, n_groups + 0.5),
     xlim = c(0, 1),
     ylab = "", xlab = "p(trade-off)",
     main = "Male", yaxt = "n", xaxt = "n")

usr <- par("usr")
rect(usr[1], usr[3], 0.5, usr[4], col = col.alpha("green", 0.1), border = NA, xpd = FALSE)
rect(0.5,    usr[3], usr[2], usr[4], col = col.alpha("red",   0.1), border = NA, xpd = FALSE)
box()
abline(v = 0.5, lty = 2, col = "gray50")
axis(1, at = seq(0, 1, 0.25), labels = seq(0, 1, 0.25))
axis(2, at = 1:n_groups, labels = FALSE, tcl = -0.3)
add_corner_labels()

for (i in 1:n_groups) {
  abs_data <- male_data$p_tradeoff[male_data$dataset_comp == groups[i] &
                                     male_data$control_type == "absolute"]
  rel_data <- male_data$p_tradeoff[male_data$dataset_comp == groups[i] &
                                     male_data$control_type == "relative"]
  
  if (length(abs_data) > 0)
    points(abs_data, rep(i + 0.15, length(abs_data)) + rnorm(length(abs_data), 0, 0.02),
           pch = 16, col = col.alpha(col_abs, 0.6), cex = 0.8)
  if (length(rel_data) > 0)
    points(rel_data, rep(i - 0.15, length(rel_data)) + rnorm(length(rel_data), 0, 0.02),
           pch = 16, col = col.alpha(col_rel, 0.6), cex = 0.8)
  
  add_mean_triangle(abs_data, i + 0.15, col_abs)
  add_mean_triangle(rel_data, i - 0.15, col_rel)
}

# ============ LEGEND ============
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = c("Absolute", "Relative", "Mean"),
       col    = c(col_abs, col_rel, "black"),
       pch    = c(16, 16, 24),
       pt.bg  = c(NA, NA, NA),
       pt.cex = c(1, 1, 1.4),
       cex    = 1, bty = "n", horiz = TRUE)

par(mfrow = c(1, 1))
layout(1)
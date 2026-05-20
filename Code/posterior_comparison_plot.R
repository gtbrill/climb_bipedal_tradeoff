library(here)

comp <- read.csv(here("CSV", "posterior_comparisons.csv"))
comp=comp[1:76,]

point_size <- 2

group_colors <- c(
  "Size/Shape" = "black",
  "Breadths"     = "red",
  "Upper"        = "#2297E6",
  "Lower"        = "darkviolet",
  "Extremities"  = "#F5C710"
)

get_group_color <- function(group_name) {
  if (is.na(group_name)) return("black")
  if (group_name %in% names(group_colors)) return(as.character(group_colors[group_name]))
  return("black")
}

add_corner_labels <- function(sport_label) {
  usr <- par("usr")
  x1 <- usr[1]; x2 <- usr[2]
  y1 <- usr[3]; y2 <- usr[4]
  x_inset <- (x2 - x1) * 0.04
  y_inset <- (y2 - y1) * 0.04
  cex_lab <- 0.65
  col_lab <- "black"
  
  text(x2 - x_inset, y2 - y_inset,
       paste0("synergy:\nincreased trait is good for \nclimb and ", tolower(sport_label), " performance"),
       adj = c(1, 1), cex = cex_lab, col = col_lab)
  text(x1 + x_inset, y2 - y_inset,
       paste0("trade-off:\nincreased trait is good for climb \nand bad for ", tolower(sport_label), " performance"),
       adj = c(0, 1), cex = cex_lab, col = col_lab)
  text(x1 + x_inset, y1 + y_inset,
       paste0("synergy:\nincreased trait is bad for \nclimb and ", tolower(sport_label), " performance"),
       adj = c(0, 0), cex = cex_lab, col = col_lab)
  text(x2 - x_inset, y1 + y_inset,
       paste0("trade-off:\nincreased trait is bad for climb \nand good for ", tolower(sport_label), " performance"),
       adj = c(1, 0), cex = cex_lab, col = col_lab)
}

# ============================================================================
# LEGEND — saved separately
# ============================================================================

png(here("Figures", "Figures 2, S13-16", "legend.png"),
    width = 2400, height = 300, res = 300)

par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = names(group_colors),
       col    = as.character(group_colors),
       pch    = 16, cex = 0.8, bty = "n", horiz = TRUE)

dev.off()

# ============================================================================
# PLOT 1: MODEL 1 - CLIMB VS RUN (2x2: Female/Male x Type1/Type2)
# ============================================================================

comp_m1      <- comp[comp$Model == "Model1", ]
unique_types <- unique(comp_m1$Type)

png(here("Figures", "Figures 2, S13-16", "model1_climb_vs_run.png"),
    width = 2743, height = 2400, res = 300)

par(mfrow = c(2, 2), oma = c(0, 5, 4, 0), mar = c(4, 4, 1, 1))

plot_counter <- 0

for (type_val in unique_types) {
  for (sex_val in c("F", "M")) {
    
    plot_counter <- plot_counter + 1
    
    plot_data <- comp_m1[comp_m1$Type == type_val, ]
    
    run_col     <- paste0("Run", sex_val)
    climb_col   <- paste0("Climb", sex_val)
    run_l_col   <- paste0("Run", sex_val, ".L")
    run_u_col   <- paste0("Run", sex_val, ".U")
    climb_l_col <- paste0("Climb", sex_val, ".L")
    climb_u_col <- paste0("Climb", sex_val, ".U")
    
    xlim <- c(-0.75, 0.75)
    ylim <- c(-0.75, 0.75)
    
    plot(plot_data[[run_col]], plot_data[[climb_col]],
         type = "n", xlab = "Run", ylab = "Climb", main = "",
         xlim = xlim, ylim = ylim, asp = 1)
    
    usr <- par("usr")
    
    rect(usr[1], 0,      0,      usr[4], col = col.alpha("red",   0.1), border = NA)
    rect(0,      0,      usr[2], usr[4], col = col.alpha("green", 0.1), border = NA)
    rect(usr[1], usr[3], 0,      0,      col = col.alpha("green", 0.1), border = NA)
    rect(0,      usr[3], usr[2], 0,      col = col.alpha("red",   0.1), border = NA)
    
    box()
    
    abline(h = 0, v = 0, col = "gray75", lty = 1, lwd = 0.75)
  #  abline(a = 0, b = 1, lty = 2, col = "gray")
    
    for (i in 1:nrow(plot_data)) {
      group_col <- get_group_color(plot_data$Group[i])
      
      arrows(plot_data[[run_col]][i],   plot_data[[climb_l_col]][i],
             plot_data[[run_col]][i],   plot_data[[climb_u_col]][i],
             length = 0.03, angle = 90, code = 3,
             col = adjustcolor(group_col, alpha.f = 0.6), lwd = 0.8)
      
      arrows(plot_data[[run_l_col]][i], plot_data[[climb_col]][i],
             plot_data[[run_u_col]][i], plot_data[[climb_col]][i],
             length = 0.03, angle = 90, code = 3,
             col = adjustcolor(group_col, alpha.f = 0.6), lwd = 0.8)
      
      points(plot_data[[run_col]][i], plot_data[[climb_col]][i],
             col = adjustcolor(group_col, alpha.f = 0.75), pch = 16, cex = point_size)
    }
    
    add_corner_labels("Run")
    
    if (plot_counter == 1) mtext("Female",        side = 3, outer = FALSE, line = 1.5, font = 2, cex = 1)
    if (plot_counter == 2) mtext("Male",          side = 3, outer = FALSE, line = 1.5, font = 2, cex = 1)
    if (plot_counter == 1) mtext(unique_types[1], side = 2, outer = TRUE,  line = 1,   font = 2, cex = 1, las = 3, at = 0.78)
    if (plot_counter == 3) mtext(unique_types[2], side = 2, outer = TRUE,  line = 1,   font = 2, cex = 1, las = 3, at = 0.28)
  }
}

dev.off()

# ============================================================================
# PLOT 2: MODEL 2 - CLIMB VS RUN (2x2: Female/Male x Type1/Type2)
# ============================================================================

comp_m2         <- comp[comp$Model == "Model2", ]
unique_types_m2 <- unique(comp_m2$Type)

png(here("Figures", "Figures 2, S13-16", "model2_climb_vs_run.png"),
    width = 2743, height = 2400, res = 300)

par(mfrow = c(2, 2), oma = c(0, 5, 4, 0), mar = c(4, 4, 1, 1))

plot_counter <- 0

for (type_val in unique_types_m2) {
  for (sex_val in c("F", "M")) {
    
    plot_counter <- plot_counter + 1
    
    plot_data <- comp_m2[comp_m2$Type == type_val, ]
    
    run_col     <- paste0("Run", sex_val)
    climb_col   <- paste0("Climb", sex_val)
    run_l_col   <- paste0("Run", sex_val, ".L")
    run_u_col   <- paste0("Run", sex_val, ".U")
    climb_l_col <- paste0("Climb", sex_val, ".L")
    climb_u_col <- paste0("Climb", sex_val, ".U")
    
    xlim <- c(-1.2, 1.2)
    ylim <- c(-1.2, 1.2)
    
    plot(plot_data[[run_col]], plot_data[[climb_col]],
         type = "n", xlab = "Run", ylab = "Climb", main = "",
         xlim = xlim, ylim = ylim, asp = 1)
    
    usr <- par("usr")
    
    rect(usr[1], 0,      0,      usr[4], col = col.alpha("red",   0.1), border = NA)
    rect(0,      0,      usr[2], usr[4], col = col.alpha("green", 0.1), border = NA)
    rect(usr[1], usr[3], 0,      0,      col = col.alpha("green", 0.1), border = NA)
    rect(0,      usr[3], usr[2], 0,      col = col.alpha("red",   0.1), border = NA)
    
    box()
    
    abline(h = 0, v = 0, col = "gray75", lty = 1, lwd = 0.75)
  #  abline(a = 0, b = 1, lty = 2, col = "gray")
    
    for (i in 1:nrow(plot_data)) {
      group_col <- get_group_color(plot_data$Group[i])
      
      arrows(plot_data[[run_col]][i],   plot_data[[climb_l_col]][i],
             plot_data[[run_col]][i],   plot_data[[climb_u_col]][i],
             length = 0.03, angle = 90, code = 3,
             col = adjustcolor(group_col, alpha.f = 0.6), lwd = 0.8)
      
      arrows(plot_data[[run_l_col]][i], plot_data[[climb_col]][i],
             plot_data[[run_u_col]][i], plot_data[[climb_col]][i],
             length = 0.03, angle = 90, code = 3,
             col = adjustcolor(group_col, alpha.f = 0.6), lwd = 0.8)
      
      points(plot_data[[run_col]][i], plot_data[[climb_col]][i],
             col = adjustcolor(group_col, alpha.f = 0.75), pch = 16, cex = point_size)
    }
    
    add_corner_labels("Run")
    
    if (plot_counter == 1) mtext("Female",           side = 3, outer = FALSE, line = 1.5, font = 2, cex = 1)
    if (plot_counter == 2) mtext("Male",             side = 3, outer = FALSE, line = 1.5, font = 2, cex = 1)
    if (plot_counter == 1) mtext(unique_types_m2[1], side = 2, outer = TRUE,  line = 1,   font = 2, cex = 1, las = 3, at = 0.78)
    if (plot_counter == 3) mtext(unique_types_m2[2], side = 2, outer = TRUE,  line = 1,   font = 2, cex = 1, las = 3, at = 0.28)
  }
}

dev.off()

# ============================================================================
# PLOT 3: MODEL 2 - CLIMB VS SPRINT (2x2: Female/Male x Type1/Type2)
# ============================================================================

png(here("Figures", "Figures 2, S13-16", "model2_climb_vs_sprint.png"),
    width = 2743, height = 2400, res = 300)

par(mfrow = c(2, 2), oma = c(0, 5, 4, 0), mar = c(4, 4, 1, 1))

plot_counter <- 0

for (type_val in unique_types_m2) {
  for (sex_val in c("F", "M")) {
    
    plot_counter <- plot_counter + 1
    
    plot_data <- comp_m2[comp_m2$Type == type_val, ]
    
    sprint_col   <- paste0("Sprint", sex_val)
    climb_col    <- paste0("Climb", sex_val)
    sprint_l_col <- paste0("Sprint", sex_val, ".L")
    sprint_u_col <- paste0("Sprint", sex_val, ".U")
    climb_l_col  <- paste0("Climb", sex_val, ".L")
    climb_u_col  <- paste0("Climb", sex_val, ".U")
    
    xlim <- c(-1.2, 1.2)
    ylim <- c(-1.2, 1.2)
    
    plot(plot_data[[sprint_col]], plot_data[[climb_col]],
         type = "n", xlab = "Sprint", ylab = "Climb", main = "",
         xlim = xlim, ylim = ylim, asp = 1)
    
    usr <- par("usr")
    
    rect(usr[1], 0,      0,      usr[4], col = col.alpha("red",   0.1), border = NA)
    rect(0,      0,      usr[2], usr[4], col = col.alpha("green", 0.1), border = NA)
    rect(usr[1], usr[3], 0,      0,      col = col.alpha("green", 0.1), border = NA)
    rect(0,      usr[3], usr[2], 0,      col = col.alpha("red",   0.1), border = NA)
    
    box()
    
    abline(h = 0, v = 0, col = "gray75", lty = 1, lwd = 0.75)
  #  abline(a = 0, b = 1, lty = 2, col = "gray")
    
    for (i in 1:nrow(plot_data)) {
      group_col <- get_group_color(plot_data$Group[i])
      
      arrows(plot_data[[sprint_col]][i],  plot_data[[climb_l_col]][i],
             plot_data[[sprint_col]][i],  plot_data[[climb_u_col]][i],
             length = 0.03, angle = 90, code = 3,
             col = adjustcolor(group_col, alpha.f = 0.6), lwd = 0.8)
      
      arrows(plot_data[[sprint_l_col]][i], plot_data[[climb_col]][i],
             plot_data[[sprint_u_col]][i], plot_data[[climb_col]][i],
             length = 0.03, angle = 90, code = 3,
             col = adjustcolor(group_col, alpha.f = 0.6), lwd = 0.8)
      
      points(plot_data[[sprint_col]][i], plot_data[[climb_col]][i],
             col = adjustcolor(group_col, alpha.f = 0.75), pch = 16, cex = point_size)
    }
    
    add_corner_labels("Sprint")
    
    if (plot_counter == 1) mtext("Female",           side = 3, outer = FALSE, line = 1.5, font = 2, cex = 1)
    if (plot_counter == 2) mtext("Male",             side = 3, outer = FALSE, line = 1.5, font = 2, cex = 1)
    if (plot_counter == 1) mtext(unique_types_m2[1], side = 2, outer = TRUE,  line = 1,   font = 2, cex = 1, las = 3, at = 0.78)
    if (plot_counter == 3) mtext(unique_types_m2[2], side = 2, outer = TRUE,  line = 1,   font = 2, cex = 1, las = 3, at = 0.28)
  }
}

dev.off()

# ============================================================================
# PLOT 4: MODEL 2 - CLIMB VS WALK (2x2: Female/Male x Type1/Type2)
# ============================================================================

png(here("Figures", "Figures 2, S13-16", "model2_climb_vs_walk.png"),
    width = 2743, height = 2400, res = 300)

par(mfrow = c(2, 2), oma = c(0, 5, 4, 0), mar = c(4, 4, 1, 1))

plot_counter <- 0

for (type_val in unique_types_m2) {
  for (sex_val in c("F", "M")) {
    
    plot_counter <- plot_counter + 1
    
    plot_data <- comp_m2[comp_m2$Type == type_val, ]
    
    walk_col    <- paste0("Walk", sex_val)
    climb_col   <- paste0("Climb", sex_val)
    walk_l_col  <- paste0("Walk", sex_val, ".L")
    walk_u_col  <- paste0("Walk", sex_val, ".U")
    climb_l_col <- paste0("Climb", sex_val, ".L")
    climb_u_col <- paste0("Climb", sex_val, ".U")
    
    xlim <- c(-1.2, 1.2)
    ylim <- c(-1.2, 1.2)
    
    plot(plot_data[[walk_col]], plot_data[[climb_col]],
         type = "n", xlab = "Walk", ylab = "Climb", main = "",
         xlim = xlim, ylim = ylim, asp = 1)
    
    usr <- par("usr")
    
    rect(usr[1], 0,      0,      usr[4], col = col.alpha("red",   0.1), border = NA)
    rect(0,      0,      usr[2], usr[4], col = col.alpha("green", 0.1), border = NA)
    rect(usr[1], usr[3], 0,      0,      col = col.alpha("green", 0.1), border = NA)
    rect(0,      usr[3], usr[2], 0,      col = col.alpha("red",   0.1), border = NA)
    
    box()
    
    abline(h = 0, v = 0, col = "gray75", lty = 1, lwd = 0.75)
 #   abline(a = 0, b = 1, lty = 2, col = "gray")
    
    for (i in 1:nrow(plot_data)) {
      group_col <- get_group_color(plot_data$Group[i])
      
      arrows(plot_data[[walk_col]][i],  plot_data[[climb_l_col]][i],
             plot_data[[walk_col]][i],  plot_data[[climb_u_col]][i],
             length = 0.03, angle = 90, code = 3,
             col = adjustcolor(group_col, alpha.f = 0.6), lwd = 0.8)
      
      arrows(plot_data[[walk_l_col]][i], plot_data[[climb_col]][i],
             plot_data[[walk_u_col]][i], plot_data[[climb_col]][i],
             length = 0.03, angle = 90, code = 3,
             col = adjustcolor(group_col, alpha.f = 0.6), lwd = 0.8)
      
      points(plot_data[[walk_col]][i], plot_data[[climb_col]][i],
             col = adjustcolor(group_col, alpha.f = 0.75), pch = 16, cex = point_size)
    }
    
    add_corner_labels("Walk")
    
    if (plot_counter == 1) mtext("Female",           side = 3, outer = FALSE, line = 1.5, font = 2, cex = 1)
    if (plot_counter == 2) mtext("Male",             side = 3, outer = FALSE, line = 1.5, font = 2, cex = 1)
    if (plot_counter == 1) mtext(unique_types_m2[1], side = 2, outer = TRUE,  line = 1,   font = 2, cex = 1, las = 3, at = 0.78)
    if (plot_counter == 3) mtext(unique_types_m2[2], side = 2, outer = TRUE,  line = 1,   font = 2, cex = 1, las = 3, at = 0.28)
  }
}

dev.off()

par(mfrow = c(1, 1))
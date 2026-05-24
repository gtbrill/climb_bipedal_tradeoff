library(ggplot2)
library(dplyr)
library(ggrepel)
library(here)

fossil <- read.csv(here("CSV", "fossil_data.csv"))

shape_map <- c("F" = 16, "M" = 17, "Unknown" = 15)

subcat_colours <- c(
  "Late Pleistocene H. sapiens" = "grey50",
  "Homo"                        = "#6baed6",
  "Australopithecus"            = "#41ab5d",
  "Paranthropus"                = "#8c510a",
  "Ardipithecus"                = "#FFD700"
)

# Recode sex
fossil <- fossil %>%
  mutate(Sex = case_when(
    toupper(trimws(Sex)) == "F" ~ "F",
    toupper(trimws(Sex)) == "M" ~ "M",
    TRUE                        ~ "Unknown"
  )) %>%
  mutate(Subcategory = recode(Subcategory,
                              "Late Pleist H. sapiens" = "Late Pleistocene H. sapiens"))

# ── Data subsets ─────────────────────────────────────────────
study_sample <- fossil %>%
  filter(Category == "Study Sample", !is.na(Mass), !is.na(Stature))

other_pts <- fossil %>%
  filter(Category != "Study Sample", Species != "Undefined",
         Subcategory != "Late Pleistocene H. sapiens",
         !is.na(Mass), !is.na(Stature)) %>%
  mutate(HullGroup = ifelse(Subcategory %in% c("Australopithecus", "Paranthropus"),
                            Subcategory, Species))

# ── Convex hulls ─────────────────────────────────────────────
hull_list <- lapply(split(other_pts, other_pts$HullGroup), function(d) {
  if (nrow(d) < 3) return(NULL)
  idx <- chull(d$Mass, d$Stature)
  d[idx, ]
})
hulls <- bind_rows(hull_list)
row.names(hulls) <- NULL

# Map HullGroup → fill colour via Subcategory
hull_colours <- other_pts %>%
  distinct(HullGroup, Subcategory) %>%
  mutate(col = subcat_colours[Subcategory]) %>%
  { setNames(.$col, .$HullGroup) }

# ── Labels ───────────────────────────────────────────────────
hull_labels <- other_pts %>%
  group_by(HullGroup, Subcategory) %>%
  filter(n() >= 3) %>%
  summarise(x = mean(Mass),
            y = max(Stature),
            .groups = "drop")

# Manual label positions
label_nudges <- tribble(
  ~HullGroup,              ~nudge_x,  ~nudge_y,
  "H. sapiens",                 2,       50,
  "H. neanderthalensis",        5,      -250,
  "H. erectus",                -29,      -30,
  "H. habilis",                 0,      -280,
  "Australopithecus",          -33,     -300,
  "Paranthropus",              -24,     -100,
  "Ar. ramidus",                5,      -130,
  "H. floresiensis",            2,       -50,
  "H. antecessor",             13,       230
)

hull_labels <- hull_labels %>%
  left_join(label_nudges, by = "HullGroup")

individual_labels <- other_pts %>%
  group_by(HullGroup) %>%
  filter(n() < 3) %>%
  ungroup() %>%
  left_join(label_nudges, by = "HullGroup")

# Apply italic expressions to label columns
hull_labels <- hull_labels %>%
  mutate(label_expr = paste0("italic('", HullGroup, "')"))

individual_labels <- individual_labels %>%
  mutate(label_expr = paste0("italic('", HullGroup, "')"))

# ── Late Pleistocene hull ─────────────────────────────────────
late_pleist_pts <- fossil %>%
  filter(Subcategory == "Late Pleistocene H. sapiens", !is.na(Mass), !is.na(Stature))

late_pleist_hull <- if (nrow(late_pleist_pts) >= 3) {
  idx <- chull(late_pleist_pts$Mass, late_pleist_pts$Stature)
  late_pleist_pts[idx, ]
} else late_pleist_pts

# "Late Pleistocene" plain, "H. sapiens" italic
late_pleist_label <- data.frame(
  x     = mean(late_pleist_pts$Mass) - 28,
  y     = max(late_pleist_pts$Stature) - 20,
  label = "\"Late Pleistocene\"~italic('H. sapiens')"
)

# ── Legend labels: LP H. sapiens with only H. sapiens italic ─
legend_labels <- c(
  "Late Pleistocene H. sapiens" = expression("LP " * italic("H. sapiens")),
  "Homo"                        = "Homo",
  "Australopithecus"            = expression(italic("Australopithecus")),
  "Paranthropus"                = expression(italic("Paranthropus")),
  "Ardipithecus"                = expression(italic("Ardipithecus")),
  "Study Sample"                = "Study Sample"
)

# ── Plot ─────────────────────────────────────────────────────
ggplot() +
  # Late Pleistocene hull: shaded only, no border
  geom_polygon(data = late_pleist_hull,
               aes(x = Mass, y = Stature),
               fill = "grey50", alpha = 0.15, colour = NA) +
  geom_text(data = late_pleist_label,
            aes(x = x, y = y, label = label),
            colour = "grey50", size = 3.5, hjust = 0.5,
            parse = TRUE,
            show.legend = FALSE) +
  # Other hulls: dashed border
  geom_polygon(data = hulls,
               aes(x = Mass, y = Stature,
                   group = HullGroup, fill = HullGroup),
               alpha = 0.15, colour = "grey40",
               linetype = "dashed", linewidth = 0.4) +
  # Dummy invisible point to pull Late Pleistocene into colour legend
  geom_point(data = late_pleist_pts[1, ],
             aes(x = Mass, y = Stature,
                 colour = "Late Pleistocene H. sapiens"),
             size = 2, alpha = 0.8) +
  geom_point(data = study_sample,
             aes(x = Mass, y = Stature, colour = "Study Sample"),
             size = 1, alpha = 0.5) +
  geom_point(data = other_pts,
             aes(x = Mass, y = Stature, colour = Subcategory),
             size = 2, alpha = 0.8) +
  # Leader lines for individual labels
  geom_segment(data = individual_labels,
               aes(x = Mass, y = Stature,
                   xend = Mass + nudge_x, yend = Stature + nudge_y,
                   colour = Subcategory),
               linewidth = 0.3, alpha = 0.6,
               show.legend = FALSE) +
  geom_text(data = individual_labels,
            aes(x = Mass + nudge_x, y = Stature + nudge_y,
                label = label_expr, colour = Subcategory),
            size = 3.5, hjust = 0, parse = TRUE,
            show.legend = FALSE) +
  geom_text(data = hull_labels,
            aes(x = x + nudge_x, y = y + nudge_y,
                label = label_expr, colour = Subcategory),
            size = 3.5, hjust = 0, parse = TRUE,
            show.legend = FALSE) +
  scale_fill_manual(values = hull_colours, guide = "none") +
  scale_colour_manual(
    values = c(subcat_colours, "Study Sample" = "black"),
    labels = legend_labels,
    breaks = c("Ardipithecus", "Australopithecus", "Paranthropus", "Homo", "Late Pleistocene H. sapiens", "Study Sample"),
    name   = "Group",
    na.value = "grey70"
  ) +
  coord_cartesian(xlim = c(0, 125), ylim = c(0, 2000)) +
  labs(x = "Mass (kg)", y = "Stature (mm)") +
  theme_classic(base_size = 11)

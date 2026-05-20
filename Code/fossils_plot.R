library(ggplot2)
library(dplyr)
library(ggrepel)
library(here)

fossil <- read.csv(here("CSV", "fossil_data.csv"))

shape_map <- c("F" = 16, "M" = 17, "Unknown" = 15)

subcat_colours <- c(
  "Late Pleist H. sapiens" = "grey50",
  "Homo"                   = "#6baed6",
  "Australopithicus"       = "#41ab5d",
  "Paranthropus"           = "#8c510a",
  "Ardipithecus"           = "#FFD700"    # yellow
)

# Recode sex
fossil <- fossil %>%
  mutate(Sex = case_when(
    toupper(trimws(Sex)) == "F" ~ "F",
    toupper(trimws(Sex)) == "M" ~ "M",
    TRUE                        ~ "Unknown"
  ))

# ── Data subsets ─────────────────────────────────────────────
study_sample <- fossil %>%
  filter(Category == "Study Sample", !is.na(Mass), !is.na(Stature))

other_pts <- fossil %>%
  filter(Category != "Study Sample", Species != "Undefined",
         Subcategory != "Late Pleist H. sapiens",
         !is.na(Mass), !is.na(Stature)) %>%
  mutate(HullGroup = ifelse(Subcategory %in% c("Australopithicus", "Paranthropus"),
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
# Hulled groups: anchor label at topmost point of each hull group
hull_labels <- other_pts %>%
  group_by(HullGroup, Subcategory) %>%
  filter(n() >= 3) %>%
  summarise(x = mean(Mass),
            y = max(Stature),
            .groups = "drop")

# Manual label positions — tweak nudge_x and nudge_y to reposition
label_nudges <- tribble(
  ~HullGroup,              ~nudge_x,  ~nudge_y,
  "H. sapiens",                 2,       50,
  "H. neanderthalensis",       5,      -250,
  "H. erectus",                 -29,       -30,
  "H. habilis",                 0,       -280,
  "Australopithicus",          -33,      -300,
  "Paranthropus",               -24,       -100,
  "A. ramidus",                 5,       -130,
  "H. floresiensis",             2,       -50,
  "H. antecessor",               13,       230
)

hull_labels <- hull_labels %>%
  left_join(label_nudges, by = "HullGroup")

# Non-hulled: label each point individually with manual nudges
individual_labels <- other_pts %>%
  group_by(HullGroup) %>%
  filter(n() < 3) %>%
  ungroup() %>%
  left_join(label_nudges, by = "HullGroup")

# ── Late Pleistocene hull (no points plotted, no border) ─────
late_pleist_pts <- fossil %>%
  filter(Subcategory == "Late Pleist H. sapiens", !is.na(Mass), !is.na(Stature))

late_pleist_hull <- if (nrow(late_pleist_pts) >= 3) {
  idx <- chull(late_pleist_pts$Mass, late_pleist_pts$Stature)
  late_pleist_pts[idx, ]
} else late_pleist_pts

late_pleist_label <- data.frame(
  x     = mean(late_pleist_pts$Mass) - 22,
  y     = max(late_pleist_pts$Stature) - 20,
  label = "Late Pleist H. sapiens"
)

# ── Plot ─────────────────────────────────────────────────────
ggplot() +
  # Late Pleist hull: shaded only, no border
  geom_polygon(data = late_pleist_hull,
               aes(x = Mass, y = Stature),
               fill = "grey50", alpha = 0.15, colour = NA) +
  geom_text(data = late_pleist_label,
            aes(x = x, y = y, label = label),
            colour = "grey50", size = 3.5, hjust = 0.5,
            show.legend = FALSE) +
  # Other hulls: dashed border
  geom_polygon(data = hulls,
               aes(x = Mass, y = Stature,
                   group = HullGroup, fill = HullGroup),
               alpha = 0.15, colour = "grey40",
               linetype = "dashed", linewidth = 0.4) +
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
                label = HullGroup, colour = Subcategory),
            size = 3.5, hjust = 0, show.legend = FALSE) +
  geom_text(data = hull_labels,
            aes(x = x + nudge_x, y = y + nudge_y,
                label = HullGroup, colour = Subcategory),
            size = 3.5, hjust = 0, show.legend = FALSE) +
  scale_fill_manual(values = hull_colours, guide = "none") +
  scale_colour_manual(values = c(subcat_colours, "Study Sample" = "black"),
                      name = "Group",
                      na.value = "grey70") +
  coord_cartesian(xlim = c(0, 125), ylim = c(0, 2000)) +
  labs(x = "Mass (kg)", y = "Stature (mm)") +
  theme_classic(base_size = 11)
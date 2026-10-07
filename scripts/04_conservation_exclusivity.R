
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(patchwork)
library(rphylopic)
library(sysfonts)
library(showtext)

# Register a FontAwesome "paw" glyph (U+F1B0) to use as a real footprint icon
# in panel A, and enable showtext so it renders both on-screen and when saved.
fa_ttf <- file.path(system.file(package = "fontawesome"), "fontawesome/webfonts/fa-solid-900.ttf")
font_add("fa-solid", fa_ttf)
showtext_auto()
showtext_opts(dpi = 300)
paw_glyph <- "\uf1b0"

# --- 1. LOAD DATA ---
if(!file.exists("outputs/famd_core_results.RData")) {
  stop("Error: Run 'scripts/01_famd_core.R' first.")
}
load("outputs/famd_core_results.RData")

pc1 <- round(res_famd$eig[1, 2], 1)
pc2 <- round(res_famd$eig[2, 2], 1)
x_lims <- c(min(df_master$Dim1) - 1, max(df_master$Dim1) + 1)
y_lims <- c(min(df_master$Dim2) - 1, max(df_master$Dim2) + 1)
dist_mat <- as.matrix(dist(df_master[, c("Dim1", "Dim2")]))

# --- 2. PREPARE SCENARIO & EXCLUSIVITY DATA ---
df_scenarios <- df_master %>%
  select(spp, Dim1, Dim2, natural_counterfactual, present, abandonment, is_domestic) %>%
  pivot_longer(cols = c(natural_counterfactual, present, abandonment), 
               names_to = "Scenario", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(Scenario = factor(Scenario, 
                          levels = c("natural_counterfactual", "present", "abandonment"),
                          labels = c("Natural Counterfactual", "Present", "Abandonment")))

df_exclusive <- df_master %>%
  mutate(Exclusivity = case_when(
    natural_counterfactual == 1 & present == 0 ~ "Extinct (Natural Counterfactual Exclusive)",
    present == 1 & is_domestic == 1 ~ "Domestic (Livestock)",
    present == 1 & is_domestic == 0 ~ "Wild (Extant)",
    TRUE ~ "Other"
  )) %>%
  filter(Exclusivity != "Other") %>%
  mutate(Exclusivity = factor(Exclusivity, levels = c("Extinct (Natural Counterfactual Exclusive)", "Wild (Extant)", "Domestic (Livestock)")))


# --- 3. CALCULATE UNIQUENESS & PROXIES ---
# Functional Uniqueness (Current only)
df_master$Uniqueness <- sapply(1:nrow(df_master), function(i) {
  curr <- which(df_master$present == 1)
  if (df_master$present[i] == 1) curr <- curr[curr != i]
  min(dist_mat[i, curr])
})

# Proxies for Extinct Species
extinct_df <- df_master %>% filter(natural_counterfactual == 1 & present == 0)
proxies <- t(apply(extinct_df, 1, function(row) {
  orig_idx <- which(df_master$spp == row["spp"])
  curr <- which(df_master$present == 1)
  closest <- curr[which.min(dist_mat[orig_idx, curr])]
  return(c(df_master$is_domestic[closest], df_master$spp[closest], dist_mat[orig_idx, closest]))
}))
extinct_df$Proxy_Type <- ifelse(proxies[,1]=="1", "Domestic", "Wild")
extinct_df$Proxy_Spp <- proxies[,2]
extinct_df$Proxy_Dist <- as.numeric(proxies[,3])

# Shared species order for the extinct-species axis.
spp_levels <- extinct_df$spp[order(extinct_df$Proxy_Dist)]
extinct_df$spp_f <- factor(extinct_df$spp, levels = spp_levels)
extinct_df$spp_num <- as.numeric(extinct_df$spp_f)

# PhyloPic silhouette names: a couple of taxa in df_master aren't indexed
# under their exact binomial, so map them to the nearest available name.
# Used by both panel A (proxy silhouettes) and panel B (current community).
phylopic_name_map <- c(
  "Capra pyrenaica" = "Capra ibex",
  "Ovis orientalis aries" = "Ovis aries"
)
extinct_df$Proxy_phylo_name <- recode(extinct_df$Proxy_Spp, !!!phylopic_name_map)

# Layout along the (flipped) distance axis: a trail of footprints walks from
# 0 up to the actual functional distance; the proxy's silhouette sits right
# after the last print (as if the animal left the trail while walking
# towards its own icon); the name follows the icon. Icon height is fixed so
# the row layout stays predictable.
proxy_icon_height <- 0.5
gap_paw_to_icon <- 0.35
gap_icon_to_text <- proxy_icon_height * 1.6
extinct_df$icon_y <- extinct_df$Proxy_Dist + gap_paw_to_icon
extinct_df$text_y <- extinct_df$icon_y + gap_icon_to_text

# Build a trail of footprints from 0 to Proxy_Dist, one species per row of
# `extinct_df`. Longer distances get more prints; very short ones still get
# at least a single print at the endpoint. Prints alternate slightly left and
# right of the row's x position to suggest a walking gait, all pointing right
# (towards the proxy's icon).
make_pawprint_trail <- function(df, step = 0.13, spread = 0.05) {
  rows <- lapply(seq_len(nrow(df)), function(i) {
    d <- df$Proxy_Dist[i]
    n_steps <- max(1, round(d / step))
    step_centers <- seq(d / n_steps, d, length.out = n_steps)
    data.frame(
      spp_num = df$spp_num[i],
      Proxy_Type = df$Proxy_Type[i],
      x = df$spp_num[i] + rep(c(-spread, spread), length.out = n_steps),
      y = step_centers
    )
  })
  do.call(rbind, rows)
}
footprint_trail_df <- make_pawprint_trail(extinct_df)


# --- 4. VISUALIZATION ---

# A. Temporal Scenarios (Conservative Ellipses)
plot_scenario <- function(data, title, color) {
  n_points <- nrow(data)
  ggplot(data, aes(x = Dim1, y = Dim2)) +
    stat_ellipse(geom = "polygon", alpha = 0.15, fill = color, color = color, level = 0.8) +
    geom_point(aes(shape = as.factor(is_domestic)), color = color, size = 3, alpha = 0.7) +
    geom_text_repel(aes(label = spp), size = 3, max.overlaps = 10, fontface = "italic") +
    scale_shape_manual(values = c("0" = 16, "1" = 17), guide = "none") +
    labs(title = title, x = paste0("Dim 1 (", pc1, "%)"), y = paste0("Dim 2 (", pc2, "%)")) +
    theme_minimal(base_size = 12) + coord_cartesian(xlim = x_lims, ylim = y_lims)
}

p_lig <- plot_scenario(df_scenarios %>% filter(Scenario == "Natural Counterfactual"), "Natural Counterfactual", "#E41A1C")
p_pres <- plot_scenario(df_scenarios %>% filter(Scenario == "Present"), "Present Day", "#377EB8")
p_aban <- plot_scenario(df_scenarios %>% filter(Scenario == "Abandonment"), "Abandonment", "#4DAF4A")

# B. Exclusivity Analysis
title_theme <- theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

p_excl <- ggplot(df_exclusive, aes(x = Dim1, y = Dim2, color = Exclusivity, fill = Exclusivity)) +
  stat_ellipse(geom = "polygon", alpha = 0.1, color = NA,level=0.8) +
  geom_point(aes(shape = Exclusivity), size = 5, alpha = 0.9) +
  geom_text_repel(aes(label = spp), size = 4.5, max.overlaps = 15, fontface = "italic", show.legend = FALSE) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 15)) +
  labs(title = NULL,
       x = paste0("Dim 1 (", pc1, "%)"), y = paste0("Dim 2 (", pc2, "%)")) +
  theme_minimal(base_size = 16) + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  coord_cartesian(xlim = x_lims, ylim = y_lims)

# C. Uniqueness & Proxies
# Unify labels and factors for shared legend
df_master <- df_master %>%
  mutate(Type = ifelse(is_domestic == 1, "Domestic", "Wild"),
         label_uniqueness = ifelse(present == 1, 
                                   as.character(round(Uniqueness, 2)), 
                                   NA))

p_proxy <- ggplot(extinct_df, aes(x = spp_num)) +
  # A trail of footprints (FontAwesome paw glyph, rotated to point right)
  # walks from 0 to the functional distance -- as if the proxy animal left
  # the trail while walking towards its own silhouette, which sits right
  # after the last print.
  geom_text(data = footprint_trail_df, mapping = aes(x = x, y = y, color = Proxy_Type),
            label = paw_glyph, family = "fa-solid", angle = -90, size = 3,
            inherit.aes = FALSE) +
  geom_phylopic(aes(y = icon_y, name = Proxy_phylo_name, fill = Proxy_Type),
                height = proxy_icon_height, hjust = 0, alpha = 0.95) +
  geom_text(aes(y = text_y, label = Proxy_Spp), hjust = 0, size = 7, fontface = "italic") +
  coord_flip(ylim = c(0, 8)) + 
  scale_color_manual(values = c("Wild" = "#1B9E77", "Domestic" = "#D95F02"), labels = c("Wild", "Domestic")) +
  scale_fill_manual(values = c("Wild" = "#1B9E77", "Domestic" = "#D95F02")) +
  scale_x_continuous(breaks = seq_along(spp_levels), labels = spp_levels,
                      expand = expansion(add = 0.6)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0))) +
  labs(title = "A) Extinct Megafauna Substitution", 
       x = NULL, 
       y = "Functional Distance",
       color = "Type") +
  theme_minimal(base_size = 18) +
  theme(axis.text.y = element_text(face = "italic", size = 15),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  title_theme

df_master <- df_master %>%
  mutate(phylo_name = recode(spp, !!!phylopic_name_map))

p_lonely <- ggplot(df_master %>% filter(present == 1), aes(x = Dim1, y = Dim2)) +
  # Icon height scales with Uniqueness (geom_phylopic's `height` aesthetic,
  # rescaled via scale_height_continuous), so more functionally unique species
  # render as visibly larger silhouettes. The numeric label is kept alongside
  # as an exact readout.
  geom_phylopic(aes(name = phylo_name, fill = Type, height = Uniqueness), alpha = 0.8,
                show.legend = c(fill = FALSE, height = TRUE)) +
  geom_text_repel(data = df_master %>% filter(present == 1),
                  aes(label = label_uniqueness), color = "black",
                  size = 5.5, fontface = "bold", show.legend = FALSE,
                  box.padding = 0.6, point.padding = 0.15, min.segment.length = 0,
                  force = 3, max.overlaps = Inf, seed = 7419) +
  scale_fill_manual(values = c("Wild" = "#1B9E77", "Domestic" = "#D95F02")) +
  scale_height_continuous(name = "Functional\nUniqueness", range = c(0.2, 0.55), guide = "legend") +
  scale_x_continuous(expand = expansion(mult = 0.12)) +
  scale_y_continuous(expand = expansion(mult = 0.12)) +
  labs(title = "B) Functional Uniqueness of current community", 
       x = paste0("Dim 1 (", pc1, "%)"), 
       y = paste0("Dim 2 (", pc2, "%)")) + 
  theme_minimal(base_size = 18) +
  title_theme



# SAVE ALL PLOTS
ggsave("outputs/Figure4_exclusivity_analysis.png", p_excl, width = 12, height = 10)

p_combined_5 <- (p_proxy + p_lonely) & 
  theme(legend.position = "none")

ggsave("outputs/Figure5_conservation_proxies.png", p_combined_5, width = 18, height = 10)


cat('\nStep 04 Finished: Integrated conservation and exclusivity analysis generated.\n')

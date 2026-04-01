
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(patchwork)

# --- 1. LOAD DATA ---
if(!file.exists("outputs/famd_core_results.RData")) {
  stop("Error: Run 'scripts/01_famd_core.R' first.")
}
load("outputs/famd_core_results.RData")

# --- 2. PREPARE LONG FORMAT ---
df_long <- df_master %>%
  select(spp, Dim1, Dim2, all_of(groups_names)) %>%
  pivot_longer(cols = all_of(groups_names), names_to = 'Group', values_to = 'Presence') %>%
  filter(Presence == 1) %>%
  mutate(Group = factor(Group, levels = groups_names))

# --- 3. VISUALIZATION HELPERS ---
x_lims <- c(min(df_master$Dim1) - 0.5, max(df_master$Dim1) + 0.5)
y_lims <- c(min(df_master$Dim2) - 0.5, max(df_master$Dim2) + 0.5)
pc1 <- round(res_famd$eig[1, 2], 1)
pc2 <- round(res_famd$eig[2, 2], 1)

title_theme <- theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

create_famd_subplot <- function(pts_data, title, target_group_name) {
  colors_palette <- RColorBrewer::brewer.pal(3, "Set1")
  group_idx <- which(groups_names == target_group_name)
  target_color <- colors_palette[group_idx]
  ellipse_data <- df_long %>% filter(Group == target_group_name)

  ggplot() +
    stat_ellipse(data = ellipse_data, aes(x = Dim1, y = Dim2), 
                 color = target_color, fill = target_color, geom = 'polygon', alpha = 0.1,, level = 0.8) +
    geom_point(data = pts_data, aes(x = Dim1, y = Dim2), color = target_color, size = 2, alpha = 0.6) +
    geom_text_repel(data = pts_data, aes(x = Dim1, y = Dim2, label = spp), 
                    size = 3.5, max.overlaps = 15, color = "black", fontface = "italic") +
    labs(title = title, x = paste0('Dim 1 (', pc1, '%)'), y = paste0('Dim 2 (', pc2, '%)')) +
    theme_minimal(base_size = 14) + coord_cartesian(xlim = x_lims, ylim = y_lims) +
    title_theme
}

# --- 4. PRODUCE PLOTS ---

# A. SCENARIO COMPARISON
p_all <- ggplot() +
  stat_ellipse(data = df_long, aes(x = Dim1, y = Dim2, color = Group, fill = Group), 
               geom = 'polygon', alpha = 0.1, level = 0.8) +
  geom_point(data = df_master, aes(x = Dim1, y = Dim2), color = "black", size = 1.5, alpha = 0.4) +
  geom_text_repel(data = df_master, aes(x = Dim1, y = Dim2, label = spp), size = 4, max.overlaps = 15, fontface = "italic") +
  scale_color_brewer(palette = 'Set1', labels = c("LIG", "Present", "Abandonment")) + 
  scale_fill_brewer(palette = 'Set1', labels = c("LIG", "Present", "Abandonment")) +
  labs(title = "A) All Scenarios", 
       x = paste0('Dim 1 (', pc1, '%)'), y = paste0('Dim 2 (', pc2, '%)')) +
  theme_minimal(base_size = 16) + theme(legend.position = "bottom") + coord_cartesian(xlim = x_lims, ylim = y_lims) +
  title_theme

p_lgi <- create_famd_subplot(df_long %>% filter(Group == 'last_interglacial'), 'B) LIG', 'last_interglacial')
p_pres <- create_famd_subplot(df_long %>% filter(Group == 'present'), 'C) Present', 'present')
p_aban <- create_famd_subplot(df_long %>% filter(Group == 'abandonment'), 'D) Abandonment', 'abandonment')

p_final <- (p_all + p_lgi) / (p_pres + p_aban) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")


ggsave('outputs/Figure3_famd_scenarios_comparison.png', p_final, width = 14, height = 12)

# Functional space calculation
calc_polygon_area <- function(x, y) {
  0.5 * abs(sum(x[-length(x)] * y[-1] - x[-1] * y[-length(x)]))
}
ellipse_data <- ggplot_build(p_all)$data[[1]]

ellipse_areas <- ellipse_data %>%
  group_by(group) %>%
  summarise(Ellipse_Area = calc_polygon_area(x, y), .groups = 'drop') %>%
  mutate(Scenario = levels(df_long$Group)[group]) %>%
  select(Scenario, Ellipse_Area)
print(ellipse_areas)


cat('\nStep 03 Finished: Scenario comparison plot generated.\n')

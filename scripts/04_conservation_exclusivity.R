
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

pc1 <- round(res_famd$eig[1, 2], 1)
pc2 <- round(res_famd$eig[2, 2], 1)
x_lims <- c(min(df_master$Dim1) - 1, max(df_master$Dim1) + 1)
y_lims <- c(min(df_master$Dim2) - 1, max(df_master$Dim2) + 1)
dist_mat <- as.matrix(dist(df_master[, c("Dim1", "Dim2")]))

# --- 2. PREPARE SCENARIO & EXCLUSIVITY DATA ---
df_scenarios <- df_master %>%
  select(spp, Dim1, Dim2, last_interglacial, present, abandonment, is_domestic) %>%
  pivot_longer(cols = c(last_interglacial, present, abandonment), 
               names_to = "Scenario", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(Scenario = factor(Scenario, 
                          levels = c("last_interglacial", "present", "abandonment"),
                          labels = c("LIG", "Present", "Abandonment")))

df_exclusive <- df_master %>%
  mutate(Exclusivity = case_when(
    last_interglacial == 1 & present == 0 ~ "Extinct (LIG Exclusive)",
    present == 1 & is_domestic == 1 ~ "Domestic (Livestock)",
    present == 1 & is_domestic == 0 ~ "Wild (Extant)",
    TRUE ~ "Other"
  )) %>%
  filter(Exclusivity != "Other") %>%
  mutate(Exclusivity = factor(Exclusivity, levels = c("Extinct (LIG Exclusive)", "Wild (Extant)", "Domestic (Livestock)")))


# --- 3. CALCULATE UNIQUENESS & PROXIES ---
# Functional Uniqueness (Current only)
df_master$Uniqueness <- sapply(1:nrow(df_master), function(i) {
  curr <- which(df_master$present == 1)
  if (df_master$present[i] == 1) curr <- curr[curr != i]
  min(dist_mat[i, curr])
})

# Proxies for Extinct Species
extinct_df <- df_master %>% filter(last_interglacial == 1 & present == 0)
proxies <- t(apply(extinct_df, 1, function(row) {
  orig_idx <- which(df_master$spp == row["spp"])
  curr <- which(df_master$present == 1)
  closest <- curr[which.min(dist_mat[orig_idx, curr])]
  return(c(df_master$is_domestic[closest], df_master$spp[closest], dist_mat[orig_idx, closest]))
}))
extinct_df$Proxy_Type <- ifelse(proxies[,1]=="1", "Domestic", "Wild")
extinct_df$Proxy_Spp <- proxies[,2]
extinct_df$Proxy_Dist <- as.numeric(proxies[,3])

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

p_lig <- plot_scenario(df_scenarios %>% filter(Scenario == "LIG"), "Last Interglacial", "#E41A1C")
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
                                   paste0(spp, " (", round(Uniqueness, 2), ")"), 
                                   NA))

p_proxy <- ggplot(extinct_df, aes(x = reorder(spp, Proxy_Dist), y = Proxy_Dist, fill = Proxy_Type)) +
  geom_col(alpha = 0.8) + 
  geom_text(aes(label = Proxy_Spp), hjust = -0.1, size = 5.5, fontface = "italic") +
  coord_flip() + 
  scale_fill_manual(values = c("Wild" = "#1B9E77", "Domestic" = "#D95F02"), labels = c("Wild", "Domestic")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.6))) +
  labs(title = "A) Extinct Megafauna Substitution", 
       x = "Extinct Species", 
       y = "Functional Distance",
       fill = "Type") +
  theme_minimal(base_size = 18) +
  title_theme

p_lonely <- ggplot(df_master %>% filter(present == 1), aes(x = Dim1, y = Dim2)) +
  geom_point(aes(size = Uniqueness, fill = Type), shape = 21, alpha = 0.6, color = "white") +
  geom_text_repel(data = df_master %>% filter(present == 1 & Uniqueness > median(Uniqueness)),
                  aes(label = label_uniqueness, color = Type), 
                  size = 5.5, fontface = "italic", show.legend = FALSE) +
  scale_fill_manual(values = c("Wild" = "#1B9E77", "Domestic" = "#D95F02")) +
  scale_color_manual(values = c("Wild" = "#1B9E77", "Domestic" = "#D95F02")) +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  labs(title = "B) Functional Uniqueness of current community", 
       x = paste0("Dim 1 (", pc1, "%)"), 
       y = paste0("Dim 2 (", pc2, "%)")) + 
  theme_minimal(base_size = 18) +
  title_theme +
  guides(fill = "none")



# SAVE ALL PLOTS
ggsave("outputs/Figure4_exclusivity_analysis.png", p_excl, width = 12, height = 10)

p_combined_5 <- (p_proxy + p_lonely) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

ggsave("outputs/Figure5_conservation_proxies.png", p_combined_5, width = 18, height = 10)


cat('\nStep 04 Finished: Integrated conservation and exclusivity analysis generated.\n')


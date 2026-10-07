library(dplyr)
library(FactoMineR)
library(factoextra)
library(tibble)
library(ggplot2)
library(ggrepel)
library(patchwork)

# --- 1. LOAD CORE RESULTS ---
if(!file.exists("outputs/famd_core_results.RData")) {
  stop("Error: Run 'scripts/01_famd_core.R' first.")
}
load("outputs/famd_core_results.RData")

# --- 2. CLUSTER ANALYSIS (HCPC) & RE-MAPPING ---
res_hcpc <- HCPC(res_famd, nb.clust = -1, graph = FALSE)

arbol <- res_hcpc$call$t$tree
spp_ordenadas_izq_der <- arbol$labels[arbol$order]
clusters_en_orden <- res_hcpc$data.clust[spp_ordenadas_izq_der, "clust"]
orden_aparicion <- unique(as.character(clusters_en_orden))
diccionario_visual <- setNames(1:length(orden_aparicion), orden_aparicion)

df_plot_clusters <- df_master %>%
  select(spp, Dim1, Dim2, Status, is_domestic)

clusters_viejos_spp <- as.character(res_hcpc$data.clust[df_plot_clusters$spp, "clust"])
df_plot_clusters$Cluster <- as.factor(diccionario_visual[clusters_viejos_spp])

n_clusters <- length(orden_aparicion)
cluster_colors <- RColorBrewer::brewer.pal(max(3, n_clusters), "Dark2")[1:n_clusters]

# --- 3. VISUALIZATION ---

title_theme <- theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

# A. Cluster Map
# In-panel key for Species Type (Wild/Extinct vs Domestic), placed in an
# empty area of the functional space (top-right).
key_x <- max(df_plot_clusters$Dim1) - 1.5
key_y_top <- max(df_plot_clusters$Dim2) - 0.3
species_type_key <- tibble(
  x = key_x,
  y = c(key_y_top, key_y_top - 0.4),
  shape_id = c("0", "1"),
  label = c("Wild / Extinct", "Domestic")
)
# Frame around the in-panel key so it reads as a legend box, not extra data points.
key_box <- data.frame(
  xmin = key_x - 0.35, xmax = key_x + 1.85,
  ymin = key_y_top - 0.65, ymax = key_y_top + 0.3
)

p_map <- ggplot(df_plot_clusters, aes(x = Dim1, y = Dim2, color = Cluster, fill = Cluster)) +
  stat_ellipse(geom = "polygon", alpha = 0.1, color = NA, level = 0.85) +
  geom_point(aes(shape = as.factor(is_domestic)), size = 4, alpha = 0.8) +
  geom_text_repel(aes(label = spp), size = 6, max.overlaps = 15, show.legend = FALSE,
                   fontface = "italic", bg.color = "white", bg.r = 0.18) +
  geom_rect(data = key_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "white", color = "grey40", linewidth = 0.4) +
  geom_point(data = species_type_key, aes(x = x, y = y, shape = shape_id),
             inherit.aes = FALSE, color = "black", size = 4) +
  geom_text(data = species_type_key, aes(x = x + 0.2, y = y, label = label),
            inherit.aes = FALSE, hjust = 0, size = 4.5) +
  scale_shape_manual(values = c("0" = 16, "1" = 1), guide = "none") +
  scale_color_manual(values = cluster_colors, guide = "none") +
  scale_fill_manual(values = cluster_colors, guide = "none") +
  labs(title = "B. HFTs in functional space",
       x = paste0("Dim 1 (", round(res_famd$eig[1,2], 1), "%)"),
       y = paste0("Dim 2 (", round(res_famd$eig[2,2], 1), "%)")) +
  theme_minimal(base_size = 16) +
  title_theme

# B. Dendrogram (Phylodiagram)
p_dend <- fviz_dend(res_hcpc, 
                    rect = FALSE, 
                    cex = 1.1,
                    palette = cluster_colors,
                    main = "A. Dendrogram",
                    ylab = "Inertia",
                    ggtheme = theme_minimal(base_size = 16))

for(i in seq_along(p_dend$layers)){
  if(!is.null(p_dend$layers[[i]]$aes_params$label) || inherits(p_dend$layers[[i]]$geom, "GeomText")){
    p_dend$layers[[i]]$aes_params$fontface <- "italic"
  }
}

segment_idx <- which(sapply(p_dend$layers, function(l) inherits(l$geom, "GeomSegment")))
p_dend$layers[[segment_idx]]$aes_params$linewidth <- 2.5

# fviz_dend draws leaf labels via a GeomText layer with the rotation baked
# into its data (angle/hjust/vjust columns), so theme(axis.text.x = ...) has
# no effect on them. Edit that layer's data directly to get a 45deg tilt.
leaf_text_idx <- which(sapply(p_dend$layers, function(l) inherits(l$geom, "GeomText")))
p_dend$layers[[leaf_text_idx]]$data$angle <- 45
p_dend$layers[[leaf_text_idx]]$data$hjust <- 1
p_dend$layers[[leaf_text_idx]]$data$vjust <- 1

# Leaf labels inherit the (sometimes pale) branch colors, which hurts
# legibility against a white background. Darken just the label text color
# -- the branches themselves keep the original palette.
darken <- function(hex, factor = 0.7) {
  rgb <- col2rgb(hex) * factor
  rgb(rgb[1,], rgb[2,], rgb[3,], maxColorValue = 255)
}
p_dend$layers[[leaf_text_idx]]$data$col <- darken(p_dend$layers[[leaf_text_idx]]$data$col)
p_dend$layers[[leaf_text_idx]]$data$cex <- p_dend$layers[[leaf_text_idx]]$data$cex * 1.2

# HFT cluster labels placed directly on the dendrogram. Cluster membership is
# contiguous along the left-to-right leaf order, so a run-length encoding
# gives each HFT's leaf-index span without needing rect().
cluster_runs <- rle(as.character(clusters_en_orden))
run_end <- cumsum(cluster_runs$lengths)
run_start <- run_end - cluster_runs$lengths + 1
hft_labels <- data.frame(
  x_mid = (run_start + run_end) / 2,
  y_pos = 0.35,
  HFT = seq_along(cluster_runs$values)
)
hft_labels$col <- cluster_colors[hft_labels$HFT]

p_dend <- p_dend + 
  geom_label(data = hft_labels, aes(x = x_mid, y = y_pos, label = HFT),
             inherit.aes = FALSE, color = hft_labels$col, fill = "white",
             linewidth = 0, fontface = "bold", size = 5) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 5, r = 10, b = 160, l = 110)) + 
  title_theme + 
  guides(lwd = "none") +
  coord_cartesian(clip = "off")

# C. Combined Plot
# Species Type is embedded in panel B and HFT is labelled directly on the
# dendrogram, so no shared legend is needed.
p_combined <- (p_dend | p_map) + 
  plot_layout(widths = c(1.3, 1)) & 
  theme(legend.position = "none") &
  guides(lwd = "none", size = "none", linewidth = "none")

ggsave('outputs/Figure2_functional_clusters.png', p_combined, width = 22, height = 11, dpi = 300)
save(res_hcpc, df_plot_clusters, cluster_colors, file = "outputs/famd_cluster_results.RData")
cat(sprintf('\nStep 02 Finished: Functional clusters generated (n=%d, map + dendrogram).\n', n_clusters))
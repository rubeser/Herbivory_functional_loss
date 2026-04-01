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
p_map <- ggplot(df_plot_clusters, aes(x = Dim1, y = Dim2, color = Cluster, fill = Cluster)) +
  stat_ellipse(geom = "polygon", alpha = 0.1, color = NA, level = 0.85) +
  geom_point(aes(shape = as.factor(is_domestic)), size = 4, alpha = 0.8) +
  geom_text_repel(aes(label = spp), size = 4, max.overlaps = 15, show.legend = FALSE, fontface = "italic") +
  scale_shape_manual(values = c("0" = 16, "1" = 1), labels = c("Wild/Extinct", "Domestic")) +
  scale_color_manual(name = "HFT", values = cluster_colors) +
  scale_fill_manual(name = "HFT", values = cluster_colors) +
  labs(title = "B. Functional Space Clusters",
       x = paste0("Dim 1 (", round(res_famd$eig[1,2], 1), "%)"),
       y = paste0("Dim 2 (", round(res_famd$eig[2,2], 1), "%)"),
       shape = "Species Type") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = alpha("white", 0.7), color = "grey90")) +
  title_theme

# B. Dendrogram (Phylodiagram)
p_dend <- fviz_dend(res_hcpc, 
                    rect = TRUE, 
                    cex = 0.8,
                    palette = cluster_colors,
                    main = "A. Hierarchical Clustering (Dendrogram)",
                    ylab = "Inertia",
                    ggtheme = theme_minimal(base_size = 16))

for(i in seq_along(p_dend$layers)){
  if(!is.null(p_dend$layers[[i]]$aes_params$label) || inherits(p_dend$layers[[i]]$geom, "GeomText")){
    p_dend$layers[[i]]$aes_params$fontface <- "italic"
  }
}

p_dend <- p_dend + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  title_theme + 
  guides(lwd = "none")

# C. Combined Plot
p_combined <- (p_dend | p_map) + 
  plot_layout(widths = c(1, 1.1), guides = "collect") & 
  theme(legend.position = "bottom") &
  guides(lwd = "none", size = "none", linewidth = "none")

ggsave('outputs/Figure2_functional_clusters.png', p_combined, width = 18, height = 10, dpi = 300)
save(res_hcpc, df_plot_clusters, cluster_colors, file = "outputs/famd_cluster_results.RData")
cat(sprintf('\nStep 02 Finished: Functional clusters generated (n=%d, map + dendrogram).\n', n_clusters))
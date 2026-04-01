
library(readxl)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(ggplot2)
library(patchwork)

# --- 1. DATA PREPARATION ---
data <- read_excel('data/species_updated.xlsx')
mapping_fermentation <- read_excel('data/species_updated.xlsx', sheet='fermentation') %>%
  select(Fermentation.Type, fermentation_type)
mapping_guild <- read_excel('data/species_updated.xlsx', sheet='guild') %>%
  select(Guild.w.Omnivory, feed)

domestic_spp <- c("Bos taurus", "Ovis aries", "Capra hircus", "Equus caballus")
groups_names <- c('last_interglacial', 'present', 'abandonment')

df_clean <- data %>%
  filter(!is.na(spp)) %>%
  left_join(mapping_fermentation, by = 'Fermentation.Type') %>%
  left_join(mapping_guild, by = 'Guild.w.Omnivory') %>%
  mutate(
    Mass_g = log10(as.numeric(Mass.g)),
    water_dep = as.factor(water_dependence),
    group_behav = as.factor(group_behaviour),
    Guild = as.factor(feed),
    Fermentation = as.factor(fermentation_type),
    is_domestic = ifelse(spp %in% domestic_spp, 1, 0),
    Status = case_when(
      last_interglacial == 1 & present == 0 ~ "Extinct",
      present == 1 & is_domestic == 1 ~ "Domestic",
      present == 1 & is_domestic == 0 ~ "Wild",
      TRUE ~ "Other"
    )
  ) %>%
  filter(complete.cases(Mass_g, water_dep, group_behav, Guild, Fermentation))

# --- 2. FAMD EXECUTION & TRAIT SIGNIFICANCE ---
traits_only <- df_clean %>% 
  select(Mass_g, water_dep, group_behav, Guild, Fermentation) %>%
  as.data.frame()
rownames(traits_only) <- df_clean$spp
res_famd <- FAMD(traits_only, graph = FALSE)

# Save variable importance plot
p_var <- fviz_famd_var(res_famd, 'var', repel = TRUE, labelsize = 5) +
  labs(title = NULL, subtitle = NULL) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
ggsave('outputs/Figure1_famd_variables.png', p_var, width = 8, height = 7)

# --- 3. ROBUSTNESS TESTS ---
cat('\n--- Running Robustness Tests ---\n')
n_boot <- 100
boot_eigs <- matrix(0, nrow = n_boot, ncol = 2)
set.seed(123)
for (i in 1:n_boot) {
  df_boot <- traits_only[sample(nrow(traits_only), replace = TRUE), ]
  res_boot <- FAMD(df_boot, graph = FALSE)
  boot_eigs[i, ] <- res_boot$eig[1:2, 1]
}

loo_corrs <- numeric(nrow(traits_only))
original_dim1 <- res_famd$ind$coord[, 1]
for (i in 1:nrow(traits_only)) {
  res_loo <- FAMD(traits_only[-i, ], graph = FALSE)
  loo_corrs[i] <- cor(original_dim1[-i], res_loo$ind$coord[, 1])
}

p_rob1 <- ggplot(data.frame(eig = boot_eigs[, 1]), aes(x = eig)) +
  geom_histogram(fill = "steelblue", alpha = 0.7, bins = 15) +
  geom_vline(xintercept = res_famd$eig[1, 1], color = "red", linetype = "dashed") +
  labs(title = "Bootstrap Stability (Eig 1)", x = "Eigenvalue", y = "Frequency") + 
  theme_minimal(base_size = 14)

p_rob2 <- ggplot(data.frame(corr = loo_corrs), aes(x = corr)) +
  geom_histogram(fill = "darkgreen", alpha = 0.7, bins = 15) +
  labs(title = "LOO Stability (Dim 1)", x = "Correlation", y = "Frequency") + 
  theme_minimal(base_size = 14)

ggsave('outputs/SM2_famd_robustness.png', p_rob1 | p_rob2, width = 12, height = 5)

# Save objects for subsequent scripts
ind_coords <- as.data.frame(res_famd$ind$coord[, 1:2])
colnames(ind_coords) <- c('Dim1', 'Dim2')
df_master <- cbind(df_clean, ind_coords)

if(!dir.exists("outputs")) dir.create("outputs")
save(res_famd, df_master, groups_names, domestic_spp, traits_only, file = "outputs/famd_core_results.RData")

cat('\nStep 01 Finished: FAMD core results and robustness generated.\n')

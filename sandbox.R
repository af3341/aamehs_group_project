
# PCA very messy was just playing

metabolites_ids_orig <- phthte_creatinine %>%
  na.omit() %>% 
  pivot_wider(id_cols = seqn,
              names_from = analyte_code,
              values_from = adj_conc)
combined <- inner_join(metabolites_ids_orig, agp_serum, by = "seqn")

metabolites_ids_rownames <- metabolites_ids_orig %>% 
  column_to_rownames("seqn") 

library(Hmisc)

pca <- princmp(metabolites_ids_rownames)
plot(pca, k = 2)


pca_result <- prcomp(metabolites_ids_rownames, scale. = TRUE)
summary(pca_result)


# Convert PCA results to a data frame
df_pca <- as.data.frame(pca_result$x)

subj_ids_pca <- df_pca %>% 
  rownames_to_column(var = "seqn") %>% 
  mutate(seqn = as.numeric(seqn))

a <- left_join(subj_ids_pca, agp_serum %>% select(seqn, ssagp), by = "seqn")

# Add the agp information
df_pca$AGP <- a$ssagp  

df_pca2 <- df_pca %>% 
  na.omit()


library(ggplot2)
ggplot(df_pca2, aes(x = PC3, y = PC2, color = AGP)) +
  geom_point() +
  xlim(c(-2.5,5)) +
  ylim(c(-5, 15)) +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  ggtitle("PCA of Metabolites Colored by AGP Level") +
  theme_minimal() 

df_pca2

loadings <- pca_result$rotation
print(loadings)


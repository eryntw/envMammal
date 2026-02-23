tar_load(joined_table, store = tars$bird_db$store)

jt0 <- joined_table %>% 
  dplyr::select(
    search_term, common, # Names
    bl_Family, # ID raptors
    contains("AgriculturalLands"),
    contains("Urban"),
    aub_FeedingHB,
    aub_BreedingHB,
    aub_Migratory6,
    bb_Db,
    bb_db_simpson,
    bb_Rr,
    bb_Hb,
    bb_Esi,
    bb_ElevationalRange,
    bb_mig_score,
    bl_MigratoryStatus,
    bl_GenerationLength,
    bl_RlEooSmallerOfBreedingAndNonBreedingEoo,
    bl_scaledHB_L1,
    bl_logscaledHBscore_L2,
    bl_anthro_LogHabitat_scaled,
    rec_stern_dehoedt_2000_minor_simpson,
    rec_geom_90M_s10e110_simpson, 
    contains("range_90_10")
  ) %>% 
  score_ag_urb_habitats()

jt <- joined_table %>% 
  dplyr::select(
    search_term, common, # Names
    bl_Family, # ID raptors
    contains("AgriculturalLands"),
    contains("Urban"),
    aub_FeedingHB,
    aub_BreedingHB,
    aub_Migratory6,
    bb_Db,
    bb_db_simpson,
    bb_Rr,
    bb_Hb,
    bb_Esi,
    bb_ElevationalRange,
    bb_mig_score,
    bl_MigratoryStatus,
    bl_GenerationLength,
    bl_RlEooSmallerOfBreedingAndNonBreedingEoo,
    bl_scaledHB_L1,
    bl_logscaledHBscore_L2,
    bl_anthro_LogHabitat_scaled,
    rec_stern_dehoedt_2000_minor_simpson,
    rec_geom_90M_s10e110_simpson, 
    contains("range_90_10")
  ) %>% 
  dplyr::mutate(
    bl_eoo_log10 = log10(bl_RlEooSmallerOfBreedingAndNonBreedingEoo),
    bl_genlen_log = log(bl_GenerationLength),
    bl_log10ElevScaled = log10(`rec_dem-9s_range_90_10_norm`),
  ) %>% 
  dplyr::inner_join(info_table %>% dplyr::select("search_term"),
                    by = "search_term") %>% 
  score_ag_urb_habitats() %>% 
  score_raptor() %>% 
  select(search_term,
         common,
         bl_eoo_log10,
         rec_stern_dehoedt_2000_minor_simpson,
         aub_BreedingHB,
         aub_FeedingHB,
         bb_db_simpson,
         aub_scaled_adapt,
         bb_mig_score,
         bl_genlen_log,
         bl_raptorscore)

jt %>%
  dplyr::select(where(is.character)) %>%
  utils::head()

jt_num <- jt %>%
  mutate(
    bl_MigratoryStatus = case_when(
      bl_MigratoryStatus == "Not a migrant" ~ "0",
      bl_MigratoryStatus == "Full migrant" ~ "3",
      bl_MigratoryStatus == "Altitudinal migrant" ~ "2",
      bl_MigratoryStatus %in% c("Nomadic", "Unknown") ~ "1"
    ),
    aub_Migratory6 = case_when(
      aub_Migratory6 == "not listed" ~ "0",
      is.na(aub_Migratory6) ~ "0",
      TRUE ~ "1"
    )
  ) %>% 
  readr::type_convert()

jt_num %>%
  dplyr::select(where(is.character)) %>%
  utils::head()

## ------ Correlation plot ------

install.packages("corrplot")
library("corrplot")

jt_cor <- jt %>% 
  dplyr::select(-any_of(c("search_term", "common", "bl_Family"))) %>% 
  dplyr::rename_with(~abbreviate(., method = "both.sides",
                                 minlength = 8)) %>% 
  na.omit()

cor.mat <- cor(jt_cor, method = "spearman") %>% round(2)
corrplot(cor.mat, 
         type="upper", order="hclust", 
         tl.col="black")

## ------ PCA -------
library(FactoMineR)
library(factoextra)
res.pca <- FactoMineR::PCA(jt_cor, scale.unit = TRUE, graph = TRUE, ncp = 10)
print(res.pca)
get_eigenvalue(res.pca)
factoextra::fviz_eig(res.pca, addlabels = TRUE, ncp=15)

# Chosen dim should have eigs > 1
# Contribute individual variance > 10 %
# Culmulative variance > 60%
27+16.9+9.4+7.7
res.pca <- FactoMineR::PCA(jt_cor, scale.unit = TRUE, graph = TRUE, ncp = 4)
factoextra::fviz_eig(res.pca, addlabels = TRUE, ncp = 4)
get_pca_var(res.pca)

# The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates) . A high cos2 indicates a good representation of the variable on the principal component. In this case the variable is positioned close to the circumference of the correlation circle. A low cos2 indicates that the variable is not perfectly represented by the component.
head(res.pca$var$cos2)

# The contributions of variables in accounting for the variability in a given principal component are expressed in percentage. Variables that are correlated with PC1 (i.e., Dim. 1) and PC2 (i.e., Dim. 2) are the most important in explaining the variability in the data set. Variables that are not correlated with any PC or correlated with the last dimensions are variables with low contribution and might be removed to simplify the overall analysis.
head(res.pca$var$contrib)
corrplot(res.pca$var$contrib, is.corr = FALSE)

# Calculate rotation
t(apply(res.pca$var$coord, 1, function(x) {x/sqrt(res.pca$eig[,1])})) %>% head()
sweep(res.pca$var$coord, 2, sqrt(res.pca$eig[,1]),'/') %>% round(2)




#install.packages("COINr")
library(COINr)
library(targets)
library(dplyr)

## Read data ------

tar_load(scaled_infotable, store = tars$bird_score$store)
names(scaled_infotable)
scaled_infotable %>% select(contains("scale")) %>% names()

## select variables

tbl <- scaled_infotable %>% 
  select(search_term,
         common,
         bl_eoo_log10,
         rec_stern_dehoedt_2000_minor_simpson,
         aub_BreedingHB,
         aub_FeedingHB,
         bb_db_simpson,
         aub_adaptscore,
         bb_mig_score,
         bl_genlen_log,
         bl_raptorscore)

## iData ------

iData <- tbl %>% 
  mutate(common = janitor::make_clean_names(common)) %>% 
  rename(uName = search_term, 
         uCode = common)
check_iData(iData)

# Get % of dominance pairs: The greater the number of dominance (or robust) pairs in a classification, the less sensitive the units will be to methodological assumptions. 
outlist <- outrankMatrix(iData[, -c(1:2)])
outlist$nDominant # 14
outlist$fracDominant # 0.005 very small, subject to methodology assumptions# indicators with negative directionality

## iMeta ------

iMeta <- data.frame(
  iCode = names(iData)[-c(1:2)],
  iName = c(
    # Lv1
    "Breeding Extent (EOO)", 
    "Climate Zone Diversity", 
    "Breeding Habitat Breadth", 
    "Feeding Habitat Breadth",
    "Diet Diversity", 
    "Usage of Modified Environment",
    "Migration Constraints", 
    "Generation Length", 
    "Raptor Status"),
  Direction = 1,
  Level = 1,
  Weight = 1,
  Type = "Indicator"
)

neg_directions <- c("bl_eoo_log10", "rec_stern_dehoedt_2000_minor_simpson", # Climate
                    "aub_BreedingHB", "aub_FeedingHB",                       # Habitat
                    "bb_db_simpson",                                        # Diet
                    "aub_adaptscore")

# update iMeta
iMeta$Direction[iMeta$iCode %in% neg_directions] <- -1

# assign level 2 groupings
iMeta$Parent <- c("Climate",
                  "Climate",
                  "Habitat",
                  "Habitat",
                  "Diet", 
                  "Adaptability", 
                  "Constraints",
                  "Constraints",
                  "Constraints")

iMeta_L2 <- data.frame(
  iCode = c("Climate", 
            "Habitat", 
            "Diet", 
            "Adaptability", 
            "Constraints"),
  iName = c("Climate Dimension", 
            "Habitat Dimension", 
            "Diet Dimension", 
            "Adaptability Dimension", 
            "Constraints Dimension"),
  Direction = 1,
  Level = 2,
  Weight = 1,
  Type = "Aggregate",
  Parent = c("Sensitivity")
)

iMeta_L3 <- data.frame(
  iCode = "Sensitivity",
  iName = "Species Sensitivity Index",
  Direction = 1,
  Level = 3,
  Weight = 1,
  Type = "Aggregate",
  Parent = NA
)

# add to iMeta
iMeta <- rbind(iMeta, iMeta_L2, iMeta_L3)

# Check the structure
check_iMeta(iMeta)


## Build a coin ------

# build a new coin using example data
coin <- new_coin(iData = iData,
                 iMeta = iMeta,
                 level_names = c("Indicator", "Sub-index", "Index")) # can exclude ind
coin
data_raw <- get_dset(coin, "Raw")
head(data_raw[1:5], 5)

plot_framework(coin)
plot_framework(coin, type = "stack", colour_level = 2)

# Check low data availability, having a high proportion of zeros and/or a low proportion of unique values as well as skewness

get_stats(coin, dset = "Raw") |>
  DT::datatable(rownames = F)

# Correlation
plot_corr(coin, dset = "Raw", grouplev = 3, box_level = 2, use_directions = TRUE)

# Distribution
plot_dist(coin, dset = "Raw", iCodes = "Sensitivity", Level = 1, type = "Dot")

# Skip imputation, denomination, unit screening

# Skip treating outliers as there is none

# Normalisation
coin <- qNormalise(coin, dset = "Raw", 
                   f_n = "n_minmax",
                   f_n_para = list(l_u = c(0, 1)))
plot_dist(coin, dset = "Normalised", iCodes = "Sensitivity", Level = 1, type = "Dot")

get_corr_flags(coin, dset = "Normalised", cor_thresh = 0.5,
               thresh_type = "high", grouplev = 2)
get_corr_flags(coin, dset = "Normalised", cor_thresh = 0,
               thresh_type = "low", grouplev = 2) # No negative correlation within group

# Stat consistency within group
# A high c-alpha, or equivalently a high “reliability”, indicates that the individual indicators measure the latent phenomenon well. Nunnally (1978) suggests 0.7 as an acceptable reliability threshold. Yet some authors use .75 or .80 as a cut-off value, while others are as lenient as to go to 0.6. In general this varies by discipline. If the variances of the individual indicators vary widely, as in our test case, a standard practice is to standardise the individual indicators to a standard deviation of 1 before computing the coefficient alpha.

get_cronbach(coin, dset = "Raw", iCodes = "Climate", Level = 1) # 0.47
coin_std <- coin
coin_std <- qNormalise(coin_std, dset = "Raw", 
                       f_n = "n_zscore",
                       f_n_para = list(m_sd = c(0, 1)))
get_cronbach(coin_std, dset = "Normalised", iCodes = "Climate", Level = 1) # 0.77
get_cronbach(coin_std, dset = "Normalised", iCodes = "Habitat", Level = 1) # 0.69
get_cronbach(coin_std, dset = "Normalised", iCodes = "Constraints", Level = 1) # 0.41

# effective weight of each component at the highest level of aggregation (the index)
w_eff <- get_eff_weights(coin)
w_eff

# aggregate with equal weight specified in iMeta
coin <- Aggregate(coin, dset = "Normalised",
                  f_ag = "a_amean", write_to = "agg_amean")
dset_aggregated <- get_dset(coin, dset = "agg_amean")

# check correlations between level 2 and index
get_corr(coin, dset = "agg_amean", 
         cortype = "spearman",
         pval = 0.05, # p > pval will be returned as NA
         Levels = c(2, 3))

# optimise weights at level 2
coin <- get_opt_weights(coin, itarg = "equal", optype = "balance",
                        dset = "agg_amean", maxiter = 5000, cortype = "spearman",
                        Level = 2, weights_to = "BalanceOptLev2", out2 = "coin")

coin$Meta$Weights$BalanceOptLev2[coin$Meta$Weights$BalanceOptLev2$Level == 2, ]

coin <- get_opt_weights(coin, itarg = "equal", optype = "infomax",
                        dset = "agg_amean", maxiter = 5000, cortype = "spearman",
                        Level = 2, weights_to = "InfomaxOptLev2", out2 = "coin")

coin$Meta$Weights$InfomaxOptLev2[coin$Meta$Weights$InfomaxOptLev2$Level == 2, ]

### optimise using specified weights

# copy original weights
w_lv2 <- coin$Meta$Weights$Original

# modify weights of Conn and Sust to 0.3 and 0.7 respectively
w_lv2$Weight[w_lv2$iCode %in% c("Habitat", "Climate", "Diet")] <- 2

coin <- Aggregate(coin, dset = "Normalised",
                  f_ag = "a_amean", write_to = "agg_amean",
                  w = w_lv2)

coin <- get_opt_weights(coin, itarg = c(2,2,1,1,1), optype = "infomax",
                        dset = "agg_amean", maxiter = 5000, cortype = "spearman",
                        Level = 2, weights_to = "CusOptLev2", out2 = "coin")

coin$Meta$Weights$CusOptLev2[coin$Meta$Weights$CusOptLev2$Level == 2, ]

# re-aggregate - Balanced
coin <- Aggregate(coin, dset = "Normalised", w = "BalanceOptLev2",
                  f_ag = "a_amean", write_to = "agg_gmean_Bopt")

# check correlations between level 3 and index
get_corr(coin, dset = "agg_gmean_Bopt", 
         cortype = "spearman",
         pval = 0.05, # p > pval will be returned as NA
         Levels = c(2, 3))

# re-aggregate - Infomax
coin <- Aggregate(coin, dset = "Normalised", w = "InfomaxOptLev2",
                  f_ag = "a_amean", write_to = "agg_gmean_Iopt")

# check correlations between level 3 and index
get_corr(coin, dset = "agg_gmean_Iopt", 
         cortype = "spearman",
         pval = 0.05, # p > pval will be returned as NA
         Levels = c(2, 3))

# The Optimisation didn't have much impact
# re-aggregate - Infomax + specified weight
coin <- Aggregate(coin, dset = "Normalised", w = "InfomaxOptLev2",
                  f_ag = "a_amean", write_to = "agg_gmean_Iopt")

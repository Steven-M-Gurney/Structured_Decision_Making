###############################################################################
# Structured Decision-Making: Fish Management
# Author: Steven M. Gurney
# Last updated: 28 DEC 2025
#
# Purpose:
#   • Implement rank-based utility normalization for SDM
#   • Calculate equal-weight and weighted total utility scores
#   • Export clean tables and generate figures
###############################################################################


# =============================================================================
# 📦 1. Core SDM Utility Functions
# =============================================================================

library(ggplot2)
library(dplyr)
library(reshape2)

# Equation (1) – Normalization formula:
#   U_ij = (R_max - R_ij) / (R_max - 1)
# Normalizes ranks to utilities (0–1), where BEST rank = 1.
normalize_rank <- function(rank, max_rank) {
  (max_rank - rank) / (max_rank - 1) # Eq. (1)
}

# Weighted utility uses Eq. (3) only if weights are provided.
# Equation (3): 
#   U_i = Σ_j (w_j * U_ij)
# If no weights are passed, Eq. (2) is used by setting equal weights.
# Equation (2): 
#   U_i = (1/J) * Σ_j U_ij
calculate_utility <- function(rank_df, weights = NULL) {
  
  criteria <- colnames(rank_df)[-1]  # exclude alternative name
  
  # Default to equal weights if none supplied
  if (is.null(weights)) {
    weights <- rep(1 / length(criteria), length(criteria))
    names(weights) <- criteria
  }
  
  # Safety checks
  stopifnot(all(criteria %in% names(weights)))
  stopifnot(abs(sum(weights) - 1) < 1e-6)
  
  max_rank <- max(rank_df[criteria], na.rm = TRUE)
  
  # Normalize ranks → utilities
  utility_df <- rank_df
  for (c in criteria) {
    utility_df[[c]] <- normalize_rank(rank_df[[c]], max_rank)
  }
  
  # Calculate total and weighted utility
  utility_df$Total_Utility <- rowSums(utility_df[criteria])
  utility_df$Utility_Score <- rowSums(
    sweep(utility_df[criteria], 2, weights, `*`)
  )
  
  return(utility_df)
}


# =============================================================================
# 📁 2. Input Data: Alternatives & Ranks
# =============================================================================

# Relative rankings by objective for each alternative (assigned valueas)
sdm_ranks <- read.csv("Rankings.csv", header = TRUE, stringsAsFactors = FALSE)

# Ensure first column is Alternative (character)
sdm_ranks$Alternative <- as.character(sdm_ranks$Alternative)

# Ensure all ranking columns are numeric
criteria_cols <- colnames(sdm_ranks)[-1]
sdm_ranks[criteria_cols] <- lapply(
  sdm_ranks[criteria_cols],
  as.numeric
)


# =============================================================================
# ⚖️ 3. Equal-Weight Utility (Baseline SDM)
# =============================================================================

# Equation (2): Equal-weight additive utility aggregation:
#   U_i = (1/J) * Σ_j U_ij
equal_weight_results <- calculate_utility(sdm_ranks)  # Eq. (2)

equal_weight_results <- equal_weight_results |>
  dplyr::arrange(desc(Utility_Score))

# Take a look at normalized results
equal_weight_results

# Save output, and round here only (no impact on internal math)
write.csv(
  equal_weight_results |>
    dplyr::mutate(
      dplyr::across(
        .cols = -Alternative,
        .fns  = ~ round(.x, 3)
      )
    ),
  "Normalized_baseline_results.csv",
  row.names = FALSE
)


# =============================================================================
# 🎯 4. Weighted Utility (User-Defined Weights)
# =============================================================================

# Value-based weights assigned to each objective
weights <- c(
  Cost = 0.05,
  Effectiveness = 0.30, # Carried the second-most weight
  Frequency = 0.10,
  Difficulty = 0.10,
  Liability = 0.40, # Carried the most weight
  Attractiveness = 0.05
)

# Save weights
write.csv(weights, "Weights.csv")

# Equation (3): Weighted additive utility model:
#   U_i = Σ_j (w_j * U_ij)
weighted_results <- calculate_utility(
  rank_df = sdm_ranks,
  weights = weights # Eq. (3)
)

weighted_results <- weighted_results |>
  dplyr::arrange(desc(Utility_Score))

# Take a look at normalized results
weighted_results

# Save output, and round here only (no impact on internal math)
write.csv(
  weighted_results |>
    dplyr::mutate(
      dplyr::across(
        .cols = -Alternative,
        .fns  = ~ round(.x, 3)
      )
    ),
  "Normalized_weighted_results.csv",
  row.names = FALSE
)


# =============================================================================
# 📊 5. Clean Output Tables 
# =============================================================================

# Equal-weight utility scores (Equation 2)
equal_weight_table <- equal_weight_results |>
  dplyr::select(
    Alternative,
    Utility_Score
  )

# Take a look
equal_weight_table

# Save output
write.csv(equal_weight_table, "Baseline_utility_scores.csv", row.names = FALSE)

# Weighted utility scores (Equation 3)
weighted_utility_table <- weighted_results |>
  dplyr::select(
    Alternative,
    Utility_Score
  )

# Take a look
weighted_utility_table

# Save output
write.csv(weighted_utility_table, "Weighted_utility_scores.csv", row.names = FALSE)


# =============================================================================
# 🌡️6. Visualization: Utility Scores by Criterion (Heat Map)
# =============================================================================

# Load normalized weighted utilities
norm_weighted <- read.csv(
  "Normalized_weighted_results.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Drop the total and weighted utility columns
heatmap_df <- norm_weighted |>
  dplyr::select(
    -Total_Utility,
    -Utility_Score
  ) |>
  reshape2::melt(id.vars = "Alternative")

# Plot heat map
ggplot(heatmap_df,
       aes(x = variable, y = Alternative, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "deeppink4") +
  labs(
    x = "Decision criteria",
    y = "Management alternatives",
    fill = "Utility score"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )

# Save high-definition image scaled for Word document
ggsave("Utility_scores_objectives.tiff", width = 6.5, height = 5, dpi = 600)


# =============================================================================
# 📉 7. Visualization: Total Utility Scores (Bar Plot)
# =============================================================================

# Build the combined data frame
combined <- dplyr::bind_rows(
  weighted_results      |> dplyr::mutate(Model = "Weighted"),
  equal_weight_results  |> dplyr::mutate(Model = "Equal")
)

# Order by ascending + coordinate flip → highest value at the top
order_df <- combined %>%
  filter(Model == "Weighted") %>%
  arrange(Utility_Score) %>%        
  pull(Alternative)

combined <- combined %>%
  mutate(Alternative = factor(Alternative, levels = order_df))

# Plot comparing equal vs weighted models
ggplot(combined,
       aes(x = Alternative, y = Utility_Score, fill = Model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(
    name = "Model type",
    values = c("Equal" = "gray80", "Weighted" = "darkslategray4")
  ) +
  labs(
    x = "Management alternative",
    y = "Total utility score"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.position = c(0.8, 0.3),
    legend.direction = "vertical",
    legend.justification = "center",
    legend.key.height = unit(0.9, "line")
  )

# Save high-definition image scaled for Word document
ggsave("Utility_scores.tiff", width = 6.5, height = 5, dpi = 600)



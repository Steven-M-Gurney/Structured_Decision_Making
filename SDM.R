# =============================================================================
# 📦 1. Core SDM Utility Functions
# =============================================================================

# Normalize ranks to utilities (0–1), where BEST rank = 1
normalize_rank <- function(rank, max_rank) {
  (max_rank - rank) / (max_rank - 1)
}

# Calculate utility scores
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

sdm_ranks <- read.csv("rankings.csv", header = TRUE, stringsAsFactors = FALSE)

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

equal_weight_results <- calculate_utility(sdm_ranks)

equal_weight_results <- equal_weight_results |>
  dplyr::arrange(desc(Utility_Score))

equal_weight_results


# =============================================================================
# 🎯 4. Weighted Utility (User-Defined Weights)
# ======================a=======================================================

weights <- c(
  Cost = 0.05,
  Effectiveness = 0.30,
  Time = 0.10,
  Difficulty = 0.10,
  Liability = 0.40,
  Attractiveness = 0.05
)

weighted_results <- calculate_utility(
  rank_df = sdm_ranks,
  weights = weights
)

weighted_results <- weighted_results |>
  dplyr::arrange(desc(Utility_Score))

weighted_results


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
write.csv(equal_weight_table, "equal_weight_table.csv", row.names = FALSE)


# Weighted utility scores (Equation 3)
weighted_utility_table <- weighted_results |>
  dplyr::select(
    Alternative,
    Utility_Score
  )

# Take a look
weighted_utility_table

# Save output
write.csv(weighted_utility_table, "weighted_utility_table.csv", row.names = FALSE)



get_corr_v_cramer <- function(factor1, factor2){
  # Create a contingency table
  contingency_table <- table(factor1, factor2)
  cramers_v <- assocstats(contingency_table)
  cramers_v$cramer
}


#### LOOP FOR IDENTIFYING CORRELATED VARIABLES (We can decide to remove, or do something with them before MLR)
# Get only the categorical (factor) columns
categorical_vars <- names(Filter(is.factor, new.rw.data))

# Get all unique pairs of categorical columns
combinations <- combn(categorical_vars, 2, simplify = FALSE)

# Loop through each pair
for (combo in combinations) {
  v <- get_corr_v_cramer(new.rw.data[[combo[1]]], new.rw.data[[combo[2]]])
  
  if (!is.na(v) && v > 0.5) { # Print out columns with quite high correlation (can change the threshold here)
    cat("Cramér's V between", combo[1], "and", combo[2], "is", round(v, 3), "\n")
  }
}
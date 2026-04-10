
#TRANSFORMATIONS
library(ggplot2)

# Function to analyze distribution and statistics of target variable
analyze_distribution <- function(data, target_col) {
  print(target_col)
  
  # Plot the original target variable distribution
  p1 <- ggplot(data, aes(x = .data[[target_col]])) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
    labs(title = paste("Distribution of Original Variable (", target_col, ")", sep = ""),
         x = target_col, y = "Frequency") +
    theme_minimal()
  print(p1)
  
  # Plot the log-transformed target variable distribution
  p2 <- ggplot(data, aes(x = log(pmax(.data[[target_col]], 1)))) +
    geom_histogram(bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = paste("Distribution of Log-Transformed Variable (", target_col, ")", sep = ""),
         x = paste("Log(", target_col, ")", sep = ""), y = "Frequency") +
    theme_minimal()
  print(p2)
  
  # Calculate skewness and kurtosis for the original and log-transformed target
  original_skew <- sum((data[[target_col]] - mean(data[[target_col]], na.rm = TRUE))^3, na.rm = TRUE) / 
    (length(data[[target_col]]) * (sd(data[[target_col]], na.rm = TRUE))^3)
  original_kurtosis <- sum((data[[target_col]] - mean(data[[target_col]], na.rm = TRUE))^4, na.rm = TRUE) / 
    (length(data[[target_col]]) * (sd(data[[target_col]], na.rm = TRUE))^4) - 3
  
  log_skew <- sum((log(pmax(data[[target_col]], 1)) - mean(log(pmax(data[[target_col]], 1)), na.rm = TRUE))^3, na.rm = TRUE) / 
    (length(log(pmax(data[[target_col]], 1))) * (sd(log(pmax(data[[target_col]], 1)), na.rm = TRUE))^3)
  log_kurtosis <- sum((log(pmax(data[[target_col]], 1)) - mean(log(pmax(data[[target_col]], 1)), na.rm = TRUE))^4, na.rm = TRUE) / 
    (length(log(pmax(data[[target_col]], 1))) * (sd(log(pmax(data[[target_col]], 1)), na.rm = TRUE))^4) - 3
  
  # Print skewness and kurtosis values
  cat("\nOriginal Skewness: ", original_skew, "\n")
  cat("Original Kurtosis: ", original_kurtosis, "\n")
  cat("Log-Transformed Skewness: ", log_skew, "\n")
  cat("Log-Transformed Kurtosis: ", log_kurtosis, "\n")
}

# using the func on all our continuous columns
numeric_cols <- names(main_model)[sapply(main_model, is.numeric)]
for (col in numeric_cols) {
  analyze_distribution(main_model, col)
}

#based on the results we will log transform: W1GrssyrMP, W1GrssyrHH, W8QDEB2, W8DINCW
variables_to_log_trans<-c("W1GrssyrMP(Gross annual salary of main parent)", "W1GrssyrHH(Gross annual salary of household)",
                          "W8QDEB2(Total amount owe)", "W8DINCW(Continuous weekly income)")
# Log-transform the chosen cols for the model 
for (col in variables_to_log_trans) {
  main_model[[col]] <- log(pmax(main_model[[col]], 0.5))  # Log transform with pmax to avoid log(0) we have to justify this, in lectures they said to use 0.5
  #for the cols chosen, it is justifiable as 0.5 is a tiny increase compared to 0 for what the columns represent
  new_col_name <- paste("log_of_", col, sep = "")
  colnames(main_model)[colnames(main_model) == col] <- new_col_name #change the name so we know which predictors got log transformed
  print(summary(main_model[[new_col_name]])) #check changes
}
colnames(main_model)

#we will also standardize all continuous columns so that they are easier to interpret (except for the target)
#we also remove both health questionnaire variables, as they are categorical, but still represented as continuous so it makes no sense to standardize them
numeric_cols<-names(main_model)[sapply(main_model, is.numeric)]
numeric_cols_no_log <- setdiff(numeric_cols, c("log_of_W1GrssyrHH(Gross annual salary of household)", "log_of_W1GrssyrMP(Gross annual salary of main parent)", 
                                               "log_of_W8DINCW(Continuous weekly income)","log_of_W8QDEB2(Total amount owe)"))
numeric_cols_no_log


# Use the function from lectures
standardise.fn <- function(v) { (v - mean(v)) / sd(v) }

for (col in numeric_cols_no_log) {
  # Standardize the non-target continuous columns
  cat("The mean of ", col, " is ", mean(main_model[[col]], na.rm=T), "\n")
  main_model[[col]] <- standardise.fn(main_model[[col]])
  
  # Create the new column name
  new_col_name <- paste("standardised_", col, sep = "")
  
  # Add the standardized column with the new name
  main_model[[new_col_name]] <- main_model[[col]]
  
  # Remove the original column after standardizing
  main_model[[col]] <- NULL
  
  # Check changes by printing summary of the newly standardized column
  print(new_col_name)
  print(summary(main_model[[new_col_name]]))
}

# Taking note of the means (for this folder's version):
# W1yschat1 : 35.70156
# W4schatYP: 13.8765
# W6DebtattYP: 12.6869
# W1hiqualparents_score: 11.91

dim(main_model)
summary(main_model)

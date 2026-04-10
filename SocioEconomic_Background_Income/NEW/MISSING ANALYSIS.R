# Identify continuous columns with NA
cont_missing <- names(new.rw.data)[sapply(new.rw.data, function(col)any(is.na(col)))]

# Identify factor columns where "missing" is a level
factor_missing <- names(new.rw.data)[sapply(new.rw.data, function(col)"missing" %in% levels(col))]

# Combine both into missing_cols
missing_cols <- c(cont_missing, factor_missing)

#exploring the distribution of missing data for each col with missing data
for (col in missing_cols){print(summary(new.rw.data[col]))}

#checking correlation between main parent gross salary and household gross salary
cor(new.rw.data$W1GrssyrMP, new.rw.data$W1GrssyrHH, use = "complete.obs") # 0.5225
#as they are moderately correlated, we will only include the gross household salary in the analysis,

#only keeping interesting columns for analysis (ie when columns have a big enough sample size of missing data and other levels)
interesting_missing<-c("W1empsdad(Employment status of father)", "W1GrssyrHH(Gross annual salary of household)",
                       "W4AlcFreqYP(Frequency of having an alcoholic drink in last 12 months)", "W6EducYP(YP: Whether currently going to school or college)",
                       "W6NEETAct(Activities of NEETs (Not in Employment or Education))", "W8QDEB2(Total amount owe)")

# Visualizing if there is a pattern in the missing data compared to the weekly income for each interesting column
for (col in interesting_missing) {
  # Check if the column is numeric
  if (is.numeric(new.rw.data[[col]])) {
    # Calculate means for the dashed lines
    mean_na <- mean(new.rw.data[[target_col]][is.na(new.rw.data[[col]])])
    mean_non_na <- mean(new.rw.data[[target_col]][!is.na(new.rw.data[[col]])])
    
    # Create a scatter plot
    p <- ggplot(new.rw.data, aes(x=.data[[col]], y=.data[[target_col]])) +
      geom_point(color="blue") +
      labs(title=paste("Scatter plot of", target_col, "by", col),
           x=col, y=target_col)
    
    # Add lines for the mean values
    if (any(is.na(new.rw.data[[col]]))) {
      p <- p + geom_hline(aes(yintercept=mean_na, color="Mean of NA values"))
    }
    
    p <- p + geom_hline(aes(yintercept=mean_non_na, color="Mean of Non-NA values"))
    
    # Add the legend
    p <- p + scale_color_manual(name="Dashed Lines", values=c("Mean of NA values"="red", "Mean of Non-NA values"="black"))
    
  } else {
    # If it's not numeric, create a box plot
    p <- ggplot(new.rw.data, aes(x=.data[[col]], y=.data[[target_col]])) +
      geom_boxplot(fill="lightblue", color="black") +
      labs(title=paste("Boxplot of", target_col, "by", col),
           x=col, y=target_col)
  }
  
  # Print the plot
  print(p)
}


#for W1empsdad, the mean income for the missing data lands approximately in between the other two levels (working/education and not working)
#for W1GrssyrHH and W8QDEB2, the mean income for the missing data is significantly lower than the non-missing data
#for the W4alcfreqYP, the pattern is even more obvious, with the missing level having a much lower mean than all other levels

#for the w6educYP we initially see a breaking in the pattern, as the missing level is higher than the other levels
#however, when checking the summary, we see that the missing data mostly comes from -91(not applicable) whereas only 2 YP refused
#thus the missing level here is a bit misleading as they may already be in work due to them being above school age or having graduated from college already
summary(assig.dat$`W6EducYP(YP: Whether currently going to school or college)`)
#We note that this variable is completely explained by W6UnivYP(Already removed.. but all missingness in this predictor has a value of 1 in W6UnivYP)

#finally for the W6neetact, the missing level is once again misleading, the data isn't really missing, nearly all the missing data, bar 1 row comes
#from people being not applicable to be a NEET, this means that they are in employment, education or training, and we thus expect their mean income to be higher
summary(assig.dat$`W6NEETAct(Activities of NEETs (Not in Employment or Education))`)

#finally for both continuous predictors, from the plots, it is clear that the mean weekly income is lower when the data is NA compared to when there were responses

#to conclude, we see that 4 of the interesting missing columns out of the original 6 we analysed have "real" missing data
#for those 4 columns, we see that there is a clear pattern showing that missing data contributes to a lower mean weekly income
#we will now look to prove this by taking a different approach, analyzing the mean weekly income for when all 4 of the relevant columns

# Identify rows where ALL actual_missing columns have "missing"
actual_missing<-c("W1empsdad(Employment status of father)", 
                  "W4AlcFreqYP(Frequency of having an alcoholic drink in last 12 months)", "W8QDEB2(Total amount owe)",
                  "W1GrssyrHH(Gross annual salary of household)")

missing_any_rows <- rowSums(is.na(new.rw.data[actual_missing]) | new.rw.data[actual_missing] == "missing") > 0
sum(missing_any_rows) # Rows with any missing values on the interesting columns (2574)

# Identify rows where ALL actual_missing columns have "missing" or NA values
missing_all_rows <- rowSums(is.na(new.rw.data[actual_missing]) | new.rw.data[actual_missing] == "missing") == length(actual_missing)
sum(missing_all_rows) #checking our sample size off rows with missing values in all those columns, 47 is a small sample size

# Identify rows where ALL or minus 1 actual_missing columns have "missing" or NA values
missing_minus1_rows <- rowSums(is.na(new.rw.data[actual_missing]) | new.rw.data[actual_missing] == "missing") >= length(actual_missing) - 1
sum(missing_minus1_rows) #checking our sample size of rows with missing values in all (minus 1 at most) those columns, 381 is a significant sample size

# Compute means for W8DINCW
mean_missing_minus1 <- mean(new.rw.data$W8DINCW[missing_minus1_rows])
mean_not_missing_minus1 <- mean(new.rw.data$W8DINCW[!missing_minus1_rows])

overall_mean <- mean(new.rw.data$W8DINCW)
overall_sd <- sd(new.rw.data$W8DINCW)

# Compute how many standard deviations the missing group's mean is from the overall mean
sd_from_mean <- (mean_missing_minus1 - overall_mean) / overall_sd

#running a t test to show the missing data's impact on weekly income is significant and not random
t.test(new.rw.data$W8DINCW[missing_minus1_rows], 
       new.rw.data$W8DINCW[!missing_minus1_rows], 
       alternative = "less")

# Print results
print(paste("Number of rows missing across all 4 (minus at most 1) analysed columns:", sum(missing_minus1_rows)))
print(paste("Mean W8DINCW for rows missing in all 4 (minus at most 1) analysed columns:", round(mean_missing_minus1, 2)))
print(paste("Mean W8DINCW for rows not missing in all 4 (minus at most 1) analysed columns:", round(mean_not_missing_minus1, 2)))
print(paste("Overall mean W8DINCW:", round(overall_mean, 2)))
print(paste("Overall SD of W8DINCW:", round(overall_sd, 2)))
print(paste("Mean of missing group is", round(sd_from_mean, 2), "SDs away from the overall mean"))

# Create a histogram to compare income distributions
ggplot(new.rw.data, aes(x = `W8DINCW(Continuous weekly income)`)) +
  geom_histogram(aes(fill = as.factor(missing_minus1_rows)), bins = 30, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Not Missing All (minus 1 at most)", "Missing All (minus 1 at most)")) +
  labs(title = "Income Distribution: Missing vs Not Missing All",
       x = "Weekly Income (W8DINCW)", 
       y = "Count", 
       fill = "Missing Columns") +
  theme_minimal()
# we can see that there is a big difference between the mean weekly income for the group missing all data from our 4 analysed columns
#compared to the other group, this reinforces the pattern that we saw earlier, missing data in those 4 columns mean a lower weekly income on average
#the difference is statistically significant too based on the t-test, with the mean being -1.13 SDs away from the actual mean of weekly income
#finally, the histogram reinforces our pattern; missing data implies a lower weekly income on average

#bias implications: Missing data is not completely random—it’s more common among lower-income individuals, 
#leading to an under representation of financially struggling groups as we have more missing data for these groups compared to higher income groups
#To reduce bias, increasing response rates in these groups is crucial (e.g., targeted follow-ups, incentives).
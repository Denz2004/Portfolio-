#cleaning the dataset

#removing all na values in the FSMband, IDACI_n, singlepar, fsm and sen columns
new.rw.data<-subset(assig.dat,(is.na(IDACI_n)==FALSE & is.na(FSMband)==FALSE & sen!='missing' & fsm!='missing' & singlepar!="missing"))
dim(new.rw.data) #checking new dimensions

#in the exploratory part, we saw that exclude and absent seem to be correlated
#to test if they are correlated, we turn the data into categorical numerical data to be able to analyse their correlation
exclude <- ifelse(new.rw.data$exclude == "Yes", 1,
                              ifelse(new.rw.data$exclude == "missing", 0.4, 0))

absent <- ifelse(new.rw.data$absent == "Yes", 1,
                             ifelse(new.rw.data$absent == "missing", 0.4, 0))
library(rcompanion)

# Here we check if absent and exclude are correlated
tbl <- table(exclude, absent)

# Compute Cramér’s V
cramerV(tbl)
#since they are highly correlated, we will remove the least significant of the 2 predictors in the regression part

# Check correlation between k3ma, k3sc, k3en
cor(new.rw.data$k3ma, new.rw.data$k3sc)
cor(new.rw.data$k3ma, new.rw.data$k3en)
cor(new.rw.data$k3sc, new.rw.data$k3en)
#as the correlation is high between all columns, combine all scores into one column:
new.rw.data$k3_combined <- rowMeans(new.rw.data[, c("k3ma", "k3sc", "k3en")])
#drop the original columns as we have the new summed column
new.rw.data <- new.rw.data[, !names(new.rw.data) %in% c("k3ma", "k3sc", "k3en")]

#getting the number of unqiue values per col to merge levels of categorical predictors when we have too many levels
print(sapply(new.rw.data, function(col) length(unique(col))))


#making a function that will help identify the levels we can merge in categorical predicors
get_level_stats <- function(data, column_name, custom_order) {
  # Loop through each custom level in the specified order
  for (level in custom_order) {
    # Subset the data for the current level
    subset_data <- data[data[[column_name]] == level, ]
    
    # Count how many rows there are for the current level
    count_values <- nrow(subset_data)
    
    # Calculate the mean of ks4score for the current level
    mean_ks4score <- mean(subset_data$ks4score)
    
    # Print the level, length of values for that level, and mean of ks4score
    cat(level, "LENGTH:", count_values, "\n")
    print(mean_ks4score)
  }
}

# Starting with FSMband
custom_order_FSMband <- c("<5pr", "5pr-9pr", "9pr-13pr", "13pr-21pr", "21pr-35pr", "35pr+")
get_level_stats(new.rw.data, "FSMband", custom_order_FSMband)

# From the function, we can see that we can merge 9-13pr and 13-21 pr and also 21-35pr and 35+pr
levels(new.rw.data$FSMband)[levels(new.rw.data$FSMband) %in% c("9pr-13pr", "13pr-21pr")] <- "9pr-21pr"
levels(new.rw.data$FSMband)[levels(new.rw.data$FSMband) %in% c("21pr-35pr", "35pr+")] <- "21pr-35pr+"

# Checking our changes
summary(new.rw.data$FSMband)

# Now for attitude
custom_order_attitude <- c("missing", "very_low", "low", "high", "very_high")
get_level_stats(new.rw.data, "attitude", custom_order_attitude)

# From the function, we can see that we can merge high and very high
#we do not merge low and very_low as they have substantialy different score means
levels(new.rw.data$attitude)[levels(new.rw.data$attitude) %in% c("high", "very_high")] <- "high"

# Checking our changes
summary(new.rw.data$attitude)

# Homework is the next column with too many levels
custom_order_hmwrk <- c("missing", "none", "1_evening", "2_evenings", "3_evenings", "4_evenings", "5_evenings")
get_level_stats(new.rw.data, "homework", custom_order_hmwrk)

# From the function, we can see that we can merge 4 and 5 evenings and 0 and 1 evenings
levels(new.rw.data$homework)[levels(new.rw.data$homework) %in% c("4_evenings", "5_evenings")] <- "4-5_evenings"
levels(new.rw.data$homework)[levels(new.rw.data$homework) %in% c("1_evening", "none")] <- "0-1_evenings"

# Checking our changes
summary(new.rw.data$homework)


# Hiquamum is the next categorical predictor with too many levels
custom_order_hiquamum <- c("missing", "No_qualification", "Other_qualifications", "GCSE_grades_A-C_or_equiv", "GCE_A_Level_or_equivalent", "HE_below_degree_level", "Degree_or_equivalent")
get_level_stats(new.rw.data, "hiquamum", custom_order_hiquamum)

# From the function, we can see that we can merge no and other qualifications and he below degree level and gcse A level
levels(new.rw.data$hiquamum)[levels(new.rw.data$hiquamum) %in% c("No_qualification", "Other_qualifications")] <- "No/Other_qualifications"
levels(new.rw.data$hiquamum)[levels(new.rw.data$hiquamum) %in% c("HE_below_degree_level", "GCE_A_Level_or_equivalent")] <- "GCE_A_Level_or_equivalent_or_HE_below_degree_level"

# Checking our changes
summary(new.rw.data$hiquamum)

#writing the new cleaned CSV file that we will use in the MLR
write.csv(new.rw.data, file = "new_rw_data.csv", row.names = FALSE)

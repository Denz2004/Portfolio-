library(ggplot2)
library(arm)
library(car)

# EXPLORING THE DATA

assig.dat <- read.csv("INC.csv", header=TRUE, stringsAsFactors=TRUE) #read the assignment data

# Remove NSID column as it is useless
assig.dat <- subset(assig.dat, select = -NSID)

# Used to find the proportion of missing data in each predictor
count_negative_positive <- function(column) {
  # Count negative values (-1 or less) and positive values (0 or greater)
  neg <- as.integer(sum(column <= -1, na.rm = TRUE))
  pos <- as.integer(sum(column >= 0, na.rm = TRUE))
  
  # Compute the percentage of missing values
  percentage_missing <- round(100 * neg / (neg + pos), 2)
  
  # Count the number of unique values
  unique_values <- length(unique(na.omit(column)))  # Excluding NA values
  
  # Return all computed values
  return(c(negative = neg, positive = pos, percentage_missing = percentage_missing, unique_values = unique_values))
}


# Finding proportion of missing values for all columns and print results
information_for_cleaning<-sapply(assig.dat, count_negative_positive)
information_for_cleaning

# Extract row indices for reference
row_names <- rownames(information_for_cleaning)

# Convert matrix to data frame for easier processing
df <- as.data.frame(information_for_cleaning)

# Assign row names as column names
colnames(df) <- colnames(information_for_cleaning)

# Extract values
unique_values <- as.numeric(df["unique_values", ])
percentage_missing <- as.numeric(df["percentage_missing", ])


# Hardcoding the continuous columns with less than 30% missing:
cont_list <- c("W1yschat1", "W2ghq12scr", "W4schatYP", "W6DebtattYP", "W8DINCW", "W8DGHQSC")

# List 1: continuous columns with more than 30 percent missing values (columns to be removed from a final model)
cont_with_more_than_30_per <- names(df)[unique_values > 300 & percentage_missing >= 30]

# List 2: categorical columns with less than 10% missing (rows to be removed in cleaning part)
cat_with_less_than_10_per <- names(df)[(percentage_missing < 10 & unique_values <= 300) 
                                       & !(names(df) %in% cont_list)] 
# List 3: categorical columns with not many levels or more than 10% missing values (we will transform the rows in the cleaning part to missing)
transform_into_missing <- c(names(df)[unique_values < 300 & percentage_missing >= 10], cont_list, cont_with_more_than_30_per)

# Print the lists with their lengths
cat("cont_with_more_than_30_per:", cont_with_more_than_30_per, "\nLength:", length(cont_with_more_than_30_per), "\n")
cat("cat_with_less_than_10_per:", cat_with_less_than_10_per, "\nLength:", length(cat_with_less_than_10_per), "\n")
cat("transform_into_missing:", transform_into_missing, "\nLength:", length(transform_into_missing), "\n")

# Read the dictionary file as this will help us with readability
dict.dat <- read.csv("DataDictTranslation.csv", header=TRUE, stringsAsFactors=FALSE)

# Filter dict.dat to keep only rows where Variable exists in assig.dat
dict.dat <- dict.dat[dict.dat$Variable %in% names(assig.dat), ]

# Convert columns to categorical or continuous based on "Type" column
for (col in dict.dat$Variable) {
  var_type <- dict.dat$Type[dict.dat$Variable == col]
  
  if (var_type == "categorical" || var_type == "binary") {
    assig.dat[[col]] <- as.factor(assig.dat[[col]])  # Convert to categorical
  } else if (var_type == "continuous") {
    assig.dat[[col]] <- as.numeric(as.character(assig.dat[[col]]))  # Convert to numeric
  }
}

# Create new column names by combining variable names with meanings
dict.dat$NewName <- paste0(dict.dat$Variable, "(", dict.dat$Meaning, ")")

# Rename columns in assig.dat using dictionary mapping
names(assig.dat) <- dict.dat$NewName[match(names(assig.dat), dict.dat$Variable)]

# Print count of each type in dict.dat
print(table(dict.dat$Type))  

# Get and print the actual data types in assig.dat
assig_types <- sapply(assig.dat, class)
print(table(assig_types))  

dim(assig.dat) #looking at the dimensions
head(assig.dat) #looking at the data
summary(assig.dat) #getting the summary of each col

#looping through each col, and looking at it's impact on the weekly income

target_col <- "W8DINCW(Continuous weekly income)"
for (col in names(assig.dat)) {
  if (col == target_col) next  # Skip the weekly income column itself
  
  # Check if the column is numeric or categorical
  if (is.numeric(assig.dat[[col]])) {
    # Scatter plot for numeric columns
    p <- ggplot(na.omit(assig.dat), aes(x=.data[[col]], y=.data[[target_col]])) +
      geom_point(alpha=0.6, color="blue") + geom_smooth(method = "lm")
    labs(title=paste("Scatter Plot of", col, "vs", target_col),
         x=col, y=target_col)
  } else {
    # Boxplot for categorical columns
    p <- ggplot(assig.dat, aes(x=.data[[col]], y=.data[[target_col]])) +
      geom_boxplot(fill="lightblue", color="black") +
      labs(title=paste("Boxplot of", target_col, "by", col),
           x=col, y=target_col)
  }
  
  # Print the plot
  print(p)
}

#based on these result:
#we will remove rows with missing values if it is a categorical predictor and has less than 10% missing values
#take note of continuous predictors with more than 30% missing data to be removed from a final model
#if the categorical predictor has more than 10% missing rows, we will group them into a missing level
#we will also explore if their is a missingness pattern for some interesting predictors









# CLEANING THE DATASET

new.rw.data <- assig.dat

# Create a helper function to match column names ignoring bracketed info
clean_col_names <- function(col_names) {
  return(sub("\\(.*\\)", "", col_names))  # Removes anything inside parentheses
}

# Clean column names from the new.rw.data
col_names_cleaned <- clean_col_names(names(new.rw.data))

# Step 1: Remove rows where columns in cat_with_less_than_10_per
for (col in names(new.rw.data)) {
  if (clean_col_names(col) %in% cat_with_less_than_10_per) {
    # Convert factor to numeric temporarily
    new.rw.data[[col]] <- as.numeric(as.character(new.rw.data[[col]]))
    new.rw.data <- new.rw.data[new.rw.data[[col]] >= 0, ]
    # Convert back to factor only if NOT in cont_list
    if (!(clean_col_names(col) %in% cont_list)) {
      new.rw.data[[col]] <- as.factor(new.rw.data[[col]])
    }
  }
}

# Step 2: Replace values < 0 with "missing" in transform_into_missing columns
for (col in names(new.rw.data)) {
  if (clean_col_names(col) %in% transform_into_missing) {
    # Convert factor to numeric temporarily
    new.rw.data[[col]] <- as.numeric(as.character(new.rw.data[[col]]))
    
    # Replace negative values with NA for numeric columns in cont_list, or "missing" for others
    if (clean_col_names(col) %in% cont_list | clean_col_names(col) %in% cont_with_more_than_30_per) {
      new.rw.data[[col]][new.rw.data[[col]] < 0] <- NA  # Use NA for numeric columns in cont_list
    } else {
      new.rw.data[[col]][new.rw.data[[col]] < 0] <- "missing"  # Use "missing" for other columns
    }
    
    # Convert back to factor only if NOT in cont_list
    if (!(clean_col_names(col) %in% cont_list)&!(clean_col_names(col) %in% cont_with_more_than_30_per)) {
      new.rw.data[[col]] <- as.factor(new.rw.data[[col]])
    }
  }
}

# Replotting everything after the missing/NAs are added
target_col <- "W8DINCW(Continuous weekly income)"
dev.off() # Clearing the previous plots
for (col in names(new.rw.data)) {
  if (col == target_col) next  # Skip the weekly income column itself
  
  # Check if the column is numeric or categorical
  if (is.numeric(new.rw.data[[col]])) {
    # Scatter plot for numeric columns
    p <- ggplot(na.omit(new.rw.data), aes(x=.data[[col]], y=.data[[target_col]])) +
      geom_point(alpha=0.6, color="blue") + geom_smooth(method = "lm")
    labs(title=paste("Scatter Plot of", col, "vs", target_col),
         x=col, y=target_col)
  } else {
    # Boxplot for categorical columns
    p <- ggplot(new.rw.data, aes(x=.data[[col]], y=.data[[target_col]])) +
      geom_boxplot(fill="lightblue", color="black") +
      labs(title=paste("Boxplot of", target_col, "by", col),
           x=col, y=target_col)
  }
  
  # Print the plot
  print(p)
}

# Data set is now clean, we just need to re level some of the categorical columns that have too many levels

# Count and print the number of unique levels for each factor column where it has more than 6 levels
#we will transform the data so that we have at most 5 levels per factor
for (col in names(new.rw.data)) {
  if (is.factor(new.rw.data[[col]])) {
    length_of_col<-length(levels(new.rw.data[[col]]))
    if (length_of_col>5){
      cat(col, "has", length(levels(new.rw.data[[col]])), "unique levels\n")}
  } else {
    # Checking proportion of 0 of a continuous predictor, to see if converting it to binary is justifiable
    prop_zeros <- sum(new.rw.data[[col]] == 0, na.rm = TRUE) / sum(!is.na(new.rw.data[[col]]))
    
    if (prop_zeros > 30) {  # we can adjust this threshold as needed
      cat(col, "has", round(prop_zeros * 100, 1), "% zeros — consider binarizing\n")
    }
  }
}

# We see that W2ghqscr, and W8DGHQSC have a high percentage of zero values. We will convert these into binary
# These would mean that we are comparing between abscense mental health symptoms / presence of symptoms
new.rw.data$W2ghq12scr_binary <- factor(ifelse(new.rw.data$W2ghq12scr > 0, 1, 0),
                                        levels = c(0, 1),
                                        labels = c("Zero", "Non-Zero"))

new.rw.data$W8DGHQSC_binary <- factor(ifelse(new.rw.data$W8DGHQSC > 0, 1, 0),
                                      levels = c(0, 1),
                                      labels = c("Zero", "Non-zero"))

#removing the old continuous columns:
new.rw.data$`W2ghq12scr(YP GHQ12 score)` <- NULL
new.rw.data$`W8DGHQSC(General Health Questionnaire (GHQ12) score )` <- NULL

# Checking multicollinearity on all predictors
tryCatch({ 
  vif(lm(`W8DINCW(Continuous weekly income)` ~ ., data=new.rw.data))   
}, error = function(e) {   
  print(paste("Caught an error:", e$message)) 
})
# It seems that there is an error about aliased coefficients..
summary(lm(`W8DINCW(Continuous weekly income)` ~ ., data=new.rw.data)) # Even without doing anything, our R2 is 0.7344, but our adjusted R2 is 0.6638
# Looking at the summary, and taking note of the predictors where the estimated coefficients are NA:
# - W1wrkfulldad, W1empsmum, W1empsdad, W1ch3_11HH, W6EducYP, W6als, W8DWRK, W8DACTIVITYC

# Hence, based on the data description, we suspect that W6UnivYP-W6EducYP might be the issue
table(new.rw.data$`W6EducYP(YP: Whether currently going to school or college)`, new.rw.data$`W6UnivYP(YP: Whether currently at university)`)
# The table tells us that these two predictors are perfectly correlated.. 
tryCatch({
  vif(lm((`W8DINCW(Continuous weekly income)` ~ `W6EducYP(YP: Whether currently going to school or college)` + `W6UnivYP(YP: Whether currently at university)`), data = new.rw.data))
}, error = function(e) {
  print(paste("Caught an error:", e$message))
})
# When only considering the two predictors mentioned, it shows there is an error on aliased coefficients
# so it is correct to say these two are aliased coefficients

# we will remove W6UnivYP because it has less levels, in order to reduce loss of information
new.rw.data$`W6UnivYP(YP: Whether currently at university)` <- NULL

# Checking the VIF of all predictors
tryCatch({    
  vif(lm(`W8DINCW(Continuous weekly income)` ~ ., data=new.rw.data))    
}, error = function(e) {      
  print(paste("Caught an error:", e$message))  
})

# It seems that there is an error about aliased coefficients still..
# Hence, based on the data description, we suspect that W8DACTIVITYC-W8DACTIVITY-W8DWRK might be the issue
table(new.rw.data$`W8DACTIVITYC(Current activity of CM)`, new.rw.data$`W8DACTIVITY(Current activity)`)
# There is not perfect correlation case for W8DACTIVITIYC-W8DACTIVITY, but they seem very highly correlated
table(new.rw.data$`W8DWRK(Whether CM currently employed)`, new.rw.data$`W8DACTIVITY(Current activity)`)
# This is not a perfect correlation case for W8DWRK-W8DACTIVITY
table(new.rw.data$`W8DWRK(Whether CM currently employed)`, new.rw.data$`W8DACTIVITYC(Current activity of CM)`)
# The table tells us that these two predictors are perfectly correlated.. 
tryCatch({
  vif(lm((`W8DINCW(Continuous weekly income)` ~ `W8DWRK(Whether CM currently employed)` + `W8DACTIVITYC(Current activity of CM)`), data = new.rw.data))
}, error = function(e) {
  print(paste("Caught an error:", e$message))
})
# Only considering W8DWRK and W8DACTIVITYC, the vif shows there are aliased coefficients
# so we will remove W8DWRK because it has less levels, in order to reduce loss of information
new.rw.data$`W8DWRK(Whether CM currently employed)` <- NULL
# Checking if W8DACTIVITY and W8DACTIVITYC are aliased
tryCatch({
  vif(lm((`W8DINCW(Continuous weekly income)` ~ `W8DACTIVITY(Current activity)` + `W8DACTIVITYC(Current activity of CM)`), data = new.rw.data))
}, error = function(e) {
  print(paste("Caught an error:", e$message))
})
# Still error showing there are aliased coefficients... so we will remove the one that minimises data loss
new.rw.data$`W8DACTIVITYC(Current activity of CM)` <- NULL

# Checking the VIF of all predictors
tryCatch({    
  vif(lm(`W8DINCW(Continuous weekly income)` ~ ., data=new.rw.data))    
}, error = function(e) {      
  print(paste("Caught an error:", e$message))  
})

# It seems that there is an error about aliased coefficients still..
# Based on the data description, we suspect that W1wrkfulldad-W1empsdad & W1wrkfullmum-W1empsmum might be the issue
table(new.rw.data$`W1wrkfulldad(Whether father works full-time)`, new.rw.data$`W1empsdad(Employment status of father)`)
table(new.rw.data$`W1wrkfullmum(Whether mother works full-time)`, new.rw.data$`W1empsmum(Employment status of mother)`)
# Based on the two tables, both are perfectly correlated..
tryCatch({ 
  vif(lm((`W8DINCW(Continuous weekly income)` ~ `W1wrkfullmum(Whether mother works full-time)` + `W1empsmum(Employment status of mother)`), data = new.rw.data))
}, error = function(e) {   
  print(paste("Caught an error:", e$message)) 
})

tryCatch({ 
  vif(lm((`W8DINCW(Continuous weekly income)` ~ `W1wrkfulldad(Whether father works full-time)` + `W1empsdad(Employment status of father)`), data = new.rw.data))
}, error = function(e) {   
  print(paste("Caught an error:", e$message)) 
})

# Both shows errors of aliased coefficients

# so we will remove W1wrkfulldad and W1wrkfullmum because it has less levels, in order to reduce loss of information
new.rw.data$`W1wrkfullmum(Whether mother works full-time)` <- NULL
new.rw.data$`W1wrkfulldad(Whether father works full-time)` <- NULL

# Checking the VIF of all predictors
tryCatch({    
  vif(lm(`W8DINCW(Continuous weekly income)` ~ ., data=new.rw.data))    
}, error = function(e) {      
  print(paste("Caught an error:", e$message))  
})

# Referring back to the current summary
summary(lm(`W8DINCW(Continuous weekly income)` ~ ., data=new.rw.data))
# It seems there are two levels of W6als, 1 level of W8DACTIVITY, the missing level for W1empsdad, and 1 level of W1ch3_11HH with NA coefficients in the model
# It is not really clear to us what predictors might be related to these... so we will proceed with merging of levels first
# After the merging process we will again use the VIF to see if there are still aliased coefficients

# We note that the variables W1hiqualdad and W1hiqualmum are ordinal (lower number = higher qualification).
# We decided to merge the two columns using the average.
new.rw.data$W1hiqualparents_score <- # I changed the name here but stick to what is used in the original file also works
  (as.numeric(new.rw.data$`W1hiqualmum(Mother’s highest qualification)`) + 
     as.numeric(new.rw.data$`W1hiqualdad(Father’s highest qualification)`)) / 2

summary(new.rw.data$W1hiqualparents_score)

# Remove the original individual columns
new.rw.data$`W1hiqualmum(Mother’s highest qualification)` <- NULL
new.rw.data$`W1hiqualdad(Father’s highest qualification)` <- NULL

dim(new.rw.data) #check the new dimensions

#for the remaining columns with too many levels, we will merge them
for (col in names(new.rw.data)) {
  if (is.factor(new.rw.data[[col]])) {
    length_of_col<-length(levels(new.rw.data[[col]]))
    if (length_of_col>5){
      cat(col, "has", length(levels(new.rw.data[[col]])), "unique levels\n")}
  }
}


#making a function that will help identify the levels we can merge in categorical predictors
get_level_stats <- function(data, column_name, custom_order) {
  # Loop through each custom level in the specified order
  for (level in custom_order) {
    # Subset the data for the current level
    subset_data <- data[data[[column_name]] == level, ]
    
    # Count how many rows there are for the current level
    count_values <- nrow(subset_data)
    
    # Calculate the mean of weekly income for the current level
    mean_W8DINCW <- mean(subset_data$`W8DINCW(Continuous weekly income)`)
    
    # Print the level, length of values for that level, and mean of W8DINCW(Continuous weekly income)
    cat(level, "LENGTH:", count_values, "\n")
    print(mean_W8DINCW)
  }
}

# Starting with W1wrk1aMP(MP: current working status)

# Custom order for the levels
custom_order_W1wrk1aMP <- 1:12

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1wrk1aMP(MP: current working status)", custom_order_W1wrk1aMP)

# Merging levels 1-4(working) and all the others (reasoning: levels make sense & similar shape boxplot)
levels(new.rw.data$`W1wrk1aMP(MP: current working status)`)[levels(new.rw.data$`W1wrk1aMP(MP: current working status)`) %in% 1:4] <- "Working"
levels(new.rw.data$`W1wrk1aMP(MP: current working status)`)[levels(new.rw.data$`W1wrk1aMP(MP: current working status)`) %in% 5:12] <- "Not Working"

# Checking the changes
summary(new.rw.data$`W1wrk1aMP(MP: current working status)`)

# Now for W1NoldBroHS(Number of younger siblings)

# Custom order for the levels
custom_order_W1NoldBroHS <- c(0,1,2,3,4,5,6,7,9)

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1NoldBroHS(Number of younger siblings)", custom_order_W1NoldBroHS)

# Merging levels 0-1 and 2+ (reasoning: similar boxplot + to balance distributions since some have very little observations)
levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`)[levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`) %in% 0:1 ] <- "0-1"
levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`)[levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`) %in% c(2,3,4,5,6,7,9,12)] <- "2+"

# Checking the changes
summary(new.rw.data$`W1NoldBroHS(Number of younger siblings)`)

# W1hous12HH(Tenure) is the next column with too many levels

# Custom order for the levels
custom_order_W1hous12HH <- 1:8

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1hous12HH(Tenure)", custom_order_W1hous12HH)

# Merging levels 1-3(house is owned) and 4-8(house is rented/other) ... (based on levels that makes sense as a category)
levels(new.rw.data$`W1hous12HH(Tenure)`)[levels(new.rw.data$`W1hous12HH(Tenure)`) %in% 1:3] <- "Owned"
levels(new.rw.data$`W1hous12HH(Tenure)`)[levels(new.rw.data$`W1hous12HH(Tenure)`) %in% 4:8] <- "Rented/Other"

# Checking the changes
summary(new.rw.data$`W1hous12HH(Tenure)`)

# W1empsdad(Employment status of father) is next

# Custom order for the levels
custom_order_W1empsdad <- 1:9

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1empsdad(Employment status of father)", custom_order_W1empsdad)

# Merging levels 1-2and 5(working) and 3,4,6,7,8,9(not working)... (reasoning: based on boxplot and to balance out distributions)
levels(new.rw.data$`W1empsdad(Employment status of father)`)[levels(new.rw.data$`W1empsdad(Employment status of father)`) %in% c(1,2)] <- "Working/Education"
levels(new.rw.data$`W1empsdad(Employment status of father)`)[levels(new.rw.data$`W1empsdad(Employment status of father)`) %in% c(3,4,5,6,7,8,9)] <- "Not Working"

# Checking the changes
summary(new.rw.data$`W1empsdad(Employment status of father)`)

# W1empsmum(Employment status of mother) is next  ... (Not in main folder) ...

# Custom order for the levels
custom_order_W1empsmum <- 1:9

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1empsmum(Employment status of mother)", custom_order_W1empsmum)

# Merging levels 1,2(working) and 3,4,5,6,7,8,9(not working)... (reasoning: based on boxplot and to balance out distributions)
levels(new.rw.data$`W1empsmum(Employment status of mother)`)[levels(new.rw.data$`W1empsmum(Employment status of mother)`) %in% c(1,2)] <- "Working"
levels(new.rw.data$`W1empsmum(Employment status of mother)`)[levels(new.rw.data$`W1empsmum(Employment status of mother)`) %in% c(3,4,5,6,7,8,9)] <- "Not Working"

# Checking the changes
summary(new.rw.data$`W1empsmum(Employment status of mother)`)

# W1ch3_11HH(Number of children aged 3-11 in HH) is next

# Custom order for the levels
custom_order_W1ch3_11HH <- 0:5

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1ch3_11HH(Number of children aged 3-11 in HH)", custom_order_W1ch3_11HH)

# Merging levels 0-1 and 2+ ... (reasoning: to balance distributions + based on boxplot)
levels(new.rw.data$`W1ch3_11HH(Number of children aged 3-11 in HH)`)[levels(new.rw.data$`W1ch3_11HH(Number of children aged 3-11 in HH)`) %in% 0:1] <- "0-1"
levels(new.rw.data$`W1ch3_11HH(Number of children aged 3-11 in HH)`)[levels(new.rw.data$`W1ch3_11HH(Number of children aged 3-11 in HH)`) %in% 2:5] <- "2+"

# Checking the changes
summary(new.rw.data$`W1ch3_11HH(Number of children aged 3-11 in HH)`)

# W1marstatmum(Marital status of mother) is next

# Custom order for the levels
custom_order_W1marstatmum <- 1:7

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1marstatmum(Marital status of mother)", custom_order_W1marstatmum)

# Merging levels 2&3(living with partner) and all others(single divorced etc)... (based on what makes sense to be made into a binary predictor)
levels(new.rw.data$`W1marstatmum(Marital status of mother)`)[levels(new.rw.data$`W1marstatmum(Marital status of mother)`) %in% 2:3] <- "Living With Partner"
levels(new.rw.data$`W1marstatmum(Marital status of mother)`)[levels(new.rw.data$`W1marstatmum(Marital status of mother)`) %in% c(1,4,5,6,7)] <- "Not Living With Partner"

# Checking the changes
summary(new.rw.data$`W1marstatmum(Marital status of mother)`)

# W1depkids(Number of dependent children in HH) is next

# Custom order for the levels
custom_order_W1depkids <- 1:10

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1depkids(Number of dependent children in HH)", custom_order_W1depkids)

# Merging levels 1-3 and 4+ (Changed to binary, based on boxplot shape)
levels(new.rw.data$`W1depkids(Number of dependent children in HH)`)[levels(new.rw.data$`W1depkids(Number of dependent children in HH)`) %in% 1:3] <- "1-3"
levels(new.rw.data$`W1depkids(Number of dependent children in HH)`)[levels(new.rw.data$`W1depkids(Number of dependent children in HH)`) %in% 4:10] <- "4+"

# Checking the changes
summary(new.rw.data$`W1depkids(Number of dependent children in HH)`)

# W1nssecfam(Family’s NS-SEC class)

# Custom order for the levels
custom_order_W1nssecfam <- 1:8

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1nssecfam(Family’s NS-SEC class)", custom_order_W1nssecfam)

# Merging levels 1-5(modern occupations) and 6+(routine occupations/no job)... (Based on what makes sense to be changed to a binary + based on boxplot)
levels(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`)[levels(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`) %in% 1:5] <- "Professional/Modern occupations"
levels(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`)[levels(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`) %in% 6:8] <- "Routine occupations/Unemployed"


# Checking the changes
summary(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`)

# W1ethgrpYP(Young person’s ethnic group) is next

# Custom order for the levels
custom_order_W1ethgrpYP <- 1:8

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1ethgrpYP(Young person’s ethnic group)", custom_order_W1ethgrpYP)

# Merging levels 1(white) and 2+(other ethnicities)... (Changed to binary, makes sense based on the mean on the boxplots)
levels(new.rw.data$`W1ethgrpYP(Young person’s ethnic group)`)[levels(new.rw.data$`W1ethgrpYP(Young person’s ethnic group)`) %in% 1:1] <- "White"
levels(new.rw.data$`W1ethgrpYP(Young person’s ethnic group)`)[levels(new.rw.data$`W1ethgrpYP(Young person’s ethnic group)`) %in% 2:8] <- "Other"

# Checking the changes
summary(new.rw.data$`W1ethgrpYP(Young person’s ethnic group)`)

# W1hwndayYP(Number of evenings of HWK per week) is next

# Custom order for the levels
custom_order_W1hwndayYP <- 0:5

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1hwndayYP(Number of evenings of HWK per week)", custom_order_W1hwndayYP)

# Merging levels 0 and 1 (Just to reduce to only 5 levels, since level 0 has the least amount of observations)
levels(new.rw.data$`W1hwndayYP(Number of evenings of HWK per week)`)[levels(new.rw.data$`W1hwndayYP(Number of evenings of HWK per week)`) %in% 0:1] <- "0-1"

# Checking the changes
summary(new.rw.data$`W1hwndayYP(Number of evenings of HWK per week)`)

# The next predictor with too many levels is W4AlcFreqYP(Frequency of having an alcoholic drink in last 12 months)

# Custom order for the levels
custom_order_W4AlcFreqYP <- 1:6

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W4AlcFreqYP(Frequency of having an alcoholic drink in last 12 months)", custom_order_W4AlcFreqYP)

# Merging levels 1-4, and 5-6 (similar boxplots... reduced to binary)
levels(new.rw.data$`W4AlcFreqYP(Frequency of having an alcoholic drink in last 12 months)`)[levels(new.rw.data$`W4AlcFreqYP(Frequency of having an alcoholic drink in last 12 months)`) %in% 1:4] <- "1-4"
levels(new.rw.data$`W4AlcFreqYP(Frequency of having an alcoholic drink in last 12 months)`)[levels(new.rw.data$`W4AlcFreqYP(Frequency of having an alcoholic drink in last 12 months)`) %in% 5:6] <- "5-6"

# Checking the changes
summary(new.rw.data$`W4AlcFreqYP(Frequency of having an alcoholic drink in last 12 months)`)

# The next predictor with too many levels is W4empsYP(Employment status of young person)

# Custom order for the levels
custom_order_W4empsYP <- c(1,2,3,4,5,6,9)
# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W4empsYP(Employment status of young person)", custom_order_W4empsYP)

# Merging levels 1,2 (working), 5 (studying) and the others(not in work or education)... (Based on what makes sense, and following the boxplot distributions)
levels(new.rw.data$`W4empsYP(Employment status of young person)`)[levels(new.rw.data$`W4empsYP(Employment status of young person)`) %in% c(1,2)] <- "Working"
levels(new.rw.data$`W4empsYP(Employment status of young person)`)[levels(new.rw.data$`W4empsYP(Employment status of young person)`) %in% c(5)] <- "In Education"
levels(new.rw.data$`W4empsYP(Employment status of young person)`)[levels(new.rw.data$`W4empsYP(Employment status of young person)`) %in% c(3,4,6,9)] <- "Not Working Or In Education"

# Checking the changes
summary(new.rw.data$`W4empsYP(Employment status of young person)`)

# The next predictor with too many levels is W6acqno(Highest academic qualification studied at Wave 6)

# Custom order for the levels
custom_order_W6acqno <- 1:9
# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W6acqno(Highest academic qualification studied at Wave 6)", custom_order_W6acqno)

# Merging levels 1-2(Higher Education) and 3-8(Alevel/lower), and 9(no academic study aim)... (Based on what makes sense, and to balance out the distribution + referred to boxplot)
levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`)[levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`) %in% 1:2] <- "Higher Education"
levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`)[levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`) %in% 3:8] <- "A level/lower"
levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`)[levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`) %in% 9:9] <- "No Academic Study Aim"

# Checking the changes
summary(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`)

# We will now merge levels in W8DMARSTAT(Legal marital status)

# Custom order for the levels
custom_order_W8DMARSTAT <- 1:8
# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W8DMARSTAT(Legal marital status)", custom_order_W8DMARSTAT)

# Merging levels 1(single) and the others(has been in a relationship at one point)... (Changed to binary on what makes sense)
levels(new.rw.data$`W8DMARSTAT(Legal marital status)`)[levels(new.rw.data$`W8DMARSTAT(Legal marital status)`) %in% 2:8] <- "Not Single/Formerly Not Single"
levels(new.rw.data$`W8DMARSTAT(Legal marital status)`)[levels(new.rw.data$`W8DMARSTAT(Legal marital status)`) %in% 1:1] <- "Never"

# Checking the changes
summary(new.rw.data$`W8DMARSTAT(Legal marital status)`)

# We will now merge levels in W8TENURE(Tenure)
# Get unique values for the column 'W1wrk1aMP(MP: current working status)'
unique_values <- unique(new.rw.data$`W8TENURE(Tenure)`)

# Custom order for the levels
custom_order_W8TENURE <- 1:7
# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W8TENURE(Tenure)", custom_order_W8TENURE)

# Merging levels 1,2(Own the house) and 3-4(rent the house), and 6 and 7(squatting/other)... (Grouped levels based on what makes sense)
levels(new.rw.data$`W8TENURE(Tenure)`)[levels(new.rw.data$`W8TENURE(Tenure)`) %in% c(1,2)] <- "Owned"
levels(new.rw.data$`W8TENURE(Tenure)`)[levels(new.rw.data$`W8TENURE(Tenure)`) %in% 3:4] <- "Rented"
levels(new.rw.data$`W8TENURE(Tenure)`)[levels(new.rw.data$`W8TENURE(Tenure)`) %in% 5:7] <- "Other"

# Checking the changes
summary(new.rw.data$`W8TENURE(Tenure)`)

# We will now merge levels in W8DACTIVITY(Current activity)
# Get unique values for the column 'W1wrk1aMP(MP: current working status)'
unique_values <- unique(new.rw.data$`W8DACTIVITY(Current activity)`)

# Custom order for the levels
custom_order_W8DACTIVITY <- 1:14
# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W8DACTIVITY(Current activity)", custom_order_W8DACTIVITY)

# Merging levels 3,4(Self-Employed) and 5,9,10,11,13,14(unemployed/unpaid work) and 6,7,8,12(education/apprenticeship)... (Based on all factors, makes sense, boxplots and to balance distributions)
levels(new.rw.data$`W8DACTIVITY(Current activity)`)[levels(new.rw.data$`W8DACTIVITY(Current activity)`) %in% c(1)] <- "Full Time Employee"
levels(new.rw.data$`W8DACTIVITY(Current activity)`)[levels(new.rw.data$`W8DACTIVITY(Current activity)`) %in% c(2)] <- "Part Time Employee"
levels(new.rw.data$`W8DACTIVITY(Current activity)`)[levels(new.rw.data$`W8DACTIVITY(Current activity)`) %in% c(3,4)] <- "Self-Employed"
levels(new.rw.data$`W8DACTIVITY(Current activity)`)[levels(new.rw.data$`W8DACTIVITY(Current activity)`) %in% c(5,9,10,11,13,14)] <- "Unemployed/Unpaid work"
levels(new.rw.data$`W8DACTIVITY(Current activity)`)[levels(new.rw.data$`W8DACTIVITY(Current activity)`) %in% c(6,7,8,12)] <- "Education/Apprenticechip/Training"


# Checking the changes
summary(new.rw.data$`W8DACTIVITY(Current activity)`)

# Checking all columns to detect any levels that have too little observations in the data
summary(new.rw.data)

# Merging levels 1,2,3 in W1ch0_2HH
levels(new.rw.data$`W1ch0_2HH(Number of children aged 0-2 in HH)`)[levels(new.rw.data$`W1ch0_2HH(Number of children aged 0-2 in HH)`) %in% c(1, 2,3)] <- "At least 1"

# Merging levels 0,1 and 2,3,4 in W1ch12_15HH
levels(new.rw.data$`W1ch12_15HH(Number of children aged 12-15 in HH)`)[levels(new.rw.data$`W1ch12_15HH(Number of children aged 12-15 in HH)`) %in% c(0, 1)] <- "At most 1"
levels(new.rw.data$`W1ch12_15HH(Number of children aged 12-15 in HH)`)[levels(new.rw.data$`W1ch12_15HH(Number of children aged 12-15 in HH)`) %in% c(2, 3, 4)] <- "At least 2"

# Merging levels 1, 2, 3 in W1ch16_17HH
levels(new.rw.data$`W1ch16_17HH(Number of children aged 16-17 in HH)`)[levels(new.rw.data$`W1ch16_17HH(Number of children aged 16-17 in HH)`) %in% c(1, 2, 3)] <- "At least 1"

# Merging levels 1, 2, 3 in W6gcse
levels(new.rw.data$`W6gcse(Number of GCSEs studied at Wave 6)`)[levels(new.rw.data$`W6gcse(Number of GCSEs studied at Wave 6)`) %in% c(1, 2, 3)] <- "Less than 4"

col_names_cleaned <- clean_col_names(names(new.rw.data))

summary(new.rw.data)

# Checking the VIF after all merging
tryCatch({    
  vif(lm(`W8DINCW(Continuous weekly income)` ~ ., data=new.rw.data))    
}, error = function(e) {      
  print(paste("Caught an error:", e$message))  
})

# There is no longer aliased coefficient issue..
# Looking at the GVIF:
# - W1empsdad: 43, W1marstatmum: 11.96, W1famtyp2: 36, W6EducYP: 11, W6acqno: 62, W6als: 10.27
# It seems to be two clusters of predictors, one about the family background of YP at wave 1, 
# The other is the education background of YP at W6.
# Based on the description of W1marstatmum and W1famtyp2.. both of these might be correlated
table(new.rw.data$`W1marstatmum(Marital status of mother)`, new.rw.data$`W1famtyp2(Whether single parent HH)`)
# Looking at the relation involving W1empsdad
table(new.rw.data$`W1empsdad(Employment status of father)`, new.rw.data$`W1marstatmum(Marital status of mother)`)
table(new.rw.data$`W1empsdad(Employment status of father)`, new.rw.data$`W1famtyp2(Whether single parent HH)`)
# It seems that they are all highly correlated.. as when the mother is alone or the the HH is a single parent, we have missing data for W1empsdad
# But W1famtyp2 seems to be the one connecting both W1empsdad and W1marstatmum the most.. so we remove this first
new.rw.data$`W1famtyp2(Whether single parent HH)` <- NULL

vif(lm(`W8DINCW(Continuous weekly income)` ~ ., data=new.rw.data))
# Both W1empsdad and W1marstatmum still has VIF > 10 but relatively close to 10..
# We will look at the MLR process to determine which is more significant

# For the W6 academic background cluster:
table(new.rw.data$`W6EducYP(YP: Whether currently going to school or college)`, new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`)
table(new.rw.data$`W6EducYP(YP: Whether currently going to school or college)`, new.rw.data$`W6als(Number of A/A2/AS levels being studied at Wave 6)`)
table(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`, new.rw.data$`W6als(Number of A/A2/AS levels being studied at Wave 6)`)
# We notice that most of the data that are left are put in level 4 of the W6als
# We decide that W6als should be removed as we would like predictors with a bit more power
# An argument could be made to remove either of the 3, but this is the choice we stick to
new.rw.data$`W6als(Number of A/A2/AS levels being studied at Wave 6)` <- NULL

vif(lm(`W8DINCW(Continuous weekly income)` ~ ., data=new.rw.data))
# W6EducYP and W6acqno still has VIF > 10 but we will depend on the MLR process to pick the most relevant out of the 2.

# Replotting everything after cleaning
target_col <- "W8DINCW(Continuous weekly income)"
dev.off() # Clearing the previous plots
for (col in names(new.rw.data)) {
  if (col == target_col) next  # Skip the weekly income column itself
  
  # Check if the column is numeric or categorical
  if (is.numeric(new.rw.data[[col]])) {
    # Scatter plot for numeric columns
    p <- ggplot(na.omit(new.rw.data), aes(x=.data[[col]], y=.data[[target_col]])) +
      geom_point(alpha=0.6, color="blue") + geom_smooth(method = "lm")
    labs(title=paste("Scatter Plot of", col, "vs", target_col),
         x=col, y=target_col)
  } else {
    # Boxplot for categorical columns
    p <- ggplot(new.rw.data, aes(x=.data[[col]], y=.data[[target_col]])) +
      geom_boxplot(fill="lightblue", color="black") +
      labs(title=paste("Boxplot of", target_col, "by", col),
           x=col, y=target_col)
  }
  
  # Print the plot
  print(p)
}

# Renaming the data as our main model
main_model<-new.rw.data
dim(main_model)
dim(main_model[complete.cases(main_model), ])











#MISSING ANALYSIS
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










# REGRESSION
#we will run the regression starting with all predictors, using a backwards elimination strategy

summary(main_model)
# Re level the categorical columns of both models to have the most common baseline level

model <- main_model 

model$`W1hea2MP(Whether MP has longstanding illness or disability)` <- relevel(model$`W1hea2MP(Whether MP has longstanding illness or disability)`, ref = "2")
model$`W1empsdad(Employment status of father)` <- relevel(model$`W1empsdad(Employment status of father)`, ref = "Working/Education")
model$`W1marstatmum(Marital status of mother)` <- relevel(model$`W1marstatmum(Marital status of mother)`, ref = "Living With Partner")
model$`W1hwndayYP(Number of evenings of HWK per week)` <- relevel(model$`W1hwndayYP(Number of evenings of HWK per week)`, ref="3")
model$`W1truantYP(Whether YP played truant in last 12 months)` <- relevel(model$`W1truantYP(Whether YP played truant in last 12 months)`, ref = "2")
model$`W1alceverYP(Whether YP ever had alcohol)` <- relevel(model$`W1alceverYP(Whether YP ever had alcohol)`, ref = "2")
model$`W1bulrc(Whether YP bullied in last 12 months)` <- relevel(model$`W1bulrc(Whether YP bullied in last 12 months)`, ref = "2")
model$`W1disabYP(Whether YP has any disability/long term illness or health problem)` <- relevel(model$`W1disabYP(Whether YP has any disability/long term illness or health problem)`, ref = "3")
model$`W2disc1YP(Whether YP thinks they have ever been treated unfairly by teachers because of skin colour or ethnic origin)` <- relevel(model$`W2disc1YP(Whether YP thinks they have ever been treated unfairly by teachers because of skin colour or ethnic origin)`, ref = "2")
model$`W4CannTryYP(Whether YP ever tried cannabis)` <- relevel(model$`W4CannTryYP(Whether YP ever tried cannabis)`, ref = "2")
model$`W4NamesYP(Whether YP has been called names, sworn at or insulted in last 12 months)` <- relevel(model$`W4NamesYP(Whether YP has been called names, sworn at or insulted in last 12 months)`, ref = "2")
model$`W4RacismYP(Whether YP has been threatened/insulted in the last 12 months due to skin colour/ethnicity)` <- relevel(model$`W4RacismYP(Whether YP has been threatened/insulted in the last 12 months due to skin colour/ethnicity)`, ref = "2")
model$`W4empsYP(Employment status of young person)` <- relevel(model$`W4empsYP(Employment status of young person)`, ref = "In Education")
model$`W5Apprent1YP(YP: Whether currently doing an apprenticeship)` <- relevel(model$`W5Apprent1YP(YP: Whether currently doing an apprenticeship)`, ref = "2")
model$`W6EducYP(YP: Whether currently going to school or college)` <- relevel(model$`W6EducYP(YP: Whether currently going to school or college)`, ref = "2")
model$`W6acqno(Highest academic qualification studied at Wave 6)` <- relevel(model$`W6acqno(Highest academic qualification studied at Wave 6)`, ref = "No Academic Study Aim")
model$`W6OwnchiDV(Whether Respondents have own Child/Children)` <- relevel(model$`W6OwnchiDV(Whether Respondents have own Child/Children)`, ref = "2")
model$`W8TENURE(Tenure)` <- relevel(model$`W8TENURE(Tenure)`, ref = "Rented")
model$`W8DACTIVITY(Current activity)` <- relevel(model$`W8DACTIVITY(Current activity)`, ref = "Full Time Employee")
model$`W8QMAFI(How managing financially these days)` <- relevel(model$`W8QMAFI(How managing financially these days)`, ref = "2")

main_model <- model

#checking changes
categorical_cols <- sapply(main_model, is.factor)

# Loop through the column names that are factors
for (col_name in names(categorical_cols)[categorical_cols]) {
  
  # Get all levels of the factor column from main model
  levels_list <- levels(main_model[[col_name]])
  
  cat("Column:", col_name, "\n")
  cat("Baseline level:", levels_list[1], "\n")  # Print reference level
  cat("levels:\n")
  print(summary(main_model[[col_name]]))
}

#we will do the main model first, then once we have our final model, we will test the model without the continuous columns that have less than 30%
#starting with all predictors, and removing the insignificant predictors until all predictors are significant
# Rename the columns without parenthesis for both models as lm doesn't like col names with parentheses
clean_main_model<-main_model
# Remove anything after the first parenthesis in column names of clean_main_model
colnames(clean_main_model) <- gsub("\\(.*\\)", "", colnames(clean_main_model))


# Run the regression using backwards elimination
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W6OwnchiDV column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W2ghq12scr_binary column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W4empsYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W4RacismYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1InCarHH column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1empsdad column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1ch16_17HH column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1bulrc column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W4NamesYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the log_of_W8QDEB2 column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W5EducYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1ch12_15HH column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the standardised_W1yschat1 column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1truantYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1empsmum column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W8TENURE column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W8DMARSTAT column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W5JobYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W6acqno column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W4AlcFreqYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1alceverYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1condur5MP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1ch0_2HH column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W8DGHQSC_binary column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the IndSchool column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W2depressYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1depkids column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W8QMAFI column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the standardised_W1hiqualparents_score column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1hous12HH column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1usevcHH column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W6gcse column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the standardised_W4schatYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W4Childck1YP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1ch3_11HH column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1NoldBroHS column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                     - W1NoldBroHS, data = clean_main_model)
Anova(W8DINCW.all.lm)

# Moving forward are borderline significant predictors (chosen to remove since we have a lot of predictors in the current model {29})

# we remove the W1hwndayYP column
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                     - W1NoldBroHS - W1hwndayYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

#display the coefficients of the final model
display(W8DINCW.all.lm)
#display the final model summary
summary(W8DINCW.all.lm)
#check that all vif values are less than 5 (multicollinearity checks)
vif(W8DINCW.all.lm) #we see that all adjusted VIF are less than 5 so there are no multicollinearity issues

#checking the residuals of our final model
par(mfrow=c(2,2))
plot(W8DINCW.all.lm,which=c(1,2))
hist(W8DINCW.all.lm$residuals,main="Histogramofresiduals",
     font.main=1,xlab="Residuals")

#we see that the residuals vs fitted plot displays a slight curvature (a mild violation of linearity) and maybe slight heteroscedasticity, but not obvious. 
#however, the model itself has okay diagnostics, the both tails of qq plot drops below the line which suggests non-normality in both tails
#the histogram is decently symmetric, and looks like a normal distribution, with a mild skew to the right
#note that 2 of the 3 continuous columns with more than 30% missing values appear in the final model
#we will do another MLR without these columns
#and for our bonus model, we will be replacing all "missing" values with NA.


























# MLR WITHOUT THE 3 CONT COLS WITH MORE THAN 30% MISSING
# Removing continuous predictors with more than 30% missing values
rm_missing_cont_data <- clean_main_model[, !colnames(clean_main_model) %in% c("log_of_W1GrssyrMP", "log_of_W1GrssyrHH", "log_of_W8QDEB2")]
summary(rm_missing_cont_data)

# Running through backwards elimination process to see if the regression differs a lot compared to when the continuous columns with more than 30% missing is included
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1bulrc column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1empsdad column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W6OwnchiDV column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W8DMARSTAT column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT
                               , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1truantYP column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W4RacismYP column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1alceverYP column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1depkids column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1NoldBroHS column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W2depressYP column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W2ghq12scr_binary column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W4empsYP column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W4Childck1YP column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W6acqno column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1InCarHH column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1ch16_17HH column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W8TENURE column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1ch12_15HH column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W5JobYP column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the standardised_W1yschat1 column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W8QMAFI column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# Moving forward are borderline significant predictors

# we remove the W1empsmum column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1ch3_11HH column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W4NamesYP column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the IndSchool column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP - IndSchool
                               , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W6gcse column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP - IndSchool
                               - W6gcse, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1ch0_2HH column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP - IndSchool
                               - W6gcse - W1ch0_2HH, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1hous12HH column
W8DINCW.rm_continuous.lm <- lm(log_of_W8DINCW ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP - IndSchool
                               - W6gcse - W1ch0_2HH - W1hous12HH, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

## Comparing the main model and this model...
# All predictors that are significant in the main model is still significant in this model (except the ones we removed from the start)
# Additionally, this model includes : 
# "W1condur5MP", "W1usevcHH", "W1hwndayYP", "W4AlcFreqYP", "W5EducYP",
# "W8DGHQSC_binary", "standardised_W4schatYP", "standardised_W1hiqualparents_score"

#display the coefficients of this model
display(W8DINCW.rm_continuous.lm)
#display the final model summary
summary(W8DINCW.rm_continuous.lm)
#check that all vif values are less than 5 (multicollinearity checks)
vif(W8DINCW.rm_continuous.lm) #we see that all adjusted VIF are less than 5 so there are no multicollinearity issues

#checking the residuals of our final model
par(mfrow=c(2,2))
plot(W8DINCW.rm_continuous.lm,which=c(1,2))
hist(W8DINCW.rm_continuous.lm$residuals,main="Histogramofresiduals",
     font.main=1,xlab="Residuals")

# The plots and the diagnostics suggests that this is a much better model compared to the main_model
# The histogram of residual does not appear skewed compared to the main model
# The QQ residuals also show an improvement where the upper tail now lies on the diagonal
# The residuals vs fitted plot also does not show curvature, and does not have a clear funnel shape









# REGRESSION FOR THE BONUS MODEL (missing levels are replaced with NA)
#we will run the regression using a backwards elimination strategy

model_na_data<-clean_main_model #creating a copy where missing will be replaced by NA

# Loop through each column
for (col in colnames(model_na_data)) {
  # Check if missing is a level, and replace it by NA
  levels(model_na_data[[col]])[levels(model_na_data[[col]]) == "missing"] <- NA
}

summary(model_na_data) #checking the summary
# We need to remove columns that only have 1 level after missing is removed, as it doesn't make sense to have a predictor with only 1 level.
# These are: W4Childck1YP
model_na_data <- model_na_data[, !colnames(model_na_data) %in% "W4Childck1YP"] 

# Next we decided to remove W6NEETAct, as even though there are multiple levels, 
# the missing values are too dominant and would cause the complete case analysis to have too little data
# As shown in the Missing Analysis part, this missingness is believed to be caused by from people being not applicable to be a NEET, 
# this means that they are in employment, education or training.
# Since we have W6JobYP and W6EducYP in the data, this should be okay
model_na_data <- model_na_data[, !colnames(model_na_data) %in% "W6NEETAct"] 

complete_cases <- complete.cases(model_na_data) # we now need to do a complete cases analysis 
model_na_data<-model_na_data[complete_cases, ] #keeping only complete rows (361)
dim(model_na_data) #the dimensions are much smaller, this is the impact of replacing the missing levels with NA

#starting with all predictors 
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . , data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1usevcHH
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1alceverYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W2ghq12scr_binary
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1ch12_15HH
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH
                    , data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor standardised_W4schatYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W6acqno
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor IndSchool
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W4empsYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1InCarHH
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W8QMAFI
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W4AlcFreqYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1empsdad
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W6gcse
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1empsmum
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W8DMARSTAT
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1truantYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1hous12HH
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    , data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W5JobYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor log_of_W8QDEB2
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1bulrc
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W4NamesYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W2depressYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1condur5MP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W8TENURE
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    , data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1heposs9YP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1ch3_11HH
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor log_of_W1GrssyrHH
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor standardised_W1yschat1
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W8DACTIVITY
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    , data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W6EducYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W8DGHQSC_binary
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP - W8DGHQSC_binary, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1ch0_2HH
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP - W8DGHQSC_binary - W1ch0_2HH, data = model_na_data)
Anova(W8DINCW.na.lm)

# Moving on are borderline significant predictors

#we remove the least significant predictor W6OwnchiDV
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP - W8DGHQSC_binary - W1ch0_2HH - W6OwnchiDV, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1depkids
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP - W8DGHQSC_binary - W1ch0_2HH - W6OwnchiDV - W1depkids, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1NoldBroHS
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP - W8DGHQSC_binary - W1ch0_2HH - W6OwnchiDV - W1depkids - W1NoldBroHS
                    , data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1ch16_17HH
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP - W8DGHQSC_binary - W1ch0_2HH - W6OwnchiDV - W1depkids - W1NoldBroHS
                    - W1ch16_17HH, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor log_of_W1GrssyrMP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP - W8DGHQSC_binary - W1ch0_2HH - W6OwnchiDV - W1depkids - W1NoldBroHS
                    - W1ch16_17HH - log_of_W1GrssyrMP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor standardised_W1hiqualparents_score
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP - W8DGHQSC_binary - W1ch0_2HH - W6OwnchiDV - W1depkids - W1NoldBroHS
                    - W1ch16_17HH - log_of_W1GrssyrMP - standardised_W1hiqualparents_score, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W2disc1YP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP - W8DGHQSC_binary - W1ch0_2HH - W6OwnchiDV - W1depkids - W1NoldBroHS
                    - W1ch16_17HH - log_of_W1GrssyrMP - standardised_W1hiqualparents_score - W2disc1YP
                    , data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W4RacismYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - W1usevcHH - W1alceverYP - W2ghq12scr_binary - W1ch12_15HH 
                    - standardised_W4schatYP - W6acqno - IndSchool - W4empsYP - W1InCarHH - W8QMAFI
                    - W4AlcFreqYP - W1empsdad - W6gcse - W1empsmum - W8DMARSTAT - W1truantYP - W1hous12HH
                    - W5JobYP - log_of_W8QDEB2 - W1bulrc - W4NamesYP - W2depressYP - W1condur5MP - W8TENURE
                    - W1heposs9YP - W1ch3_11HH - log_of_W1GrssyrHH - standardised_W1yschat1 - W8DACTIVITY
                    - W6EducYP - W8DGHQSC_binary - W1ch0_2HH - W6OwnchiDV - W1depkids - W1NoldBroHS
                    - W1ch16_17HH - log_of_W1GrssyrMP - standardised_W1hiqualparents_score - W2disc1YP
                    - W4RacismYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#display the coefficients of the final model
display(W8DINCW.na.lm)
#display the final model summary
summary(W8DINCW.na.lm)
#check that all vif values are less than 5
vif(W8DINCW.na.lm)

#checking the residuals of our final model
par(mfrow=c(2,2))
plot(W8DINCW.na.lm,which=c(1,2))
hist(W8DINCW.na.lm$residuals,main="Histogramofresiduals",
     font.main=1,xlab="Residuals")

#The diagnostics for this model is worse than our initial model.
#We see that the residuals vs fitted plot does not show clear heteroscedasticity. However, the range in the middle is slightly higher than both ends.
#The qq plot has also improved on the lower tail, however the histogram looks less like a normal distribution.
#What was also interesting was that we had to remove even more predictors in this model.
#List of additional predictors removed: 
#log_of_W1GrssyrMP, log_of_W1GrssyrHH, W1heposs9YP, W2disc1YP, W6NEETAct (removed at start), W8DACTIVITY
#List of predictors not in main_model but present in this model:
#W1hwndayYP, W5EducYP
#We will validate this model using Cross Validation




















#for the outlier analysis, we will focus on our main model: W8DINCW.all.lm

show_outliers<-function(the.linear.model,topN){
  #length of data
  n=length(fitted(the.linear.model))
  #number of parameters estimated
  p=length(coef(the.linear.model))
  #standardized residuals over 3
  res.out<-which(abs(rstandard(the.linear.model))>3) #sometimes >2
  #topN values
  res.top<-head(rev(sort(abs(rstandard(the.linear.model)))),topN)
  #high leverage values
  lev.out<-which(lm.influence(the.linear.model)$hat>2*p/n)
  #topN values
  lev.top<-head(rev(sort(lm.influence(the.linear.model)$hat)),topN)
  #high diffits
  dffits.out<-which(dffits(the.linear.model)>2*sqrt(p/n))
  #topN values
  dffits.top<-head(rev(sort(dffits(the.linear.model))),topN)
  #Cook's over 1
  cooks.out<-which(cooks.distance(the.linear.model)>1)
  #topN cooks
  cooks.top<-head(rev(sort(cooks.distance(the.linear.model))),topN)
  #Create a list with the statistics -- cant do a data frame as different lengths 
  list.of.stats<-list(Std.res=res.out,Std.res.top=res.top, Leverage=lev.out, Leverage.top=lev.top, DFFITS=dffits.out, DFFITS.top=dffits.top, Cooks=cooks.out,Cooks.top=cooks.top)
  #return the statistics
  list.of.stats}

HP.out<-show_outliers(W8DINCW.all.lm,10)
HP.out 
#from the output of the function shown in lectures, we see that there are no outliers in terms of cook distance
#however, there still exist points with high standardized residuals, leverage and DFFITS, 
#we will explore these points and decide what to do with them

common.out<-intersect(HP.out$Leverage,
                      intersect(HP.out$Std.res,HP.out$DFFITS))
common.out #none of the potential outliers appear in all 3 categories, we will now look for outliers that appear in at least 2 categories

# In two of three sets
leverage_stdres <- intersect(HP.out$Leverage, HP.out$Std.res)
leverage_dffits <- intersect(HP.out$Leverage, HP.out$DFFITS)
stdres_dffits   <- intersect(HP.out$Std.res, HP.out$DFFITS)

# Combine for inspection
two_out_of_three <- unique(c(leverage_stdres, leverage_dffits, stdres_dffits))
two_out_of_three #our outlier analysis will focus on these points

# Predict the values using the model
predicted_values <- fitted(W8DINCW.all.lm)

# Actual values are the original dependent variable (W8DINCW)
actual_values <- clean_main_model$log_of_W8DINCW

# Calculate absolute differences between predicted and actual values
absolute_differences <- abs(predicted_values[two_out_of_three] - actual_values[two_out_of_three])

cat("Predicted: ", predicted_values[two_out_of_three])
cat("Actual: ", actual_values[two_out_of_three])
cat("Diff: ", absolute_differences)

# looking at the outliers, 
# An obvious pattern is that these outliers are being underpredicted.

significant_predictors <- c( # Based on MLR file
  "log_of_W1GrssyrMP", "log_of_W1GrssyrHH", "W1wrk1aMP", "W1hea2MP", "W1marstatmum",
  "W1nssecfam", "W1ethgrpYP", "W1heposs9YP", "W1disabYP", "W2disc1YP", "W4CannTryYP",
  "W5Apprent1YP", "W6JobYP", "W6EducYP", "W6NEETAct", "W8CMSEX", "W8DACTIVITY",
  "standardised_W6DebtattYP"
)

clean_main_model[two_out_of_three, significant_predictors]
# The only clear pattern seen is that these outliers are mostly have a good background, and have a high log_of_W1GrssyrMP and log_of_W1GrssyrHH.
# From previous plots, it is clear that the model have some trouble predicting those.
# In addition, the model only uses ~900 out of the ~3000 observations in the main_model due to the complete case analysis, so this might affect the model's ability.
# This is the only explanation we come up with why these are considered outliers
# none of these outliers appear to show data errors (as they are realistic), so we can't justify removing them, however, we will explore their impact on the model

# We will see if removing them improves the model significantly
clean_data_no_outliers <- clean_main_model[-two_out_of_three, ]

# 2. Refit the model on the data set with no outliers
W8DINCW.all.lm.no_outliers <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                                 - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                                 - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                                 - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                                 - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                                 - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                                 - W1NoldBroHS - W1hwndayYP, data = clean_data_no_outliers)

summary(W8DINCW.all.lm)
summary(W8DINCW.all.lm.no_outliers)

Anova(W8DINCW.all.lm.no_outliers) 
#the model without the outliers is only marginally better, (based on the R2 and AdjustedR2) 
#so we will keep the outliers in the final model 
#We will use our initial model with outliers in the next part and look to see if there are any significant interactions to add

avPlots(W8DINCW.all.lm) #looking at the av plots, 
#these identify the two points with the highest residual and the most extreme horizontal values for each predictor
#again although these plots identify potential outliers, we do not remove any as the cook distance for all points are very low











#we will now explore some potential interactions that make sense for the model
#for all interactions we are looking to add, we need their sample size to be big enough, 
#and then for them to also have a significant impact on the target in the model
summary(W8DINCW.all.lm)
#we will explore interactions for our best model, reminder it is:
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                     - W1NoldBroHS - W1hwndayYP, data = clean_main_model)

# we will explore interaction related to parent gross income and working status
W8DINCW.interaction.lm <- lm(log_of_W8DINCW ~ . + log_of_W1GrssyrMP*W1wrk1aMP - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                             - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                             - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                             - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                             - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                             - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                             - W1NoldBroHS - W1hwndayYP, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
# This is a significant interaction, taking a look at its effects...
display(W8DINCW.interaction.lm)
# When the main parent is working, the MP's salary has a slight negative relationship with the outcome.
# If the main parent is not working, income tends to be much lower overall.
# However, when the main parent is not working, the MP's salary has a positive effect.

# Next, we will explore interaction related to the state of the YP at Wave 6
W8DINCW.interaction.lm <- lm(log_of_W8DINCW ~ . + W6EducYP*W6JobYP + log_of_W1GrssyrMP*W1wrk1aMP - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                             - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                             - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                             - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                             - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                             - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                             - W1NoldBroHS - W1hwndayYP, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
# This is a borderline significant interaction, taking a look at its effects...
display(W8DINCW.interaction.lm)
# When the YP is not doing paid work, it has a slight negative impact on the outcome
# When the YP answers Yes for W6EducYP, the magnitude of the negative impact on the outcome is increased when YP is not doing paid work
# When W6EducYP is missing, the additional interaction with not doing paid work approaches zero, suggesting mitigating effect.
# However, to keep consistent with our previous decisions, we will not be keeping borderline significant interactions

# we will explore interaction related to main parent's health and family status
W8DINCW.interaction.lm <- lm(log_of_W8DINCW ~ . + W1nssecfam*W1hea2MP + log_of_W1GrssyrMP*W1wrk1aMP - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                             - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                             - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                             - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                             - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                             - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                             - W1NoldBroHS - W1hwndayYP, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
# This is not a significant interaction

# we will explore interaction related to the YPs Sex and Activity in Wave 8
W8DINCW.interaction.lm <- lm(log_of_W8DINCW ~ . + W8CMSEX*W8DACTIVITY + log_of_W1GrssyrMP*W1wrk1aMP - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                             - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                             - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                             - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                             - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                             - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                             - W1NoldBroHS - W1hwndayYP, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
# This is not a significant interaction

#we only added one interaction to the model, despite having attempted several plausible interactions
W8DINCW.interaction.lm <- lm(log_of_W8DINCW ~ . + log_of_W1GrssyrMP*W1wrk1aMP - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                             - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                             - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                             - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                             - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                             - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                             - W1NoldBroHS - W1hwndayYP, data = clean_main_model)

#display the coefficients of the model with interactions
display(W8DINCW.interaction.lm)
#display the interaction model summary
summary(W8DINCW.interaction.lm)

#we will use this interactions model for the final stage of cross validation
#note the model's diagnostics have improved marginally.















# we will now validate and see how accurate our final model is on out of sample data
# we will also compare it to our model with all other models, to see which one is better
#NOTE FOR THIS PART YOU MAY HAVE TO RUN CODE SEVERAL TIMES DUE TO SAMPLE SPLITTING DATA INTO LEVELS NOT SEEN IN THE TRAINING
library(gridExtra)
#first we need to add some fake observations to avoid errors during cross validation due to some levels having little observations thus not represented in training set
pred_df<-clean_main_model
relevant_vars<-rownames(as.data.frame(Anova(W8DINCW.all.lm)))
relevant_vars<-c("log_of_W8DINCW", relevant_vars) # Adding our outcome
relevant_vars<-relevant_vars[-length(relevant_vars)] # Removing 'residuals'
pred_df<-pred_df[, relevant_vars]
pref_df<-pred_df[complete.cases(pred_df), ]

# Some columns that the complete.cases miss
cols_to_check <- c("log_of_W1GrssyrMP", "log_of_W1GrssyrHH")
# Filter to only keep rows where ALL of these columns are not NA
pred_df <- pred_df[ rowSums(is.na(pred_df[, cols_to_check])) == 0, ]

# Threshold to consider adding a fake row
threshold <- 300
# Identify factor variables
factor_vars <- names(Filter(is.factor, pred_df))
# Prepare list to hold all fake rows
fake_rows <- list()

# Loop over each factor variable
for (var in factor_vars) {
  # Get the frequency of each level
  level_counts <- table(pred_df[[var]])
  
  # Get levels that occur less than the threshold
  rare_levels <- names(level_counts[level_counts < threshold])
  
  # Only process if there are rare levels
  if (length(rare_levels) > 0) {
    for (lvl in rare_levels) {
      # Use a template row
      fake_row <- pred_df[1, , drop = FALSE]
      # Set this factor variable to the rare level
      fake_row[[var]] <- factor(lvl, levels = levels(pred_df[[var]]))
      
      fake_rows <- append(fake_rows, list(fake_row))
    }
  }
}

# Combine fake rows into a dataframe
fake_data <- do.call(rbind, fake_rows)
dim(fake_data) # Only 16 rows (out of 1635) so it will not affect the validation results greatly

set.seed(70)
#code from lectures to test our model on out of sample data
# First doing 3 iterations of 90-10 split
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.9*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 90% to fit the model
  print(dim(training.set))
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 10% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . , data = training.set)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.all.lm, test.set), 
                           #predicted vs original
                           original=test.set$log_of_W8DINCW,error=(predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW))
  print(summary(pred.val.set))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE,) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)
#from the plots, we can see that the predicted vs original lines are generally very close, meaning the model is generalizing to out of sample data really well
#The regression of the original outcome on the predicted outcome is very close to the line meaning that there is no non-linear trend that the predictions are not picking up.
#for the predicted vs error plot, there are a lot of points beyond +-1 sd, but the error is not increasing with the size of the predictions.
#We can also see that while there is some variability in the predictions for different training/test set splits, they are broadly similar in terms of spread 

# Next, doing 3 iterations of 70-30 split to compare with the 90-10 spl-it
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 70% to fit the model
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 30% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ ., data = training.set)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.all.lm, test.set), 
                           #predicted vs original
                           original=test.set$log_of_W8DINCW,error=(predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW))
  print(summary(pred.val.set))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE,) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)
#Comparing with the 90-10 split, the 70-30 splits mostly have the same characteristics which shows that the model is consistent enough

#we will now compare our model accuracy from within and out of training sample using a 70/30 split iterated 100 times
mse.pred.main<-array(dim=c(100,2))
#code from lectures
for(i in 1:100){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 70% to fit the model
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 30% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ ., data = training.set)
  
  #in sample prediction error
  in.sample.error=predict(cv.W8DINCW.all.lm,training.set)-training.set$log_of_W8DINCW
  #out of sample prediction error
  out.sample.error=predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW
  #in sample mse
  mse.pred.main[i,1]<-mean(in.sample.error^2, na.rm = TRUE)
  #out of sample mse
  mse.pred.main[i,2]<-mean(out.sample.error^2, na.rm = TRUE)
}
# Take the means of the two
mse <- data.frame(main = colMeans(mse.pred.main))
rownames(mse) <- c("in sample", "out of sample")

# Add a percentage difference row
mse["percentage increase", ] <- 100 * (mse["out of sample", ] - mse["in sample", ]) / mse["in sample", ]

# View the result
mse
income_variance <- var(pred_df$log_of_W8DINCW)
income_variance 
#the MSEs are much lower than the sample variance in the weekly income, showing the model fits well, and predicts well.
#The model is very accurate at predicting in and out of sample data, and only has a difference of 1.23% between its out of sample and in sample predictions, which is really good.
# In sample: 0.01274925
# Out sample: 0.01290646
# Percentage increase: 1.23
# var(outcome): 0.03946939
summary(pred_df)
# Note that the summary of the pred_df suggests that this model mostly works for the rows with W1wrk1amp as Working 
# (which is a possible bias, because of the we only consider those who have non-missing values for the GrssyrHH incomes)














# we will now validate and see how accurate our final model (with interactions) is on out of sample data
# we will also compare it to our model with all other models, to see which one is better
#NOTE FOR THIS PART YOU MAY HAVE TO RUN CODE SEVERAL TIMES DUE TO SAMPLE SPLITTING DATA INTO LEVELS NOT SEEN IN THE TRAINING

#first we need to add some fake observations to avoid errors during cross validation due to some levels having little observations thus not represented in training set
pred_df<-clean_main_model
relevant_vars<-rownames(as.data.frame(Anova(W8DINCW.interaction.lm)))
relevant_vars<-c("log_of_W8DINCW", relevant_vars) # Adding our outcome
relevant_vars<-relevant_vars[-length(relevant_vars)] # Removing 'residuals'
relevant_vars<-relevant_vars[-length(relevant_vars)] # Removing interaction term
pred_df<-pred_df[, relevant_vars]
pref_df<-pred_df[complete.cases(pred_df), ]

# Some columns that the complete.cases miss
cols_to_check <- c("log_of_W1GrssyrMP", "log_of_W1GrssyrHH")
# Filter to only keep rows where ALL of these columns are not NA
pred_df <- pred_df[ rowSums(is.na(pred_df[, cols_to_check])) == 0, ]

# Threshold to consider adding a fake row
threshold <- 300
# Identify factor variables
factor_vars <- names(Filter(is.factor, pred_df))
# Prepare list to hold all fake rows
fake_rows <- list()

# Loop over each factor variable
for (var in factor_vars) {
  # Get the frequency of each level
  level_counts <- table(pred_df[[var]])
  
  # Get levels that occur less than the threshold
  rare_levels <- names(level_counts[level_counts < threshold])
  
  # Only process if there are rare levels
  if (length(rare_levels) > 0) {
    for (lvl in rare_levels) {
      # Use a template row
      fake_row <- pred_df[1, , drop = FALSE]
      # Set this factor variable to the rare level
      fake_row[[var]] <- factor(lvl, levels = levels(pred_df[[var]]))
      
      fake_rows <- append(fake_rows, list(fake_row))
    }
  }
}

# Combine fake rows into a dataframe
fake_data <- do.call(rbind, fake_rows)
dim(fake_data) # Only 16 rows (out of 1635) so it will not affect the validation results greatly

set.seed(70)
#code from lectures to test our model on out of sample data
# First doing 3 iterations of 90-10 split
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.9*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 90% to fit the model
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 10% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . + log_of_W1GrssyrMP*W1wrk1aMP , data = training.set)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.all.lm, test.set), 
                           #predicted vs original
                           original=test.set$log_of_W8DINCW,error=(predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW))
  print(summary(pred.val.set))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE,) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)
#from the plots, we can see that the predicted vs original lines are generally very close, meaning the model is generalizing to out of sample data really well
#The regression of the original outcome on the predicted outcome is very close to the line meaning that there is no non-linear trend that the predictions are not picking up.
#for the predicted vs error plot, there are a lot of points beyond +-1 sd, but the error is not increasing with the size of the predictions.
#We can also see that while there is some variability in the predictions for different training/test set splits, they are broadly similar in terms of spread 

# Next, doing 3 iterations of 70-30 split to compare with the 90-10 spl-it
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 70% to fit the model
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 30% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . + log_of_W1GrssyrMP*W1wrk1aMP, data = training.set)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.all.lm, test.set), 
                           #predicted vs original
                           original=test.set$log_of_W8DINCW,error=(predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW))
  print(summary(pred.val.set))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE,) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)
#Comparing with the 90-10 split, the 70-30 splits mostly have the same characteristics which shows that the model is consistent enough

#we will now compare our model accuracy from within and out of training sample using a 70/30 split iterated 100 times
mse.pred.interaction<-array(dim=c(100,2))
#code from lectures
for(i in 1:100){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 70% to fit the model
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 30% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . + log_of_W1GrssyrMP*W1wrk1aMP, data = training.set)
  
  #in sample prediction error
  in.sample.error=predict(cv.W8DINCW.all.lm,training.set)-training.set$log_of_W8DINCW
  #out of sample prediction error
  out.sample.error=predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW
  #in sample mse
  mse.pred.interaction[i,1]<-mean(in.sample.error^2, na.rm = TRUE)
  #out of sample mse
  mse.pred.interaction[i,2]<-mean(out.sample.error^2, na.rm = TRUE)
}
# Take the means of the two
mse <- data.frame(main = colMeans(mse.pred.main), interaction=colMeans(mse.pred.interaction))
rownames(mse) <- c("in sample", "out of sample")

# Add a percentage difference row
mse["percentage increase", ] <- 100 * (mse["out of sample", ] - mse["in sample", ]) / mse["in sample", ]

# View the result
mse
income_variance <- var(pred_df$log_of_W8DINCW)
income_variance 
#the MSEs are much lower than the sample variance in the weekly income, showing the model fits well, and predicts well.
#The model is very accurate at predicting in and out of sample data, and only has a difference of 1.5% between its out of sample and in sample predictions, which is really good.
#Adding the interaction seems to have minimal effect on the MSPE (~0.0001 difference). 
#A bit better MSPE in sample compared to without interaction, but a bit worse MSPE generalizing to out of sample data compared to without interaction
# In sample: 0.01272764
# Out sample: 0.01291888
# Percentage increase: 1.50
# var(outcome): 0.03946939
summary(pred_df)
# Same case as main model, almost of the data in pred_df has W1wrk1aMP as Working























# CRV WITHOUT THE CONT COLS WITH MISSING
# we will now validate and see how accurate our final model is on out of sample data
# we will also compare it to our model with all other models, to see which one is better

#first we need to add some fake observations to avoid errors during cross validation due to some levels having little observations thus not represented in training set
pred_df<-rm_missing_cont_data
relevant_vars<-rownames(as.data.frame(Anova(W8DINCW.rm_continuous.lm)))
relevant_vars<-c("log_of_W8DINCW", relevant_vars) # Adding our outcome
relevant_vars<-relevant_vars[-length(relevant_vars)] # Removing 'residuals'
pred_df<-pred_df[, relevant_vars]
pref_df<-pred_df[complete.cases(pred_df), ]

# removing rows with any NA (a missed column using complete.cases)
pred_df <- pred_df[ is.na(pred_df[, "W8DGHQSC_binary"]) == FALSE, ]

# Threshold to consider adding a fake row
threshold <- 400
# Identify factor variables
factor_vars <- names(Filter(is.factor, pred_df))
# Prepare list to hold all fake rows
fake_rows <- list()

# Loop over each factor variable
for (var in factor_vars) {
  # Get the frequency of each level
  level_counts <- table(pred_df[[var]])
  
  # Get levels that occur less than the threshold
  rare_levels <- names(level_counts[level_counts < threshold])
  
  # Only process if there are rare levels
  if (length(rare_levels) > 0) {
    for (lvl in rare_levels) {
      # Use a template row
      fake_row <- pred_df[1, , drop = FALSE]
      # Set this factor variable to the rare level
      fake_row[[var]] <- factor(lvl, levels = levels(pred_df[[var]]))
      
      fake_rows <- append(fake_rows, list(fake_row))
    }
  }
}

# Combine fake rows into a dataframe
fake_data <- do.call(rbind, fake_rows)
dim(fake_data) # Only 15 rows (out of 3303) so it will not affect the validation results greatly

set.seed(70)
#code from lectures to test our model on out of sample data
# First doing 3 iterations of 90-10 split
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.9*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 90% to fit the model
  print(dim(training.set))
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 10% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . , data = training.set)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.all.lm, test.set), 
                           #predicted vs original
                           original=test.set$log_of_W8DINCW,error=(predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW))
  print(summary(pred.val.set))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE,) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)
#from the plots, we can see that the predicted vs original lines are generally very close, meaning the model is generalizing to out of sample data really well
#The regression of the original outcome on the predicted outcome is almost identical to the diagonal line meaning that there is no non-linear trend that the predictions are not picking up.
#for the predicted vs error plot, there are a lot of points beyond +-1 sd, but the error is not increasing with the size of the predictions.
#We can also see that while there is some variability in the predictions for different training/test set splits, they are broadly similar in terms of spread 

# Next, doing 3 iterations of 70-30 split to compare with the 90-10 split
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 70% to fit the model
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 30% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ ., data = training.set)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.all.lm, test.set), 
                           #predicted vs original
                           original=test.set$log_of_W8DINCW,error=(predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW))
  print(summary(pred.val.set))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE,) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)
#Comparing with the 90-10 split, the 70-30 splits mostly have the same characteristics which shows that the model is consistent enough

#we will now compare our model accuracy from within and out of training sample using a 70/30 split iterated 100 times
mse.pred.rm_continuous<-array(dim=c(100,2))
#code from lectures
for(i in 1:100){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 70% to fit the model
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 30% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ ., data = training.set)
  
  #in sample prediction error
  in.sample.error=predict(cv.W8DINCW.all.lm,training.set)-training.set$log_of_W8DINCW
  #out of sample prediction error
  out.sample.error=predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW
  #in sample mse
  mse.pred.rm_continuous[i,1]<-mean(in.sample.error^2, na.rm = TRUE)
  #out of sample mse
  mse.pred.rm_continuous[i,2]<-mean(out.sample.error^2, na.rm = TRUE)
}
# Take the means of the two
mse <- data.frame(main = colMeans(mse.pred.main), rm_continuous=colMeans(mse.pred.rm_continuous))
rownames(mse) <- c("in sample", "out of sample")

# Add a percentage difference row
mse["percentage increase", ] <- 100 * (mse["out of sample", ] - mse["in sample", ]) / mse["in sample", ]

# View the result
mse
income_variance <- var(pred_df$log_of_W8DINCW)
income_variance 
#the MSEs are much lower than the sample variance in the weekly income, showing the model fits well, and predicts well.
#The model is very accurate at predicting in and out of sample data, and only has a difference of 0.66% between its out of sample and in sample predictions, which is really good.
#This means that this model generalizes better from in sample data to predicting out of sample data
#However, the main model is marginally better in terms of overall MSPE
# In sample: 0.01292030
# Out sample: 0.01300561
# Percentage increase: 0.66
# var(outcome): 0.05664817

# Note that the income variance are much higher than original for the rows left after a complete case analysis where the continuous columns that have more than 30% missing are removed,
# This means that although the MSPEs are quite similar, the effect is stronger when we are estimating the outcome only considering the columns this model uses
summary(pred_df)
# The pred_df seems to have the least amount of bias compared to the models before














# we will now validate and see how accurate our final model (NA instead of missing) is on out of sample data
# we will also compare it to our model with all other models, to see which one is better
#NOTE FOR THIS PART YOU MAY HAVE TO RUN CODE SEVERAL TIMES DUE TO SAMPLE SPLITTING DATA INTO LEVELS NOT SEEN IN THE TRAINING

#first we need to add some fake observations to avoid errors during cross validation due to some levels having little observations thus not represented in training set
pred_df<-model_na_data
relevant_vars<-rownames(as.data.frame(Anova(W8DINCW.na.lm)))
relevant_vars<-c("log_of_W8DINCW", relevant_vars) # Adding our outcome
relevant_vars<-relevant_vars[-length(relevant_vars)] # Removing 'residuals'
pred_df<-pred_df[, relevant_vars]
pref_df<-pred_df[complete.cases(pred_df), ]

# Threshold to consider adding a fake row
threshold <- 10
# Identify factor variables
factor_vars <- names(Filter(is.factor, pred_df))
# Prepare list to hold all fake rows
fake_rows <- list()

# Loop over each factor variable
for (var in factor_vars) {
  # Get the frequency of each level
  level_counts <- table(pred_df[[var]])
  
  # Get levels that occur less than the threshold
  rare_levels <- names(level_counts[level_counts < threshold])
  
  # Only process if there are rare levels
  if (length(rare_levels) > 0) {
    for (lvl in rare_levels) {
      # Use a template row
      fake_row <- pred_df[1, , drop = FALSE]
      # Set this factor variable to the rare level
      fake_row[[var]] <- factor(lvl, levels = levels(pred_df[[var]]))
      
      fake_rows <- append(fake_rows, list(fake_row))
    }
  }
}

# Combine fake rows into a dataframe
fake_data <- do.call(rbind, fake_rows)
dim(fake_data) # Only 2 rows (out of 361) so it will not affect the validation results greatly

set.seed(70)
#code from lectures to test our model on out of sample data
# First doing 3 iterations of 90-10 split
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.9*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 90% to fit the model
  print(dim(training.set))
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 10% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . , data = training.set)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.all.lm, test.set), 
                           #predicted vs original
                           original=test.set$log_of_W8DINCW,error=(predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW))
  print(summary(pred.val.set))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE,) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)
#from the plots, we can see that the predicted vs original lines are generally very close, meaning the model is generalizing to out of sample data really well
#The regression of the original outcome on the predicted outcome is very close to the line meaning that there is no non-linear trend that the predictions are not picking up.
#for the predicted vs error plot, there are a lot of points beyond +-1 sd, but the error is not increasing with the size of the predictions.
#We can also see that while there is some variability in the predictions for different training/test set splits, they are broadly similar in terms of spread 

# Next, doing 3 iterations of 70-30 split to compare with the 90-10 spl-it
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 70% to fit the model
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 30% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ ., data = training.set)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.all.lm, test.set), 
                           #predicted vs original
                           original=test.set$log_of_W8DINCW,error=(predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW))
  print(summary(pred.val.set))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE,) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", data = pred.val.set, aes(x = predicted, y = original), se = FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)
#Comparing with the 90-10 split, the 70-30 splits mostly have the same characteristics which shows that the model is consistent enough

#we will now compare our model accuracy from within and out of training sample using a 70/30 split iterated 100 times
mse.pred.na<-array(dim=c(100,2))
#code from lectures
for(i in 1:100){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 70% to fit the model
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 30% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log_of_W8DINCW ~ ., data = training.set)
  
  #in sample prediction error
  in.sample.error=predict(cv.W8DINCW.all.lm,training.set)-training.set$log_of_W8DINCW
  #out of sample prediction error
  out.sample.error=predict(cv.W8DINCW.all.lm,test.set)-test.set$log_of_W8DINCW
  #in sample mse
  mse.pred.na[i,1]<-mean(in.sample.error^2, na.rm = TRUE)
  #out of sample mse
  mse.pred.na[i,2]<-mean(out.sample.error^2, na.rm = TRUE)
}
# Take the means of the two
mse <- data.frame(main = colMeans(mse.pred.main), na= colMeans(mse.pred.na))
rownames(mse) <- c("in sample", "out of sample")

# Add a percentage difference row
mse["percentage increase", ] <- 100 * (mse["out of sample", ] - mse["in sample", ]) / mse["in sample", ]

# View the result
mse
income_variance <- var(pred_df$log_of_W8DINCW)
income_variance 
#the MSEs are much lower than the sample variance in the weekly income, showing the model fits well, and predicts well.
#The model is very accurate at predicting in and out of sample data, and only has a difference of 0.85% between its out of sample and in sample predictions, which is really good.
#The model is better at generalizing from in sample data to be used to predict outcome using out of sample data
#However, the overall MSPE is marginally higher than the main model, which shows that this model performs marginally worse than the other models.
# In sample: 0.014299
# Out sample: 0.016581
# Percentage increase: 15.96
# var(outcome): 0.02940207

# Same conclusion about variance on outcome - MSPEs as the model with >30% missing continuous removed model
summary(pred_df)
# There seems to be biasness towards W1wrk1aMP of level Working

# Since at this point we are choosing the rm_continuous as our final model, we check if the (borderline significant) interactions for our model 1 is also significant here
final.lm <- lm(log_of_W8DINCW ~ . + W6JobYP*W6EducYP - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                     - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                     - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                     - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP - IndSchool
                     - W6gcse - W1ch0_2HH - W1hous12HH, data = rm_missing_cont_data)

Anova(final.lm)
display(final.lm) # For final analysis & interpretation

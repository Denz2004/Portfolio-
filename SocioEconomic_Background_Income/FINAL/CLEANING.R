
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

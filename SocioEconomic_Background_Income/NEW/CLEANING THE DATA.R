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
  if (clean_col_names(col) %in% transform_into_missing | clean_col_names(col) %in% cont_list | clean_col_names(col) %in% cont_with_more_than_30_per) {
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
    
    if (prop_zeros > 0.3) {  # we can adjust this threshold as needed
      cat(col, "has", round(prop_zeros * 100, 1), "% zeros — consider binarizing\n")
    }
  }
}

# We see that W2ghqscr, and W8DGHQSC have a high percentage of zero values. We will convert these into binary
# These would mean that we are comparing between abscense mental health symptoms / presence of symptoms
new.rw.data$W2ghq12scr_binary <- factor(ifelse(new.rw.data$W2ghq12scr > 3, 1, 0),
                                        levels = c(0, 1),
                                        labels = c("Not Distressed", "Distressed"))

new.rw.data$W8DGHQSC_binary <- factor(ifelse(new.rw.data$W8DGHQSC > 3, 1, 0),
                                      levels = c(0, 1),
                                      labels = c("Not Distressed", "Distressed"))
#removing the old continuous columns:
new.rw.data$`W2ghq12scr(YP GHQ12 score)` <- NULL
new.rw.data$`W8DGHQSC(General Health Questionnaire (GHQ12) score )` <- NULL



#we see that there are quite a lot of columns to re level, we will first check if any of these are correlated so we can eliminate them
library(vcd)

get_corr_v_cramer <- function(factor1, factor2){
  # Create a contingency table
  contingency_table <- table(factor1, factor2)
  cramers_v <- assocstats(contingency_table)
  cramers_v$cramer
}


#### LOOP FOR IDENTIFYING CORRELATED CATEGORICAL VARIABLES (We can decide to remove, or do something with them before MLR)
# Get only the categorical variables
categorical_vars <- names(Filter(is.factor, new.rw.data))

# Get all unique pairs of categorical variables
combinations <- combn(categorical_vars, 2, simplify = FALSE)

# Loop through each pair
for (combo in combinations) {
  v <- get_corr_v_cramer(new.rw.data[[combo[1]]], new.rw.data[[combo[2]]])
  
  if (!is.na(v) && v > 0.9) { # Print out columns with quite high correlation (can change the threshold here)
    # Chosen 0.9 to just get the very correlated ones, the rest should be eliminated during backwards elimination process
    print(table(new.rw.data[[combo[1]]], new.rw.data[[combo[2]]]))
    cat("Cramér's V between", combo[1], "and", combo[2], "is", round(v, 3), "\n")
  }
}

# W1famtyp2, W1wrkfulldad, W1empsdad are all highly correlated with one another
# W1hiqhqualdad is highly correlated with W1famtyp2 (but not the other 2)
# W1wrkfullmum is highly correlated with W1empsmum
# W1marstatmum is highly correlated with W1famtyp2
# W6UnivYP is highly correlated with W6EducYP and W6acqno
# W6acqno is highly correlated with W6als and W6UnivYP
# W8DACTIVITYC, W8DWRK and W8DACTIVITY are also all highly correlated with one another

# From the above observations, and based on their contingency tables, the following decision was made:
# The main reasoning is motivated by an attempt to keep more levels, and minimise loss of information (will then be refined by merging/transforming levels)

# Keep W1empsdad, removing W1wrkfulldad 
new.rw.data$`W1wrkfulldad(Whether father works full-time)` <- NULL
# Keep W1empsmum, removing W1wrkfullmum
new.rw.data$`W1wrkfullmum(Whether mother works full-time)` <- NULL
# Keep W1hiqualdad and W1marstatmum, removing W1famtyp2 
new.rw.data$`W1famtyp2(Whether single parent HH)` <- NULL
# Keep W6EducYP, removing W6UnivYP (note that here it explains perfectly where the missingness of W6EducYP comes from)
new.rw.data$`W6UnivYP(YP: Whether currently at university)` <- NULL
# Keep W6acqno, removing W6als
new.rw.data$`W6als(Number of A/A2/AS levels being studied at Wave 6)` <- NULL
# Keep W8DACTIVITY, removing W8DACTIVITYC and W8DWRK
new.rw.data$`W8DACTIVITYC(Current activity of CM)` <- NULL
new.rw.data$`W8DWRK(Whether CM currently employed)` <- NULL

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
levels(new.rw.data$`W1wrk1aMP(MP: current working status)`)[levels(new.rw.data$`W1wrk1aMP(MP: current working status)`) %in% c(1,3)] <- "Full Time Work"
levels(new.rw.data$`W1wrk1aMP(MP: current working status)`)[levels(new.rw.data$`W1wrk1aMP(MP: current working status)`) %in% c(2,4)] <- "Part Time Work"
levels(new.rw.data$`W1wrk1aMP(MP: current working status)`)[levels(new.rw.data$`W1wrk1aMP(MP: current working status)`) %in% 5:12] <- "Not Working"

# Checking the changes
summary(new.rw.data$`W1wrk1aMP(MP: current working status)`)

# Now for W1NoldBroHS(Number of younger siblings)

# Custom order for the levels
custom_order_W1NoldBroHS <- c(0,1,2,3,4,5,6,7,9)

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1NoldBroHS(Number of younger siblings)", custom_order_W1NoldBroHS)

# Merging levels 0-1 and 2+ (reasoning: similar boxplot + to balance distributions since some have very little observations)
levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`)[levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`) %in% c(0) ] <- "0"
levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`)[levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`) %in% c(1) ] <- "1"
levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`)[levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`) %in% c(2) ] <- "2"
levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`)[levels(new.rw.data$`W1NoldBroHS(Number of younger siblings)`) %in% c(3,4,5,6,7,9,12)] <- "3+"

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
levels(new.rw.data$`W1ch3_11HH(Number of children aged 3-11 in HH)`)[levels(new.rw.data$`W1ch3_11HH(Number of children aged 3-11 in HH)`) %in% c(0)] <- "0"
levels(new.rw.data$`W1ch3_11HH(Number of children aged 3-11 in HH)`)[levels(new.rw.data$`W1ch3_11HH(Number of children aged 3-11 in HH)`) %in% c(1)] <- "1"
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
levels(new.rw.data$`W1depkids(Number of dependent children in HH)`)[levels(new.rw.data$`W1depkids(Number of dependent children in HH)`) %in% c(1)] <- "1"
levels(new.rw.data$`W1depkids(Number of dependent children in HH)`)[levels(new.rw.data$`W1depkids(Number of dependent children in HH)`) %in% c(2)] <- "2"
levels(new.rw.data$`W1depkids(Number of dependent children in HH)`)[levels(new.rw.data$`W1depkids(Number of dependent children in HH)`) %in% c(3)] <- "3"
levels(new.rw.data$`W1depkids(Number of dependent children in HH)`)[levels(new.rw.data$`W1depkids(Number of dependent children in HH)`) %in% 4:10] <- "4+"

# Checking the changes
summary(new.rw.data$`W1depkids(Number of dependent children in HH)`)

# W1nssecfam(Family’s NS-SEC class)

# Custom order for the levels
custom_order_W1nssecfam <- 1:8

# check the mean weekly income for each level along with the length of each level
get_level_stats(new.rw.data, "W1nssecfam(Family’s NS-SEC class)", custom_order_W1nssecfam)

# Merging levels 1-5(modern occupations) and 6+(routine occupations/no job)... (Based on what makes sense to be changed to a binary + based on box plot)
levels(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`)[levels(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`) %in% 1:2] <- "Professional occupations"
levels(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`)[levels(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`) %in% 3:5] <- "Lower/intermediate occupations"
levels(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`)[levels(new.rw.data$`W1nssecfam(Family’s NS-SEC class)`) %in% 6:8] <- "Routine Occupations/Unemployed"


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

# Merging levels 1,2 (working), 5 (studying) and the others(not in work or education)... (Based on what makes sense, and following the box plot distributions)
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

# Merging levels 1-2(Higher Education) and 3-8(Alevel/lower), and 9(no academic study aim)... (Based on what makes sense, and to balance out the distribution + referred to box plot)
levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`)[levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`) %in% 1:2] <- "Higher Education"
levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`)[levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`) %in% 3:8] <- "A level/lower"
levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`)[levels(new.rw.data$`W6acqno(Highest academic qualification studied at Wave 6)`) %in% c(9)] <- "No Academic Study Aim"

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
levels(new.rw.data$`W8TENURE(Tenure)`)[levels(new.rw.data$`W8TENURE(Tenure)`) %in% 1:2] <- "Owned"
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
levels(new.rw.data$`W8DACTIVITY(Current activity)`)[levels(new.rw.data$`W8DACTIVITY(Current activity)`) %in% c(5,9,10,11,13,14)] <- "Unemployed/Other/Unpaid work"
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

target_col <- "W8DINCW(Continuous weekly income)"
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
main_model <- new.rw.data
dim(main_model)
dim(main_model[complete.cases(main_model), ])
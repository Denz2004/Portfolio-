library(ggplot2)

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

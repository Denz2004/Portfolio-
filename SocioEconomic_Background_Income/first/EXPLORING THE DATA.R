library(ggplot2)

# EXPLORING THE DATA

assig.dat<-read.csv("RWNS_final.csv", header=TRUE, stringsAsFactors=T) #reading the data

dim(assig.dat) #looking at the dimensions
head(assig.dat) #looking at the data
summary(assig.dat) #getting the summary of each col

#looping through each col, and looking at it's impact on the k4score

target_col <- "ks4score"
for (col in names(assig.dat)) {
  if (col == target_col) next  # Skip ks4score itself
  
  # Check if the column is numeric or categorical
  if (is.numeric(assig.dat[[col]])) {
    # Scatter plot for numeric columns
    p <- ggplot(na.omit(assig.dat), aes(x=.data[[col]], y=.data[[target_col]])) +
      geom_point(alpha=0.6, color="blue") +
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

#looking at how many na or missing values we have per column, to then decide whether to remove them or not
count_na <- function(column) {
  if (is.factor(column)) {
    return(sum(is.na(column) | column == "missing"))  # Treat "missing" as NA
  } else {
    return(sum(is.na(column)))
  }
}

# Apply function to all columns and print results
print(sapply(assig.dat, count_na))

#based on these results, we will remove rows with missing values in the FSMband, IDACI_n, singlepar, fsm and sen columns
#as there is a small amount of missing values so in these columns, so removing these rows have minimal impact

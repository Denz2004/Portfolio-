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

# Scatterplot with Line of Best Fit for IDACI_n vs ks4score
ggplot(assig.dat, aes(x = IDACI_n, y = ks4score)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatterplot points (semi-transparent)
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line with confidence interval
  labs(title = "Scatter Plot of IDACI_n vs ks4score",
       x = "IDACI_n (Income Deprivation Index)",
       y = "ks4score") +
  theme_minimal()

# Apply function to all columns and print results
print(sapply(assig.dat, count_na))

#based on these results, we will remove rows with missing values in the FSMband, IDACI_n, singlepar, fsm and sen columns
#as there is a small amount of missing values so in these columns, so removing these rows have minimal impact


# CLEANING THE DATASET

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

# REGRESSION
#we will run the regression starting with all predictors, using a backwards elimination strategy
library(arm)
library(car)

#relevelling the data for categorical predictors that have more than 2 levels
new.rw.data$hiquamum <- relevel(new.rw.data$hiquamum, ref = "GCSE_grades_A-C_or_equiv")
new.rw.data$computer <- relevel(new.rw.data$computer, ref = "No")
new.rw.data$homework <- relevel(new.rw.data$homework, ref = "0-1_evenings")
new.rw.data$attitude <- relevel(new.rw.data$attitude, ref = "very_low")
new.rw.data$truancy <- relevel(new.rw.data$truancy, ref = "No")
new.rw.data$house <- relevel(new.rw.data$house, ref = "rented")
new.rw.data$SECshort <- relevel(new.rw.data$SECshort, ref = "Intermediate")
new.rw.data$parasp <- relevel(new.rw.data$parasp, ref = "No")
new.rw.data$tuition <- relevel(new.rw.data$tuition, ref = "No")
new.rw.data$absent <- relevel(new.rw.data$absent, ref = "No")
new.rw.data$exclude <- relevel(new.rw.data$exclude, ref = "No")
new.rw.data$FSMband <- relevel(new.rw.data$FSMband, ref = "<5pr")

#starting with all predictors, and removing the insignificant predictors until all predictors are significant
ks4.all.lm<-lm(ks4score~. - ks4score, data = new.rw.data)
Anova(ks4.all.lm)

# we remove the most insignificant predictor SECshort
#we also remove absent, as it is highly correlated with exclude, but is less significant than exclude
#this is to avoid multicollinearity
ks4.all.lm<-lm(ks4score~. - ks4score - SECshort - absent, data = new.rw.data)
Anova(ks4.all.lm)

# we remove the most insignificant predictor tuition
ks4.all.lm<-lm(ks4score~. - ks4score - SECshort - absent - tuition, data = new.rw.data)
Anova(ks4.all.lm)

# we remove the most insignificant predictor parasp
ks4.all.lm<-lm(ks4score~. - ks4score - SECshort - absent - tuition - parasp, data = new.rw.data)
Anova(ks4.all.lm)

# we remove the most insignificant predictor fsm
ks4.all.lm<-lm(ks4score~. - ks4score - SECshort - absent - tuition - parasp - fsm, data = new.rw.data)
Anova(ks4.all.lm)

# Since IDACI_n is significant at the 10% level, we decided to take a look more closely on IDACI_n
ks4.IDACI.lm <- lm(ks4score~IDACI_n, data = new.rw.data)
summary(ks4.IDACI.lm)

# Since our observation is that IDACI_n is relevant, we kept it in our model

#display the coefficients
display(ks4.all.lm)
#display the final model summary
summary(ks4.all.lm)
#check that all vif values are less than 10
vif(ks4.all.lm)

#checking the residuals of our final model
par(mfrow=c(2,2))
plot(ks4.all.lm,which=c(1,2))
hist(ks4.all.lm$residuals,main="Histogramofresiduals",
     font.main=1,xlab="Residuals")

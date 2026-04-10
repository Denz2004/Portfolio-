# REGRESSION FOR THE BONUS MODEL
#we will run the regression using a backwards elimination strategy
library(arm)
library(car)

model_na_data<-clean_main_model #creating a copy where missing will be replaced by NA
cols <- c("W1wrk1aMP", "W1hea2MP", "W1marstatmum", "W1depkids", "W1nssecfam", 
          "W1ethgrpYP", "W1heposs9YP", "W1disabYP", "W2disc1YP", "W4CannTryYP", 
          "W5Apprent1YP", "W6EducYP", "W6NEETAct", "W8CMSEX", "standardised_W6DebtattYP", "log_of_W8DINCW") #columns to keep

model_na_data <- model_na_data[, colnames(model_na_data) %in% cols] #keeping only the significant columns

# Loop through each column
for (col in colnames(model_na_data)) {
  # Check if missing is a level, and replace it by NA
      levels(model_na_data[[col]])[levels(model_na_data[[col]]) == "missing"] <- NA
}

complete_cases <- complete.cases(model_na_data) # we now need to do a complete cases analysis 
model_na_data<-model_na_data[complete_cases, ] #keeping only complete rows
summary(model_na_data) #checking the summary
#we have to remove this column as it only has 1 level after the transformation of missing to NA
model_na_data <- model_na_data[, !colnames(model_na_data) %in% "W6EducYP"] 
dim(model_na_data) #the dimensions are much smaller, this is the impact of replacing the missing levels with NA

#starting with all predictors 
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW, data = model_na_data)
Anova(W8DINCW.na.lm)
 
#we remove the least significant predictor standardised_W6DebtattYP 
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW - standardised_W6DebtattYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W2disc1YP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW - standardised_W6DebtattYP - W2disc1YP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1hea2MP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW - standardised_W6DebtattYP - W2disc1YP - W1hea2MP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W5Apprent1YP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW - standardised_W6DebtattYP - W2disc1YP - W1hea2MP - W5Apprent1YP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1depkids
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW - standardised_W6DebtattYP - W2disc1YP - W1hea2MP - W5Apprent1YP
                    - W1depkids, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1disabYP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW - standardised_W6DebtattYP - W2disc1YP - W1hea2MP - W5Apprent1YP
                    - W1depkids - W1disabYP, data = model_na_data)
Anova(W8DINCW.na.lm)

#we remove the least significant predictor W1heposs9YP
W8DINCW.na.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW - standardised_W6DebtattYP - W2disc1YP - W1hea2MP - W5Apprent1YP
                    - W1depkids - W1disabYP - W1heposs9YP, data = model_na_data)
Anova(W8DINCW.na.lm) #we are left with half the predictors from the original model

#display the coefficients of the final model
display(W8DINCW.na.lm)
#display the final model summary
summary(W8DINCW.na.lm)
#check that all vif values are less than 10
vif(W8DINCW.na.lm)

#checking the residuals of our final model
par(mfrow=c(2,2))
plot(W8DINCW.na.lm,which=c(1,2))
hist(W8DINCW.na.lm$residuals,main="Histogramofresiduals",
     font.main=1,xlab="Residuals")
#Our bonus model is better than our initial model.
#We see that the residuals vs fitted plot displays less heteroscedasticity than the initial model.
#The qq plot has also improved, however the histogram looks less like a normal distribution.
#it is interesting to see the impact of moving to a complete case analysis had, as the R^2 significantly improved
#What was also interesting was that we had to remove even more predictors in this model.
#However, we will proceed with outlier analysis, and interactions on our initial model with no NA values as it has more data
#and more predictors to choose from for interactions. We will validate both models.
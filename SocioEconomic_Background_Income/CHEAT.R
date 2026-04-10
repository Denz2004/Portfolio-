# Remove rows with any NA values before fitting the model
new.rw.data<-new.rw.data[, !col_names_cleaned %in% cont_with_more_than_30_per]
colnames(new.rw.data) <- gsub("\\(.*\\)", "", colnames(new.rw.data))
dim(new.rw.data)
new.rw.data.cleaned <- new.rw.data
dim(new.rw.data.cleaned)
# Now fit the model on the cleaned dataset
full_model <- lm(W8DINCW ~ ., data = new.rw.data.cleaned )

# Perform backward elimination
backward_model <- step(full_model, direction = "backward")

# Show final model
summary(backward_model)

display(backward_model)
library(car)
Anova(backward_model)
vif(backward_model)
# Get the coefficients of the model
model_coefficients <- coef(backward_model)
# Count the number of numeric columns

# Print the model equation
intercept <- model_coefficients[1]  # Intercept
coefficients <- model_coefficients[-1]  # Slopes for predictors

# Create the regression equation
equation <- paste("ks4score = ", round(intercept, 2), 
                  " + ", paste(round(coefficients, 2), names(coefficients), sep="*", collapse=" + "), 
                  sep="")

# Print the equation
cat("Regression Model Equation: ", equation)
#checking the residuals of our final model
par(mfrow=c(2,2))
plot(backward_model,which=c(1,2))
hist(backward_model$residuals,main="Histogramofresiduals",
     font.main=1,xlab="Residuals")

#Residual standard error: 35.52 on 3258 degrees of freedom
#(80 observations deleted due to missingness)
#Multiple R-squared:  0.7513,	Adjusted R-squared:  0.7476 
#F-statistic:   205 on 48 and 3258 DF,  p-value: < 2.2e-16

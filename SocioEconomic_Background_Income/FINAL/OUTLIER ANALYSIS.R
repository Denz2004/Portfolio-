
#for the outlier analysis, we will focus on our best model: W8DINCW.all.lm
library(car)
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

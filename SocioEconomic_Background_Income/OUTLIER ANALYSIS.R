#for the outlier analysis, we will focus on our best model: W8DINCW.all.lm
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
HP.out #from the output of the function shown in lectures, we see that there are no outliers in terms of cook distance
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

clean_main_model[two_out_of_three,] # looking at the outliers, we do not see any obvious pattern between them
#none of these outliers appear to show data errors as well, so we can't justify removing them, however, we will explore their impact on the model
#we will now check to see how badly the model performs on these outliers

# Predict the values using the model
predicted_values <- fitted(W8DINCW.all.lm)

# Actual values are the original dependent variable (W8DINCW)
actual_values <- clean_main_model$W8DINCW

# Calculate absolute differences between predicted and actual values
absolute_differences <- abs(predicted_values - actual_values)

# compute the differences between predicted and actual values for outliers and non outliers
outliers <- absolute_differences[which(1:nrow(clean_main_model) %in% two_out_of_three)]
non_outliers <- absolute_differences[which(!(1:nrow(clean_main_model) %in% two_out_of_three))]

# Calculate the Mean Absolute Error for outliers and non-outliers
mae_outliers <- mean(outliers)
mae_non_outliers <- mean(non_outliers)

# Display the results
cat("Mean Absolute Error for Outliers: ", mae_outliers, "\n")
cat("Mean Absolute Error for Non-Outliers: ", mae_non_outliers, "\n")

# the outliers clearly aren't being predicted well, we will see if removing them helps the model
clean_data_no_outliers <- clean_main_model[-two_out_of_three, ]

# 2. Refit the model on the data set with no outliers
W8DINCW.all.lm.no_outliers <- lm(W8DINCW ~ . - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                                 - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                                 - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                                 - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                                 - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                                 - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_data_no_outliers)

summary(W8DINCW.all.lm)
summary(W8DINCW.all.lm.no_outliers)

Anova(W8DINCW.all.lm.no_outliers) #the model without the outliers is only marginally better, 
#so we will keep the outliers in the final model 
#We will use our old model in the next part and look to see if there are any significant interactions to add

avPlots(W8DINCW.all.lm) #looking at the av plots, 
#these identify the two points with the highest residual and the most extreme horizontal values for each predictor
#again although these plots identify potential outliers, we do not remove any as the cook distance for all points are very low
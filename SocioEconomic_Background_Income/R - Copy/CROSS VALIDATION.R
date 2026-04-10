# we will now validate and see how accurate our final model is on out of sample data
# we will also compare it to our model with all other models, to see which one is better
#NOTE FOR THIS PART YOU MAY HAVE TO RUN CODE SEVERAL TIMES DUE TO SAMPLE SPLITTING DATA INTO LEVELS NOT SEEN IN THE TRAINING
library(gridExtra)
#first we need to add some fake observations to avoid errors during cross validation due to some levels having little observations thus not represented in training set
pred_df<-clean_main_model
relevant_vars<-rownames(as.data.frame(Anova(W8DINCW.all.lm)))
relevant_vars<-c("W8DINCW", relevant_vars) # Adding our outcome
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
  cv.W8DINCW.all.lm <- lm(log(W8DINCW) ~ . , data = training.set)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.all.lm, test.set), 
                           #predicted vs original
                           original=log(test.set$W8DINCW),error=(predict(cv.W8DINCW.all.lm,test.set)-log(test.set$W8DINCW)))
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
  cv.W8DINCW.all.lm <- lm(log(W8DINCW) ~ ., data = training.set)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.all.lm, test.set), 
                           #predicted vs original
                           original=log(test.set$W8DINCW),error=(predict(cv.W8DINCW.all.lm,test.set)-log(test.set$W8DINCW)))
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
mse.pred<-array(dim=c(100,2))
#code from lectures
for(i in 1:100){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the 70% to fit the model
  training.set <- rbind(training.set, fake_data) # Add fake data to prevent level mismatch in training-test data
  test.set<-pred_df[-cross.val,] # the 30% to use as validation sample
  
  #fit the model
  cv.W8DINCW.all.lm <- lm(log(W8DINCW) ~ ., data = training.set)
  
  #in sample prediction error
  in.sample.error=predict(cv.W8DINCW.all.lm,training.set)-log(training.set$W8DINCW)
  #out of sample prediction error
  out.sample.error=predict(cv.W8DINCW.all.lm,test.set)-log(test.set$W8DINCW)
  #in sample mse
  mse.pred[i,1]<-mean(in.sample.error^2, na.rm = TRUE)
  #out of sample mse
  mse.pred[i,2]<-mean(out.sample.error^2, na.rm = TRUE)
}
# Take the means of the two
mse <- data.frame(full = colMeans(mse.pred))
rownames(mse) <- c("in sample", "out of sample")

# Add a percentage difference row
mse["percentage increase", ] <- 100 * (mse["out of sample", ] - mse["in sample", ]) / mse["in sample", ]

# View the result
mse
income_variance <- var(log(pred_df$W8DINCW))
income_variance 
#the MSEs are much lower than the sample variance in the weekly income, showing the model fits well, and predicts well.
#The model is very accurate at predicting in and out of sample data, and only has a difference of 1.23% between its out of sample and in sample predictions, which is really good.
# In sample: 0.01274925
# Out sample: 0.01290646
# Percentage increase: 1.23
# var(log(outcome)): 0.03946939
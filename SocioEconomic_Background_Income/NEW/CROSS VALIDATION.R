# we will now validate and see how accurate our final model is on out of sample data
#we will also compare it to our model with NA, to see which one is better
#NOTE FOR THIS PART YOU MAY HAVE TO RUN CODE SEVERAL TIMES DUE TO SAMPLE SPLITTING DATA INTO LEVELS NOT SEEN IN THE TRAINING

#first we need to remove some factor levels with small sample sizes, 
#this is because we were getting test data that had categorical predictors with only 1 level
pred_df<-clean_main_model
dim(pred_df)
pred_df <- pred_df[, !colnames(pred_df) %in% c("W1InCarHH", "W4Childck1YP")]#we only remove 2 predictors that aren't in the final model anyway, so the analysis is still relevant
library(gridExtra)

#code from lectures to test our model on out of sample data
split.proportions<-c(0.5,0.7,0.9) #we use 50,70,90 splits to assess if our training model does well at predicting with large and small amounts of data to train on
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),split.proportions[i]*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the training data to fit the model
  test.set<-pred_df[-cross.val,] # the test data to use as validation sample
  #fit the full model
  cv.W8DINCW.lm<- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - log_of_W1GrssyrMP - log_of_W1GrssyrHH
                     - W1hous12HH - W8DACTIVITY,
                     data = training.set)
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.lm,test.set), 
                           #predicted vs original
                           original=test.set$log_of_W8DINCW,error=(predict(cv.W8DINCW.lm,test.set)-test.set$log_of_W8DINCW))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", se=FALSE) 
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
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)
#from the plots, we can see that the predicted vs original lines are very close, meaning the model is generalizing to out of sample data really well
#for the predicted vs error plot, there are a lot of points beyond +-1 sd, but there is no clear pattern of the error increasing with the size of predictions
#the black dots have bigger errors as these represent the 50% splits, this is expected as the model had less data to train on and had to generalise to a bigger subset of data


#we will now compare our model accuracy
mse.pred<-array(dim=c(100,2))
#code from lectures
for(i in 1:100){
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df), replace=FALSE)
  training.set<-pred_df[cross.val,] 
  test.set<-pred_df[-cross.val,]  
  #full regression model
  cv.W8DINCW.lm<- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - log_of_W1GrssyrMP - log_of_W1GrssyrHH
                     - W1hous12HH - W8DACTIVITY,
                     data = training.set)
  #in sample prediction error
  in.sample.error=predict(cv.W8DINCW.lm,training.set)-training.set$log_of_W8DINCW
  #out of sample prediction error
  out.sample.error=predict(cv.W8DINCW.lm,test.set)-test.set$log_of_W8DINCW
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
income_variance <- var(pred_df$log_of_W8DINCW)
income_variance #the MSEs are much lower than the sample variance in the weekly income, showing the model fits well, and predicts well.
#The model is very accurate at predicting in and out of sample data, and only has a difference of 2% between its out of sample and in sample predictions, which is really good.
#we will now compare it to our NA model


pred_df<-model_na_data

split.proportions<-c(0.5,0.7,0.9) 
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(pred_df),split.proportions[i]*nrow(pred_df) , replace=FALSE)
  training.set<-pred_df[cross.val,] #the training data to fit the model
  test.set<-pred_df[-cross.val,] # the test data to use as validation sample
  #fit the full model
  cv.W8DINCW.lm<- lm(log_of_W8DINCW ~ . - log_of_W8DINCW - standardised_W6DebtattYP - W2disc1YP - W1hea2MP - W5Apprent1YP
                     - W1depkids - W1disabYP - W1heposs9YP,
                     data = training.set) #reduced model with NA instead of missing
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.lm,test.set), 
                           #predicted vs original
                           original=test.set$log_of_W8DINCW,error=(predict(cv.W8DINCW.lm,test.set)-test.set$log_of_W8DINCW))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", se=FALSE) 
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
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error, na.rm=TRUE), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)
#from the plots, we can see that the predicted vs original relatively close. The smaller sample size has a big impact on the variance of results here
#for the predicted vs error plot, there are a lot of points beyond +-1 sd, but there is no clear pattern of the error increasing with the size of predictions
#so far the model looks ok at predicting values however, we will run a larger amount of validations to prove or disprove this

#we will now compare compare our NA model to the one with missing levels
mse.pred<-array(dim=c(100,2))
for(i in 1:100){
  cross.val<-sample(1:nrow(pred_df),0.7*nrow(pred_df), replace=FALSE)
  training.set<-pred_df[cross.val,] 
  test.set<-pred_df[-cross.val,]  
  #model with NA
  cv.W8DINCW.lm<- lm(log_of_W8DINCW ~ . - log_of_W8DINCW - standardised_W6DebtattYP - W2disc1YP - W1hea2MP - W5Apprent1YP
                     - W1depkids - W1disabYP - W1heposs9YP,
                     data = training.set)
  #in sample prediction error
  in.sample.error=predict(cv.W8DINCW.lm,training.set)-training.set$log_of_W8DINCW
  #out of sample prediction error
  out.sample.error=predict(cv.W8DINCW.lm,test.set)-test.set$log_of_W8DINCW
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
#although the MSEs are much lower than the sample variance in the weekly income, the NA model clearly underperforms the model with missing as a level
#we see that unlike the other model, the model with NA does not generalise well to out of sample data, 
#having around 20% increase in MSE when predicting out of sample data.
#this is due to the small sample size of the NA model, due to it only considering complete cases, 
#it has less data to train on and predicts the target worse than the larger model
#we prefer the full model with missing as it predicts values much better than the NA model despite and has much less overfit.
#note the NA model is still better than the variance of the target, however, it is clearly unbalanced and doesn't do as well as our preferred model when predicting values.
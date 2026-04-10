# we will now validate and see how accurate our final model with interactions is on out of sample data
#we will also compare it to our final model but without interactions, to see which one is better

#first we need to remove some factor levels with small sample sizes, 
#this is because we were getting test data that had levels that weren't in the training data
cleaner_main_model <- clean_main_model[clean_main_model$W1ch0_2HH != 3 & !clean_main_model$W1ch12_15HH %in% c(0,4)
                                     & !clean_main_model$W6gcse %in% c(0,3, 1) & !clean_main_model$W6als %in% c(3), ]
dim(cleaner_main_model)
dim(clean_main_model) #as can be seen, we only remove 22 rows, so the analysis is still relevant
library(gridExtra)

#code from lectures to test our model on out of sample data
#note you may have to run this multiple times if the out of sample subset has levels not seen in the training subset
split.proportions<-c(0.5,0.7,0.9) #we use 50,70,90 splits to assess if our training model does well at predicting with large and small amounts of data to train on
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(cleaner_main_model),split.proportions[i]*nrow(cleaner_main_model) , replace=FALSE)
  training.set<-cleaner_main_model[cross.val,] #the training data to fit the model
  test.set<-cleaner_main_model[-cross.val,] # the test data to use as validation sample
  #fit the full model
  cv.W8DINCW.lm<- lm(W8DINCW ~ . 
                     + standardised_W6DebtattYP * W6EducYP
                     - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH,
                     data = training.set)
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.W8DINCW.lm,test.set), 
                           #predicted vs original
                           original=test.set$W8DINCW,error=(predict(cv.W8DINCW.lm,test.set)-test.set$W8DINCW))
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


#we will now compare our full model to the full model without interactions
mse.pred<-array(dim=c(100,2))
mse.pred.red<-array(dim=c(100,2))
#code from lectures
#you may have to run this multiple times, as sometimes the test data has all but one of the levels, meaning the training data has factors with only one level or vice versa
for(i in 1:100){
  cross.val<-sample(1:nrow(cleaner_main_model),0.7*nrow(cleaner_main_model), replace=FALSE)
  training.set<-cleaner_main_model[cross.val,] 
  test.set<-cleaner_main_model[-cross.val,]  
  #full regression model
  cv.W8DINCW.lm<- lm(W8DINCW ~ . 
                     + standardised_W6DebtattYP * W6EducYP
                     - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH,
                     data = training.set)
  #reduced regression model with no interactions
  cv.W8DINCW.lm.red<- lm(W8DINCW ~ . 
                         - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                         - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                         - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                         - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                         - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                         - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH,
                     data = training.set)
  #in sample prediction error
  in.sample.error=predict(cv.W8DINCW.lm,training.set)-training.set$W8DINCW
  in.sample.error.red=predict(cv.W8DINCW.lm.red,training.set)-training.set$W8DINCW
  #out of sample prediction error
  out.sample.error=predict(cv.W8DINCW.lm,test.set)-test.set$W8DINCW
  out.sample.error.red=predict(cv.W8DINCW.lm.red,test.set)-test.set$W8DINCW
  #in sample mse
  mse.pred[i,1]<-mean(in.sample.error^2, na.rm = TRUE)
  mse.pred.red[i,1]<-mean(in.sample.error.red^2, na.rm = TRUE)
  #out of sample mse
  mse.pred[i,2]<-mean(out.sample.error^2, na.rm = TRUE)
  mse.pred.red[i,2]<-mean(out.sample.error.red^2, na.rm = TRUE)
}
# Take the means of the two
mse <- data.frame(full = colMeans(mse.pred), reduced = colMeans(mse.pred.red))
rownames(mse) <- c("in sample", "out of sample")

# Add a percentage difference row
mse["percentage increase", ] <- 100 * (mse["out of sample", ] - mse["in sample", ]) / mse["in sample", ]

# View the result
mse
income_variance <- var(cleaner_main_model$W8DINCW)
income_variance #the MSEs are much lower than the sample variance in the weekly income, showing both models are valid and performant models
#from the result we see that although the full model with interactions predicts the data better than the model without interactions,
#it has a bigger difference in MSEs, meaning that the reduced model generalizes better to out of sample data
#this is due to over fitting from the added interaction predictor
#ultimately both models are performing really well and have small MSEs compared to the sample variance.
#we prefer the full model as it predicts values better than the reduced model despite the over fitting



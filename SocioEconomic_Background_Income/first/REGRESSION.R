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

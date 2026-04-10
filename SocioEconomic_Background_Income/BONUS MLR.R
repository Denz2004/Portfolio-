# REGRESSION FOR THE BONUS MODEL
#we will run the regression starting with all predictors, using a backwards elimination strategy
library(arm)
library(car)

#starting with all predictors that were significant in the main model, and removing the insignificant predictors until all predictors are significant

# Run the regression using backwards elimination
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)


# we remove the least significant predictor log_of_W8QDEB2
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)

# we remove the least significant predictor W4AlcFreqYP
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)

# we remove the least significant predictor W1condur5MP
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP - W1condur5MP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)

# we remove the least significant predictor W1wrkfullmum
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP - W1condur5MP - W1wrkfullmum
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)

# we remove the least significant predictor W5JobYP
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP - W1condur5MP - W1wrkfullmum - W5JobYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)

# we remove the least significant predictor W2ghq12scr
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP - W1condur5MP - W1wrkfullmum - W5JobYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH - W2ghq12scr
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)


# we remove the least significant predictor standardised_W1hiqualparents_combined_qualification, this is counter intuitive for me
#I would expect income to have some relation with the qualification of your parents
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP - W1condur5MP - W1wrkfullmum - W5JobYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH - W2ghq12scr - W1usevcHH - standardised_W1hiqualparents_combined_qualification
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)


# we remove the least significant predictor W8TENURE
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP - W1condur5MP - W1wrkfullmum - W5JobYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH - W2ghq12scr - W1usevcHH - standardised_W1hiqualparents_combined_qualification
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - IndSchool - W8TENURE
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)

# we remove the least significant predictor W5EducYP, another counter intuitive one for me, I would expect going to school at 16 to play a significant role in future income
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP - W1condur5MP - W1wrkfullmum - W5JobYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH - W2ghq12scr - W1usevcHH - standardised_W1hiqualparents_combined_qualification
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - IndSchool - W8TENURE - W5EducYP
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)

# we remove the least significant predictor W8DWRK this is counter intuitive for me, I think being employed or unemployed would significantly contribute to your income
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP - W1condur5MP - W1wrkfullmum - W5JobYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH - W2ghq12scr - W1usevcHH - standardised_W1hiqualparents_combined_qualification
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - IndSchool - W8TENURE - W5EducYP - W8DWRK
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)

# we remove the least significant predictor W1hwndayYP
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP - W1condur5MP - W1wrkfullmum - W5JobYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH - W2ghq12scr - W1usevcHH - standardised_W1hiqualparents_combined_qualification
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - IndSchool - W8TENURE - W5EducYP - W8DWRK
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC - W1hwndayYP
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_missing_model)
Anova(W8DINCW.na.lm)

# we now have 2 borderline significant predictors, we will remove both of them, as the project advice
#tells us to do so if our model has a lot of predictors
W8DINCW.na.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV - log_of_W8QDEB2 - W4AlcFreqYP
                    - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP - W1condur5MP - W1wrkfullmum - W5JobYP
                    - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH - W2ghq12scr - W1usevcHH - standardised_W1hiqualparents_combined_qualification
                    - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - IndSchool - W8TENURE - W5EducYP - W8DWRK
                    - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC - W1hwndayYP
                    - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH - W8DACTIVITY - standardised_W4schatYP, data = clean_missing_model)
Anova(W8DINCW.na.lm)

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
#our bonus model is worse than our initial model, this is to be expected as it has predictors that have more than 30% NA values
#we would recommend the our first model, however, 
#it is interesting to see that this 2nd model made us remove even more predictors than the 1st model
#we see that the residuals vs fitted plot also displays heteroscedasticity as in the first model
#overall, our bonus model is worse than our initial model, which was expected. What was interesting was that we had to remove even more predictors in this model.
#we will proceed with validation on our initial model with no NA values
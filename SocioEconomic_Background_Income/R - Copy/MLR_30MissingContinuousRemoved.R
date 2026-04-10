# Removing continuous predictors with more than 30% missing values
rm_missing_cont_data <- clean_main_model[, !colnames(clean_main_model) %in% c("log_of_W1GrssyrMP", "log_of_W1GrssyrHH", "log_of_W8QDEB2")]
summary(rm_missing_cont_data)

# Running through backwards elimination process to see if the regression differs a lot compared to when the continuous columns with more than 30% missing is included
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1bulrc column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1empsdad column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W6OwnchiDV column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W8DMARSTAT column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT
                               , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1truantYP column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W4RacismYP column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1alceverYP column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1depkids column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1NoldBroHS column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W2depressYP column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W2ghq12scr_binary column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W4empsYP column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W4Childck1YP column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W6acqno column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1InCarHH column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1ch16_17HH column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W8TENURE column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1ch12_15HH column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W5JobYP column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the standardised_W1yschat1 column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W8QMAFI column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1empsmum column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1ch3_11HH column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W4NamesYP column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# Moving forward are borderline significant predictors

# we remove the IndSchool column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP - IndSchool
                               , data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W6gcse column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP - IndSchool
                               - W6gcse, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1ch0_2HH column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP - IndSchool
                               - W6gcse - W1ch0_2HH, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

# we remove the W1hous12HH column
W8DINCW.rm_continuous.lm <- lm(log(W8DINCW) ~ . - W1bulrc - W1empsdad - W6OwnchiDV - W8DMARSTAT - W1truantYP
                               - W4RacismYP - W1alceverYP - W1depkids - W1NoldBroHS - W2depressYP - W2ghq12scr_binary
                               - W4empsYP - W4Childck1YP - W6acqno - W1InCarHH - W1ch16_17HH - W8TENURE - W1ch12_15HH
                               - W5JobYP - standardised_W1yschat1 - W8QMAFI - W1empsmum - W1ch3_11HH - W4NamesYP - IndSchool
                               - W6gcse - W1ch0_2HH - W1hous12HH, data = rm_missing_cont_data)
Anova(W8DINCW.rm_continuous.lm)

## Comparing the main model and this model...
# All predictors that are significant in the main model is still significant in this model (except the ones we removed from the start)
# Additionally, this model includes : 
# "W1condur5MP", "W1usevcHH", "W1hwndayYP", "W4AlcFreqYP", "W5EducYP",
# "W8DGHQSC_binary", "standardised_W4schatYP", "standardised_W1hiqualparents_score"
# As a replacement to the removed continuous predictors due to having >30% missing values

#display the coefficients of this model
display(W8DINCW.rm_continuous.lm)
#display the final model summary
summary(W8DINCW.rm_continuous.lm)
#check that all vif values are less than 5 (multicollinearity checks)
vif(W8DINCW.rm_continuous.lm) #we see that all adjusted VIF are less than 5 so there are no multicollinearity issues

#checking the residuals of our final model
par(mfrow=c(2,2))
plot(W8DINCW.rm_continuous.lm,which=c(1,2))
hist(W8DINCW.rm_continuous.lm$residuals,main="Histogramofresiduals",
     font.main=1,xlab="Residuals")

# The plots and the diagnostics suggests that this is a much better model compared to the main_model
# The histogram of residual does not appear skewed compared to the main model
# The QQ residuals also show an improvement where the upper tail now lies on the diagonal
# The residuals vs fitted plot also does not show curvature, and does not have a clear funnel shape
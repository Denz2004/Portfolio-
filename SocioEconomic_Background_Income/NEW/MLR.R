# REGRESSION
#we will run the regression starting with all predictors, using a backwards elimination strategy
library(arm)
library(car)

# Re level the categorical columns 
  
  main_model$`W1wrk1aMP(MP: current working status)` <- relevel(main_model$`W1wrk1aMP(MP: current working status)`, ref = "Not Working")
  main_model$`W1condur5MP(Whether there is a computer in HH)` <- relevel(main_model$`W1condur5MP(Whether there is a computer in HH)`, ref = "2")
  main_model$`W1hea2MP(Whether MP has longstanding illness or disability)` <- relevel(main_model$`W1hea2MP(Whether MP has longstanding illness or disability)`, ref = "2")
  main_model$`W1usevcHH(Whether anyone uses a motor vehicle)` <- relevel(main_model$`W1usevcHH(Whether anyone uses a motor vehicle)`, ref = "2")
  main_model$`W1empsmum(Employment status of mother)` <- relevel(main_model$`W1empsmum(Employment status of mother)`, ref = "Not Working")
  main_model$`W1empsdad(Employment status of father)` <- relevel(main_model$`W1empsdad(Employment status of father)`, ref = "Not Working")
  main_model$`W1nssecfam(Family’s NS-SEC class)` <- relevel(main_model$`W1nssecfam(Family’s NS-SEC class)`, ref = "Routine Occupations/Unemployed")
  main_model$`W1heposs9YP(Likelihood of YP going to university)` <- relevel(main_model$`W1heposs9YP(Likelihood of YP going to university)`, ref = "4")
  main_model$`W1truantYP(Whether YP played truant in last 12 months)` <- relevel(main_model$`W1truantYP(Whether YP played truant in last 12 months)`, ref = "2")
  main_model$`W1alceverYP(Whether YP ever had alcohol)` <- relevel(main_model$`W1alceverYP(Whether YP ever had alcohol)`, ref = "2")
  main_model$`W1bulrc(Whether YP bullied in last 12 months)` <- relevel(main_model$`W1bulrc(Whether YP bullied in last 12 months)`, ref = "2")
  main_model$`W1disabYP(Whether YP has any disability/long term illness or health problem)` <- relevel(main_model$`W1disabYP(Whether YP has any disability/long term illness or health problem)`, ref = "3")
  main_model$`W2disc1YP(Whether YP thinks they have ever been treated unfairly by teachers because of skin colour or ethnic origin)` <- relevel(main_model$`W2disc1YP(Whether YP thinks they have ever been treated unfairly by teachers because of skin colour or ethnic origin)`, ref = "2")
  main_model$`W4CannTryYP(Whether YP ever tried cannabis)` <- relevel(main_model$`W4CannTryYP(Whether YP ever tried cannabis)`, ref = "2")
  main_model$`W4NamesYP(Whether YP has been called names, sworn at or insulted in last 12 months)` <- relevel(main_model$`W4NamesYP(Whether YP has been called names, sworn at or insulted in last 12 months)`, ref = "2")
  main_model$`W4RacismYP(Whether YP has been threatened/insulted in the last 12 months due to skin colour/ethnicity)` <- relevel(main_model$`W4RacismYP(Whether YP has been threatened/insulted in the last 12 months due to skin colour/ethnicity)`, ref = "2")
  main_model$`W4empsYP(Employment status of young person)` <- relevel(main_model$`W4empsYP(Employment status of young person)`, ref = "Not Working Or In Education")
  main_model$`W5JobYP(YP: Whether currently doing paid work)` <- relevel(main_model$`W5JobYP(YP: Whether currently doing paid work)`, ref = "2")
  main_model$`W5EducYP(YP: Whether currently going to school or college)` <- relevel(main_model$`W5EducYP(YP: Whether currently going to school or college)`, ref = "2")
  main_model$`W5Apprent1YP(YP: Whether currently doing an apprenticeship)` <- relevel(main_model$`W5Apprent1YP(YP: Whether currently doing an apprenticeship)`, ref = "2")
  main_model$`W6JobYP(YP: Whether currently doing paid work)` <- relevel(main_model$`W6JobYP(YP: Whether currently doing paid work)`, ref = "2")
  main_model$`W6EducYP(YP: Whether currently going to school or college)` <- relevel(main_model$`W6EducYP(YP: Whether currently going to school or college)`, ref = "2")
  main_model$`W6acqno(Highest academic qualification studied at Wave 6)` <- relevel(main_model$`W6acqno(Highest academic qualification studied at Wave 6)`, ref = "No Academic Study Aim")
  main_model$`W6OwnchiDV(Whether Respondents have own Child/Children)` <- relevel(main_model$`W6OwnchiDV(Whether Respondents have own Child/Children)`, ref = "2")
  main_model$`W8DACTIVITY(Current activity)` <- relevel(main_model$`W8DACTIVITY(Current activity)`, ref = "Unemployed/Other/Unpaid work")
  main_model$`W8QMAFI(How managing financially these days)` <- relevel(main_model$`W8QMAFI(How managing financially these days)`, ref = "5")

  
#checking changes
categorical_cols <- sapply(main_model, is.factor)

# Loop through the column names that are factors
for (col_name in names(categorical_cols)[categorical_cols]) {
  
  # Get all levels of the factor column from main model
  levels_list <- levels(main_model[[col_name]])
  
  cat("Column:", col_name, "\n")
  cat("Baseline level:", levels_list[1], "\n")  # Print reference level
  cat("All levels:", paste(levels_list, collapse = ", "), "\n\n")  # Print all levels
}
#we will do the main model first, then once we have our final model, we will test the model without the continuous columns that have less than 30%
#starting with all predictors, and removing the insignificant predictors until all predictors are significant
# Rename the columns without parenthesis as lm doesn't like col names with parentheses
clean_main_model<-main_model
# Remove anything after the first parenthesis in column names of clean_main_model
colnames(clean_main_model) <- gsub("\\(.*\\)", "", colnames(clean_main_model))

# Run the regression using backwards elimination
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW, data = clean_main_model)
Anova(W8DINCW.all.lm)


# we remove empsdad first, as it is insignificant
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the  predictor W1ch3_11HH
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W6OwnchiDV
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W4empsYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1alceverYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W4RacismYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP, data = clean_main_model)
Anova(W8DINCW.all.lm)


# we remove the predictor W1truantYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1bulrc
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W8DMARSTAT
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W2depressYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1InCarHH
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W8QMAFI, this is another one that seems counter intuitive to me
#surely you'd have a significantly higher income if you are managing well financially
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1ch12_15HH
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1ch16_17HH
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W6JobYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W6acqno, another one that seems counter intuitive to me, 
#I thought study level and income would have a significant link
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1NoldBroHS
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1ch0_2HH
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor standardised_W1yschat1
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W4NamesYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W6gcse
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc  - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse, data = clean_main_model)
Anova(W8DINCW.all.lm)


# we remove the predictor W1hous12HH
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                      - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W5JobYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W8TENURE
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor log_of_W8QDEB2
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1empsmum
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W5EducYP, this one is also counter intuitive for me, surely going to school 
#at 16 would affect future income, I believe that the sample sizes of no isn't powerful enough to make this predictor significant
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1condur5MP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor standardised_W1hiqualparents_score, this is also somewhat counter intuitive for me,
#I would expect your parents' education level to impact your income.
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W4AlcFreqYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor IndSchool, potentially counter intuitive as well, I would expect students
#attending independent schools to earn a higher income down the line
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1usevcHH
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor standardised_W4schatYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W1hwndayYP
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W8DGHQSC_binary
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the predictor W2ghq12scr_binary
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse 
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we now have a few borderline significant predictors, we will remove them as our model has a lot of predictors
#and we don't want the model to over fit, the hint slides explain to remove predictors, even if they are borderline sig, 
#if you have a lot of variables in your model. we remove W4Childck1YP first 
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse- W4Childck1YP
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

#we remove log_of_W1GrssyrMP next
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse- W4Childck1YP - log_of_W1GrssyrMP
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

#we remove log_of_W1GrssyrHH next
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse- W4Childck1YP - log_of_W1GrssyrMP - log_of_W1GrssyrHH
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

#next we remove W8DACTIVITY (which seems counter-intuitive as someone's work related activity should impact their income)
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse- W4Childck1YP - log_of_W1GrssyrMP - log_of_W1GrssyrHH
                     - W1hous12HH - W8DACTIVITY, data = clean_main_model)
Anova(W8DINCW.all.lm)

#display the coefficients of the final model
display(W8DINCW.all.lm)
#display the final model summary
summary(W8DINCW.all.lm)
#check that all vif values are less than 10
vif(W8DINCW.all.lm) #we see that all adjusted VIF are less than 10 so it is fine

#checking the residuals of our final model
par(mfrow=c(2,2))
plot(W8DINCW.all.lm,which=c(1,2))
hist(W8DINCW.all.lm$residuals,main="Histogramofresiduals",
     font.main=1,xlab="Residuals")

#we see that the residuals vs fitted plot displays heteroscedasticity, 
#however, the model itself has good diagnostics, the qq plot is decent and the histogram looks like a normal distribution
#note none of the 3 continuous columns with more than 30% missing values appear in the final model
#so for our bonus model, we will be replacing all "missing" values with NA.
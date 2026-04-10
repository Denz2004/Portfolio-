# REGRESSION
#we will run the regression starting with all predictors, using a backwards elimination strategy
library(arm)
library(car)

# Re level the categorical columns of both models to have the logical baseline level

for (i in seq_along(list(model_with_missing, main_model))) {
  model <- list(model_with_missing, main_model)[[i]]
  
  model$`W1wrk1aMP(MP: current working status)` <- relevel(model$`W1wrk1aMP(MP: current working status)`, ref = "Not Working")
  model$`W1condur5MP(Whether there is a computer in HH)` <- relevel(model$`W1condur5MP(Whether there is a computer in HH)`, ref = "2")
  model$`W1hea2MP(Whether MP has longstanding illness or disability)` <- relevel(model$`W1hea2MP(Whether MP has longstanding illness or disability)`, ref = "2")
  model$`W1usevcHH(Whether anyone uses a motor vehicle)` <- relevel(model$`W1usevcHH(Whether anyone uses a motor vehicle)`, ref = "2")
  model$`W1wrkfulldad(Whether father works full-time)` <- relevel(model$`W1wrkfulldad(Whether father works full-time)`, ref = "3")
  model$`W1wrkfullmum(Whether mother works full-time)` <- relevel(model$`W1wrkfullmum(Whether mother works full-time)`, ref = "3")
  model$`W1empsdad(Employment status of father)` <- relevel(model$`W1empsdad(Employment status of father)`, ref = "Not Working")
  model$`W1nssecfam(Family’s NS-SEC class)` <- relevel(model$`W1nssecfam(Family’s NS-SEC class)`, ref = "Routine occupations/unemployed")
  model$`W1heposs9YP(Likelihood of YP going to university)` <- relevel(model$`W1heposs9YP(Likelihood of YP going to university)`, ref = "4")
  model$`W1truantYP(Whether YP played truant in last 12 months)` <- relevel(model$`W1truantYP(Whether YP played truant in last 12 months)`, ref = "2")
  model$`W1alceverYP(Whether YP ever had alcohol)` <- relevel(model$`W1alceverYP(Whether YP ever had alcohol)`, ref = "2")
  model$`W1bulrc(Whether YP bullied in last 12 months)` <- relevel(model$`W1bulrc(Whether YP bullied in last 12 months)`, ref = "2")
  model$`W1disabYP(Whether YP has any disability/long term illness or health problem)` <- relevel(model$`W1disabYP(Whether YP has any disability/long term illness or health problem)`, ref = "3")
  model$`W2disc1YP(Whether YP thinks they have ever been treated unfairly by teachers because of skin colour or ethnic origin)` <- relevel(model$`W2disc1YP(Whether YP thinks they have ever been treated unfairly by teachers because of skin colour or ethnic origin)`, ref = "2")
  model$`W4CannTryYP(Whether YP ever tried cannabis)` <- relevel(model$`W4CannTryYP(Whether YP ever tried cannabis)`, ref = "2")
  model$`W4NamesYP(Whether YP has been called names, sworn at or insulted in last 12 months)` <- relevel(model$`W4NamesYP(Whether YP has been called names, sworn at or insulted in last 12 months)`, ref = "2")
  model$`W4RacismYP(Whether YP has been threatened/insulted in the last 12 months due to skin colour/ethnicity)` <- relevel(model$`W4RacismYP(Whether YP has been threatened/insulted in the last 12 months due to skin colour/ethnicity)`, ref = "2")
  model$`W4empsYP(Employment status of young person)` <- relevel(model$`W4empsYP(Employment status of young person)`, ref = "Not Working Or In Education")
  model$`W5JobYP(YP: Whether currently doing paid work)` <- relevel(model$`W5JobYP(YP: Whether currently doing paid work)`, ref = "2")
  model$`W5EducYP(YP: Whether currently going to school or college)` <- relevel(model$`W5EducYP(YP: Whether currently going to school or college)`, ref = "2")
  model$`W5Apprent1YP(YP: Whether currently doing an apprenticeship)` <- relevel(model$`W5Apprent1YP(YP: Whether currently doing an apprenticeship)`, ref = "2")
  model$`W6JobYP(YP: Whether currently doing paid work)` <- relevel(model$`W6JobYP(YP: Whether currently doing paid work)`, ref = "2")
  model$`W6UnivYP(YP: Whether currently at university)` <- relevel(model$`W6UnivYP(YP: Whether currently at university)`, ref = "2")
  model$`W6EducYP(YP: Whether currently going to school or college)` <- relevel(model$`W6EducYP(YP: Whether currently going to school or college)`, ref = "2")
  model$`W6acqno(Highest academic qualification studied at Wave 6)` <- relevel(model$`W6acqno(Highest academic qualification studied at Wave 6)`, ref = "No Academic Study Aim")
  model$`W6als(Number of A/A2/AS levels being studied at Wave 6)` <- relevel(model$`W6als(Number of A/A2/AS levels being studied at Wave 6)`, ref = "4")
  model$`W6OwnchiDV(Whether Respondents have own Child/Children)` <- relevel(model$`W6OwnchiDV(Whether Respondents have own Child/Children)`, ref = "2")
  model$`W8DACTIVITYC(Current activity of CM)` <- relevel(model$`W8DACTIVITYC(Current activity of CM)`, ref = "Unemployed")
  model$`W8DWRK(Whether CM currently employed)` <- relevel(model$`W8DWRK(Whether CM currently employed)`, ref = "2")
  model$`W8DACTIVITY(Current activity)` <- relevel(model$`W8DACTIVITY(Current activity)`, ref = "Unemployed")
  model$`W8QMAFI(How managing financially these days)` <- relevel(model$`W8QMAFI(How managing financially these days)`, ref = "5")
  
  # Assign modified model back
  if (i == 1) {
    model_with_missing <- model
  } else {
    main_model <- model
  }
}
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
#we will do the main model first, then the bonus model with na values
#starting with all predictors, and removing the insignificant predictors until all predictors are significant
# Rename the columns without parenthesis for both models as lm doesn't like col names with parentheses
clean_main_model<-main_model
# Remove anything after the first parenthesis in column names of clean_main_model
colnames(clean_main_model) <- gsub("\\(.*\\)", "", colnames(clean_main_model))
#same thing for the bonus model
clean_missing_model<-model_with_missing
colnames(clean_missing_model) <- gsub("\\(.*\\)", "", colnames(clean_missing_model))


# Run the regression using backwards elimination
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW, data = clean_main_model)
Anova(W8DINCW.all.lm)


# we remove empsdad first, as it is insignificant and causes singularities
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad, data = clean_main_model)
Anova(W8DINCW.all.lm)

#next we see univ YP seems to be causing problems, we will check if it is correlated to educ YP
library(vcd)

# Calculate Cramér's V for the 2 columns
factor1 <- clean_main_model$W6EducYP
factor2 <- clean_main_model$W6UnivYP

# Create a contingency table
contingency_table <- table(factor1, factor2)
cramers_v <- assocstats(contingency_table)
print(cramers_v$cramer)
# they have a perfect correlation, we will remove W6 Univ YP

# we remove the W6 Univ YP column
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W6als (counter-intuitive), I thought number of A levels studied would significantly impact future income
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1ch3_11HH
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W6OwnchiDV
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W4empsYP
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1alceverYP
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W4RacismYP
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1famtyp2
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1depkids
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids, data = clean_main_model)
Anova(W8DINCW.all.lm)


# we remove the least significant predictor W1truantYP
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1bulrc
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1wrkfulldad
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W8DMARSTAT
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W4Childck1YP
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W2depressYP
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1InCarHH
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W8QMAFI, this is another one that seems counter intuitive to me
#surely you'd have a significantly higher income if you are managing well financially
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1ch12_15HH
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1ch16_17HH
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W6JobYP
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W6acqno, another one that seems counter intuitive to me, I thought study level and income would have a significant link
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1NoldBroHS
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1ch0_2HH
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor standardised_W1yschat1
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W4NamesYP
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W6gcse
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W8DGHQSC
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W8DACTIVITYC, another counter intuitive one for me, surely their activity should impact income
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the least significant predictor W1hous12HH
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we now have 2 borderline significant predictors, we will remove both of them, 
#as the project advice document tells us that if we have many predictors in our model, 
#it is ok to remove borderline significant predictors
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW- W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_main_model)
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
#however, the model itself has good diagnostics, the qq plot is very good and the histogram looks like a normal distribution
#a log transformation of the target would help with the resid vs fitted plots, however it would skew the distribution of residuals
#as seen during the transformation phase, so we will stick with this model and the original target
#we will now do the model for the bonus DF with NA values in "BONUS MLR"
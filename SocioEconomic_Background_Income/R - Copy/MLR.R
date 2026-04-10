# REGRESSION
#we will run the regression starting with all predictors, using a backwards elimination strategy
library(arm)
library(car)

summary(main_model)
# Re level the categorical columns of both models to have the most common baseline level

model <- main_model # 
  
model$`W1hea2MP(Whether MP has longstanding illness or disability)` <- relevel(model$`W1hea2MP(Whether MP has longstanding illness or disability)`, ref = "2")
model$`W1empsdad(Employment status of father)` <- relevel(model$`W1empsdad(Employment status of father)`, ref = "Working/Education")
model$`W1marstatmum(Marital status of mother)` <- relevel(model$`W1marstatmum(Marital status of mother)`, ref = "Living With Partner")
model$`W1hwndayYP(Number of evenings of HWK per week)` <- relevel(model$`W1hwndayYP(Number of evenings of HWK per week)`, ref="3")
model$`W1truantYP(Whether YP played truant in last 12 months)` <- relevel(model$`W1truantYP(Whether YP played truant in last 12 months)`, ref = "2")
model$`W1alceverYP(Whether YP ever had alcohol)` <- relevel(model$`W1alceverYP(Whether YP ever had alcohol)`, ref = "2")
model$`W1bulrc(Whether YP bullied in last 12 months)` <- relevel(model$`W1bulrc(Whether YP bullied in last 12 months)`, ref = "2")
model$`W1disabYP(Whether YP has any disability/long term illness or health problem)` <- relevel(model$`W1disabYP(Whether YP has any disability/long term illness or health problem)`, ref = "3")
model$`W2disc1YP(Whether YP thinks they have ever been treated unfairly by teachers because of skin colour or ethnic origin)` <- relevel(model$`W2disc1YP(Whether YP thinks they have ever been treated unfairly by teachers because of skin colour or ethnic origin)`, ref = "2")
model$`W4CannTryYP(Whether YP ever tried cannabis)` <- relevel(model$`W4CannTryYP(Whether YP ever tried cannabis)`, ref = "2")
model$`W4NamesYP(Whether YP has been called names, sworn at or insulted in last 12 months)` <- relevel(model$`W4NamesYP(Whether YP has been called names, sworn at or insulted in last 12 months)`, ref = "2")
model$`W4RacismYP(Whether YP has been threatened/insulted in the last 12 months due to skin colour/ethnicity)` <- relevel(model$`W4RacismYP(Whether YP has been threatened/insulted in the last 12 months due to skin colour/ethnicity)`, ref = "2")
model$`W4empsYP(Employment status of young person)` <- relevel(model$`W4empsYP(Employment status of young person)`, ref = "In Education")
model$`W5Apprent1YP(YP: Whether currently doing an apprenticeship)` <- relevel(model$`W5Apprent1YP(YP: Whether currently doing an apprenticeship)`, ref = "2")
model$`W6EducYP(YP: Whether currently going to school or college)` <- relevel(model$`W6EducYP(YP: Whether currently going to school or college)`, ref = "2")
model$`W6acqno(Highest academic qualification studied at Wave 6)` <- relevel(model$`W6acqno(Highest academic qualification studied at Wave 6)`, ref = "No Academic Study Aim")
model$`W6OwnchiDV(Whether Respondents have own Child/Children)` <- relevel(model$`W6OwnchiDV(Whether Respondents have own Child/Children)`, ref = "2")
model$`W8TENURE(Tenure)` <- relevel(model$`W8TENURE(Tenure)`, ref = "Rented")
model$`W8DACTIVITY(Current activity)` <- relevel(model$`W8DACTIVITY(Current activity)`, ref = "Full Time Employee")
model$`W8QMAFI(How managing financially these days)` <- relevel(model$`W8QMAFI(How managing financially these days)`, ref = "2")

main_model <- model

#checking changes
categorical_cols <- sapply(main_model, is.factor)

# Loop through the column names that are factors
for (col_name in names(categorical_cols)[categorical_cols]) {
  
  # Get all levels of the factor column from main model
  levels_list <- levels(main_model[[col_name]])
  
  cat("Column:", col_name, "\n")
  cat("Baseline level:", levels_list[1], "\n")  # Print reference level
  cat("levels:\n")
  print(summary(main_model[[col_name]]))
}

#we will do the main model first, then once we have our final model, we will test the model without the continuous columns that have less than 30%
#starting with all predictors, and removing the insignificant predictors until all predictors are significant
# Rename the columns without parenthesis for both models as lm doesn't like col names with parentheses
clean_main_model<-main_model
# Remove anything after the first parenthesis in column names of clean_main_model
colnames(clean_main_model) <- gsub("\\(.*\\)", "", colnames(clean_main_model))


# Run the regression using backwards elimination
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W6OwnchiDV column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W2ghq12scr_binary column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W4empsYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W4RacismYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1InCarHH column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1empsdad column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad, data = clean_main_model)
Anova(W8DINCW.all.lm)
 
# we remove the W1ch16_17HH column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1bulrc column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W4NamesYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the log_of_W8QDEB2 column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W5EducYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1ch12_15HH column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the standardised_W1yschat1 column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1truantYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1empsmum column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W8TENURE column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W8DMARSTAT column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W5JobYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W6acqno column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W4AlcFreqYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1alceverYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1condur5MP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1ch0_2HH column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W8DGHQSC_binary column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the IndSchool column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W2depressYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1depkids column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W8QMAFI column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the standardised_W1hiqualparents_score column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1hous12HH column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1usevcHH column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W6gcse column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the standardised_W4schatYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W4Childck1YP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP, data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1ch3_11HH column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                     , data = clean_main_model)
Anova(W8DINCW.all.lm)

# we remove the W1NoldBroHS column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                     - W1NoldBroHS, data = clean_main_model)
Anova(W8DINCW.all.lm)

# Moving forward are borderline significant predictors (chosen to remove since we have a lot of predictors in the current model {29})

# we remove the W1hwndayYP column
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                     - W1NoldBroHS - W1hwndayYP, data = clean_main_model)
Anova(W8DINCW.all.lm)

#display the coefficients of the final model
display(W8DINCW.all.lm)
#display the final model summary
summary(W8DINCW.all.lm)
#check that all vif values are less than 5 (multicollinearity checks)
vif(W8DINCW.all.lm) #we see that all adjusted VIF are less than 5 so there are no multicollinearity issues

#checking the residuals of our final model
par(mfrow=c(2,2))
plot(W8DINCW.all.lm,which=c(1,2))
hist(W8DINCW.all.lm$residuals,main="Histogramofresiduals",
     font.main=1,xlab="Residuals")

#we see that the residuals vs fitted plot displays a slight curvature (a mild violation of linearity) and maybe slight heteroscedasticity, but not obvious. 
#however, the model itself has okay diagnostics, the both tails of qq plot drops below the line which suggests non-normality in both tails
#the histogram is decently symmetric, and looks like a normal distribution, with a mild skew to the right
#note that 2 of the 3 continuous columns with more than 30% missing values appear in the final model
#so for our bonus model, we will be replacing all "missing" values with NA.
#we will now explore some potential interactions that make sense for the model
#for all interactions we are looking to add, we need their sample size to be big enough, 
#and then for them to also have a significant impact on the target in the model
summary(W8DINCW.all.lm)
#we will explore interactions for our best model, reminder it is:
W8DINCW.all.lm <- lm(log(W8DINCW) ~ . - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                     - W1NoldBroHS - W1hwndayYP, data = clean_main_model)

# we will explore interaction related to parent gross income and working status
W8DINCW.interaction.lm <- lm(log(W8DINCW) ~ . + log_of_W1GrssyrMP*W1wrk1aMP - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                     - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                     - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                     - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                     - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                     - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                     - W1NoldBroHS - W1hwndayYP, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
# This is a significant interaction, taking a look at its effects...
display(W8DINCW.interaction.lm)
# When the main parent is working, the MP's salary has a slight negative relationship with the outcome.
# If the main parent is not working, income tends to be much lower overall.
# However, when the main parent is not working, the MP's salary has a positive effect.

# Next, we will explore interaction related to the state of the YP at Wave 6
W8DINCW.interaction.lm <- lm(log(W8DINCW) ~ . + W6EducYP*W6JobYP + log_of_W1GrssyrMP*W1wrk1aMP - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                             - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                             - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                             - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                             - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                             - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                             - W1NoldBroHS - W1hwndayYP, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
# This is a borderline significant interaction, taking a look at its effects...
display(W8DINCW.interaction.lm)
# When the YP is not doing paid work, it has a slight negative impact on the outcome
# When the YP answers Yes for W6EducYP, the magnitude of the negative impact on the outcome is increased when YP is not doing paid work
# When W6EducYP is missing, the additional interaction with not doing paid work approaches zero, suggesting mitigating effect.
# However, to keep consistent with our previous decisions, we will not be keeping borderline significant interactions

# we will explore interaction related to main parent's health and family status
W8DINCW.interaction.lm <- lm(log(W8DINCW) ~ . + W1nssecfam*W1hea2MP + log_of_W1GrssyrMP*W1wrk1aMP - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                             - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                             - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                             - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                             - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                             - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                             - W1NoldBroHS - W1hwndayYP, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
# This is not a significant interaction

# we will explore interaction related to the YPs Sex and Activity in Wave 8
W8DINCW.interaction.lm <- lm(log(W8DINCW) ~ . + W8CMSEX*W8DACTIVITY + log_of_W1GrssyrMP*W1wrk1aMP - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                             - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                             - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                             - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                             - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                             - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                             - W1NoldBroHS - W1hwndayYP, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
# This is not a significant interaction

#we only added one interaction to the model, despite having attempted several plausible interactions
W8DINCW.interaction.lm <- lm(log(W8DINCW) ~ . + log_of_W1GrssyrMP*W1wrk1aMP - W6OwnchiDV - W2ghq12scr_binary - W4empsYP - W4RacismYP - W1InCarHH
                             - W1empsdad - W1ch16_17HH - W1bulrc - W4NamesYP - log_of_W8QDEB2 - W5EducYP - W1ch12_15HH
                             - standardised_W1yschat1 - W1truantYP - W1empsmum - W8TENURE - W8DMARSTAT - W5JobYP
                             - W6acqno - W4AlcFreqYP - W1alceverYP - W1condur5MP - W1ch0_2HH - W8DGHQSC_binary
                             - IndSchool - W2depressYP - W1depkids - W8QMAFI - standardised_W1hiqualparents_score
                             - W1hous12HH - W1usevcHH - W6gcse - standardised_W4schatYP - W4Childck1YP - W1ch3_11HH
                             - W1NoldBroHS - W1hwndayYP, data = clean_main_model)

#display the coefficients of the model with interactions
display(W8DINCW.interaction.lm)
#display the interaction model summary
summary(W8DINCW.interaction.lm)

#we will use this interactions model for the final stage of cross validation
#note the model's diagnostics have improved marginally.
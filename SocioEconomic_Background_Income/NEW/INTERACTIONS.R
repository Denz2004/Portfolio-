#we will now explore some potential interactions that make sense for the model
#for all interactions we are looking to add, we need their sample size to be big enough, 
#and then for them to also have a significant impact on the target in the model
summary(W8DINCW.all.lm)
#we will explore interactions for our best model, reminder it is:
W8DINCW.all.lm <- lm(log_of_W8DINCW ~ . - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                     - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                     - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse- W4Childck1YP - log_of_W1GrssyrMP - log_of_W1GrssyrHH
                     - W1hous12HH - W8DACTIVITY, data = clean_main_model)

# we will explore a health related interaction during childhood first
W8DINCW.interaction.lm <- lm(log_of_W8DINCW ~ . + W1disabYP*W1hea2MP
                             - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                             - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                             - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                             - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                             - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse- W4Childck1YP - log_of_W1GrssyrMP - log_of_W1GrssyrHH
                             - W1hous12HH - W8DACTIVITY, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
#we wont keep this interaction as it is isn't significant.

#next interaction we are trying is between ethnicity and whether the YP thinks they have been treated unfairly due to their ethnicity
sum(clean_main_model$W1ethgrpYP == "Other" & clean_main_model$W2disc1YP == 1) #the sample size is big enough
W8DINCW.interaction.lm <- lm(log_of_W8DINCW ~ . + W1ethgrpYP*W2disc1YP
                             - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                             - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                             - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                             - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                             - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse- W4Childck1YP - log_of_W1GrssyrMP - log_of_W1GrssyrHH
                             - W1hous12HH - W8DACTIVITY, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
#again, the interaction isn't significant, so we won't keep it

#next interaction we are trying is a "drug abuse" interaction between cannabis use and nssec
W8DINCW.interaction.lm <- lm(log_of_W8DINCW ~ . + W4CannTryYP*W1nssecfam
                             - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                             - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                             - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                             - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                             - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse- W4Childck1YP - log_of_W1GrssyrMP - log_of_W1GrssyrHH
                             - W1hous12HH - W8DACTIVITY, data = clean_main_model)
Anova(W8DINCW.interaction.lm)
#another insignificant interaction that we won't keep

sum(clean_main_model$W5Apprent1YP == 1 & clean_main_model$W2disc1YP == 1) #next interaction we are checking is if the YP is doing an apprenticeship and felt unfairly treated at school
#the sample size isn't sufficient for this interaction so we won't proceed

#the next interaction we try is between attitude to debt, and whether the YP is in education at 17
W8DINCW.interaction.lm <- lm(log_of_W8DINCW ~ . + standardised_W6DebtattYP * W6EducYP
                             - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                             - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                             - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                             - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                             - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse- W4Childck1YP - log_of_W1GrssyrMP - log_of_W1GrssyrHH
                             - W1hous12HH - W8DACTIVITY, data = clean_main_model)
Anova(W8DINCW.interaction.lm) #another insignificant interaction that we won't keep

#our final interaction will be the impact of debt and gender: the gender of the YP and their attitude towards debt
W8DINCW.interaction.lm <- lm(log_of_W8DINCW ~ . + W8CMSEX * standardised_W6DebtattYP
                             - log_of_W8DINCW- W1empsdad   - W1ch3_11HH - W6OwnchiDV - W5JobYP - W8TENURE - log_of_W8QDEB2
                             - W4empsYP - W1alceverYP - W4RacismYP   - W1truantYP - W1empsmum - W5EducYP - W1condur5MP - standardised_W1hiqualparents_score
                             - W1bulrc - W8DMARSTAT  - W2depressYP - W1InCarHH - W4AlcFreqYP - IndSchool - W1usevcHH - standardised_W4schatYP
                             - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS - W1hwndayYP - W8DGHQSC_binary - W2ghq12scr_binary
                             - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse- W4Childck1YP - log_of_W1GrssyrMP - log_of_W1GrssyrHH
                             - W1hous12HH - W8DACTIVITY, data = clean_main_model)
Anova(W8DINCW.interaction.lm) #another insignificant interaction

#we attempted an interaction for each age level in the model but found no significant interactions.
#We will proceed with the initial model and validate both models.
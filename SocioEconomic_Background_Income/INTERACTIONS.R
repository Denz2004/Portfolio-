#we will now explore some potential interactions that make sense for the model
#for all interactions we are looking to add, we need their sample size to be big enough, 
#and then for them to also have a significant impact on the target in the model

#we will explore interactions for our best model, reminder it is:
W8DINCW.all.lm <- lm(W8DINCW ~ . - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH, data = clean_main_model)

# we will explore a technology access during childhood first, whether the household had access to a computer and vehicule
sum(clean_main_model$W1condur5MP == 2 & clean_main_model$W1usevcHH == 2) #the sample size is big enough

W8DINCW.interaction.lm <- lm(W8DINCW ~ . 
                     + W1condur5MP * W1usevcHH
                     - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH,
                     data = clean_main_model)
Anova(W8DINCW.interaction.lm)
#we wont keep this interaction as it is isn't significant.

#next interaction we are trying is between mental health score and whether the YP has a disability
W8DINCW.interaction.lm <- lm(W8DINCW ~ . 
                     + W2ghq12scr * W1disabYP
                     - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH,
                     data = clean_main_model)
Anova(W8DINCW.interaction.lm)
summary(W8DINCW.interaction.lm) #the interaction is borderline significant, however, it is quite hard to interpret
#we can't really easily understand the link between a continuous mental health score, and 3 disability levels,
#and as is it only borderline significant, we won't keep it in the model.

#next interaction we are trying is a "drug abuse" interaction between alcohol and cannabis frequency and use
W8DINCW.interaction.lm <- lm(W8DINCW ~ . 
                     + W4AlcFreqYP * W4CannTryYP
                     - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH,
                     data = clean_main_model)
Anova(W8DINCW.interaction.lm)
#the interaction between alcohol frequency and cannabis use isn't even significant, we won't keep it in our model
#it was an interesting "drug abuse" interaction to try and it may be insignificant due to the high amount of levels in alcohol frequency

sum(clean_main_model$W5JobYP == 1 & clean_main_model$W5EducYP == 1) #next interaction we are checking is if the YP is in education and has a job at 16
#the sample size is sufficient for this interaction
W8DINCW.interaction.lm <- lm(W8DINCW ~ . 
                     + W5JobYP * W5EducYP
                     - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH,
                     data = clean_main_model)
Anova(W8DINCW.interaction.lm)
#another insignificant interaction that we won't keep

#the next interaction we try is between attitude to debt, and whether the YP is in education at 17
W8DINCW.interaction.lm <- lm(W8DINCW ~ . 
                     + standardised_W6DebtattYP * W6EducYP
                     - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH,
                     data = clean_main_model)
Anova(W8DINCW.interaction.lm)
summary(W8DINCW.interaction.lm) #here we have our first significant interaction, and it goes in the direction we expect it to, so we will keep it
#for me, if you are in education, a more positive attitude towards debt will increase your future weekly income, 
#as you will be willing to take more risks with money(go into debt currently to pursue education etc) to increase your future earnings

sum(clean_main_model$W1wrk1aMP == "Working" & clean_main_model$W8DWRK == 1) #our final interaction will be the impact of work: the work status of the YP at 25, and whether their parent worked when they were 14
#the sample size is sufficient

W8DINCW.interaction.lm <- lm(W8DINCW ~ . 
                      + standardised_W6DebtattYP * W6EducYP
                     - W8DINCW - W1empsdad - W6UnivYP - W6als - W1ch3_11HH - W6OwnchiDV
                     - W4empsYP - W1alceverYP - W4RacismYP - W1famtyp2 - W1depkids - W1truantYP
                     - W1bulrc - W1wrkfulldad - W8DMARSTAT - W4Childck1YP - W2depressYP - W1InCarHH
                     - W8QMAFI - W1ch12_15HH - W1ch16_17HH - W6JobYP - W6acqno - W1NoldBroHS
                     - W1ch0_2HH - standardised_W1yschat1 - W4NamesYP - W6gcse - W8DGHQSC
                     - W8DACTIVITYC - W1hous12HH - IndSchool - W1usevcHH,
                     data = clean_main_model)
Anova(W8DINCW.interaction.lm)
summary(W8DINCW.interaction.lm)#another insignificant interaction, we won't keep it

#we only added one interaction to the model, despite having attempted an interaction for each age level
#we will use this interactions model for the final stage of cross validation
#although interactions can lead to your model over fitting, we made sure to only add the only significant interaction, and drop the non-significant ones
#note the model's diagnostics have improved marginally.
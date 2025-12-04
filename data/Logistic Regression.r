##Logistic Regression

#Logistic No Commorbidity
no_comorb <- subset(covid19A, Number_of_Comorbidities == 0)

#Logistic With Commorbidities
with_comorb <- subset(covid19A, Number_of_Comorbidities > 0)

#Logistic Regression - No Commorbidity
logit_no <- glm(COVID_Death ~ 1,        
                data = no_comorb,
                family = binomial)

summary(logit_no)

#Logistic Regression - With Commorbidities
logit_with <- glm(COVID_Death ~ Diabetes + Hypertension + Obesity + Smoking,
                  data = with_comorb,
                  family = binomial)

summary(logit_with)

#Logistic Regression - No. of Commorbidities
logit_noc <- glm(COVID_Death ~ Number_of_Comorbidities,
                 data = with_comorb,
                 family = binomial)

summary(logit_noc)

#Interpretation of Logistic Regression
exp(coef(logit_with))



#Moderation Full
logit_moderation_full <- glm(COVID_Death ~ 
                               Diabetes + Hypertension + Obesity + Smoking +
                               Number_of_Comorbidities +
                               (Number_of_Comorbidities * Smoking),
                             data = covid19A,
                             family = binomial)

summary(logit_moderation_full)

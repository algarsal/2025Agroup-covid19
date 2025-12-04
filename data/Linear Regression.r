#Analyses
#Linear Regression

#No Commorbidity - Age + Gender
lm_no_comorb <- lm(COVID_Death ~ Age + Gender, data = table_no_commorb)
summary(lm_no_comorb)

#With Commorbidity - Age + Gender
lm_with_comorb <- lm(COVID_Death ~ Age + Gender + Number_of_Comorbidities, data = table_with_comorb)
summary(lm_with_comorb)

#No Commorbidity - 
lm_no_comorb <- lm(COVID_Death ~ Diabetes + Hypertension + Obesity + Smoking,data = table_no_commorb)
summary(lm_no_comorb)

#With Commorbidity -
lm_with_comorb <- lm(COVID_Death ~ Diabetes + Hypertension + Obesity + Smoking + Number_of_Comorbidities,data = table_with_comorb)

summary(lm_with_comorb)

#------------------------------BETTER------------------------------------------------------

#Table - Diabetes
diabetes_table <- table(covid19A$COVID_Death, covid19A$Diabetes)
diabetes_table <- as.data.frame.matrix(diabetes_table)
diabetes_table

#Chi-square of Diabetes
chisq.test(diabetes_table)
#p-value = p-value < 2.2e-16

#------------------------------BETTER------------------------------------------------------

#Table - Hypertension
hypertension_table <- table(covid19A$COVID_Death, covid19A$Hypertension)
hypertension_table <- as.data.frame.matrix(hypertension_table)
hypertension_table

#Chi-square of Hypertension
chisq.test(hypertension_table)
#p-value = p-value < 2.2e-16

#------------------------------BETTER------------------------------------------------------

#Table - Obesity
obesity_table <- table(covid19A$COVID_Death, covid19A$Obesity)
obesity_table <- as.data.frame.matrix(obesity_table)
obesity_table

#Chi-square of Obesity
chisq.test(obesity_table)
#p-value = p-value < 2.2e-16

#------------------------------BETTER------------------------------------------------------

#Table - Smoking
smoking_table <- table(covid19A$COVID_Death, covid19A$Smoking)
smoking_table <- as.data.frame.matrix(obesity_table)
smoking_table

#Chi-square of Obesity
chisq.test(smoking_table)
#p-value = p-value < 2.2e-16

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

#Individual Logistic Regression 

# Load needed package
install.packages("broom")

# Logistic Regression 1: Diabetes + Hypertension
library(broom)
model_Diabetes_Hypertension <- glm(
  COVID_Death ~ Diabetes + Hypertension,
  data = covid19A,
  family = binomial()
)
summary(model_Diabetes_Hypertension)
tidy(model_Diabetes_Hypertension, exponentiate = TRUE, conf.int = TRUE)
broom::tidy(model_Diabetes_Smoking, exponentiate = TRUE, conf.int = TRUE)

# Logistic Regression 2: Diabetes + Smoking
model_Diabetes_Smoking <- glm(
  COVID_Death ~ Diabetes + Smoking,
  data = covid19A,
  family = binomial()
)
summary(model_Diabetes_Smoking)
tidy(model_Diabetes_Smoking, exponentiate = TRUE, conf.int = TRUE)

# Logistic Regression 3: Diabetes + Obesity
model_Diabetes_Obesity <- glm(
  COVID_Death ~ Diabetes + Obesity,
  data = covid19A,
  family = binomial()
)
summary(model_Diabetes_Obesity)
tidy(model_Diabetes_Obesity, exponentiate = TRUE, conf.int = TRUE)


# Logistic Regression 4: Hypertension + Smoking
model_Hypertension_Smoking <- glm(
  COVID_Death ~ Hypertension + Smoking,
  data = covid19A,
  family = binomial()
)
summary(model_Hypertension_Smoking)
tidy(model_Hypertension_Smoking, exponentiate = TRUE, conf.int = TRUE)


# Logistic Regression 5: Obesity + Smoking
model_Obesity_Smoking <- glm(
  COVID_Death ~ Obesity + Smoking,
  data = covid19A,
  family = binomial()
)
summary(model_Obesity_Smoking)
tidy(model_Obesity_Smoking, exponentiate = TRUE, conf.int = TRUE)


# Logistic Regression 6: Hypertension + Obesity
model_Hypertension_Obesity <- glm(
  COVID_Death ~ Hypertension + Obesity,
  data = covid19A,
  family = binomial()
)
summary(model_Hypertension_Obesity)
tidy(model_Hypertension_Obesity, exponentiate = TRUE, conf.int = TRUE)

#Logistic Regression 7: Diabetes
model_Diabetes <- glm(
  COVID_Death ~ Diabetes,
  data = covid19A,
  family = binomial()
)
summary(model_Diabetes)
tidy(model_Diabetes, exponentiate = TRUE, conf.int = TRUE)

#Logistic Regression 8: Smoking
model_Smoking <- glm(
  COVID_Death ~ Smoking,
  data = covid19A,
  family = binomial()
)
summary(model_Smoking)
tidy(model_Smoking, exponentiate = TRUE, conf.int = TRUE)

#Logistic Regression 9: Obesity
model_Obesity <- glm(
  COVID_Death ~ Obesity,
  data = covid19A,
  family = binomial()
)
summary(model_Obesity)
tidy(model_Obesity, exponentiate = TRUE, conf.int = TRUE)

#Logistic Regression 10: Hypertension
model_Hypertension <- glm(
  COVID_Death ~ Hypertension,
  data = covid19A,
  family = binomial()
)
summary(model_Hypertension)
tidy(model_Hypertension, exponentiate = TRUE, conf.int = TRUE)

#Single combined output table
library(dplyr)

models <- list(
  Diabetes_Hypertension = model_Diabetes_Hypertension,
  Diabetes_Smoking = model_Diabetes_Smoking,
  Diabetes_Obesity = model_Diabetes_Obesity,
  Hypertension_Smoking = model_Hypertension_Smoking,
  Obesity_Smoking = model_Obesity_Smoking,
  Hypertension_Obesity = model_Hypertension_Obesity,
  Hypertension = model_Hypertension,
  Obesity = model_Obesity,
  Smoking = model_Smoking,
  Diabetes = model_Diabetes
)

results <- lapply(names(models), function(name) {
  tidy(models[[name]], exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(Model = name)
})

combined_results <- bind_rows(results)
combined_results


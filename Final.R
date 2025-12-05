## ----------------Statistical Analysis-----------------------------
## ---------------(Code from the start)------------------------------

##----------------- Load the data base-------------------------------

dir("data")
data <- "data/covid_mexico_comorbidity_dataset_100k.csv"
covid19A <- read.table(data, header = T, sep = ",")
str(covid19A)
summary(covid19A)

library(dplyr) ###

### ------------------Chi square Test--------------------------------

# The Chi square is test is gonna allow us to find association between
# DEATHS & each COMORBIDITY 


comorbidities <- c("Diabetes", "Hypertension", "Obesity", "Smoking")

for (c in comorbidities) {
  cat("\n==========================\n")
  cat("Chi-square test for", c, "\n")
  tab <- table(covid19A$COVID_Death, covid19A[[c]])
  print(tab)
  print(chisq.test(tab))
}

      ## Made a loop for all the comorbidities instead of typing
      ## by hand each contingency table & Chi test

# results table

chi_results <- data.frame(
  Comorbidity = character(),
  Chi_square = numeric(),
  df = numeric(),
  P_value = numeric()
)

for (c in comorbidities) {
  tab <- table(covid19A$COVID_Death, covid19A[[c]])
  chi <- chisq.test(tab)
  
  chi_results <- rbind(
    chi_results,
    data.frame(
      Comorbidity = c,
      Chi_square = chi$statistic,
      df = chi$parameter,
      P_value = chi$p.value
    )
  )
}

chi_results

#  All tests resulted in highly significant p-values, 
#  indicating that the distribution of deaths differs significantly 
#  between patients with and without the comorbidity (CM).

##  This supports the conclusion that comorbidities are strongly 
##  associated with increased mortality.

#  Now that we found statistically significant association between each 
#  and every comorbidity, to investigate about the probability of death 
#  based on the presence or absence of each CM, combinations &
#  interactions...

#  The logistic model is ideal for this analysis because our outcome
#  variables are binary 

###-------------LOGISTIC REGRESION MODEL (LRM)-------------------------

           # We start doing individual LRM for each CM

## load library 

library(broom)

# Indiv. LGM Diabetes

model_Diabetes <- glm(
  COVID_Death ~ Diabetes,
  data = covid19A,
  family = binomial()
)
summary(model_Diabetes)
tidy(model_Diabetes, exponentiate = TRUE, conf.int = TRUE)
 
# Indiv. LGM Smoking

model_Smoking <- glm(
  COVID_Death ~ Smoking,
  data = covid19A,
  family = binomial()
)
summary(model_Smoking)
tidy(model_Smoking, exponentiate = TRUE, conf.int = TRUE)

# Indiv. LGM  Obesity

model_Obesity <- glm(
  COVID_Death ~ Obesity,
  data = covid19A,
  family = binomial()
)
summary(model_Obesity)
tidy(model_Obesity, exponentiate = TRUE, conf.int = TRUE)

# Indiv. LGM  Hypertension

model_Hypertension <- glm(
  COVID_Death ~ Hypertension,
  data = covid19A,
  family = binomial()
)
summary(model_Hypertension)
tidy(model_Hypertension, exponentiate = TRUE, conf.int = TRUE)




# Logistic Regression 1: Diabetes + Hypertension
model_Diabetes_Hypertension <- glm(
  COVID_Death ~ Diabetes + Hypertension,
  data = covid19A,
  family = binomial()
)
summary(model_Diabetes_Hypertension)
tidy(model_Diabetes_Hypertension, exponentiate = TRUE, conf.int = TRUE)

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


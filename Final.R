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

# The Chi square test is gonna allow us to find association between
# DEATHS & each COMORBIDITY 

      ## Made a loop for all the comorbidities instead of typing
      ## by hand each contingency table & Chi test

comorbidities <- c("Diabetes", "Hypertension", "Obesity", "Smoking")

for (c in comorbidities) {
  cat("\n==========================\n")
  cat("Chi-square test for", c, "\n")
  tab <- table(covid19A$COVID_Death, covid19A[[c]])
  print(tab)
  print(chisq.test(tab))
}


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

# Indiv. LRM Diabetes

model_Diabetes <- glm(
  COVID_Death ~ Diabetes,
  data = covid19A,
  family = binomial()
)
summary(model_Diabetes)
tidy(model_Diabetes, exponentiate = TRUE, conf.int = TRUE)
 
# Indiv. LRM Smoking

model_Smoking <- glm(
  COVID_Death ~ Smoking,
  data = covid19A,
  family = binomial()
)
summary(model_Smoking)
tidy(model_Smoking, exponentiate = TRUE, conf.int = TRUE)

# Indiv. LRM  Obesity

model_Obesity <- glm(
  COVID_Death ~ Obesity,
  data = covid19A,
  family = binomial()
)
summary(model_Obesity)
tidy(model_Obesity, exponentiate = TRUE, conf.int = TRUE)

# Indiv. LRM  Hypertension

model_Hypertension <- glm(
  COVID_Death ~ Hypertension,
  data = covid19A,
  family = binomial()
)
summary(model_Hypertension)
tidy(model_Hypertension, exponentiate = TRUE, conf.int = TRUE)


      ## However, running individual LRM is measuring the association of each
      ## individual CM, while for isolating effect we might need to incorporate 
      ## all the CM analyzed in this study as a Multivariable Model (MVM)

                         ##Multivariable Model (MVM)

MVM <- glm(COVID_Death ~ Diabetes + Hypertension + Obesity + Smoking,
                  data = covid19A,
                  family = binomial)

summary(MVM)

       # This model is considering the whole dataset. Answering the question:
       # Across the entire population, does each comorbidity increase 
       # the odds of dying?
    
     # Another version for the MVM would be only considering only the people
     # with at least 1 comorbility (a subset of the data (Number of CM >=1))
     # that would attenuate the effect of the CM. Answering the question

     # Among people who already have comorbidities, which specific diseases
     # increase the odds of death?

#MVM with CM

with_comorb <- subset(covid19A, Number_of_Comorbidities > 0)

MVMwithCM <- glm(COVID_Death ~ Diabetes + Hypertension + Obesity + Smoking,
                  data = with_comorb,
                  family = binomial)

summary(MVMwithCM)

    ######## To compare which model is more fitted (MVMwithCM or MVM)
    ######## AIC & ANOVA models could be run


     # which can be compared with a mortality baseline given by a LGM of 
     # only the people WITH OUT any CM

#Mortality baseline (Deaths WITH OUT CM)

no_comorb <- subset(covid19A, Number_of_Comorbidities == 0)

LRMnoCM <- glm(COVID_Death ~ 1,        
                data = no_comorb,
                family = binomial)

summary(LRMnoCM)

     # Another interesting question to analyse would be if the addition of 
     # multiple CM increases the odds of death, To answer this question we 
     # run a LRM considering Number_of_Comorbidities (given in our dataset)

                       # LRM Number of comorbidities

LRM_NCM <- glm(COVID_Death ~ Number_of_Comorbidities,
                 data = with_comorb, #again with the subset 
                 family = binomial)

summary(LRM_NCM)

     ## Now that we know that the risk of death increases as the Number of CM 
     ## increases. Identifying which combination of CM's is the most dangerous 
     ## would help us shape the profile for a high risk of death infected person
     ## given that usually CM's appear simultaneously 
   
  # To analyse that running Paired LRM (PLRM) and the compare. Would help us 
  # identify the most dangerous combinations

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

#Single models for Individual CM
models2 <- list(
  Hypertension = model_Hypertension,
  Obesity = model_Obesity,
  Smoking = model_Smoking,
  Diabetes = model_Diabetes
)

results2 <- lapply(names(models2), function(name) {
  tidy(models2[[name]], exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(Model = name)
})
combined_results2 <- bind_rows(results2)
combined_results2

#Tables of combined results
library(knitr)

combined_results_clean <- combined_results %>%
  filter(term != "(Intercept)") %>%
  select(-contains("stat", ignore.case = TRUE)) %>%
  as.data.frame()

names(combined_results_clean)
kable(combined_results_clean, digits = 3, caption = "Paired Logistic Regression Results")

combined_results2_clean <- combined_results2 %>%
  filter(term != "(Intercept)") %>%
  select(-contains("stat", ignore.case = TRUE)) %>%
  as.data.frame()

names(combined_results2_clean)
kable(combined_results2_clean, digits = 3, caption = "Single-Predictor Logistic Regression Results")

#AIC and BIC Analyses
models_paired <- list(
  Diabetes_Hypertension = model_Diabetes_Hypertension,
  Diabetes_Smoking      = model_Diabetes_Smoking,
  Diabetes_Obesity      = model_Diabetes_Obesity,
  Hypertension_Smoking  = model_Hypertension_Smoking,
  Obesity_Smoking       = model_Obesity_Smoking,
  Hypertension_Obesity  = model_Hypertension_Obesity
)

info_criteria_paired <- lapply(names(models_paired), function(name) {
  m <- models_paired[[name]]
  
  data.frame(
    Model = name,
    AIC  = AIC(m),
    BIC  = BIC(m)
  )
})

AIC_BIC_table_paired <- dplyr::bind_rows(info_criteria_paired)
AIC_BIC_table_paired

models2 <- list(
  Hypertension = model_Hypertension,
  Obesity = model_Obesity,
  Smoking = model_Smoking,
  Diabetes = model_Diabetes
)

info_criteria2 <- lapply(names(models2), function(name) {
  m <- models2[[name]]
  
  data.frame(
    Model = name,
    AIC  = AIC(m),
    BIC  = BIC(m)
  )
})

AIC_BIC_table2 <- dplyr::bind_rows(info_criteria2)
AIC_BIC_table2

knitr::kable(AIC_BIC_table2, caption = "AIC and BIC for Single-Predictor Models")
knitr::kable(AIC_BIC_table_paired, caption = "AIC and BIC for Paired Models")

#Graph Heatmap to show represent combined result table - 
library(ggplot2)

ls(combined_results)
head(combined_results)
combined_results %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = Model, y = term, fill = -log10(p.value))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Comorbidities Risk Factor Heatmap",
    x = "Model",
    y = "Risk Factor",
    fill = "-log10(p)"
  )

#Heatmap for Individual CM
ls(combined_results2)
head(combined_results2)
combined_results2 %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = Model, y = term, fill = -log10(p.value))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Individual Comorbidities Risk Factor Heatmap ",
    x = "Model",
    y = "Risk Factor",
    fill = "-log10(p)"
  )

#11/12/2025
###-------------AIV and GIV Analyses-------------------------
#library(dplyr)
#library(stringr)
#library(tidyr)

# Extract variables from model name
if (is.list(results) && !is.data.frame(results)) {
  results <- bind_rows(results)
}
model_vars <- results %>%
  distinct(Model) %>%
  mutate(vars = strsplit(Model, "_"))

#AIV (Average Incremental Value) = average of the absolute standardized coefficients of a variable across all models where it appears. Measures the average strength of each variable across all the model specifications where it appears. Higher AIV = stronger, more stable effect.
# Compute AIV: mean(|β| / SE) across models where variable appears
aiv <- results %>%
  filter(term != "(Intercept)") %>%
  mutate(standardized = abs(estimate / std.error)) %>%
  group_by(term) %>%
  summarise(AIV = mean(standardized)) %>%
  arrange(desc(AIV))

# Expand model × variable pairs
library(tidyr)
model_var_pairs <- model_vars %>%
  unnest(vars)%>%
  rename(term = vars)

#GIV (General Incremental Value) = sum of AIV contributions across all models in which the variable appears.Measures the overall importance of each variable across all models. Higher GIV = more robust predictive importance.
# Join AIV back onto model-variable pairs
giv <- model_var_pairs %>%
  left_join(aiv, by = "term") %>%
  group_by(term) %>%
  summarise(GIV = sum(AIV)) %>%
  arrange(desc(GIV))

#Interpreting results: AIV results tell me that if I add each variable to different models, hypertension, obesity, and diabetes each add a large amount of predictive value, while smoking adds somewhat less.Hypertension has the strongest individual effect across models. Its contribution to the outcome is consistently high, regardless of which other variables are included. GIV takes into consideration how strong the variable is (AIV) and how often and in what combinations it improves model performance. Across all models and variable combinations, hypertension is the most influential predictor, while smoking is consistently the least influential. Our AIV and GIV results are consistent and confirm that hypertension is the dominant predictor, followed by obesity and diabetes, with smoking having the smallest (yet still relevant) effect.

#AIV bar chart
library(ggplot2)
ggplot(aiv, aes(x = reorder(term, AIV), y = AIV)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Incremental Value (AIV) by Predictor",
    x = "Predictor",
    y = "AIV"
  ) +
  theme_minimal(base_size = 10)

#GIV bar chart 
ggplot(giv, aes(x = reorder(term, GIV), y = GIV)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "General Incremental Value (GIV) by Predictor",
    x = "Predictor",
    y = "GIV"
  ) +
  theme_minimal(base_size = 10)

###-------------ANOVA Analyses-------------------------
library(car)

Anova(model_Diabetes_Hypertension, type = "II", test.statistic = "LR")
Anova(model_Diabetes_Smoking, type = "II", test.statistic = "LR")
Anova(model_Diabetes_Obesity, type = "II", test.statistic = "LR")
Anova(model_Hypertension_Smoking, type = "II", test.statistic = "LR")
Anova(model_Obesity_Smoking, type = "II", test.statistic = "LR")
Anova(model_Hypertension_Obesity, type = "II", test.statistic = "LR")
Anova(model_Diabetes, type = "II", test.statistic = "LR")
Anova(model_Smoking, type = "II", test.statistic = "LR")
Anova(model_Obesity, type = "II", test.statistic = "LR")
Anova(model_Hypertension, type = "II", test.statistic = "LR")

#Interpret results - This is Pair wise analysis ANOVA. ANOVA test tells me if this predictor improves the model fit after adjusting for the other predictor. In our case the answer is yes with overwhelmeing evidence. Both Diabetes and Hypertension are highly significant independent predictors of COVID-19 death. Hypertension has a larger LR Chi-square, meaning it explains a bit more variation than diabetes (but both are extremely strong)
#The purpose of ANOVA (Analysis of Variance) is to determine if there are statistically significant differences between the means of three or more groups. Furthermore it tests if variations in a dependent variable are due to an independent variable or random chance.

#Comparisons of ANOVA
anova(model_Diabetes, model_Hypertension, test = "LRT")
anova(model_Diabetes, model_Smoking, test = "LRT")
anova(model_Diabetes, model_Obesity, test = "LRT")
anova(model_Hypertension, model_Smoking, test = "LRT")
anova(model_Obesity, model_Smoking, test = "LRT")
anova(model_Hypertension, model_Obesity, test = "LRT")
A_Diab_Hyp <- anova(model_Diabetes, model_Hypertension, test = "LRT")
A_Diab_Smok <- anova(model_Diabetes, model_Smoking, test = "LRT")
A_Diab_Obe <- anova(model_Diabetes, model_Obesity, test = "LRT")
A_Hyp_Smok <- anova(model_Hypertension, model_Smoking, test = "LRT")
A_Obe_Smok <- anova(model_Obesity, model_Smoking, test = "LRT")
A_Hyp_Obe <- anova(model_Hypertension, model_Obesity, test = "LRT")

anova_list <- list(
  "Diabetes vs Hypertension" = A_Diab_Hyp,
  "Diabetes vs Smoking"      = A_Diab_Smok,
  "Diabetes vs Obesity"      = A_Diab_Obe,
  "Hypertension vs Smoking"  = A_Hyp_Smok,
  "Obesity vs Smoking"       = A_Obe_Smok,
  "Hypertension vs Obesity"  = A_Hyp_Obe
)

res$'Pr(>Chi)'
summary.table <- do.call(rbind, lapply(names(anova_list), function(name) {
  res <- anova_list[[name]]
  data.frame(
    Comparison = name,
    Df        = res$Df[2],
    Deviance  = res$Deviance[2],
    Chisq     = res$Chisq[2],
    P_value   = res$Pr(>Chi)[2],  
    row.names = NULL
  )
}))

summary.table

  
 

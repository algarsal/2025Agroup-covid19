# LOGISTIC REGRESION MODEL (LRM)

LRM for each Comorbidity Individualy

```{r}
## load library 
library(broom)

# Indiv. LRM Diabetes
model_Diabetes <- glm(
  COVID_Death ~ Diabetes,
  data = covid19A,
  family = binomial()
)

# Indiv. LRM Smoking
model_Smoking <- glm(
  COVID_Death ~ Smoking,
  data = covid19A,
  family = binomial()
)

# Indiv. LRM  Obesity
model_Obesity <- glm(
  COVID_Death ~ Obesity,
  data = covid19A,
  family = binomial()
)

# Indiv. LRM  Hypertension
model_Hypertension <- glm(
  COVID_Death ~ Hypertension,
  data = covid19A,
  family = binomial()
)

#Results table

# List of your models
models_individual <- list(
  Diabetes = model_Diabetes,
  Smoking = model_Smoking,
  Obesity = model_Obesity,
  Hypertension = model_Hypertension
)

# Function to extract results
extract_results <- function(model) {
  
  tidy_res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  OR <- tidy_res$estimate[2]            # Odds Ratio
  p_val <- tidy_res$p.value[2]         # p-value
  
  # Predicted probabilities
  probs <- predict(model, type = "response")
  prob_0 <- mean(probs[model$model[,2] == 0])   # Probability of death if predictor = 0
  prob_1 <- mean(probs[model$model[,2] == 1])   # Probability of death if predictor = 1
  
  data.frame(
    Predictor = names(model$model)[2],
    Odds_Ratio = OR,
    P_value = p_val,
    Prob_Death_0 = prob_0,
    Prob_Death_1 = prob_1
  )
}

# Apply to all models
results_individual <- bind_rows(lapply(models_individual, extract_results))

results_individual

```

Another interesting question to analyse would be if the addition of multiple CM increases the odds of death, To answer this question we run a LRM considering Number_of_Comorbidities (given in our dataset)

#LRM Number of comorbidities

```{r}
LRM_NCM <- glm(COVID_Death ~ Number_of_Comorbidities,
               data = covid19A,  
               family = binomial)
LRM_NCM_results <- tidy(LRM_NCM) %>%
  filter(term != "(Intercept)") %>%       # remove intercept
  mutate(
    Odds_Ratio = exp(estimate),           # convert log-odds to OR
    P_value = p.value
  ) %>%
  select(
    Predictor = term,
    Estimate = estimate,
    Odds_Ratio,
    P_value
  )

LRM_NCM_results

```

####COMENTS on the results!!!!!!

#Another interesting question to analyse would be if the addition of #multiple CM increases the odds of death, To answer this question we #run a LRM considering Number_of_Comorbidities (given in our dataset)

#LRM Number of comorbidities

```{r}
LRM_NCM <- glm(COVID_Death ~ Number_of_Comorbidities,
               data = covid19A,  
               family = binomial)
LRM_NCM_results <- tidy(LRM_NCM) %>%
  filter(term != "(Intercept)") %>%       # remove intercept
  mutate(
    Odds_Ratio = exp(estimate),           # convert log-odds to OR
    P_value = p.value
  ) %>%
  select(
    Predictor = term,
    Estimate = estimate,
    Odds_Ratio,
    P_value
  )

LRM_NCM_results

```

####COMENTS on the results!!!

#However, running individual LRM is measuring the association of each #individual CM, while for isolating effect we might need to incorporate #all the CM analyzed in this study as a Multivariable Model (MVM)

# Multivariable Model (MVM)

```{r}
MVM <- glm(COVID_Death ~ Diabetes + Hypertension + Obesity + Smoking,
           data = covid19A,
           family = binomial)
MVM_results <- tidy(MVM) %>%
  filter(term != "(Intercept)") %>%       # remove intercept
  mutate(
    Odds_Ratio = exp(estimate),           # convert log-odds to OR
    P_value = p.value
  ) %>%
  select(
    Predictor = term,
    Estimate = estimate,
    Odds_Ratio,
    P_value
  )

MVM_results

```

####COMENTS on the results!!!!!!

Now that we know that the risk of death increases as the Number of CM #increases.
Identifying which combination of CM's is the most dangerous #would help us shape the profile for a high risk of death infected person #given that usually CM's appear simultaneously

To analyse that running Paired LRM (PLRM) and the compare.
Would help us #identify the most dangerous combinations

# Results Table
models_pairs <- list( Diabetes_Hypertension = model_Diabetes_Hypertension, Diabetes_Smoking = model_Diabetes_Smoking, Diabetes_Obesity = model_Diabetes_Obesity, Hypertension_Smoking = model_Hypertension_Smoking, Obesity_Smoking = model_Obesity_Smoking, Hypertension_Obesity = model_Hypertension_Obesity )

extract_pair_results <- function(model, model_name) { tidy(model) %\>% filter(term != "(Intercept)") %\>% \
  # remove intercept mutate( Model = model_name, Odds_Ratio = exp(estimate), P_value = p.value ) %\>% select( Model, Predictor = term, Estimate = estimate, Odds_Ratio, P_value ) }
  
  pairwise_results <- bind_rows( lapply(names(models_pairs), function(name) { extract_pair_results(models_pairs[[name]], name) }) )
  
  #Heat map
  
  # Pairwise results table
  
  pairwise_results <- bind_rows( lapply(names(models_pairs), function(name) { extract_pair_results(models_pairs[[name]], name) }) )
  
  # Heatmap data
  
  heatmap_PAIRWISE <- pairwise_results %\>% mutate(Odds_Ratio = round(Odds_Ratio, 2)) %\>% select(Model, Predictor, Odds_Ratio)
  
  library(ggplot2)
  
  ggplot(heatmap_PAIRWISE, aes(x = Model, y = Predictor, fill = Odds_Ratio)) + geom_tile(color = "white") + scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") + geom_text(aes(label = Odds_Ratio), color = "black", size = 4) + theme_minimal() + theme( axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank() ) + ggtitle("Heatmap of Odds Ratios for Pairwise Logistic Regression Models")

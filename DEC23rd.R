
### Table for chi test

```{r}
#| label: tbl-chi-cell11
#| tbl-cap: "Chi-square Test for Each Comorbidity"

library(dplyr)
library(purrr)
library(knitr)
library(kableExtra)

comorbidities <- c("Diabetes", "Hypertension", "Obesity", "Smoking")

cell11_table <- map_df(comorbidities, function(cmb) {
  
  tab <- table(covid19A$COVID_Death, covid19A[[cmb]])
  chi <- chisq.test(tab)
  
  obs_11 <- unname(tab["1", "1"])
  exp_11 <- unname(chi$expected["1", "1"])
  
  data.frame(
    Comorbidity = cmb,
    Observed = as.numeric(obs_11),
    Expected = as.numeric(exp_11),
    P_value_num = chi$p.value
  )
}) %>%
  arrange(P_value_num) %>%
  mutate(
    Expected = round(Expected, 2),
    Signif = case_when(
      P_value_num < 0.001 ~ "***",
      P_value_num < 0.01  ~ "**",
      P_value_num < 0.05  ~ "*",
      TRUE ~ ""
    ),
    P_value = paste0(
      formatC(P_value_num, format = "e", digits = 3),
      " ",
      Signif
    )
  ) %>%
  select(Comorbidity, Observed, Expected, P_value)

cell11_table %>%
  kable(
    align = "c",
    col.names = c("Comorbidity", "Observed", "Expected", "P_value")
  ) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover")
  ) %>%
  footnote(
    general = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05.",
    general_title = ""
  )
```

### Graph for Chi- test

```{r}
#| label: fig-chi-observed-expected
#| fig-cap: "Observed vs Expected COVID-19 deaths by comorbidity. Error bars represent ±√Expected under the null hypothesis of independence."
#| fig-align: center

library(ggplot2)
library(dplyr)
library(tidyr)

plot_data <- cell11_table %>%
  mutate(
    Expected = as.numeric(Expected),
    Observed = as.numeric(Observed),
    SE_Expected = sqrt(Expected)
  ) %>%
  pivot_longer(
    cols = c(Observed, Expected),
    names_to = "Type",
    values_to = "Count"
  )

ggplot(plot_data, aes(x = Comorbidity, y = Count, fill = Type)) +
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  geom_errorbar(
    data = plot_data %>% filter(Type == "Expected"),
    aes(
      ymin = Count - SE_Expected,
      ymax = Count + SE_Expected
    ),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    x = "Comorbidity",
    y = "Number of deaths (COVID_Death = 1)"
  ) +
  theme_minimal()
```

#Error bars are shown only for expected counts and represent the uncertainty 
#under the null hypothesis; in all cases, observed deaths lie well above this 
#range, supporting the chi-square results.

##Indivudal Regression Models 

```{r}
#| label: tbl-indiv-lrm
#| tbl-cap: "Individual logistic regression models predicting COVID-19 mortality"

library(broom)
library(dplyr)
library(knitr)
library(kableExtra)


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
model <- list(
  Diabetes = model_Diabetes,
  Smoking = model_Smoking,
  Obesity = model_Obesity,
  Hypertension = model_Hypertension
)

##Results table

extract_results <- function(model) {
  tidy_res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  OR    <- tidy_res$estimate[2]
  p_val <- tidy_res$p.value[2]
  
  probs <- predict(model, type = "response")
  pred  <- model$model[, 2]
  
  prob_0 <- mean(probs[pred == 0])
  prob_1 <- mean(probs[pred == 1])
  
  data.frame(
    Predictor      = names(model$model)[2],
    Prob_Death_0   = prob_0 * 100,
    Prob_Death_1   = prob_1 * 100,
    Odds_Ratio     = OR,
    P_value_num    = p_val
  )
}

results_individual <- bind_rows(
  lapply(models_individual, extract_results)
) %>%
  arrange(desc(Odds_Ratio)) %>%
  mutate(
    Signif = case_when(
      P_value_num < 0.001 ~ "***",
      P_value_num < 0.01  ~ "**",
      P_value_num < 0.05  ~ "*",
      TRUE ~ ""
    ),
    `Prob_Death_0 (%)` = round(Prob_Death_0, 2),
    `Prob_Death_1 (%)` = round(Prob_Death_1, 2),
    `Odds Ratio`       = round(Odds_Ratio, 2),
    `P value`          = paste0(
      formatC(P_value_num, format = "e", digits = 3),
      " ",
      Signif
    )
  ) %>%
  select(
    Predictor,
    `Prob_Death_0 (%)`,
    `Prob_Death_1 (%)`,
    `Odds Ratio`,
    `P value`
  )

results_individual %>%
  kable(align = "c") %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover")
  ) %>%
  footnote(
    general = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05.",
    general_title = ""
  )
```

## Graphs Individual Logistic Regression Models
```{r}
#| label: fig-logit-curves-individual
#| fig-cap: "Individual logistic regression models shown on the log-odds (logit) scale for predictor = 0 and predictor = 1. The slope equals the model coefficient (β1 = log(OR))."
#| fig-align: center

library(dplyr)
library(ggplot2)

models_individual <- list(
  Diabetes = model_Diabetes,
  Smoking = model_Smoking,
  Obesity = model_Obesity,
  Hypertension = model_Hypertension
)

logit_data <- bind_rows(
  lapply(names(models_individual), function(nm) {
    m  <- models_individual[[nm]]
    b0 <- coef(m)[1]
    b1 <- coef(m)[2]
    
    data.frame(
      Model = nm,
      Predictor = c(0, 1),
      Logit = c(b0, b0 + b1),
      Slope = b1
    )
  })
)

slope_labels <- logit_data %>%
  group_by(Model) %>%
  summarise(
    Predictor = 0.75,
    Logit = mean(Logit),
    SlopeLabel = paste0("Slope = ", round(first(Slope), 3)),
    .groups = "drop"
  )

ggplot(logit_data, aes(x = Predictor, y = Logit, group = Model)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  geom_text(
    data = slope_labels,
    aes(x = Predictor, y = Logit, label = SlopeLabel),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  facet_wrap(~ Model) +
  scale_x_continuous(breaks = c(0, 1)) +
  labs(
    x = "Predictor (0 = No, 1 = Yes)",
    y = "Log-odds of COVID-19 death"
  ) +
  theme_minimal()

##Number of Comorbidities LRM 

##Analysis
LRM_NCM <- glm(COVID_Death ~ Number_of_Comorbidities, data = covid19A, family = binomial())

#Results table

tid <- tidy(LRM_NCM)

b0 <- coef(LRM_NCM)[1]
b1 <- coef(LRM_NCM)[2]

# Predicted probabilities at N=0 and N=1 (for consistency with binary tables)
p0 <- plogis(b0 + b1 * 0)
p1 <- plogis(b0 + b1 * 1)

# OR per +1 comorbidity and p-value for the slope term
OR <- exp(b1)
p_val <- tid$p.value[tid$term == "Number_of_Comorbidities"]

# Significance stars
sig <- dplyr::case_when(
  p_val < 0.001 ~ "***",
  p_val < 0.01  ~ "**",
  p_val < 0.05  ~ "*",
  TRUE ~ ""
)
LRM_NCM_table <- data.frame(
  Predictor = "Number_of_Comorbidities",
  `Prob_Death_0 (%)` = round(100 * p0, 2),
  `Odds Ratio` = round(OR, 2),
  `P value` = paste0(formatC(p_val, format = "e", digits = 3), " ", sig)
)

LRM_NCM_table %>%
  kable(align = "c") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  footnote(
    general = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05.",
    general_title = ""
  )

##Graph

n_min <- min(covid19A$Number_of_Comorbidities, na.rm = TRUE)
n_max <- max(covid19A$Number_of_Comorbidities, na.rm = TRUE)

grid <- data.frame(
  Number_of_Comorbidities = seq(n_min, n_max, by = 0.1)
) %>%
  mutate(Predicted_Prob = predict(LRM_NCM, newdata = ., type = "response"))

int_points <- data.frame(
  Number_of_Comorbidities = seq(ceiling(n_min), floor(n_max), by = 1)
) %>%
  mutate(
    Predicted_Prob = predict(LRM_NCM, newdata = ., type = "response"),
    Label = paste0(round(100 * Predicted_Prob, 1), "%")
  )

ggplot(grid, aes(x = Number_of_Comorbidities, y = Predicted_Prob)) +
  geom_line(linewidth = 1) +
  geom_point(data = int_points, size = 2) +
  geom_text(
    data = int_points,
    aes(label = Label),
    vjust = -0.8,
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Number of comorbidities",
    y = "Predicted probability of death"
  ) +
  theme_minimal()
´´´

##MVLRM

```{r}

#| label: fig-mvm-or
#| fig-cap: "Multivariable model (MVM): adjusted odds ratios (95% CI) for each comorbidity."
#| fig-align: center


mvm_or <- tidy(MVM, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = factor(term, levels = c("Diabetes", "Hypertension", "Obesity", "Smoking")),
    p_sci = formatC(p.value, format = "e", digits = 3),
    signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    label = paste0(
      "OR = ", round(estimate, 2),
      "\nP = ", p_sci, " ", signif
    ),
    # label placed safely to the right of CI
    x_label = conf.high * 1.15
  )

ggplot(mvm_or, aes(x = estimate, y = term)) +
  # CI first
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.25,
    linewidth = 0.6
  ) +
  # Points on top (never disappear)
  geom_point(size = 3, shape = 21, fill = "black") +
  # Labels
  geom_text(
    aes(x = x_label, label = label),
    hjust = 0,
    size = 3.2,
    lineheight = 0.95
  ) +
  # OR = 1 reference
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.35))   # ← space for labels, keeps OR=1 visible
  ) +
  scale_y_discrete(expand = expansion(add = 0.6)) +
  labs(
    x = "Adjusted Odds Ratio (95% CI)",
    y = "Comorbidity",
    caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.line = element_line(linewidth = 0.6, color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 100, b = 10, l = 10),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )
````

## Paired models (Interactions)
```{r}

model_Dia_Hyp_int <- glm(
  COVID_Death ~ Diabetes * Hypertension + Obesity + Smoking,
  data = covid19A,
  family = binomial()
)
model_Dia_Obe_int <- glm(
  COVID_Death ~ Diabetes * Obesity + Hypertension + Smoking,
  data = covid19A,
  family = binomial()
)

model_Dia_Smo_int <- glm(
  COVID_Death ~ Diabetes * Smoking + Hypertension + Obesity,
  data = covid19A,
  family = binomial()
)

model_Hyp_Obe_int <- glm(
  COVID_Death ~ Hypertension * Obesity + Diabetes + Smoking,
  data = covid19A,
  family = binomial()
)

model_Hyp_Smo_int <- glm(
  COVID_Death ~ Hypertension * Smoking + Diabetes + Obesity,
  data = covid19A,
  family = binomial()
)

model_Obe_Smo_int <- glm(
  COVID_Death ~ Obesity * Smoking + Diabetes + Hypertension,
  data = covid19A,
  family = binomial()
)

#| label: tbl-interactions
#| tbl-cap: "Interaction effects between comorbidities from multivariable logistic regression models."

# List of interaction models
interaction_models <- list(
  "Diabetes × Hypertension" = model_Dia_Hyp_int,
  "Diabetes × Obesity"      = model_Dia_Obe_int,
  "Diabetes × Smoking"     = model_Dia_Smo_int,
  "Hypertension × Obesity" = model_Hyp_Obe_int,
  "Hypertension × Smoking" = model_Hyp_Smo_int,
  "Obesity × Smoking"      = model_Obe_Smo_int
)
# Extract ONLY interaction terms
interaction_results <- bind_rows(
  lapply(names(interaction_models), function(name) {
    m <- interaction_models[[name]]
    
    tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(grepl(":", term)) %>%
      mutate(
        Comorbidities = name,
        `Odd Ratio`   = estimate,
        P_value_num   = p.value,
        Signif = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          TRUE ~ ""
        ),
        `P value` = paste0(
          formatC(p.value, format = "e", digits = 3),
          " ",
          Signif
        )
      ) %>%
      select(
        Comorbidities,
        `Odd Ratio`,
        `P value`
      )
  })
) %>%
  arrange(desc(`Odd Ratio`))

# Display table
interaction_results %>%
  kable(align = "c") %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover")
  ) %>%
  footnote(
    general = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05.",
    general_title = ""
  )
```


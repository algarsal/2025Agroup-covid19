
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

##Table for ILR






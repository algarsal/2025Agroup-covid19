
### Table for chi test

```{r}
#| label: tbl-chi-cell11
#| tbl-cap: "Observed and expected counts for COVID-19 deaths among patients with each comorbidity (COVID_Death = 1)"

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
    P_value = chi$p.value
  )
}) %>%
  arrange(P_value) %>%
  mutate(
    Expected = round(Expected, 2),
    P_value = formatC(P_value, format = "e", digits = 3)
  )

cell11_table %>%
  kable(
    align = "c",
    col.names = c("Comorbidity", "Observed", "Expected", "P_value")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
´´´

##Table for ILR




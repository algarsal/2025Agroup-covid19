## ----------------Statistical Analysis-----------------------------
## ---------------(Code from the start)----------------------------

##----------------- Load the data base---------------------------

dir("data")
data <- "data/covid_mexico_comorbidity_dataset_100k.csv"
covid19A <- read.table(data, header = T, sep = ",")
str(covid19A)
summary(covid19A)

library(dplyr) ###

### ------------------Chi square Test--------------------------

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

#  All tests resulted in highly significant p-values (p < 2.2e-16), 
#  indicating that the distribution of deaths differs significantly 
#  between patients with and without the comorbidity.

##  This supports the conclusion that comorbidities are strongly 
##  associated with increased mortality.

# Now that we found statistically significant association between each 
# and every comorbidity 

###-------------LOGISTIC REGRESION MODEL-------------------------


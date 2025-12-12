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

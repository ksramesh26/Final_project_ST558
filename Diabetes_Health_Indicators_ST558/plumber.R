library(tidyverse)
library(tidymodels)
library(plumber)
library(ranger)

# Load and format data
data_raw <- read_csv("../diabetes_binary_health_indicators_BRFSS2015.csv")

diabetes_data <- data_raw %>%
  mutate(
    Diabetes_binary = factor(Diabetes_binary, levels = c(0, 1), labels = c("No Diabetes", "Diabetes")),
    PhysActivity = factor(PhysActivity, levels = c(0, 1), labels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c(0, 1), labels = c("No", "Yes")),
    HighBP = factor(HighBP, levels = c(0, 1), labels = c("No", "Yes")),
    AgeGroup = factor(Age, levels = 1:13, labels = c(
      "18–24", "25–29", "30–34", "35–39", "40–44",
      "45–49", "50–54", "55–59", "60–64", "65–69",
      "70–74", "75–79", "80+"))
  )

# Final model and recipe
diabetes_rec3 <- recipe(Diabetes_binary ~ BMI + AgeGroup + PhysActivity + HighChol + HighBP, data = diabetes_data) %>%
  step_normalize(all_numeric(), -Diabetes_binary)

rf_spec = rand_forest(mtry = 2) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

final_model = workflow() %>% 
  add_recipe(diabetes_rec3) %>%
  add_model(rf_spec) %>%
  fit(diabetes_data)

#* Predict diabetes probability
#* @post /pred
#* @param BMI:double
#* @param AgeGroup:string
#* @param PhysActivity:string
#* @param HighChol:string
#* @param HighBP:string
predict_diabetes <- function(BMI = 27.5,
                             AgeGroup = "60–64",
                             PhysActivity = "Yes",
                             HighChol = "No",
                             HighBP = "No") {
  
  input_data <- tibble(
    BMI = as.numeric(BMI),
    AgeGroup = factor(AgeGroup, levels = c(
      "18–24", "25–29", "30–34", "35–39", "40–44",
      "45–49", "50–54", "55–59", "60–64", "65–69",
      "70–74", "75–79", "80+")),
    PhysActivity = factor(PhysActivity, levels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c("No", "Yes")),
    HighBP = factor(HighBP, levels = c("No", "Yes"))
  )
  
  predict(final_model, new_data = input_data, type = "prob") %>%
    bind_cols(predict(final_model, new_data = input_data)) %>%
    rename(Probability = .pred_Diabetes, Prediction = .pred_class)
}


#* Model info
#* @get /info
get_info <- function() {
  list(
    name = "Keshav Ramesh",
    github_pages = "https://ksramesh26.github.io/Final_project_ST558/EDA.html"
  )
}


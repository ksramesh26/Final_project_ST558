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

rf_spec = rand_forest(mtry = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification") %>%
  fit(diabetes_data)

final_model = workflow() %>% 
  add_recipe(diabetes_rec3) %>%
  add_model(rf_spec)

final_model <- workflow() %>%
  add_recipe(diabetes_rec3) %>%
  add_model(rand_forest(mtry = 2) %>% set_engine("ranger") %>% set_mode("classification")) %>%
  fit(diabetes_data)


